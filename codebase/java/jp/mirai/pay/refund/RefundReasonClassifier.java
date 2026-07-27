package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.0   2025-02-25  みらいペイ システム部 返金・チャージバックチーム  返金理由分類サービスの初版作成
 */
public class RefundReasonClassifier {
    private static final String GROUP_CUSTOMER = "顧客申告";
    private static final String GROUP_MERCHANT = "加盟店起因";
    private static final String GROUP_SYSTEM = "システム起因";
    private static final String GROUP_BANK = "銀行連携";
    private static final String GROUP_FRAUD = "不正懸念";
    private static final String GROUP_INVESTIGATION = "要調査";

    private static final ReasonRow[] PRRSNF = {
            new ReasonRow("REQ-DUP-001", GROUP_CUSTOMER, 4, false),
            new ReasonRow("REQ-CAN-002", GROUP_CUSTOMER, 3, false),
            new ReasonRow("REQ-RET-003", GROUP_CUSTOMER, 5, false),
            new ReasonRow("REQ-PRICE-004", GROUP_MERCHANT, 6, true),
            new ReasonRow("REQ-NOSHIP-005", GROUP_MERCHANT, 7, true),
            new ReasonRow("REQ-SYS-006", GROUP_SYSTEM, 5, false),
            new ReasonRow("REQ-TIMEOUT-007", GROUP_SYSTEM, 6, true),
            new ReasonRow("REQ-BANK-008", GROUP_BANK, 7, true),
            new ReasonRow("REQ-ACCT-009", GROUP_BANK, 8, true),
            new ReasonRow("REQ-FRD-010", GROUP_FRAUD, 10, true)
    };

    private static final ReasonRow UNKNOWN_REASON =
            new ReasonRow("REQ-UNK-000", GROUP_INVESTIGATION, 9, true);

    public static void main(String[] a) {
        String[] requests = a == null || a.length == 0 ? sampleRequests() : a;
        ClassificationSummary summary = classifyBatch(requests);

        System.out.println("返金理由分類サービス");
        System.out.println("処理件数=" + summary.totalCount);
        System.out.println("未知件数=" + summary.unknownCount);
        System.out.println("自動審査件数=" + summary.autoReviewCount);
        System.out.println("平均リスク=" + formatAverage(summary.totalRiskWeight, summary.totalCount));
        System.out.println("最高優先度=" + summary.highestQueuePriority);
        System.out.println("通知要否=" + (summary.notificationRequired ? "要" : "不要"));

        for (GroupCount count : summary.groupCounts) {
            if (count.count > 0) {
                System.out.println("理由グループ=" + count.group + ", 件数=" + count.count + ", リスク合計=" + count.riskWeightTotal);
            }
        }
    }

    private static ClassificationSummary classifyBatch(String[] reasonCodes) {
        ClassificationSummary summary = new ClassificationSummary(groupBuckets());

        for (String reasonCode : reasonCodes) {
            ReasonRow row = findReason(reasonCode);
            boolean unknown = row == UNKNOWN_REASON;
            int priority = queuePriority(row, unknown);

            summary.totalCount++;
            summary.totalRiskWeight += row.riskWeight;
            summary.highestQueuePriority = Math.max(summary.highestQueuePriority, priority);

            if (unknown) {
                summary.unknownCount++;
            }
            if (row.autoReview) {
                summary.autoReviewCount++;
            }
            if (row.riskWeight >= 8 || unknown) {
                summary.notificationRequired = true;
            }

            addGroup(summary.groupCounts, row.reasonGroup, row.riskWeight);
        }

        return summary;
    }

    private static ReasonRow findReason(String reasonCode) {
        if (reasonCode == null) {
            return UNKNOWN_REASON;
        }

        String normalized = reasonCode.trim();
        if (normalized.isEmpty() || normalized.length() > 16) {
            return UNKNOWN_REASON;
        }

        for (ReasonRow row : PRRSNF) {
            if (row.reasonCode.equals(normalized)) {
                return row;
            }
        }
        return UNKNOWN_REASON;
    }

    private static int queuePriority(ReasonRow row, boolean unknown) {
        int priority = row.riskWeight;
        if (row.autoReview) {
            priority += 2;
        }
        if (unknown) {
            priority += 1;
        }
        return Math.min(priority, 12);
    }

    private static GroupCount[] groupBuckets() {
        return new GroupCount[] {
                new GroupCount(GROUP_CUSTOMER),
                new GroupCount(GROUP_MERCHANT),
                new GroupCount(GROUP_SYSTEM),
                new GroupCount(GROUP_BANK),
                new GroupCount(GROUP_FRAUD),
                new GroupCount(GROUP_INVESTIGATION)
        };
    }

    private static void addGroup(GroupCount[] counts, String group, int riskWeight) {
        for (GroupCount count : counts) {
            if (count.group.equals(group)) {
                count.count++;
                count.riskWeightTotal += riskWeight;
                return;
            }
        }
    }

    private static String formatAverage(int totalRiskWeight, int totalCount) {
        if (totalCount == 0) {
            return "0.0";
        }
        double average = (double) totalRiskWeight / totalCount;
        return String.format(java.util.Locale.ROOT, "%.1f", average);
    }

    private static String[] sampleRequests() {
        return new String[] {
                "REQ-DUP-001",
                "REQ-NOSHIP-005",
                "REQ-BANK-008",
                "REQ-OLD-991",
                "REQ-FRD-010",
                "REQ-SYS-006",
                " ",
                "REQ-PRICE-004"
        };
    }

    private static final class ReasonRow {
        private final String reasonCode;
        private final String reasonGroup;
        private final int riskWeight;
        private final boolean autoReview;

        private ReasonRow(String reasonCode, String reasonGroup, int riskWeight, boolean autoReview) {
            if (reasonCode == null || reasonCode.trim().isEmpty()) {
                throw new IllegalArgumentException("理由コード未設定");
            }
            if (reasonGroup == null || reasonGroup.trim().isEmpty()) {
                throw new IllegalArgumentException("理由グループ未設定");
            }
            if (riskWeight < 1 || riskWeight > 10) {
                throw new IllegalArgumentException("リスク重み範囲外");
            }
            this.reasonCode = reasonCode;
            this.reasonGroup = reasonGroup;
            this.riskWeight = riskWeight;
            this.autoReview = autoReview;
        }
    }

    private static final class GroupCount {
        private final String group;
        private int count;
        private int riskWeightTotal;

        private GroupCount(String group) {
            this.group = group;
        }
    }

    private static final class ClassificationSummary {
        private final GroupCount[] groupCounts;
        private int totalCount;
        private int unknownCount;
        private int autoReviewCount;
        private int totalRiskWeight;
        private int highestQueuePriority;
        private boolean notificationRequired;

        private ClassificationSummary(GroupCount[] groupCounts) {
            this.groupCounts = groupCounts;
        }
    }
}
