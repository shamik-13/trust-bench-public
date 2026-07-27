package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-11-19  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundCancellationApplyService {

    private static final String DECISION_ACCEPTED = "A";
    private static final String DECISION_DECLINED = "D";
    private static final String DECLINE_LEDGER_POSTED = "RVS";
    private static final String DEST_MERCHANT = "M";
    private static final String TEMPLATE_MERCHANT_CANCEL = "PR-CANCEL-01";
    private static final String SEND_STATUS_WAITING = "0";

    public static void main(String[] a) {
        java.util.List<CancelRecord> prcanf = java.util.Arrays.asList(
                new CancelRecord("CAN000001", "REQ000001", "20", "20260628", "OP1001"),
                new CancelRecord("CAN000002", "REQ000002", "10", "20260628", "OP1002"),
                new CancelRecord("CAN000003", "REQ000003", "30", "20260628", "OP1001")
        );

        java.util.Map<String, BalanceRecord> prbalf = new java.util.LinkedHashMap<>();
        prbalf.put("WLT000001", new BalanceRecord("WLT000001", 125000L, 15000L, "20260627"));
        prbalf.put("WLT000002", new BalanceRecord("WLT000002", 84000L, 30000L, "20260627"));
        prbalf.put("WLT000003", new BalanceRecord("WLT000003", 42000L, 5000L, "20260627"));

        java.util.List<ResponseRecord> prrspf = java.util.Arrays.asList(
                new ResponseRecord("REQ000001", "TXN900001", DECISION_ACCEPTED, "", 15000L, "WLT000001", false),
                new ResponseRecord("REQ000002", "TXN900002", DECISION_ACCEPTED, "", 30000L, "WLT000002", true),
                new ResponseRecord("REQ000003", "TXN900003", DECISION_DECLINED, "TXN", 5000L, "WLT000003", false)
        );

        ApplyResult result = new RefundCancellationApplyService().apply(prcanf, prbalf, prrspf, "20260628");

        for (BalanceRecord balance : result.updatedBalances.values()) {
            System.out.println("PRBALF更新 " + balance.walletId + " 利用可能残高=" + balance.availableBalance
                    + " 保留返金額=" + balance.pendingRefundAmount + " 最終調整日=" + balance.lastAdjustedDate);
        }
        for (NoticeRecord notice : result.notices) {
            System.out.println("PRNTF作成 " + notice.noticeId + " 依頼ID=" + notice.requestId
                    + " 宛先=" + notice.destinationCategory + " 雛形=" + notice.templateId);
        }
        for (ExceptionRecord exception : result.exceptions) {
            System.out.println("例外設定 " + exception.cancelId + " 依頼ID=" + exception.requestId
                    + " 理由=" + exception.reasonCode + " 内容=" + exception.message);
        }
    }

    public ApplyResult apply(java.util.List<CancelRecord> cancellations,
                             java.util.Map<String, BalanceRecord> balances,
                             java.util.List<ResponseRecord> responses,
                             String processDate) {
        requireProcessDate(processDate);

        java.util.Map<String, ResponseRecord> responseByRequest = new java.util.HashMap<>();
        for (ResponseRecord response : responses) {
            if (responseByRequest.put(response.requestId, response) != null) {
                throw new IllegalArgumentException("応答ファイルに依頼ID重複があります: " + response.requestId);
            }
        }

        java.util.Map<String, BalanceRecord> updatedBalances = new java.util.LinkedHashMap<>(balances);
        java.util.List<NoticeRecord> notices = new java.util.ArrayList<>();
        java.util.List<ExceptionRecord> exceptions = new java.util.ArrayList<>();
        java.util.Set<String> processedCancelIds = new java.util.HashSet<>();

        int noticeSeq = 1;
        for (CancelRecord cancel : cancellations) {
            validateCancel(cancel);
            if (!processedCancelIds.add(cancel.cancelId)) {
                exceptions.add(new ExceptionRecord(cancel.cancelId, cancel.requestId, "DUP", "取消ID重複"));
                continue;
            }

            ResponseRecord response = responseByRequest.get(cancel.requestId);
            if (response == null) {
                exceptions.add(new ExceptionRecord(cancel.cancelId, cancel.requestId, "NORSP", "返金判定応答なし"));
                continue;
            }
            validateResponse(response);

            if (DECISION_DECLINED.equals(response.decisionCategory)) {
                exceptions.add(new ExceptionRecord(cancel.cancelId, cancel.requestId,
                        response.declineReason, "否認済返金の取消候補"));
                continue;
            }

            BalanceRecord balance = updatedBalances.get(response.walletId);
            if (balance == null) {
                exceptions.add(new ExceptionRecord(cancel.cancelId, cancel.requestId, "NOWAL", "残高レコードなし"));
                continue;
            }

            if (response.ledgerPosted) {
                exceptions.add(new ExceptionRecord(cancel.cancelId, cancel.requestId,
                        DECLINE_LEDGER_POSTED, "台帳反映済みのため逆仕訳依頼対象"));
                continue;
            }

            if (balance.pendingRefundAmount < response.eligibleAmount) {
                exceptions.add(new ExceptionRecord(cancel.cancelId, cancel.requestId, "PBAL", "保留返金額不足"));
                continue;
            }

            BalanceRecord adjusted = new BalanceRecord(
                    balance.walletId,
                    balance.availableBalance + response.eligibleAmount,
                    balance.pendingRefundAmount - response.eligibleAmount,
                    processDate
            );
            updatedBalances.put(adjusted.walletId, adjusted);

            if ("20".equals(cancel.cancelReason) || "30".equals(cancel.cancelReason)) {
                notices.add(new NoticeRecord(
                        "NTC" + processDate + String.format("%06d", noticeSeq++),
                        cancel.requestId,
                        DEST_MERCHANT,
                        TEMPLATE_MERCHANT_CANCEL,
                        SEND_STATUS_WAITING,
                        ""
                ));
            }
        }

        return new ApplyResult(updatedBalances, notices, exceptions);
    }

    private static void validateCancel(CancelRecord cancel) {
        requireNonBlank(cancel.cancelId, "取消ID");
        requireNonBlank(cancel.requestId, "依頼ID");
        requireNonBlank(cancel.cancelDate, "取消日");
        requireNonBlank(cancel.operatorId, "担当者ID");
        if (!"10".equals(cancel.cancelReason) && !"20".equals(cancel.cancelReason) && !"30".equals(cancel.cancelReason)) {
            throw new IllegalArgumentException("取消理由不正: " + cancel.cancelReason);
        }
    }

    private static void validateResponse(ResponseRecord response) {
        requireNonBlank(response.requestId, "依頼ID");
        requireNonBlank(response.originalTransactionId, "原取引ID");
        if (!DECISION_ACCEPTED.equals(response.decisionCategory) && !DECISION_DECLINED.equals(response.decisionCategory)) {
            throw new IllegalArgumentException("判定区分不正: " + response.decisionCategory);
        }
        if (DECISION_DECLINED.equals(response.decisionCategory)
                && !"WIN".equals(response.declineReason)
                && !"AMT".equals(response.declineReason)
                && !"TXN".equals(response.declineReason)) {
            throw new IllegalArgumentException("否認理由不正: " + response.declineReason);
        }
        if (response.eligibleAmount < 0) {
            throw new IllegalArgumentException("返金可能額不正: " + response.eligibleAmount);
        }
    }

    private static void requireProcessDate(String processDate) {
        requireNonBlank(processDate, "処理日");
        if (!processDate.matches("\\d{8}")) {
            throw new IllegalArgumentException("処理日不正: " + processDate);
        }
    }

    private static void requireNonBlank(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "未設定");
        }
    }

    public static final class CancelRecord {
        public final String cancelId;
        public final String requestId;
        public final String cancelReason;
        public final String cancelDate;
        public final String operatorId;

        public CancelRecord(String cancelId, String requestId, String cancelReason, String cancelDate, String operatorId) {
            this.cancelId = cancelId;
            this.requestId = requestId;
            this.cancelReason = cancelReason;
            this.cancelDate = cancelDate;
            this.operatorId = operatorId;
        }
    }

    public static final class BalanceRecord {
        public final String walletId;
        public final long availableBalance;
        public final long pendingRefundAmount;
        public final String lastAdjustedDate;

        public BalanceRecord(String walletId, long availableBalance, long pendingRefundAmount, String lastAdjustedDate) {
            this.walletId = walletId;
            this.availableBalance = availableBalance;
            this.pendingRefundAmount = pendingRefundAmount;
            this.lastAdjustedDate = lastAdjustedDate;
        }
    }

    public static final class ResponseRecord {
        public final String requestId;
        public final String originalTransactionId;
        public final String decisionCategory;
        public final String declineReason;
        public final long eligibleAmount;
        public final String walletId;
        public final boolean ledgerPosted;

        public ResponseRecord(String requestId, String originalTransactionId, String decisionCategory,
                              String declineReason, long eligibleAmount, String walletId, boolean ledgerPosted) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.decisionCategory = decisionCategory;
            this.declineReason = declineReason;
            this.eligibleAmount = eligibleAmount;
            this.walletId = walletId;
            this.ledgerPosted = ledgerPosted;
        }
    }

    public static final class NoticeRecord {
        public final String noticeId;
        public final String requestId;
        public final String destinationCategory;
        public final String templateId;
        public final String sendStatus;
        public final String sendDate;

        public NoticeRecord(String noticeId, String requestId, String destinationCategory,
                            String templateId, String sendStatus, String sendDate) {
            this.noticeId = noticeId;
            this.requestId = requestId;
            this.destinationCategory = destinationCategory;
            this.templateId = templateId;
            this.sendStatus = sendStatus;
            this.sendDate = sendDate;
        }
    }

    public static final class ExceptionRecord {
        public final String cancelId;
        public final String requestId;
        public final String reasonCode;
        public final String message;

        public ExceptionRecord(String cancelId, String requestId, String reasonCode, String message) {
            this.cancelId = cancelId;
            this.requestId = requestId;
            this.reasonCode = reasonCode;
            this.message = message;
        }
    }

    public static final class ApplyResult {
        public final java.util.Map<String, BalanceRecord> updatedBalances;
        public final java.util.List<NoticeRecord> notices;
        public final java.util.List<ExceptionRecord> exceptions;

        public ApplyResult(java.util.Map<String, BalanceRecord> updatedBalances,
                           java.util.List<NoticeRecord> notices,
                           java.util.List<ExceptionRecord> exceptions) {
            this.updatedBalances = java.util.Collections.unmodifiableMap(new java.util.LinkedHashMap<>(updatedBalances));
            this.notices = java.util.Collections.unmodifiableList(new java.util.ArrayList<>(notices));
            this.exceptions = java.util.Collections.unmodifiableList(new java.util.ArrayList<>(exceptions));
        }
    }
}
