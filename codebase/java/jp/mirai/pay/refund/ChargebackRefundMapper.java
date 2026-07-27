package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.0   2024-05-27  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class ChargebackRefundMapper {
    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";

    private static final String REQ_REASON_CUSTOMER = "10";
    private static final String REQ_REASON_MERCHANT = "20";
    private static final String REQ_REASON_CHARGEBACK = "30";

    private static final String DECLINE_WINDOW = "WIN";
    private static final String DECLINE_AMOUNT = "AMT";
    private static final String DECLINE_TRANSACTION = "TXN";

    public static void main(String[] a) {
        TransactionStore transactionStore = new TransactionStore();
        transactionStore.add(new PrtxnF("T202606010001", "W0000001", "MRC10001", 12800L, "20260601"));
        transactionStore.add(new PrtxnF("T202606030008", "W0000002", "MRC24077", 5400L, "20260603"));
        transactionStore.add(new PrtxnF("T202606080021", "W0000003", "MRC01982", 30000L, "20260608"));

        ReasonStore reasonStore = new ReasonStore();
        reasonStore.add(new PrrsnF("CB001", "CHARGEBACK", 80, "1"));
        reasonStore.add(new PrrsnF("FR101", "CUSTOMER", 30, "0"));
        reasonStore.add(new PrrsnF("MR201", "MERCHANT", 45, "0"));

        ChargebackCase[] cases = new ChargebackCase[] {
                new ChargebackCase("CB-202606-0001", "T202606010001", "CB001", 12800L, "20260610"),
                new ChargebackCase("CB-202606-0002", "T202606030008", "CB001", 7000L, "20260611"),
                new ChargebackCase("CB-202606-0003", "T999999999999", "CB001", 9800L, "20260612")
        };

        MappingSummary summary = new MappingSummary();
        for (int i = 0; i < cases.length; i++) {
            MappingResult result = mapCase(cases[i], transactionStore, reasonStore);
            summary.add(result);
            System.out.println(result.toOperatorLine());
        }
        System.out.println(summary.toOperatorLine());
    }

    private static MappingResult mapCase(ChargebackCase chargebackCase,
                                         TransactionStore transactionStore,
                                         ReasonStore reasonStore) {
        PrtxnF original = transactionStore.find(chargebackCase.origTxnId);
        if (original == null) {
            return new MappingResult(
                    chargebackCase.caseId,
                    chargebackCase.origTxnId,
                    null,
                    0L,
                    chargebackCase.disputeAmount,
                    REQ_REASON_CHARGEBACK,
                    DECISION_DECLINE,
                    DECLINE_TRANSACTION,
                    false,
                    true,
                    0
            );
        }

        PrrsnF reason = reasonStore.find(chargebackCase.reasonCode);
        String reqReason = normalizeReason(reason);
        boolean amountOver = chargebackCase.disputeAmount > original.origTxnAmt;
        long mappedAmount = amountOver ? original.origTxnAmt : chargebackCase.disputeAmount;

        return new MappingResult(
                chargebackCase.caseId,
                original.origTxnId,
                original.merchantCode,
                original.origTxnAmt,
                mappedAmount,
                reqReason,
                DECISION_ACCEPT,
                amountOver ? DECLINE_AMOUNT : "",
                amountOver,
                shouldAutoReview(reason, amountOver),
                reason == null ? 0 : reason.riskWeight
        );
    }

    private static String normalizeReason(PrrsnF reason) {
        if (reason == null) {
            return REQ_REASON_CHARGEBACK;
        }
        if ("CUSTOMER".equals(reason.reasonGroup)) {
            return REQ_REASON_CUSTOMER;
        }
        if ("MERCHANT".equals(reason.reasonGroup)) {
            return REQ_REASON_MERCHANT;
        }
        return REQ_REASON_CHARGEBACK;
    }

    private static boolean shouldAutoReview(PrrsnF reason, boolean amountOver) {
        if (amountOver) {
            return true;
        }
        if (reason == null) {
            return true;
        }
        return "1".equals(reason.autoReviewKbn) || reason.riskWeight >= 70;
    }

    private static final class PrtxnF {
        private final String origTxnId;
        private final String walletId;
        private final String merchantCode;
        private final long origTxnAmt;
        private final String origTxnDt;

        private PrtxnF(String origTxnId, String walletId, String merchantCode, long origTxnAmt, String origTxnDt) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
        }
    }

    private static final class PrrsnF {
        private final String reasonCode;
        private final String reasonGroup;
        private final int riskWeight;
        private final String autoReviewKbn;

        private PrrsnF(String reasonCode, String reasonGroup, int riskWeight, String autoReviewKbn) {
            this.reasonCode = reasonCode;
            this.reasonGroup = reasonGroup;
            this.riskWeight = riskWeight;
            this.autoReviewKbn = autoReviewKbn;
        }
    }

    private static final class ChargebackCase {
        private final String caseId;
        private final String origTxnId;
        private final String reasonCode;
        private final long disputeAmount;
        private final String acceptDt;

        private ChargebackCase(String caseId, String origTxnId, String reasonCode, long disputeAmount, String acceptDt) {
            this.caseId = caseId;
            this.origTxnId = origTxnId;
            this.reasonCode = reasonCode;
            this.disputeAmount = disputeAmount;
            this.acceptDt = acceptDt;
        }
    }

    private static final class MappingResult {
        private final String caseId;
        private final String origTxnId;
        private final String merchantCode;
        private final long originalAmount;
        private final long refundAmount;
        private final String reqReason;
        private final String decisionKbn;
        private final String declineReason;
        private final boolean capAdjustCandidate;
        private final boolean autoReview;
        private final int riskWeight;

        private MappingResult(String caseId,
                              String origTxnId,
                              String merchantCode,
                              long originalAmount,
                              long refundAmount,
                              String reqReason,
                              String decisionKbn,
                              String declineReason,
                              boolean capAdjustCandidate,
                              boolean autoReview,
                              int riskWeight) {
            this.caseId = caseId;
            this.origTxnId = origTxnId;
            this.merchantCode = merchantCode;
            this.originalAmount = originalAmount;
            this.refundAmount = refundAmount;
            this.reqReason = reqReason;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.capAdjustCandidate = capAdjustCandidate;
            this.autoReview = autoReview;
            this.riskWeight = riskWeight;
        }

        private String toOperatorLine() {
            StringBuilder b = new StringBuilder();
            b.append("処理結果 ケースID=").append(caseId);
            b.append(" 原取引ID=").append(origTxnId);
            b.append(" 加盟店=").append(merchantCode == null ? "" : merchantCode);
            b.append(" 原取引額=").append(originalAmount);
            b.append(" 返金額=").append(refundAmount);
            b.append(" 理由=").append(reqReason);
            b.append(" 判定=").append(decisionKbn);
            if (!declineReason.isEmpty()) {
                b.append(" 事由=").append(declineReason);
            }
            b.append(" 上限調整候補=").append(capAdjustCandidate ? "1" : "0");
            b.append(" 自動審査=").append(autoReview ? "1" : "0");
            b.append(" リスク=").append(riskWeight);
            return b.toString();
        }
    }

    private static final class MappingSummary {
        private int totalCount;
        private int acceptedCount;
        private int declinedCount;
        private int capAdjustCount;
        private long refundTotal;

        private void add(MappingResult result) {
            totalCount++;
            if (DECISION_ACCEPT.equals(result.decisionKbn)) {
                acceptedCount++;
                refundTotal += result.refundAmount;
            } else {
                declinedCount++;
            }
            if (result.capAdjustCandidate) {
                capAdjustCount++;
            }
        }

        private String toOperatorLine() {
            return "集計 件数=" + totalCount
                    + " 受付=" + acceptedCount
                    + " 否認=" + declinedCount
                    + " 上限調整候補=" + capAdjustCount
                    + " 返金額合計=" + refundTotal;
        }
    }

    private static final class TransactionStore {
        private final PrtxnF[] rows = new PrtxnF[32];
        private int size;

        private void add(PrtxnF row) {
            rows[size] = row;
            size++;
        }

        private PrtxnF find(String origTxnId) {
            for (int i = 0; i < size; i++) {
                if (rows[i].origTxnId.equals(origTxnId)) {
                    return rows[i];
                }
            }
            return null;
        }
    }

    private static final class ReasonStore {
        private final PrrsnF[] rows = new PrrsnF[32];
        private int size;

        private void add(PrrsnF row) {
            rows[size] = row;
            size++;
        }

        private PrrsnF find(String reasonCode) {
            for (int i = 0; i < size; i++) {
                if (rows[i].reasonCode.equals(reasonCode)) {
                    return rows[i];
                }
            }
            return null;
        }
    }
}
