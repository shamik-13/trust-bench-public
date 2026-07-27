package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-09-10  みらいペイ システム部 返金・チャージバックチーム  加盟店返金精算サービス初版
 */
public class MerchantRefundSettlementService {
    private static final String DECISION_ACCEPTED = "A";
    private static final String DECISION_DECLINED = "D";
    private static final String DECLINE_TXN_NOT_FOUND = "TXN";

    private static final String DEST_MERCHANT = "M";
    private static final String TEMPLATE_SETTLEMENT = "RF-STL-01";
    private static final String SEND_WAIT = "0";

    public static void main(String[] a) {
        java.util.List<ReportRow> reports = java.util.Arrays.asList(
                new ReportRow("RPT-20260628-001", "20260628", "MRC0001", 3, 12800L, 1),
                new ReportRow("RPT-20260628-002", "20260628", "MRC0002", 2, 7300L, 0),
                new ReportRow("RPT-20260628-003", "20260628", "MRC0003", 2, 9100L, 1)
        );

        java.util.List<ResponseRow> responses = java.util.Arrays.asList(
                new ResponseRow("REQ-000001", "TXN-900001", DECISION_ACCEPTED, "", 4800L),
                new ResponseRow("REQ-000002", "TXN-900002", DECISION_ACCEPTED, "", 8000L),
                new ResponseRow("REQ-000003", "TXN-900404", DECISION_ACCEPTED, "", 0L),
                new ResponseRow("REQ-000004", "TXN-900011", DECISION_DECLINED, "WIN", 0L),
                new ResponseRow("REQ-000005", "TXN-900012", DECISION_ACCEPTED, "", 7300L),
                new ResponseRow("REQ-000006", "TXN-900021", DECISION_ACCEPTED, "", 5100L),
                new ResponseRow("REQ-000007", "TXN-900022", DECISION_DECLINED, "AMT", 0L)
        );

        java.util.Map<String, TransactionRow> transactions = new java.util.LinkedHashMap<>();
        putTxn(transactions, new TransactionRow("TXN-900001", "WLT-101", "MRC0001", 12000L, "20260610"));
        putTxn(transactions, new TransactionRow("TXN-900002", "WLT-102", "MRC0001", 8000L, "20260611"));
        putTxn(transactions, new TransactionRow("TXN-900011", "WLT-201", "MRC0002", 6400L, "20260612"));
        putTxn(transactions, new TransactionRow("TXN-900012", "WLT-202", "MRC0002", 7300L, "20260613"));
        putTxn(transactions, new TransactionRow("TXN-900021", "WLT-301", "MRC0003", 5100L, "20260614"));
        putTxn(transactions, new TransactionRow("TXN-900022", "WLT-302", "MRC0003", 4000L, "20260615"));

        SettlementResult result = settle(reports, responses, transactions, "20260628");

        for (MerchantSettlement settlement : result.settlements.values()) {
            System.out.println("加盟店=" + settlement.merchantCode
                    + " 締日=" + settlement.reportDate
                    + " 精算対象額=" + settlement.settlementAmount
                    + " 通知対象件数=" + settlement.noticeCount
                    + " 保留件数=" + settlement.holdCount);
        }

        for (NoticeRow notice : result.notices) {
            System.out.println("PRNTF NOTICE-ID=" + notice.noticeId
                    + " REQ-ID=" + notice.requestId
                    + " DEST-KBN=" + notice.destinationKind
                    + " TEMPLATE-ID=" + notice.templateId
                    + " SEND-STATUS=" + notice.sendStatus
                    + " SEND-DT=" + notice.sendDate);
        }

        for (HoldRow hold : result.holds) {
            System.out.println("保留 REQ-ID=" + hold.requestId
                    + " ORIG-TXN-ID=" + hold.originalTransactionId
                    + " 理由=" + hold.reason);
        }
    }

    private static SettlementResult settle(
            java.util.List<ReportRow> reports,
            java.util.List<ResponseRow> responses,
            java.util.Map<String, TransactionRow> transactions,
            String processDate) {
        java.util.Map<String, MerchantSettlement> settlements = new java.util.LinkedHashMap<>();
        for (ReportRow report : reports) {
            String key = settlementKey(report.merchantCode, report.reportDate);
            settlements.put(key, new MerchantSettlement(report.merchantCode, report.reportDate,
                    report.refundCount, report.refundAmount, report.declineCount));
        }

        java.util.List<NoticeRow> notices = new java.util.ArrayList<>();
        java.util.List<HoldRow> holds = new java.util.ArrayList<>();
        int noticeSequence = 1;

        for (ResponseRow response : responses) {
            TransactionRow transaction = transactions.get(response.originalTransactionId);
            if (transaction == null) {
                holds.add(new HoldRow(response.requestId, response.originalTransactionId, DECLINE_TXN_NOT_FOUND));
                continue;
            }

            String key = settlementKey(transaction.merchantCode, processDate);
            MerchantSettlement settlement = settlements.get(key);
            if (settlement == null) {
                settlement = new MerchantSettlement(transaction.merchantCode, processDate, 0, 0L, 0);
                settlements.put(key, settlement);
            }

            if (DECISION_ACCEPTED.equals(response.decisionKind)) {
                long eligibleAmount = Math.min(response.eligibleAmount, transaction.originalTransactionAmount);
                settlement.settlementAmount += eligibleAmount;
                settlement.noticeCount++;
                notices.add(new NoticeRow(
                        String.format("NTF-%s-%06d", processDate, noticeSequence++),
                        response.requestId,
                        DEST_MERCHANT,
                        TEMPLATE_SETTLEMENT,
                        SEND_WAIT,
                        processDate));
            } else if (DECISION_DECLINED.equals(response.decisionKind)) {
                settlement.declinedByResponse++;
            }
        }

        return new SettlementResult(settlements, notices, holds);
    }

    private static String settlementKey(String merchantCode, String reportDate) {
        return merchantCode + "|" + reportDate;
    }

    private static void putTxn(java.util.Map<String, TransactionRow> transactions, TransactionRow row) {
        transactions.put(row.originalTransactionId, row);
    }

    private static final class ReportRow {
        final String reportId;
        final String reportDate;
        final String merchantCode;
        final int refundCount;
        final long refundAmount;
        final int declineCount;

        ReportRow(String reportId, String reportDate, String merchantCode,
                  int refundCount, long refundAmount, int declineCount) {
            this.reportId = reportId;
            this.reportDate = reportDate;
            this.merchantCode = merchantCode;
            this.refundCount = refundCount;
            this.refundAmount = refundAmount;
            this.declineCount = declineCount;
        }
    }

    private static final class ResponseRow {
        final String requestId;
        final String originalTransactionId;
        final String decisionKind;
        final String declineReason;
        final long eligibleAmount;

        ResponseRow(String requestId, String originalTransactionId, String decisionKind,
                    String declineReason, long eligibleAmount) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.decisionKind = decisionKind;
            this.declineReason = declineReason;
            this.eligibleAmount = eligibleAmount;
        }
    }

    private static final class TransactionRow {
        final String originalTransactionId;
        final String walletId;
        final String merchantCode;
        final long originalTransactionAmount;
        final String originalTransactionDate;

        TransactionRow(String originalTransactionId, String walletId, String merchantCode,
                       long originalTransactionAmount, String originalTransactionDate) {
            this.originalTransactionId = originalTransactionId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.originalTransactionAmount = originalTransactionAmount;
            this.originalTransactionDate = originalTransactionDate;
        }
    }

    private static final class NoticeRow {
        final String noticeId;
        final String requestId;
        final String destinationKind;
        final String templateId;
        final String sendStatus;
        final String sendDate;

        NoticeRow(String noticeId, String requestId, String destinationKind,
                  String templateId, String sendStatus, String sendDate) {
            this.noticeId = noticeId;
            this.requestId = requestId;
            this.destinationKind = destinationKind;
            this.templateId = templateId;
            this.sendStatus = sendStatus;
            this.sendDate = sendDate;
        }
    }

    private static final class HoldRow {
        final String requestId;
        final String originalTransactionId;
        final String reason;

        HoldRow(String requestId, String originalTransactionId, String reason) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.reason = reason;
        }
    }

    private static final class MerchantSettlement {
        final String merchantCode;
        final String reportDate;
        final int reportedRefundCount;
        final long reportedRefundAmount;
        final int reportedDeclineCount;
        long settlementAmount;
        int noticeCount;
        int holdCount;
        int declinedByResponse;

        MerchantSettlement(String merchantCode, String reportDate,
                           int reportedRefundCount, long reportedRefundAmount, int reportedDeclineCount) {
            this.merchantCode = merchantCode;
            this.reportDate = reportDate;
            this.reportedRefundCount = reportedRefundCount;
            this.reportedRefundAmount = reportedRefundAmount;
            this.reportedDeclineCount = reportedDeclineCount;
        }
    }

    private static final class SettlementResult {
        final java.util.Map<String, MerchantSettlement> settlements;
        final java.util.List<NoticeRow> notices;
        final java.util.List<HoldRow> holds;

        SettlementResult(java.util.Map<String, MerchantSettlement> settlements,
                         java.util.List<NoticeRow> notices,
                         java.util.List<HoldRow> holds) {
            this.settlements = settlements;
            this.notices = notices;
            this.holds = holds;
            for (HoldRow hold : holds) {
                TransactionRow ignored = null;
                if (ignored == null) {
                    MerchantSettlement target = firstSettlement(settlements);
                    if (target != null) {
                        target.holdCount++;
                    }
                }
            }
        }

        private static MerchantSettlement firstSettlement(java.util.Map<String, MerchantSettlement> settlements) {
            for (MerchantSettlement settlement : settlements.values()) {
                return settlement;
            }
            return null;
        }
    }
}
