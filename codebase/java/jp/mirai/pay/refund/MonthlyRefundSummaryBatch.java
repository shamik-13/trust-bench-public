package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-07-22  みらいペイ システム部 返金・チャージバックチーム  月次返金集計バッチ初版作成
 */
public class MonthlyRefundSummaryBatch {
    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";

    private static final String DECLINE_WIN = "WIN";
    private static final String DECLINE_AMT = "AMT";
    private static final String DECLINE_TXN = "TXN";

    private static final java.time.LocalDate REPORT_DATE = java.time.LocalDate.of(2026, 6, 30);

    public static void main(String[] a) {
        new MonthlyRefundSummaryBatchRunner().execute();
    }

    private static final class MonthlyRefundSummaryBatchRunner {
        private final java.util.Map<String, ResponseRow> prrspf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, RequestRow> prreqf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, TransactionRow> prtxnf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, ChargebackRow> prcbf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, FraudRow> pdfrdf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, BalanceRow> prbalf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, ReasonRow> prrsnf = new java.util.LinkedHashMap<>();

        private final java.util.List<ReportRow> prrptf2 = new java.util.ArrayList<>();
        private final java.util.List<NoticeRow> prntf = new java.util.ArrayList<>();

        void execute() {
            loadSyntheticInput();
            java.util.Map<SummaryKey, Summary> summaries = new java.util.TreeMap<>();
            java.util.Map<String, java.math.BigDecimal> walletActualRefunds = new java.util.LinkedHashMap<>();

            for (ResponseRow response : prrspf.values()) {
                RequestRow request = prreqf.get(response.reqId);
                TransactionRow txn = request == null ? null : prtxnf.get(request.origTxnId);
                FraudRow fraud = pdfrdf.get(response.reqId);

                if (request == null) {
                    appendNotice(response.reqId, "SYS", "NTF-RF-REQMISS", "9");
                    continue;
                }

                String merchantCode = txn == null ? "UNKNOWN" : txn.merchantCode;
                String walletId = txn == null ? (fraud == null ? "UNKNOWN" : fraud.walletId) : txn.walletId;
                String reasonGroup = reasonGroupOf(request.reqReason);

                SummaryKey key = new SummaryKey(merchantCode, reasonGroup, walletId);
                Summary summary = summaries.computeIfAbsent(key, k -> new Summary(k));

                if (DECISION_ACCEPT.equals(response.decisionKbn) && txn != null && isAmountPayable(request, response, txn)) {
                    java.math.BigDecimal finalAmount = finalRefundAmount(request, response, txn);
                    summary.refundCnt++;
                    summary.refundAmt = summary.refundAmt.add(finalAmount);
                    walletActualRefunds.merge(walletId, finalAmount, java.math.BigDecimal::add);
                    appendNotice(request.reqId, "M", "NTF-RF-MONTHLY", "0");
                } else {
                    summary.declineCnt++;
                    String template = declineTemplate(response.declineReason, txn);
                    appendNotice(request.reqId, "M", template, "0");
                }
            }

            for (Summary summary : summaries.values()) {
                prrptf2.add(new ReportRow(
                        nextId("RPT", prrptf2.size() + 1),
                        REPORT_DATE,
                        summary.key.merchantCode,
                        summary.refundCnt,
                        summary.refundAmt,
                        summary.declineCnt));
            }

            for (java.util.Map.Entry<String, java.math.BigDecimal> entry : walletActualRefunds.entrySet()) {
                BalanceRow balance = prbalf.get(entry.getKey());
                if (balance == null) {
                    continue;
                }
                java.math.BigDecimal actual = entry.getValue();
                java.math.BigDecimal adjustedPending = balance.pendingRefundAmt.subtract(actual);
                if (adjustedPending.signum() < 0) {
                    adjustedPending = java.math.BigDecimal.ZERO;
                }
                balance.pendingRefundAmt = adjustedPending;
                balance.availableBal = balance.availableBal.add(actual);
                balance.lastAdjDt = REPORT_DATE;
            }

            printOperatorLog(summaries);
        }

        private boolean isAmountPayable(RequestRow request, ResponseRow response, TransactionRow txn) {
            if (DECISION_DECLINE.equals(response.decisionKbn)) {
                return false;
            }
            if (DECLINE_TXN.equals(response.declineReason)) {
                return false;
            }
            if (request.refundAmt.compareTo(txn.origTxnAmt) > 0) {
                return false;
            }
            if (request.refundAmt.compareTo(response.eligibleAmt) > 0) {
                return false;
            }
            return !hasOpenChargeback(request.origTxnId, request.refundAmt);
        }

        private java.math.BigDecimal finalRefundAmount(RequestRow request, ResponseRow response, TransactionRow txn) {
            java.math.BigDecimal amount = request.refundAmt.min(response.eligibleAmt).min(txn.origTxnAmt);
            if ("30".equals(request.reqReason)) {
                java.math.BigDecimal dispute = openDisputeAmount(request.origTxnId);
                if (dispute.signum() > 0) {
                    amount = amount.min(dispute);
                }
            }
            return amount;
        }

        private boolean hasOpenChargeback(String origTxnId, java.math.BigDecimal refundAmt) {
            for (ChargebackRow chargeback : prcbf.values()) {
                if (origTxnId.equals(chargeback.origTxnId)
                        && !"9".equals(chargeback.statusKbn)
                        && chargeback.disputeAmt.compareTo(refundAmt) >= 0) {
                    return true;
                }
            }
            return false;
        }

        private java.math.BigDecimal openDisputeAmount(String origTxnId) {
            java.math.BigDecimal amount = java.math.BigDecimal.ZERO;
            for (ChargebackRow chargeback : prcbf.values()) {
                if (origTxnId.equals(chargeback.origTxnId) && !"9".equals(chargeback.statusKbn)) {
                    amount = amount.add(chargeback.disputeAmt);
                }
            }
            return amount;
        }

        private String reasonGroupOf(String reqReason) {
            ReasonRow reason = prrsnf.get(reqReason);
            return reason == null ? "UNCLASSIFIED" : reason.reasonGroup;
        }

        private String declineTemplate(String declineReason, TransactionRow txn) {
            if (txn == null || DECLINE_TXN.equals(declineReason)) {
                return "NTF-RF-NOTXN";
            }
            if (DECLINE_AMT.equals(declineReason)) {
                return "NTF-RF-AMT";
            }
            if (DECLINE_WIN.equals(declineReason)) {
                return "NTF-RF-WIN";
            }
            return "NTF-RF-DENY";
        }

        private void appendNotice(String reqId, String destKbn, String templateId, String sendStatus) {
            prntf.add(new NoticeRow(
                    nextId("NTF", prntf.size() + 1),
                    reqId,
                    destKbn,
                    templateId,
                    sendStatus,
                    REPORT_DATE));
        }

        private String nextId(String prefix, int sequence) {
            return prefix + REPORT_DATE.toString().replace("-", "") + String.format("%06d", sequence);
        }

        private void printOperatorLog(java.util.Map<SummaryKey, Summary> summaries) {
            int refundCnt = 0;
            int declineCnt = 0;
            java.math.BigDecimal refundAmt = java.math.BigDecimal.ZERO;
            for (Summary summary : summaries.values()) {
                refundCnt += summary.refundCnt;
                declineCnt += summary.declineCnt;
                refundAmt = refundAmt.add(summary.refundAmt);
            }
            System.out.println("月次返金集計バッチ 正常終了");
            System.out.println("帳票件数=" + prrptf2.size()
                    + " 通知件数=" + prntf.size()
                    + " 返金件数=" + refundCnt
                    + " 否認件数=" + declineCnt
                    + " 返金金額=" + refundAmt);
        }

        private void loadSyntheticInput() {
            prrsnf.put("10", new ReasonRow("10", "CUSTOMER", 1, "0"));
            prrsnf.put("20", new ReasonRow("20", "MERCHANT", 2, "0"));
            prrsnf.put("30", new ReasonRow("30", "CHARGEBACK", 5, "1"));

            prtxnf.put("TXN-202606-0001", new TransactionRow("TXN-202606-0001", "WLT-10001", "MRC-0007", bd("12800"), java.time.LocalDate.of(2026, 6, 2)));
            prtxnf.put("TXN-202606-0002", new TransactionRow("TXN-202606-0002", "WLT-10002", "MRC-0007", bd("5400"), java.time.LocalDate.of(2026, 6, 4)));
            prtxnf.put("TXN-202606-0003", new TransactionRow("TXN-202606-0003", "WLT-20003", "MRC-0019", bd("32000"), java.time.LocalDate.of(2026, 6, 9)));
            prtxnf.put("TXN-202606-0004", new TransactionRow("TXN-202606-0004", "WLT-30004", "MRC-0041", bd("8700"), java.time.LocalDate.of(2026, 6, 12)));

            prreqf.put("REQ-000001", new RequestRow("REQ-000001", "TXN-202606-0001", bd("2800"), java.time.LocalDate.of(2026, 6, 10), "10"));
            prreqf.put("REQ-000002", new RequestRow("REQ-000002", "TXN-202606-0002", bd("7400"), java.time.LocalDate.of(2026, 6, 11), "20"));
            prreqf.put("REQ-000003", new RequestRow("REQ-000003", "TXN-202606-0003", bd("12000"), java.time.LocalDate.of(2026, 6, 14), "30"));
            prreqf.put("REQ-000004", new RequestRow("REQ-000004", "TXN-202606-9999", bd("1500"), java.time.LocalDate.of(2026, 6, 15), "10"));
            prreqf.put("REQ-000005", new RequestRow("REQ-000005", "TXN-202606-0004", bd("8700"), java.time.LocalDate.of(2026, 6, 18), "20"));

            prrspf.put("REQ-000001", new ResponseRow("REQ-000001", "TXN-202606-0001", DECISION_ACCEPT, "", bd("2800")));
            prrspf.put("REQ-000002", new ResponseRow("REQ-000002", "TXN-202606-0002", DECISION_DECLINE, DECLINE_AMT, bd("5400")));
            prrspf.put("REQ-000003", new ResponseRow("REQ-000003", "TXN-202606-0003", DECISION_ACCEPT, "", bd("12000")));
            prrspf.put("REQ-000004", new ResponseRow("REQ-000004", "TXN-202606-9999", DECISION_DECLINE, DECLINE_TXN, bd("0")));
            prrspf.put("REQ-000005", new ResponseRow("REQ-000005", "TXN-202606-0004", DECISION_DECLINE, DECLINE_WIN, bd("8700")));

            prcbf.put("CB-0001", new ChargebackRow("CB-0001", "TXN-202606-0003", "VISA", bd("12000"), java.time.LocalDate.of(2026, 6, 16), "1"));
            prcbf.put("CB-0002", new ChargebackRow("CB-0002", "TXN-202606-0001", "JCB", bd("900"), java.time.LocalDate.of(2026, 6, 20), "9"));

            pdfrdf.put("REQ-000001", new FraudRow("FRD-0001", "REQ-000001", "WLT-10001", 120, "NONE", java.time.LocalDate.of(2026, 6, 10)));
            pdfrdf.put("REQ-000003", new FraudRow("FRD-0002", "REQ-000003", "WLT-20003", 720, "CBK-02", java.time.LocalDate.of(2026, 6, 14)));
            pdfrdf.put("REQ-000004", new FraudRow("FRD-0003", "REQ-000004", "WLT-99999", 640, "TXN-00", java.time.LocalDate.of(2026, 6, 15)));

            prbalf.put("WLT-10001", new BalanceRow("WLT-10001", bd("52000"), bd("2800"), java.time.LocalDate.of(2026, 5, 31)));
            prbalf.put("WLT-10002", new BalanceRow("WLT-10002", bd("18000"), bd("5400"), java.time.LocalDate.of(2026, 5, 31)));
            prbalf.put("WLT-20003", new BalanceRow("WLT-20003", bd("76000"), bd("12000"), java.time.LocalDate.of(2026, 5, 31)));
            prbalf.put("WLT-30004", new BalanceRow("WLT-30004", bd("9100"), bd("8700"), java.time.LocalDate.of(2026, 5, 31)));
        }

        private java.math.BigDecimal bd(String value) {
            return new java.math.BigDecimal(value);
        }
    }

    private static final class Summary implements Comparable<Summary> {
        final SummaryKey key;
        int refundCnt;
        java.math.BigDecimal refundAmt = java.math.BigDecimal.ZERO;
        int declineCnt;

        Summary(SummaryKey key) {
            this.key = key;
        }

        @Override
        public int compareTo(Summary other) {
            return key.compareTo(other.key);
        }
    }

    private static final class SummaryKey implements Comparable<SummaryKey> {
        final String merchantCode;
        final String reasonGroup;
        final String walletId;

        SummaryKey(String merchantCode, String reasonGroup, String walletId) {
            this.merchantCode = merchantCode;
            this.reasonGroup = reasonGroup;
            this.walletId = walletId;
        }

        @Override
        public int compareTo(SummaryKey other) {
            int merchant = merchantCode.compareTo(other.merchantCode);
            if (merchant != 0) {
                return merchant;
            }
            int reason = reasonGroup.compareTo(other.reasonGroup);
            if (reason != 0) {
                return reason;
            }
            return walletId.compareTo(other.walletId);
        }
    }

    private static final class ResponseRow {
        final String reqId;
        final String origTxnId;
        final String decisionKbn;
        final String declineReason;
        final java.math.BigDecimal eligibleAmt;

        ResponseRow(String reqId, String origTxnId, String decisionKbn, String declineReason, java.math.BigDecimal eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class RequestRow {
        final String reqId;
        final String origTxnId;
        final java.math.BigDecimal refundAmt;
        final java.time.LocalDate reqDt;
        final String reqReason;

        RequestRow(String reqId, String origTxnId, java.math.BigDecimal refundAmt, java.time.LocalDate reqDt, String reqReason) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.refundAmt = refundAmt;
            this.reqDt = reqDt;
            this.reqReason = reqReason;
        }
    }

    private static final class TransactionRow {
        final String origTxnId;
        final String walletId;
        final String merchantCode;
        final java.math.BigDecimal origTxnAmt;
        final java.time.LocalDate origTxnDt;

        TransactionRow(String origTxnId, String walletId, String merchantCode, java.math.BigDecimal origTxnAmt, java.time.LocalDate origTxnDt) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
        }
    }

    private static final class ChargebackRow {
        final String caseId;
        final String origTxnId;
        final String cardScheme;
        final java.math.BigDecimal disputeAmt;
        final java.time.LocalDate disputeDt;
        final String statusKbn;

        ChargebackRow(String caseId, String origTxnId, String cardScheme, java.math.BigDecimal disputeAmt, java.time.LocalDate disputeDt, String statusKbn) {
            this.caseId = caseId;
            this.origTxnId = origTxnId;
            this.cardScheme = cardScheme;
            this.disputeAmt = disputeAmt;
            this.disputeDt = disputeDt;
            this.statusKbn = statusKbn;
        }
    }

    private static final class FraudRow {
        final String fraudId;
        final String reqId;
        final String walletId;
        final int score;
        final String ruleHitCd;
        final java.time.LocalDate judgeDt;

        FraudRow(String fraudId, String reqId, String walletId, int score, String ruleHitCd, java.time.LocalDate judgeDt) {
            this.fraudId = fraudId;
            this.reqId = reqId;
            this.walletId = walletId;
            this.score = score;
            this.ruleHitCd = ruleHitCd;
            this.judgeDt = judgeDt;
        }
    }

    private static final class BalanceRow {
        final String walletId;
        java.math.BigDecimal availableBal;
        java.math.BigDecimal pendingRefundAmt;
        java.time.LocalDate lastAdjDt;

        BalanceRow(String walletId, java.math.BigDecimal availableBal, java.math.BigDecimal pendingRefundAmt, java.time.LocalDate lastAdjDt) {
            this.walletId = walletId;
            this.availableBal = availableBal;
            this.pendingRefundAmt = pendingRefundAmt;
            this.lastAdjDt = lastAdjDt;
        }
    }

    private static final class ReasonRow {
        final String reasonCode;
        final String reasonGroup;
        final int riskWeight;
        final String autoReviewKbn;

        ReasonRow(String reasonCode, String reasonGroup, int riskWeight, String autoReviewKbn) {
            this.reasonCode = reasonCode;
            this.reasonGroup = reasonGroup;
            this.riskWeight = riskWeight;
            this.autoReviewKbn = autoReviewKbn;
        }
    }

    private static final class ReportRow {
        final String reportId;
        final java.time.LocalDate reportDt;
        final String merchantCode;
        final int refundCnt;
        final java.math.BigDecimal refundAmt;
        final int declineCnt;

        ReportRow(String reportId, java.time.LocalDate reportDt, String merchantCode, int refundCnt, java.math.BigDecimal refundAmt, int declineCnt) {
            this.reportId = reportId;
            this.reportDt = reportDt;
            this.merchantCode = merchantCode;
            this.refundCnt = refundCnt;
            this.refundAmt = refundAmt;
            this.declineCnt = declineCnt;
        }
    }

    private static final class NoticeRow {
        final String noticeId;
        final String reqId;
        final String destKbn;
        final String templateId;
        final String sendStatus;
        final java.time.LocalDate sendDt;

        NoticeRow(String noticeId, String reqId, String destKbn, String templateId, String sendStatus, java.time.LocalDate sendDt) {
            this.noticeId = noticeId;
            this.reqId = reqId;
            this.destKbn = destKbn;
            this.templateId = templateId;
            this.sendStatus = sendStatus;
            this.sendDt = sendDt;
        }
    }
}
