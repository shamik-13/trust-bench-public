package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数    年月日      担当                                概要
 * 1.00    2025-01-14  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundPerformanceReportService {
    private static final String 決定_受付 = "A";
    private static final String 決定_否認 = "D";
    private static final String 否認_期間超過 = "WIN";
    private static final String 否認_金額超過 = "AMT";
    private static final String 否認_原取引なし = "TXN";

    public static void main(String[] a) {
        Prrspf[] prrspf = {
                new Prrspf("RQ-202606-0001", "TX-20260401-0001", "A", "", 3200),
                new Prrspf("RQ-202606-0002", "TX-20260401-0002", "A", "", 1800),
                new Prrspf("RQ-202606-0003", "TX-20260402-0001", "D", "WIN", 0),
                new Prrspf("RQ-202606-0004", "TX-20260402-0002", "A", "", 8400),
                new Prrspf("RQ-202606-0005", "TX-20260402-0002", "A", "", 2600),
                new Prrspf("RQ-202606-0006", "TX-20260403-0001", "D", "AMT", 0),
                new Prrspf("RQ-202606-0007", "TX-20260404-0001", "A", "", 1200),
                new Prrspf("RQ-202606-0008", "TX-20260405-9999", "D", "TXN", 0),
                new Prrspf("RQ-202606-0009", "TX-20260405-0001", "A", "", 4500),
                new Prrspf("RQ-202606-0010", "TX-20260405-0002", "A", "", 7000),
                new Prrspf("RQ-202606-0011", "TX-20260405-0002", "A", "", 1500),
                new Prrspf("RQ-202606-0012", "TX-20260406-0001", "D", "WIN", 0)
        };

        Prtxnf[] prtxnf = {
                new Prtxnf("TX-20260401-0001", "WL-10001", "MRC-100001", 12800, "20260401"),
                new Prtxnf("TX-20260401-0002", "WL-10002", "MRC-100001", 2400, "20260401"),
                new Prtxnf("TX-20260402-0001", "WL-10003", "MRC-100002", 5600, "20260402"),
                new Prtxnf("TX-20260402-0002", "WL-10004", "MRC-100002", 10000, "20260402"),
                new Prtxnf("TX-20260403-0001", "WL-10005", "MRC-100003", 3900, "20260403"),
                new Prtxnf("TX-20260404-0001", "WL-10006", "MRC-100004", 1200, "20260404"),
                new Prtxnf("TX-20260405-0001", "WL-10007", "MRC-100001", 9800, "20260405"),
                new Prtxnf("TX-20260405-0002", "WL-10008", "MRC-100005", 8000, "20260405"),
                new Prtxnf("TX-20260406-0001", "WL-10009", "MRC-100006", 7300, "20260406")
        };

        Prbalf[] prbalf = {
                new Prbalf("WL-10001", 52000, 3200, "20260627"),
                new Prbalf("WL-10002", 18000, 0, "20260626"),
                new Prbalf("WL-10003", 7400, 0, "20260620"),
                new Prbalf("WL-10004", 23000, 8400, "20260627"),
                new Prbalf("WL-10005", 9100, 0, "20260625"),
                new Prbalf("WL-10006", 6100, 1200, "20260627"),
                new Prbalf("WL-10007", 33500, 4500, "20260627"),
                new Prbalf("WL-10008", 42800, 7000, "20260627"),
                new Prbalf("WL-10009", 15600, 0, "20260618")
        };

        java.util.Map<String, Prtxnf> 原取引索引 = new java.util.LinkedHashMap<>();
        for (Prtxnf row : prtxnf) {
            原取引索引.put(row.origTxnId, row);
        }

        java.util.Map<String, Prbalf> 残高索引 = new java.util.LinkedHashMap<>();
        for (Prbalf row : prbalf) {
            残高索引.put(row.walletId, row);
        }

        java.util.Map<String, ReportWork> 集計索引 = new java.util.LinkedHashMap<>();
        java.util.Map<String, Long> 原取引別返金額 = new java.util.LinkedHashMap<>();
        java.util.List<String> 分離明細 = new java.util.ArrayList<>();

        for (Prrspf req : prrspf) {
            Prtxnf txn = 原取引索引.get(req.origTxnId);

            if (決定_否認.equals(req.decisionKbn)) {
                String merchantCode = txn == null ? "UNKNOWN" : txn.merchantCode;
                String origTxnDt = txn == null ? "00000000" : txn.origTxnDt;
                ReportWork work = 集計索引.computeIfAbsent(key(merchantCode, origTxnDt),
                        k -> new ReportWork(merchantCode, origTxnDt));
                work.declineCnt++;
                continue;
            }

            if (!決定_受付.equals(req.decisionKbn)) {
                分離明細.add("判定区分不正," + req.reqId + "," + req.origTxnId + "," + req.decisionKbn);
                continue;
            }

            if (txn == null) {
                分離明細.add("台帳未反映候補," + req.reqId + "," + req.origTxnId + "," + 否認_原取引なし);
                continue;
            }

            long accumulated = 原取引別返金額.getOrDefault(req.origTxnId, 0L) + req.eligibleAmt;
            原取引別返金額.put(req.origTxnId, accumulated);

            ReportWork work = 集計索引.computeIfAbsent(key(txn.merchantCode, txn.origTxnDt),
                    k -> new ReportWork(txn.merchantCode, txn.origTxnDt));
            work.refundCnt++;
            work.refundAmt += req.eligibleAmt;
            work.origTxnAmt += txn.origTxnAmt;

            if (accumulated > txn.origTxnAmt) {
                分離明細.add("過大返金候補," + req.reqId + "," + req.origTxnId + "," + accumulated + "," + txn.origTxnAmt + "," + 否認_金額超過);
            }

            Prbalf bal = 残高索引.get(txn.walletId);
            if (bal == null) {
                分離明細.add("台帳未反映候補," + req.reqId + "," + txn.walletId + ",残高台帳なし");
            } else if (bal.pendingRefundAmt < req.eligibleAmt) {
                分離明細.add("台帳未反映候補," + req.reqId + "," + txn.walletId + "," + bal.pendingRefundAmt + "," + req.eligibleAmt);
            }
        }

        System.out.println("REPORT-ID,REPORT-DT,MERCHANT-CODE,REFUND-CNT,REFUND-AMT,DECLINE-CNT,ORIG-TXN-DT,REFUND-RATE");
        int seq = 1;
        for (ReportWork work : 集計索引.values()) {
            String reportId = "PRRPTF2-20260628-" + String.format("%04d", seq++);
            long ratePermil = work.origTxnAmt == 0 ? 0 : work.refundAmt * 1000 / work.origTxnAmt;
            System.out.println(reportId + ",20260628," + work.merchantCode + "," + work.refundCnt + ","
                    + work.refundAmt + "," + work.declineCnt + "," + work.origTxnDt + "," + formatRate(ratePermil));
        }

        System.out.println("分離区分,REQ-ID,照合キー,実績値,基準値,理由");
        for (String line : 分離明細) {
            System.out.println(line);
        }

        if (分離明細.isEmpty()) {
            System.out.println("分離対象なし");
        }
    }

    private static String key(String merchantCode, String origTxnDt) {
        return merchantCode + "|" + origTxnDt;
    }

    private static String formatRate(long permil) {
        return (permil / 10) + "." + (permil % 10) + "%";
    }

    private static final class Prrspf {
        final String reqId;
        final String origTxnId;
        final String decisionKbn;
        final String declineReason;
        final long eligibleAmt;

        Prrspf(String reqId, String origTxnId, String decisionKbn, String declineReason, long eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class Prtxnf {
        final String origTxnId;
        final String walletId;
        final String merchantCode;
        final long origTxnAmt;
        final String origTxnDt;

        Prtxnf(String origTxnId, String walletId, String merchantCode, long origTxnAmt, String origTxnDt) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
        }
    }

    private static final class Prbalf {
        final String walletId;
        final long availableBal;
        final long pendingRefundAmt;
        final String lastAdjDt;

        Prbalf(String walletId, long availableBal, long pendingRefundAmt, String lastAdjDt) {
            this.walletId = walletId;
            this.availableBal = availableBal;
            this.pendingRefundAmt = pendingRefundAmt;
            this.lastAdjDt = lastAdjDt;
        }
    }

    private static final class ReportWork {
        final String merchantCode;
        final String origTxnDt;
        int refundCnt;
        long refundAmt;
        int declineCnt;
        long origTxnAmt;

        ReportWork(String merchantCode, String origTxnDt) {
            this.merchantCode = merchantCode;
            this.origTxnDt = origTxnDt;
        }
    }
}
