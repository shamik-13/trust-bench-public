package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数    年月日        担当        概要
 * 1.00    2025-06-29    共通基盤    連携エラー再処理サービスの初版作成
 */
public class ImportErrorRetryService {
    private static final String STS_UNPROCESSED = "00";
    private static final String STS_RETRY_OK = "20";
    private static final String STS_RETRY_NG = "29";
    private static final String STS_FIXED = "90";

    private static final java.util.Set<String> COMPANY_CODES =
            new java.util.HashSet<String>(java.util.Arrays.asList("BK", "SC", "CD", "PY", "LF", "CM"));

    private static final java.util.Set<String> RETRYABLE_CODES =
            new java.util.HashSet<String>(java.util.Arrays.asList("E101", "E201", "E301"));

    public static void main(String[] a) {
        java.util.List<java.util.Map<String, String>> cmerrf = cmerrf();
        java.util.List<java.util.Map<String, String>> cmtxnf = cmtxnf();

        java.util.Map<String, java.util.Map<String, String>> txnByKey =
                new java.util.HashMap<String, java.util.Map<String, String>>();
        for (java.util.Map<String, String> txn : cmtxnf) {
            if (isValidCompany(txn.get("COMPANY-CODE")) && isValidTxnStatus(txn.get("TXN-STATUS-KBN"))) {
                txnByKey.put(txnKey(txn.get("COMPANY-CODE"), txn.get("LOCAL-TXN-NO")), txn);
            }
        }

        java.util.Map<String, String> fixedResultByErrorId = new java.util.LinkedHashMap<String, String>();
        int targetCount = 0;
        int numberingCount = 0;
        int matchingCount = 0;
        int rejectedCount = 0;

        for (java.util.Map<String, String> err : cmerrf) {
            String errorId = err.get("ERROR-ID");
            if (fixedResultByErrorId.containsKey(errorId)) {
                err.put("ERROR-STATUS-KBN", fixedResultByErrorId.get(errorId));
                continue;
            }

            String nextStatus = err.get("ERROR-STATUS-KBN");
            if (STS_UNPROCESSED.equals(nextStatus) && isRetryable(err)) {
                targetCount++;
                java.util.Map<String, String> txn =
                        txnByKey.get(txnKey(err.get("COMPANY-CODE"), err.get("LOCAL-TXN-NO")));
                RetryResult result = retry(err, txn);
                nextStatus = result.status;
                if ("SAIBAN".equals(result.route)) {
                    numberingCount++;
                } else if ("NAYOSE".equals(result.route)) {
                    matchingCount++;
                } else {
                    rejectedCount++;
                }
            } else if (!STS_FIXED.equals(nextStatus)) {
                nextStatus = STS_RETRY_NG;
                rejectedCount++;
            }

            err.put("ERROR-STATUS-KBN", nextStatus);
            fixedResultByErrorId.put(errorId, nextStatus);
        }

        System.out.println("処理対象件数=" + targetCount);
        System.out.println("採番補助投入件数=" + numberingCount);
        System.out.println("名寄せ同期投入件数=" + matchingCount);
        System.out.println("再処理不可件数=" + rejectedCount);
        for (java.util.Map<String, String> err : cmerrf) {
            System.out.println(err.get("ERROR-ID") + "," + err.get("IMPORT-BATCH-ID") + ","
                    + err.get("COMPANY-CODE") + "," + err.get("LOCAL-TXN-NO") + ","
                    + err.get("ERROR-CODE") + "," + err.get("ERROR-STATUS-KBN"));
        }
    }

    private static RetryResult retry(java.util.Map<String, String> err, java.util.Map<String, String> txn) {
        if (txn == null || !"01".equals(txn.get("TXN-STATUS-KBN"))) {
            return new RetryResult(STS_RETRY_NG, "KYOKA");
        }

        String code = err.get("ERROR-CODE");
        long amount = parseAmount(txn.get("TXN-AMT"));
        if ("E101".equals(code)) {
            return amount > 0L ? new RetryResult(STS_RETRY_OK, "SAIBAN") : new RetryResult(STS_RETRY_NG, "KYOKA");
        }
        if ("E201".equals(code)) {
            return sameCompany(err, txn) ? new RetryResult(STS_RETRY_OK, "NAYOSE") : new RetryResult(STS_RETRY_NG, "KYOKA");
        }
        if ("E301".equals(code)) {
            return amount >= 1000L ? new RetryResult(STS_RETRY_OK, "NAYOSE") : new RetryResult(STS_RETRY_NG, "KYOKA");
        }
        return new RetryResult(STS_RETRY_NG, "KYOKA");
    }

    private static boolean isRetryable(java.util.Map<String, String> err) {
        return isValidCompany(err.get("COMPANY-CODE"))
                && RETRYABLE_CODES.contains(err.get("ERROR-CODE"))
                && err.get("LOCAL-TXN-NO") != null
                && err.get("LOCAL-TXN-NO").length() > 0;
    }

    private static boolean sameCompany(java.util.Map<String, String> err, java.util.Map<String, String> txn) {
        return err.get("COMPANY-CODE") != null && err.get("COMPANY-CODE").equals(txn.get("COMPANY-CODE"));
    }

    private static boolean isValidCompany(String value) {
        return COMPANY_CODES.contains(value);
    }

    private static boolean isValidTxnStatus(String value) {
        return "01".equals(value) || "09".equals(value);
    }

    private static long parseAmount(String value) {
        if (value == null) {
            return 0L;
        }
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException ex) {
            return 0L;
        }
    }

    private static String txnKey(String companyCode, String localTxnNo) {
        return String.valueOf(companyCode) + "#" + String.valueOf(localTxnNo);
    }

    private static java.util.List<java.util.Map<String, String>> cmerrf() {
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<java.util.Map<String, String>>();
        rows.add(row("ERROR-ID", "ER202506290001", "IMPORT-BATCH-ID", "IB2025062901", "COMPANY-CODE", "BK", "LOCAL-TXN-NO", "BK-000001", "ERROR-CODE", "E101", "ERROR-STATUS-KBN", "00"));
        rows.add(row("ERROR-ID", "ER202506290002", "IMPORT-BATCH-ID", "IB2025062901", "COMPANY-CODE", "SC", "LOCAL-TXN-NO", "SC-000014", "ERROR-CODE", "E201", "ERROR-STATUS-KBN", "00"));
        rows.add(row("ERROR-ID", "ER202506290003", "IMPORT-BATCH-ID", "IB2025062901", "COMPANY-CODE", "CD", "LOCAL-TXN-NO", "CD-000021", "ERROR-CODE", "E999", "ERROR-STATUS-KBN", "00"));
        rows.add(row("ERROR-ID", "ER202506290002", "IMPORT-BATCH-ID", "IB2025062901", "COMPANY-CODE", "SC", "LOCAL-TXN-NO", "SC-000014", "ERROR-CODE", "E201", "ERROR-STATUS-KBN", "00"));
        rows.add(row("ERROR-ID", "ER202506290004", "IMPORT-BATCH-ID", "IB2025062902", "COMPANY-CODE", "PY", "LOCAL-TXN-NO", "PY-000033", "ERROR-CODE", "E301", "ERROR-STATUS-KBN", "00"));
        rows.add(row("ERROR-ID", "ER202506290005", "IMPORT-BATCH-ID", "IB2025062902", "COMPANY-CODE", "LF", "LOCAL-TXN-NO", "LF-000008", "ERROR-CODE", "E101", "ERROR-STATUS-KBN", "90"));
        return rows;
    }

    private static java.util.List<java.util.Map<String, String>> cmtxnf() {
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<java.util.Map<String, String>>();
        rows.add(row("TXN-ID", "TX2025062900001", "COMPANY-CODE", "BK", "LOCAL-TXN-NO", "BK-000001", "TXN-AMT", "1280000", "TXN-STATUS-KBN", "01"));
        rows.add(row("TXN-ID", "TX2025062900014", "COMPANY-CODE", "SC", "LOCAL-TXN-NO", "SC-000014", "TXN-AMT", "552000", "TXN-STATUS-KBN", "01"));
        rows.add(row("TXN-ID", "TX2025062900033", "COMPANY-CODE", "PY", "LOCAL-TXN-NO", "PY-000033", "TXN-AMT", "0", "TXN-STATUS-KBN", "09"));
        rows.add(row("TXN-ID", "TX2025062900008", "COMPANY-CODE", "LF", "LOCAL-TXN-NO", "LF-000008", "TXN-AMT", "77400", "TXN-STATUS-KBN", "01"));
        return rows;
    }

    private static java.util.Map<String, String> row(String... values) {
        if (values.length % 2 != 0) {
            throw new IllegalArgumentException("row requires key/value pairs");
        }

        java.util.Map<String, String> row = new java.util.LinkedHashMap<String, String>();
        for (int i = 0; i < values.length; i += 2) {
            row.put(values[i], values[i + 1]);
        }
        return row;
    }

    private static final class RetryResult {
        private final String status;
        private final String route;

        private RetryResult(String status, String route) {
            this.status = status;
            this.route = route;
        }
    }
}
