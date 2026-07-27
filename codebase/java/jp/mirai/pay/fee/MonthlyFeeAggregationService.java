package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日        担当      概要
 * 1.00    2024-10-28    みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class MonthlyFeeAggregationService {

    public static void main(String[] a) {
        String settleMonth = a != null && a.length > 0 ? a[0] : "202606";

        java.util.List<java.util.Map<String, Object>> pffeef = createPffeef();
        java.util.List<java.util.Map<String, Object>> pftxnf = createPftxnf();
        java.util.Map<String, java.util.Map<String, Object>> pfmerf = createPfmerf();

        java.util.Map<String, java.util.List<java.util.Map<String, Object>>> feeByMerchant = groupByMerchant(pffeef);
        java.util.Map<String, java.util.List<java.util.Map<String, Object>>> txnByMerchant = groupByMerchant(pftxnf);

        java.util.Map<String, java.util.Map<String, Object>> pfsumf = new java.util.TreeMap<>();
        java.util.List<String> rerunTargets = new java.util.ArrayList<>();

        for (String merchantCode : feeByMerchant.keySet()) {
            java.util.Map<String, Object> merchant = pfmerf.get(merchantCode);
            if (merchant == null) {
                rerunTargets.add(merchantCode);
                System.out.println("加盟店マスタ未登録のため集計保留: " + merchantCode);
                continue;
            }

            String status = stringValue(merchant, "MER-STATUS");
            if (!"01".equals(status)) {
                System.out.println("請求対象外ステータスのため集計除外: " + merchantCode + " 状態=" + status);
                continue;
            }

            java.util.List<java.util.Map<String, Object>> feeRows = feeByMerchant.get(merchantCode);
            java.util.List<java.util.Map<String, Object>> txnRows = txnByMerchant.get(merchantCode);

            if (!matchesTransactionFile(feeRows, txnRows, settleMonth)) {
                rerunTargets.add(merchantCode);
                System.out.println("取引明細欠落の疑いにより集計保留: " + merchantCode);
                continue;
            }

            long txnCount = feeRows.size();
            java.math.BigDecimal txnTotal = java.math.BigDecimal.ZERO;
            java.math.BigDecimal feeTotal = java.math.BigDecimal.ZERO;

            for (java.util.Map<String, Object> fee : feeRows) {
                txnTotal = txnTotal.add(decimalValue(fee, "TXN-AMT"));
                feeTotal = feeTotal.add(decimalValue(fee, "FEE-AMT"));
            }

            java.math.BigDecimal netSettle = txnTotal.subtract(feeTotal);
            String summaryId = settleMonth + "-" + merchantCode;

            java.util.Map<String, Object> summary = new java.util.LinkedHashMap<>();
            summary.put("SUMMARY-ID", summaryId);
            summary.put("MERCHANT-CODE", merchantCode);
            summary.put("SETTLE-MONTH", settleMonth);
            summary.put("TXN-COUNT", txnCount);
            summary.put("TXN-TOTAL-AMT", txnTotal);
            summary.put("FEE-TOTAL-AMT", feeTotal);
            summary.put("NET-SETTLE-AMT", netSettle);
            pfsumf.put(summaryId, summary);

            System.out.println("月次手数料集計作成: " + summaryId
                    + " 件数=" + txnCount
                    + " 取引総額=" + txnTotal
                    + " 手数料総額=" + feeTotal
                    + " 精算予定額=" + netSettle);
        }

        System.out.println("PFSUMF作成件数=" + pfsumf.size());
        System.out.println("MDR算定再実行対象=" + rerunTargets);
    }

    private static boolean matchesTransactionFile(
            java.util.List<java.util.Map<String, Object>> feeRows,
            java.util.List<java.util.Map<String, Object>> txnRows,
            String settleMonth) {

        if (txnRows == null || feeRows.size() != txnRows.size()) {
            return false;
        }

        java.math.BigDecimal feeTxnTotal = java.math.BigDecimal.ZERO;
        for (java.util.Map<String, Object> fee : feeRows) {
            if (decimalValue(fee, "FEE-AMT").signum() < 0) {
                return false;
            }
            if (decimalValue(fee, "MDR-RATE").signum() < 0) {
                return false;
            }
            feeTxnTotal = feeTxnTotal.add(decimalValue(fee, "TXN-AMT"));
        }

        java.math.BigDecimal txnTotal = java.math.BigDecimal.ZERO;
        for (java.util.Map<String, Object> txn : txnRows) {
            String txnDate = stringValue(txn, "TXN-DT");
            if (txnDate.length() < 6 || !settleMonth.equals(txnDate.substring(0, 6))) {
                return false;
            }
            txnTotal = txnTotal.add(decimalValue(txn, "TXN-AMT"));
        }

        return feeTxnTotal.compareTo(txnTotal) == 0;
    }

    private static java.util.Map<String, java.util.List<java.util.Map<String, Object>>> groupByMerchant(
            java.util.List<java.util.Map<String, Object>> rows) {

        java.util.Map<String, java.util.List<java.util.Map<String, Object>>> grouped = new java.util.TreeMap<>();
        for (java.util.Map<String, Object> row : rows) {
            String merchantCode = stringValue(row, "MERCHANT-CODE");
            grouped.computeIfAbsent(merchantCode, k -> new java.util.ArrayList<>()).add(row);
        }
        return grouped;
    }

    private static java.math.BigDecimal decimalValue(java.util.Map<String, Object> row, String key) {
        Object value = row.get(key);
        if (value instanceof java.math.BigDecimal) {
            return (java.math.BigDecimal) value;
        }
        if (value instanceof Number) {
            return new java.math.BigDecimal(value.toString());
        }
        if (value instanceof String) {
            return new java.math.BigDecimal((String) value);
        }
        throw new IllegalArgumentException("数値項目不正: " + key);
    }

    private static String stringValue(java.util.Map<String, Object> row, String key) {
        Object value = row.get(key);
        if (value == null) {
            throw new IllegalArgumentException("必須項目未設定: " + key);
        }
        return String.valueOf(value);
    }

    private static java.util.List<java.util.Map<String, Object>> createPffeef() {
        java.util.List<java.util.Map<String, Object>> rows = new java.util.ArrayList<>();
        rows.add(fee("FEE-202606-000001", "M000001", "12800", "0.0320", "409"));
        rows.add(fee("FEE-202606-000002", "M000001", "8200", "0.0320", "262"));
        rows.add(fee("FEE-202606-000003", "M000002", "5400", "0.0280", "151"));
        rows.add(fee("FEE-202606-000004", "M000002", "3160", "0.0280", "88"));
        rows.add(fee("FEE-202606-000005", "M000003", "210000", "0.0120", "2520"));
        rows.add(fee("FEE-202606-000006", "M000004", "43800", "0.0345", "1576"));
        rows.add(fee("FEE-202606-000007", "M000005", "97000", "0.0450", "4365"));
        return rows;
    }

    private static java.util.List<java.util.Map<String, Object>> createPftxnf() {
        java.util.List<java.util.Map<String, Object>> rows = new java.util.ArrayList<>();
        rows.add(txn("TXN-202606-100001", "M000001", "12800", "20260603"));
        rows.add(txn("TXN-202606-100002", "M000001", "8200", "20260618"));
        rows.add(txn("TXN-202606-100003", "M000002", "5400", "20260605"));
        rows.add(txn("TXN-202606-100004", "M000002", "3160", "20260625"));
        rows.add(txn("TXN-202606-100005", "M000003", "210000", "20260630"));
        rows.add(txn("TXN-202606-100006", "M000005", "97000", "20260612"));
        return rows;
    }

    private static java.util.Map<String, java.util.Map<String, Object>> createPfmerf() {
        java.util.Map<String, java.util.Map<String, Object>> rows = new java.util.TreeMap<>();
        rows.put("M000001", merchant("M000001", "未来堂銀座店", "C1", "01"));
        rows.put("M000002", merchant("M000002", "東都食堂八重洲", "C2", "01"));
        rows.put("M000003", merchant("M000003", "北浜市水道局", "C3", "01"));
        rows.put("M000004", merchant("M000004", "新宿予約販売", "C4", "02"));
        rows.put("M000005", merchant("M000005", "湾岸チケット", "C5", "01"));
        return rows;
    }

    private static java.util.Map<String, Object> fee(
            String feeId,
            String merchantCode,
            String txnAmount,
            String mdrRate,
            String feeAmount) {

        java.util.Map<String, Object> row = new java.util.LinkedHashMap<>();
        row.put("FEE-ID", feeId);
        row.put("MERCHANT-CODE", merchantCode);
        row.put("TXN-AMT", new java.math.BigDecimal(txnAmount));
        row.put("MDR-RATE", new java.math.BigDecimal(mdrRate));
        row.put("FEE-AMT", new java.math.BigDecimal(feeAmount));
        return row;
    }

    private static java.util.Map<String, Object> txn(
            String txnId,
            String merchantCode,
            String txnAmount,
            String txnDate) {

        java.util.Map<String, Object> row = new java.util.LinkedHashMap<>();
        row.put("TXN-ID", txnId);
        row.put("MERCHANT-CODE", merchantCode);
        row.put("TXN-AMT", new java.math.BigDecimal(txnAmount));
        row.put("TXN-DT", txnDate);
        return row;
    }

    private static java.util.Map<String, Object> merchant(
            String merchantCode,
            String merchantName,
            String merchantCategory,
            String merchantStatus) {

        java.util.Map<String, Object> row = new java.util.LinkedHashMap<>();
        row.put("MERCHANT-CODE", merchantCode);
        row.put("MERCHANT-NAME", merchantName);
        row.put("MER-CATEGORY", merchantCategory);
        row.put("MER-STATUS", merchantStatus);
        return row;
    }
}
