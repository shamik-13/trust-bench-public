package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-09-02  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class FeeReportGenerationService {
    private static final String STATUS_CHARGEABLE = "01";
    private static final String REPORT_TYPE_MONTHLY_FEE = "FEE_MONTHLY";
    private static final String REPORT_STATUS_CREATED = "01";

    public static void main(String[] a) {
        java.time.LocalDate businessDate = java.time.LocalDate.of(2025, 5, 12);

        java.util.List<java.util.Map<String, Object>> feeFile = createPffeeF();
        java.util.List<java.util.Map<String, Object>> summaryFile = createPfsumF();
        java.util.List<java.util.Map<String, Object>> merchantFile = createPfmerF();
        java.util.List<java.util.Map<String, Object>> categoryFile = createPmcatF();

        java.util.List<java.util.Map<String, Object>> reportFile =
                generateReports(feeFile, summaryFile, merchantFile, categoryFile, businessDate);

        for (java.util.Map<String, Object> record : reportFile) {
            System.out.println(formatPrRptF(record));
        }
    }

    private static java.util.List<java.util.Map<String, Object>> generateReports(
            java.util.List<java.util.Map<String, Object>> pffeeF,
            java.util.List<java.util.Map<String, Object>> pfsumF,
            java.util.List<java.util.Map<String, Object>> pfmerF,
            java.util.List<java.util.Map<String, Object>> pmcatF,
            java.time.LocalDate businessDate) {

        java.util.Map<String, java.util.Map<String, Object>> merchants = indexBy(pfmerF, "MERCHANT-CODE");
        java.util.Map<String, java.util.Map<String, Object>> categories = indexBy(pmcatF, "CATEGORY-CODE");
        java.util.Map<String, java.math.BigDecimal> detailFeeTotals = sumFeeByMerchant(pffeeF);

        java.util.Map<ReportKey, AggregatedAmount> totals = new java.util.TreeMap<>();

        for (java.util.Map<String, Object> summary : pfsumF) {
            String merchantCode = text(summary, "MERCHANT-CODE");
            java.util.Map<String, Object> merchant = merchants.get(merchantCode);
            if (merchant == null) {
                log("加盟店未登録のため集計対象外: 加盟店=" + merchantCode);
                continue;
            }
            if (!STATUS_CHARGEABLE.equals(text(merchant, "MER-STATUS"))) {
                continue;
            }

            String categoryCode = text(merchant, "MER-CATEGORY");
            java.util.Map<String, Object> category = categories.get(categoryCode);
            if (category == null || !"1".equals(text(category, "ACTIVE-FLAG"))) {
                log("業種区分が無効のため集計対象外: 加盟店=" + merchantCode + ", 業種=" + categoryCode);
                continue;
            }

            java.math.BigDecimal summaryFee = amount(summary, "FEE-TOTAL-AMT");
            java.math.BigDecimal detailFee = detailFeeTotals.getOrDefault(merchantCode, java.math.BigDecimal.ZERO);
            if (detailFee.signum() > 0 && summaryFee.compareTo(detailFee) > 0) {
                log("明細集計額との差異確認: 加盟店=" + merchantCode + ", 請求=" + summaryFee + ", 明細=" + detailFee);
            }

            ReportKey key = new ReportKey(categoryCode, merchantCode, text(summary, "SETTLE-MONTH"));
            AggregatedAmount aggregated = totals.computeIfAbsent(key, k -> new AggregatedAmount());
            aggregated.transactionAmount = aggregated.transactionAmount.add(amount(summary, "TXN-TOTAL-AMT"));
            aggregated.feeAmount = aggregated.feeAmount.add(summaryFee);
        }

        java.util.List<java.util.Map<String, Object>> prrptF = new java.util.ArrayList<>();
        int sequence = 1;
        for (java.util.Map.Entry<ReportKey, AggregatedAmount> entry : totals.entrySet()) {
            ReportKey key = entry.getKey();
            AggregatedAmount amount = entry.getValue();

            java.util.Map<String, Object> report = new java.util.LinkedHashMap<>();
            report.put("REPORT-ID", "RPT" + businessDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                    + String.format("%05d", sequence++));
            report.put("REPORT-TYPE", REPORT_TYPE_MONTHLY_FEE);
            report.put("BUSINESS-DT", businessDate);
            report.put("MERCHANT-CODE", key.merchantCode);
            report.put("OUTPUT-PATH", "/var/reports/fee/" + key.settleMonth + "/"
                    + key.categoryCode + "/" + key.merchantCode + ".csv");
            report.put("STATUS", REPORT_STATUS_CREATED);
            report.put("TXN-TOTAL-AMT", amount.transactionAmount);
            report.put("FEE-TOTAL-AMT", amount.feeAmount);
            prrptF.add(report);
        }
        return prrptF;
    }

    private static java.util.Map<String, java.util.Map<String, Object>> indexBy(
            java.util.List<java.util.Map<String, Object>> records, String keyName) {
        java.util.Map<String, java.util.Map<String, Object>> index = new java.util.HashMap<>();
        for (java.util.Map<String, Object> record : records) {
            String key = text(record, keyName);
            if (!key.isEmpty()) {
                index.put(key, record);
            }
        }
        return index;
    }

    private static java.util.Map<String, java.math.BigDecimal> sumFeeByMerchant(
            java.util.List<java.util.Map<String, Object>> pffeeF) {
        java.util.Map<String, java.math.BigDecimal> totals = new java.util.HashMap<>();
        for (java.util.Map<String, Object> record : pffeeF) {
            String merchantCode = text(record, "MERCHANT-CODE");
            java.math.BigDecimal feeAmount = amount(record, "FEE-AMT");
            totals.merge(merchantCode, feeAmount, java.math.BigDecimal::add);
        }
        return totals;
    }

    private static String formatPrRptF(java.util.Map<String, Object> record) {
        return record.get("REPORT-ID") + ","
                + record.get("REPORT-TYPE") + ","
                + record.get("BUSINESS-DT") + ","
                + record.get("MERCHANT-CODE") + ","
                + record.get("OUTPUT-PATH") + ","
                + record.get("STATUS");
    }

    private static String text(java.util.Map<String, Object> record, String key) {
        Object value = record.get(key);
        return value == null ? "" : String.valueOf(value).trim();
    }

    private static java.math.BigDecimal amount(java.util.Map<String, Object> record, String key) {
        Object value = record.get(key);
        if (value instanceof java.math.BigDecimal) {
            return (java.math.BigDecimal) value;
        }
        if (value instanceof Number) {
            return new java.math.BigDecimal(value.toString());
        }
        String text = text(record, key);
        return text.isEmpty() ? java.math.BigDecimal.ZERO : new java.math.BigDecimal(text);
    }

    private static void log(String message) {
        System.err.println("[手数料レポート生成] " + message);
    }

    private static java.util.List<java.util.Map<String, Object>> createPffeeF() {
        java.util.List<java.util.Map<String, Object>> records = new java.util.ArrayList<>();
        // FE-MDR-RATE は規程・MdrFeeEngine 由来の参照のみを保持し、料率値は当帳票では持たない。
        records.add(record("FEE-ID", "F000001", "MERCHANT-CODE", "M10001", "TXN-AMT", "1250000", "MDR-RATE", "規程:C1:MdrFeeEngine", "FEE-AMT", "40625"));
        records.add(record("FEE-ID", "F000002", "MERCHANT-CODE", "M10002", "TXN-AMT", "820000", "MDR-RATE", "規程:C2:MdrFeeEngine", "FEE-AMT", "22960"));
        records.add(record("FEE-ID", "F000003", "MERCHANT-CODE", "M10003", "TXN-AMT", "410000", "MDR-RATE", "規程:C3:MdrFeeEngine", "FEE-AMT", "4920"));
        records.add(record("FEE-ID", "F000004", "MERCHANT-CODE", "M10004", "TXN-AMT", "2760000", "MDR-RATE", "規程:C4:MdrFeeEngine", "FEE-AMT", "99360"));
        records.add(record("FEE-ID", "F000005", "MERCHANT-CODE", "M10005", "TXN-AMT", "670000", "MDR-RATE", "規程:C5:MdrFeeEngine", "FEE-AMT", "32160"));
        return records;
    }

    private static java.util.List<java.util.Map<String, Object>> createPfsumF() {
        java.util.List<java.util.Map<String, Object>> records = new java.util.ArrayList<>();
        records.add(record("SUMMARY-ID", "S202606001", "MERCHANT-CODE", "M10001", "SETTLE-MONTH", "202606", "TXN-COUNT", 384, "TXN-TOTAL-AMT", "1250000", "FEE-TOTAL-AMT", "40625", "NET-SETTLE-AMT", "1209375"));
        records.add(record("SUMMARY-ID", "S202606002", "MERCHANT-CODE", "M10002", "SETTLE-MONTH", "202606", "TXN-COUNT", 219, "TXN-TOTAL-AMT", "820000", "FEE-TOTAL-AMT", "22960", "NET-SETTLE-AMT", "797040"));
        records.add(record("SUMMARY-ID", "S202606003", "MERCHANT-CODE", "M10003", "SETTLE-MONTH", "202606", "TXN-COUNT", 97, "TXN-TOTAL-AMT", "410000", "FEE-TOTAL-AMT", "4920", "NET-SETTLE-AMT", "405080"));
        records.add(record("SUMMARY-ID", "S202606004", "MERCHANT-CODE", "M10004", "SETTLE-MONTH", "202606", "TXN-COUNT", 641, "TXN-TOTAL-AMT", "2760000", "FEE-TOTAL-AMT", "99360", "NET-SETTLE-AMT", "2660640"));
        records.add(record("SUMMARY-ID", "S202606005", "MERCHANT-CODE", "M10005", "SETTLE-MONTH", "202606", "TXN-COUNT", 158, "TXN-TOTAL-AMT", "670000", "FEE-TOTAL-AMT", "32160", "NET-SETTLE-AMT", "637840"));
        return records;
    }

    private static java.util.List<java.util.Map<String, Object>> createPfmerF() {
        java.util.List<java.util.Map<String, Object>> records = new java.util.ArrayList<>();
        records.add(record("MERCHANT-CODE", "M10001", "MERCHANT-NAME", "東都リテール日本橋店", "MER-CATEGORY", "C1", "MER-STATUS", "01"));
        records.add(record("MERCHANT-CODE", "M10002", "MERCHANT-NAME", "銀座ダイニング七丁目", "MER-CATEGORY", "C2", "MER-STATUS", "01"));
        records.add(record("MERCHANT-CODE", "M10003", "MERCHANT-NAME", "北関東水道収納窓口", "MER-CATEGORY", "C3", "MER-STATUS", "01"));
        records.add(record("MERCHANT-CODE", "M10004", "MERCHANT-NAME", "ミライ通販センター", "MER-CATEGORY", "C4", "MER-STATUS", "01"));
        records.add(record("MERCHANT-CODE", "M10005", "MERCHANT-NAME", "新宿ナイトサービス", "MER-CATEGORY", "C5", "MER-STATUS", "02"));
        return records;
    }

    private static java.util.List<java.util.Map<String, Object>> createPmcatF() {
        java.util.List<java.util.Map<String, Object>> records = new java.util.ArrayList<>();
        records.add(record("CATEGORY-CODE", "C1", "CATEGORY-NAME", "一般物販", "RISK-RANK", "A", "TAXABLE-FLAG", "1", "ACTIVE-FLAG", "1", "LAST-UPDATE-DT", "2026-04-01"));
        records.add(record("CATEGORY-CODE", "C2", "CATEGORY-NAME", "飲食", "RISK-RANK", "B", "TAXABLE-FLAG", "1", "ACTIVE-FLAG", "1", "LAST-UPDATE-DT", "2026-04-01"));
        records.add(record("CATEGORY-CODE", "C3", "CATEGORY-NAME", "公共・公金", "RISK-RANK", "A", "TAXABLE-FLAG", "0", "ACTIVE-FLAG", "1", "LAST-UPDATE-DT", "2026-04-01"));
        records.add(record("CATEGORY-CODE", "C4", "CATEGORY-NAME", "EC・通信販売", "RISK-RANK", "B", "TAXABLE-FLAG", "1", "ACTIVE-FLAG", "1", "LAST-UPDATE-DT", "2026-04-01"));
        records.add(record("CATEGORY-CODE", "C5", "CATEGORY-NAME", "高リスク業種", "RISK-RANK", "D", "TAXABLE-FLAG", "1", "ACTIVE-FLAG", "1", "LAST-UPDATE-DT", "2026-04-01"));
        return records;
    }

    private static java.util.Map<String, Object> record(Object... values) {
        java.util.Map<String, Object> record = new java.util.LinkedHashMap<>();
        for (int i = 0; i < values.length; i += 2) {
            record.put(String.valueOf(values[i]), values[i + 1]);
        }
        return record;
    }

    private static final class AggregatedAmount {
        private java.math.BigDecimal transactionAmount = java.math.BigDecimal.ZERO;
        private java.math.BigDecimal feeAmount = java.math.BigDecimal.ZERO;
    }

    private static final class ReportKey implements Comparable<ReportKey> {
        private final String categoryCode;
        private final String merchantCode;
        private final String settleMonth;

        private ReportKey(String categoryCode, String merchantCode, String settleMonth) {
            this.categoryCode = categoryCode;
            this.merchantCode = merchantCode;
            this.settleMonth = settleMonth;
        }

        @Override
        public int compareTo(ReportKey other) {
            int result = this.categoryCode.compareTo(other.categoryCode);
            if (result != 0) {
                return result;
            }
            result = this.merchantCode.compareTo(other.merchantCode);
            if (result != 0) {
                return result;
            }
            return this.settleMonth.compareTo(other.settleMonth);
        }
    }
}
