package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数    年月日      担当                              概要
 * 1.00    2025/02/03  みらいペイ システム部 精算・連携チーム  初版作成
 */
public class SettlementReportService {
    private static final java.math.BigDecimal HUNDRED = new java.math.BigDecimal("100");
    private static final java.time.format.DateTimeFormatter DATE_FMT = java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    public void run() {
        java.util.List<java.util.Map<String, Object>> pcSumF = pcSumF();
        java.util.List<java.util.Map<String, Object>> pcDtlF = pcDtlF();
        java.util.List<java.util.Map<String, Object>> pcKbnF = pcKbnF();
        java.util.Map<String, java.util.Map<String, Object>> pjMstF = pjMstF();

        java.util.Map<String, java.util.List<java.util.Map<String, Object>>> detailIndex = new java.util.HashMap<>();
        for (java.util.Map<String, Object> detail : pcDtlF) {
            if (!"未出力".equals(s(detail, "OUTPUT-STATUS"))) {
                continue;
            }
            String key = s(detail, "MERCHANT-CODE") + "|" + s(detail, "SETTLE-KBN");
            detailIndex.computeIfAbsent(key, k -> new java.util.ArrayList<>()).add(detail);
        }

        java.util.List<java.util.Map<String, Object>> pjRepF = new java.util.ArrayList<>();
        int reportSeq = 1;

        for (java.util.Map<String, Object> sum : pcSumF) {
            String merchantCode = s(sum, "MERCHANT-CODE");
            String settleDateText = s(sum, "SETTLE-DATE");
            String settleKbn = s(sum, "SETTLE-KBN");
            java.time.LocalDate settleDate = java.time.LocalDate.parse(settleDateText, DATE_FMT);

            java.util.Map<String, Object> merchant = pjMstF.get(merchantCode);
            if (merchant == null || !"1".equals(s(merchant, "ACTIVE-FLAG"))) {
                continue;
            }

            java.util.Map<String, Object> kbn = effectiveKbn(pcKbnF, settleKbn, settleDate);
            if (kbn == null || !"1".equals(s(kbn, "NETTABLE-FLAG"))) {
                continue;
            }

            long summaryCount = l(sum, "TXN-COUNT");
            java.math.BigDecimal totalAmt = bd(sum, "TOTAL-AMT");
            java.math.BigDecimal carryAmt = bd(sum, "CARRY-AMT");
            java.math.BigDecimal grossAmt = totalAmt.add(carryAmt);

            java.math.BigDecimal feeRate = bd(kbn, "FEE-RATE");
            java.math.BigDecimal feeAmt = grossAmt.multiply(feeRate)
                    .divide(HUNDRED, 0, java.math.RoundingMode.HALF_UP);
            java.math.BigDecimal netAmt = grossAmt.subtract(feeAmt);

            String detailKey = merchantCode + "|" + settleKbn;
            java.util.List<java.util.Map<String, Object>> details =
                    detailIndex.getOrDefault(detailKey, java.util.Collections.emptyList());

            java.math.BigDecimal detailAmt = java.math.BigDecimal.ZERO;
            long detailCount = 0L;
            for (java.util.Map<String, Object> detail : details) {
                detailAmt = detailAmt.add(bd(detail, "TXN-AMT"));
                detailCount++;
            }

            String status = "作成済";
            String riskRank = s(merchant, "RISK-RANK");
            if ("高".equals(riskRank)) {
                if (summaryCount != detailCount || totalAmt.compareTo(detailAmt) != 0) {
                    status = "審査待";
                }
            } else if (grossAmt.signum() < 0 || summaryCount < 0) {
                status = "保留";
            }

            java.util.Map<String, Object> report = new java.util.LinkedHashMap<>();
            report.put("REPORT-ID", String.format("R%08d", reportSeq++));
            report.put("MERCHANT-CODE", merchantCode);
            report.put("SETTLE-DATE", settleDateText);
            report.put("GROSS-AMT", grossAmt);
            report.put("FEE-AMT", feeAmt);
            report.put("NET-AMT", netAmt);
            report.put("REPORT-STATUS", status);
            pjRepF.add(report);
        }

        System.out.println("REPORT-ID,MERCHANT-CODE,SETTLE-DATE,GROSS-AMT,FEE-AMT,NET-AMT,REPORT-STATUS");
        for (java.util.Map<String, Object> report : pjRepF) {
            System.out.println(
                    s(report, "REPORT-ID") + "," +
                    s(report, "MERCHANT-CODE") + "," +
                    s(report, "SETTLE-DATE") + "," +
                    bd(report, "GROSS-AMT").toPlainString() + "," +
                    bd(report, "FEE-AMT").toPlainString() + "," +
                    bd(report, "NET-AMT").toPlainString() + "," +
                    s(report, "REPORT-STATUS"));
        }
    }

    private static java.util.Map<String, Object> effectiveKbn(
            java.util.List<java.util.Map<String, Object>> pcKbnF,
            String settleKbn,
            java.time.LocalDate settleDate) {
        java.util.Map<String, Object> selected = null;
        for (java.util.Map<String, Object> kbn : pcKbnF) {
            if (!settleKbn.equals(s(kbn, "SETTLE-KBN"))) {
                continue;
            }
            java.time.LocalDate from = java.time.LocalDate.parse(s(kbn, "VALID-FROM"), DATE_FMT);
            java.time.LocalDate to = java.time.LocalDate.parse(s(kbn, "VALID-TO"), DATE_FMT);
            if ((settleDate.isEqual(from) || settleDate.isAfter(from))
                    && (settleDate.isEqual(to) || settleDate.isBefore(to))) {
                selected = kbn;
            }
        }
        return selected;
    }

    private static java.util.List<java.util.Map<String, Object>> pcSumF() {
        java.util.List<java.util.Map<String, Object>> rows = new java.util.ArrayList<>();
        rows.add(row("MERCHANT-CODE", "M10001", "SETTLE-DATE", "20260626", "SETTLE-KBN", "01",
                "TXN-COUNT", 3L, "TOTAL-AMT", "128400", "CARRY-AMT", "0"));
        rows.add(row("MERCHANT-CODE", "M10002", "SETTLE-DATE", "20260626", "SETTLE-KBN", "01",
                "TXN-COUNT", 2L, "TOTAL-AMT", "74200", "CARRY-AMT", "1200"));
        rows.add(row("MERCHANT-CODE", "M90001", "SETTLE-DATE", "20260626", "SETTLE-KBN", "02",
                "TXN-COUNT", 4L, "TOTAL-AMT", "331000", "CARRY-AMT", "0"));
        rows.add(row("MERCHANT-CODE", "M20003", "SETTLE-DATE", "20260626", "SETTLE-KBN", "03",
                "TXN-COUNT", 1L, "TOTAL-AMT", "9800", "CARRY-AMT", "-300"));
        return rows;
    }

    private static java.util.List<java.util.Map<String, Object>> pcDtlF() {
        java.util.List<java.util.Map<String, Object>> rows = new java.util.ArrayList<>();
        rows.add(row("DETAIL-ID", "D000001", "SETTLE-TXN-ID", "T010001", "MERCHANT-CODE", "M10001",
                "TXN-AMT", "42000", "SETTLE-KBN", "01", "OUTPUT-STATUS", "未出力"));
        rows.add(row("DETAIL-ID", "D000002", "SETTLE-TXN-ID", "T010002", "MERCHANT-CODE", "M10001",
                "TXN-AMT", "36400", "SETTLE-KBN", "01", "OUTPUT-STATUS", "未出力"));
        rows.add(row("DETAIL-ID", "D000003", "SETTLE-TXN-ID", "T010003", "MERCHANT-CODE", "M10001",
                "TXN-AMT", "50000", "SETTLE-KBN", "01", "OUTPUT-STATUS", "未出力"));
        rows.add(row("DETAIL-ID", "D000004", "SETTLE-TXN-ID", "T020001", "MERCHANT-CODE", "M10002",
                "TXN-AMT", "74200", "SETTLE-KBN", "01", "OUTPUT-STATUS", "未出力"));
        rows.add(row("DETAIL-ID", "D000005", "SETTLE-TXN-ID", "T900001", "MERCHANT-CODE", "M90001",
                "TXN-AMT", "100000", "SETTLE-KBN", "02", "OUTPUT-STATUS", "未出力"));
        rows.add(row("DETAIL-ID", "D000006", "SETTLE-TXN-ID", "T900002", "MERCHANT-CODE", "M90001",
                "TXN-AMT", "99000", "SETTLE-KBN", "02", "OUTPUT-STATUS", "未出力"));
        rows.add(row("DETAIL-ID", "D000007", "SETTLE-TXN-ID", "T900003", "MERCHANT-CODE", "M90001",
                "TXN-AMT", "132000", "SETTLE-KBN", "02", "OUTPUT-STATUS", "未出力"));
        rows.add(row("DETAIL-ID", "D000008", "SETTLE-TXN-ID", "T030001", "MERCHANT-CODE", "M20003",
                "TXN-AMT", "9800", "SETTLE-KBN", "03", "OUTPUT-STATUS", "未出力"));
        return rows;
    }

    private static java.util.List<java.util.Map<String, Object>> pcKbnF() {
        java.util.List<java.util.Map<String, Object>> rows = new java.util.ArrayList<>();
        rows.add(row("SETTLE-KBN", "01", "KBN-NAME", "通常精算", "NETTABLE-FLAG", "1",
                "FEE-RATE", "2.40", "VALID-FROM", "20260101", "VALID-TO", "20261231"));
        rows.add(row("SETTLE-KBN", "02", "KBN-NAME", "早期精算", "NETTABLE-FLAG", "1",
                "FEE-RATE", "3.10", "VALID-FROM", "20260101", "VALID-TO", "20261231"));
        rows.add(row("SETTLE-KBN", "03", "KBN-NAME", "調整精算", "NETTABLE-FLAG", "1",
                "FEE-RATE", "1.25", "VALID-FROM", "20260101", "VALID-TO", "20261231"));
        return rows;
    }

    private static java.util.Map<String, java.util.Map<String, Object>> pjMstF() {
        java.util.Map<String, java.util.Map<String, Object>> rows = new java.util.LinkedHashMap<>();
        putMerchant(rows, "M10001", "東京中央ストア", "0001", "1234567", "1", "低");
        putMerchant(rows, "M10002", "北浜薬品", "0005", "3344556", "1", "中");
        putMerchant(rows, "M90001", "湾岸チケット", "0033", "7788990", "1", "高");
        putMerchant(rows, "M20003", "札幌市場", "0017", "1122334", "1", "低");
        return rows;
    }

    private static void putMerchant(
            java.util.Map<String, java.util.Map<String, Object>> rows,
            String merchantCode,
            String merchantName,
            String bankCode,
            String accountNo,
            String activeFlag,
            String riskRank) {
        rows.put(merchantCode, row("MERCHANT-CODE", merchantCode, "MERCHANT-NAME", merchantName,
                "BANK-CODE", bankCode, "ACCOUNT-NO", accountNo,
                "ACTIVE-FLAG", activeFlag, "RISK-RANK", riskRank));
    }

    private static java.util.Map<String, Object> row(Object... values) {
        java.util.Map<String, Object> row = new java.util.LinkedHashMap<>();
        for (int i = 0; i < values.length; i += 2) {
            row.put(String.valueOf(values[i]), values[i + 1]);
        }
        return row;
    }

    private static String s(java.util.Map<String, Object> row, String key) {
        Object value = row.get(key);
        return value == null ? "" : String.valueOf(value);
    }

    private static long l(java.util.Map<String, Object> row, String key) {
        Object value = row.get(key);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        return Long.parseLong(String.valueOf(value));
    }

    private static java.math.BigDecimal bd(java.util.Map<String, Object> row, String key) {
        Object value = row.get(key);
        if (value instanceof java.math.BigDecimal) {
            return (java.math.BigDecimal) value;
        }
        return new java.math.BigDecimal(String.valueOf(value));
    }
}
