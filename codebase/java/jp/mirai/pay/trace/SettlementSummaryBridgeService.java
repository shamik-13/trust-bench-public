package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2025/02/18  みらいペイ システム部 精算・連携チーム  精算サマリ連携サービス初版
 */
public class SettlementSummaryBridgeService {
    private static final java.math.BigDecimal FEE_RATE = new java.math.BigDecimal("0.0300");
    private static final java.math.BigDecimal ZERO = java.math.BigDecimal.ZERO;
    private static final String 正常 = "正常";
    private static final String 警告 = "警告";
    private static final String 異常 = "異常";

    private record Pcsumf(
            String merchantCode,
            java.time.LocalDate settleDate,
            String settleKbn,
            int txnCount,
            java.math.BigDecimal totalAmt,
            java.math.BigDecimal carryAmt) {
    }

    private record Ptkeyf(
            String traceKey,
            String holdId,
            String capId,
            String settleTxnId,
            String merchantCode,
            String checkResult) {
    }

    private record Pjrepf(
            String reportId,
            String merchantCode,
            java.time.LocalDate settleDate,
            java.math.BigDecimal grossAmt,
            java.math.BigDecimal feeAmt,
            java.math.BigDecimal netAmt,
            String reportStatus) {
    }

    public void run() {
        java.util.List<Pcsumf> pcsumf = java.util.List.of(
                new Pcsumf("MRC000001", java.time.LocalDate.of(2025, 3, 24), "10", 128,
                        new java.math.BigDecimal("2840150"), new java.math.BigDecimal("0")),
                new Pcsumf("MRC000002", java.time.LocalDate.of(2025, 3, 24), "10", 42,
                        new java.math.BigDecimal("918420"), new java.math.BigDecimal("12500")),
                new Pcsumf("MRC000003", java.time.LocalDate.of(2025, 3, 24), "20", 0,
                        new java.math.BigDecimal("0"), new java.math.BigDecimal("0")),
                new Pcsumf("MRC000004", java.time.LocalDate.of(2025, 3, 24), "10", 17,
                        new java.math.BigDecimal("335000"), new java.math.BigDecimal("-5000"))
        );

        java.util.List<Ptkeyf> ptkeyf = java.util.List.of(
                new Ptkeyf("TK-000001", "HD-1001", "CP-9001", "ST-7001", "MRC000001", "OK"),
                new Ptkeyf("TK-000002", "HD-1002", "CP-9002", "ST-7002", "MRC000001", "OK"),
                new Ptkeyf("TK-000003", "HD-1003", "CP-9003", "ST-7003", "MRC000002", "WARN"),
                new Ptkeyf("TK-000004", "HD-1004", "", "ST-7004", "MRC000002", "NG"),
                new Ptkeyf("TK-000005", "HD-1005", "CP-9005", "ST-7005", "MRC000004", "WARN")
        );

        java.util.List<Pjrepf> reports = buildReports(pcsumf, ptkeyf);
        for (Pjrepf report : reports) {
            System.out.println(formatReport(report));
        }
    }

    private static java.util.List<Pjrepf> buildReports(java.util.List<Pcsumf> pcsumf, java.util.List<Ptkeyf> ptkeyf) {
        java.util.Map<String, Integer> warningCountByMerchant = countWarnings(ptkeyf);
        java.util.List<Pjrepf> reports = new java.util.ArrayList<>();

        for (Pcsumf source : pcsumf) {
            validatePcsumf(source);

            java.math.BigDecimal grossAmt = source.totalAmt.add(source.carryAmt);
            java.math.BigDecimal feeAmt = calculateFee(source);
            java.math.BigDecimal netAmt = grossAmt.subtract(feeAmt);
            int warningCount = warningCountByMerchant.getOrDefault(source.merchantCode, 0);
            String status = decideStatus(source, warningCount);

            reports.add(new Pjrepf(
                    createReportId(source),
                    source.merchantCode,
                    source.settleDate,
                    grossAmt,
                    feeAmt,
                    netAmt,
                    status));
        }

        reports.sort(java.util.Comparator
                .comparing(Pjrepf::settleDate)
                .thenComparing(Pjrepf::merchantCode));
        return java.util.Collections.unmodifiableList(reports);
    }

    private static java.util.Map<String, Integer> countWarnings(java.util.List<Ptkeyf> ptkeyf) {
        java.util.Map<String, Integer> warningCountByMerchant = new java.util.HashMap<>();

        for (Ptkeyf key : ptkeyf) {
            validatePtkeyf(key);
            if (isWarningResult(key.checkResult)) {
                warningCountByMerchant.merge(key.merchantCode, 1, Integer::sum);
            }
        }

        return warningCountByMerchant;
    }

    private static java.math.BigDecimal calculateFee(Pcsumf source) {
        if ("20".equals(source.settleKbn) || source.totalAmt.signum() == 0) {
            return ZERO;
        }
        return source.totalAmt.multiply(FEE_RATE).setScale(0, java.math.RoundingMode.DOWN);
    }

    private static String decideStatus(Pcsumf source, int warningCount) {
        if (source.txnCount == 0 && source.totalAmt.signum() == 0 && source.carryAmt.signum() == 0) {
            return 正常;
        }
        if (warningCount > 0 || source.carryAmt.signum() < 0) {
            return 警告 + warningCount;
        }
        if (source.txnCount <= 0 || source.totalAmt.signum() < 0) {
            return 異常;
        }
        return 正常;
    }

    private static boolean isWarningResult(String checkResult) {
        return "WARN".equals(checkResult) || "NG".equals(checkResult) || "保留".equals(checkResult) || "不一致".equals(checkResult);
    }

    private static void validatePcsumf(Pcsumf source) {
        requireText(source.merchantCode, "加盟店コード");
        java.util.Objects.requireNonNull(source.settleDate, "精算日");
        requireText(source.settleKbn, "精算区分");
        java.util.Objects.requireNonNull(source.totalAmt, "合計金額");
        java.util.Objects.requireNonNull(source.carryAmt, "繰越金額");

        if (source.txnCount < 0) {
            throw new IllegalArgumentException("取引件数が負数です: " + source.merchantCode);
        }
        if (!"10".equals(source.settleKbn) && !"20".equals(source.settleKbn)) {
            throw new IllegalArgumentException("精算区分が不正です: " + source.merchantCode + "/" + source.settleKbn);
        }
    }

    private static void validatePtkeyf(Ptkeyf key) {
        requireText(key.traceKey, "連携キー");
        requireText(key.holdId, "保留ID");
        requireText(key.settleTxnId, "精算取引ID");
        requireText(key.merchantCode, "加盟店コード");
        requireText(key.checkResult, "検査結果");

        if ("OK".equals(key.checkResult) && isBlank(key.capId)) {
            throw new IllegalArgumentException("正常キーに売上IDがありません: " + key.traceKey);
        }
    }

    private static String createReportId(Pcsumf source) {
        return "REP-" + source.settleDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE) + "-" + source.merchantCode;
    }

    private static String formatReport(Pjrepf report) {
        return String.join(",",
                report.reportId,
                report.merchantCode,
                report.settleDate.toString(),
                report.grossAmt.toPlainString(),
                report.feeAmt.toPlainString(),
                report.netAmt.toPlainString(),
                report.reportStatus);
    }

    private static void requireText(String value, String name) {
        if (isBlank(value)) {
            throw new IllegalArgumentException(name + "が未設定です");
        }
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }
}
