package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2024/10/15  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class FeeDetailExportService {
    private static final String REPORT_TYPE = "FEE_DETAIL";
    private static final String CHARGEABLE_STATUS = "01";
    private static final java.nio.charset.Charset CSV_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    private FeeDetailExportService() {
    }

    public static void main(String[] a) {
        java.time.LocalDate businessDate = java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo"));
        java.time.YearMonth billingMonth = java.time.YearMonth.from(businessDate.minusMonths(1));
        java.nio.file.Path outputDirectory = java.nio.file.Paths.get("build", "fee-detail-export", billingMonth.toString());

        java.util.List<FeeDetail> feeDetails = createBenchmarkPffeef(billingMonth);
        java.util.Map<String, Merchant> merchants = createBenchmarkPfmerf();

        try {
            ExportResult result = exportFeeDetails(businessDate, billingMonth, outputDirectory, feeDetails, merchants);
            System.out.println("手数料明細出力完了 REPORT-ID=" + result.report.reportId
                    + " MERCHANT-CODE=" + result.report.merchantCode
                    + " OUTPUT-PATH=" + result.report.outputPath
                    + " 件数=" + result.detailCount
                    + " STATUS=" + result.report.status);
        } catch (RuntimeException e) {
            System.err.println("手数料明細出力異常 " + e.getMessage());
            throw e;
        }
    }

    private static ExportResult exportFeeDetails(
            java.time.LocalDate businessDate,
            java.time.YearMonth billingMonth,
            java.nio.file.Path outputDirectory,
            java.util.List<FeeDetail> feeDetails,
            java.util.Map<String, Merchant> merchants) {
        requireNonNull(businessDate, "業務日");
        requireNonNull(billingMonth, "請求月");
        requireNonNull(outputDirectory, "出力ディレクトリ");
        requireNonNull(feeDetails, "PFFEEF");
        requireNonNull(merchants, "PFMERF");

        java.util.List<FeeDetail> chargeable = new java.util.ArrayList<>();
        for (FeeDetail detail : feeDetails) {
            validateFeeDetail(detail);
            Merchant merchant = merchants.get(detail.merchantCode);
            if (merchant == null) {
                throw new IllegalArgumentException("加盟店マスタ未登録 MERCHANT-CODE=" + detail.merchantCode);
            }
            validateMerchant(merchant);
            if (CHARGEABLE_STATUS.equals(merchant.status) && billingMonth.equals(java.time.YearMonth.from(detail.appliedDate))) {
                chargeable.add(detail);
            }
        }

        chargeable.sort(java.util.Comparator
                .comparing((FeeDetail d) -> d.merchantCode)
                .thenComparing(d -> d.appliedDate)
                .thenComparing(d -> d.feeId));

        String representativeMerchantCode = chargeable.isEmpty() ? "ALL" : chargeable.get(0).merchantCode;
        String reportId = "RPT-FEE-" + businessDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + "-" + billingMonth.toString().replace("-", "");
        java.nio.file.Path outputPath = outputDirectory.resolve(reportId + ".csv");

        try {
            java.nio.file.Files.createDirectories(outputDirectory);
            writeCsv(outputPath, chargeable, merchants);
        } catch (java.io.IOException e) {
            throw new IllegalStateException("明細CSV出力失敗 PATH=" + outputPath, e);
        }

        ReportRecord report = new ReportRecord(
                reportId,
                REPORT_TYPE,
                businessDate,
                representativeMerchantCode,
                outputPath.toAbsolutePath().normalize().toString(),
                "00");

        return new ExportResult(report, chargeable.size());
    }

    private static void writeCsv(
            java.nio.file.Path outputPath,
            java.util.List<FeeDetail> details,
            java.util.Map<String, Merchant> merchants) throws java.io.IOException {
        try (java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(outputPath, CSV_CHARSET)) {
            writer.write("FEE-ID,MERCHANT-CODE,MERCHANT-NAME,MER-CATEGORY,TXN-AMT,MDR-RATE,FEE-AMT,APPLIED-DATE");
            writer.newLine();

            java.math.BigDecimal totalTxnAmount = java.math.BigDecimal.ZERO;
            java.math.BigDecimal totalFeeAmount = java.math.BigDecimal.ZERO;
            for (FeeDetail detail : details) {
                Merchant merchant = merchants.get(detail.merchantCode);
                totalTxnAmount = totalTxnAmount.add(detail.transactionAmount);
                totalFeeAmount = totalFeeAmount.add(detail.feeAmount);

                writer.write(csv(detail.feeId));
                writer.write(',');
                writer.write(csv(detail.merchantCode));
                writer.write(',');
                writer.write(csv(merchant.name));
                writer.write(',');
                writer.write(csv(merchant.category));
                writer.write(',');
                writer.write(detail.transactionAmount.toPlainString());
                writer.write(',');
                writer.write(csv(detail.mdrRateDisplay));
                writer.write(',');
                writer.write(detail.feeAmount.toPlainString());
                writer.write(',');
                writer.write(detail.appliedDate.toString());
                writer.newLine();
            }

            writer.write("TOTAL,,,,");
            writer.write(totalTxnAmount.toPlainString());
            writer.write(",,");
            writer.write(totalFeeAmount.toPlainString());
            writer.write(",");
            writer.newLine();
        }
    }

    private static java.util.List<FeeDetail> createBenchmarkPffeef(java.time.YearMonth billingMonth) {
        java.time.LocalDate d1 = billingMonth.atDay(5);
        java.time.LocalDate d2 = billingMonth.atDay(15);
        java.time.LocalDate d3 = billingMonth.atEndOfMonth();

        java.util.List<FeeDetail> rows = new java.util.ArrayList<>();
        rows.add(new FeeDetail("FEE-000001", "M0001001", bd("128000"), "既存明細表示値", bd("3200"), d1));
        rows.add(new FeeDetail("FEE-000002", "M0001001", bd("74200"), "既存明細表示値", bd("1855"), d2));
        rows.add(new FeeDetail("FEE-000003", "M0002001", bd("33600"), "既存明細表示値", bd("672"), d1));
        rows.add(new FeeDetail("FEE-000004", "M0003001", bd("980000"), "既存明細表示値", bd("9800"), d3));
        rows.add(new FeeDetail("FEE-000005", "M0004001", bd("218450"), "既存明細表示値", bd("6554"), d2));
        rows.add(new FeeDetail("FEE-000006", "M0005001", bd("51200"), "既存明細表示値", bd("2560"), d1));
        rows.add(new FeeDetail("FEE-000007", "M0009001", bd("44100"), "既存明細表示値", bd("1323"), d2));
        return java.util.Collections.unmodifiableList(rows);
    }

    private static java.util.Map<String, Merchant> createBenchmarkPfmerf() {
        java.util.Map<String, Merchant> rows = new java.util.LinkedHashMap<>();
        rows.put("M0001001", new Merchant("M0001001", "未来百貨店日本橋店", "C1", "01"));
        rows.put("M0002001", new Merchant("M0002001", "青葉食堂丸の内", "C2", "01"));
        rows.put("M0003001", new Merchant("M0003001", "東都水道料金センター", "C3", "01"));
        rows.put("M0004001", new Merchant("M0004001", "ミライ通販倉庫", "C4", "01"));
        rows.put("M0005001", new Merchant("M0005001", "新宿予約サービス", "C5", "02"));
        rows.put("M0009001", new Merchant("M0009001", "旧加盟店テスト", "C1", "09"));
        return java.util.Collections.unmodifiableMap(rows);
    }

    private static void validateFeeDetail(FeeDetail detail) {
        requireNonNull(detail, "PFFEEF行");
        requireText(detail.feeId, "FEE-ID");
        requireText(detail.merchantCode, "MERCHANT-CODE");
        requireNonNull(detail.transactionAmount, "TXN-AMT");
        requireText(detail.mdrRateDisplay, "MDR-RATE");
        requireNonNull(detail.feeAmount, "FEE-AMT");
        requireNonNull(detail.appliedDate, "適用日");

        if (detail.transactionAmount.signum() < 0) {
            throw new IllegalArgumentException("取引金額不正 FEE-ID=" + detail.feeId);
        }
        if (detail.feeAmount.signum() < 0) {
            throw new IllegalArgumentException("手数料金額不正 FEE-ID=" + detail.feeId);
        }
        if (detail.feeAmount.compareTo(detail.transactionAmount) > 0) {
            throw new IllegalArgumentException("手数料金額過大 FEE-ID=" + detail.feeId);
        }
    }

    private static void validateMerchant(Merchant merchant) {
        requireNonNull(merchant, "PFMERF行");
        requireText(merchant.code, "MERCHANT-CODE");
        requireText(merchant.name, "MERCHANT-NAME");

        if (!java.util.Arrays.asList("C1", "C2", "C3", "C4", "C5").contains(merchant.category)) {
            throw new IllegalArgumentException("業種区分不正 MERCHANT-CODE=" + merchant.code);
        }
        if (!java.util.Arrays.asList("01", "02", "09").contains(merchant.status)) {
            throw new IllegalArgumentException("加盟店状態不正 MERCHANT-CODE=" + merchant.code);
        }
    }

    private static String csv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') < 0 && v.indexOf('"') < 0 && v.indexOf('\n') < 0 && v.indexOf('\r') < 0) {
            return v;
        }
        return "\"" + v.replace("\"", "\"\"") + "\"";
    }

    private static java.math.BigDecimal bd(String value) {
        return new java.math.BigDecimal(value);
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + " 未設定");
        }
    }

    private static void requireNonNull(Object value, String name) {
        if (value == null) {
            throw new IllegalArgumentException(name + " 未設定");
        }
    }

    private static final class FeeDetail {
        final String feeId;
        final String merchantCode;
        final java.math.BigDecimal transactionAmount;
        final String mdrRateDisplay;
        final java.math.BigDecimal feeAmount;
        final java.time.LocalDate appliedDate;

        FeeDetail(
                String feeId,
                String merchantCode,
                java.math.BigDecimal transactionAmount,
                String mdrRateDisplay,
                java.math.BigDecimal feeAmount,
                java.time.LocalDate appliedDate) {
            this.feeId = feeId;
            this.merchantCode = merchantCode;
            this.transactionAmount = transactionAmount;
            this.mdrRateDisplay = mdrRateDisplay;
            this.feeAmount = feeAmount;
            this.appliedDate = appliedDate;
        }
    }

    private static final class Merchant {
        final String code;
        final String name;
        final String category;
        final String status;

        Merchant(String code, String name, String category, String status) {
            this.code = code;
            this.name = name;
            this.category = category;
            this.status = status;
        }
    }

    private static final class ReportRecord {
        final String reportId;
        final String reportType;
        final java.time.LocalDate businessDate;
        final String merchantCode;
        final String outputPath;
        final String status;

        ReportRecord(
                String reportId,
                String reportType,
                java.time.LocalDate businessDate,
                String merchantCode,
                String outputPath,
                String status) {
            this.reportId = reportId;
            this.reportType = reportType;
            this.businessDate = businessDate;
            this.merchantCode = merchantCode;
            this.outputPath = outputPath;
            this.status = status;
        }
    }

    private static final class ExportResult {
        final ReportRecord report;
        final int detailCount;

        ExportResult(ReportRecord report, int detailCount) {
            this.report = report;
            this.detailCount = detailCount;
        }
    }
}
