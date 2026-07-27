package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025/01/14  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class RateNoticeReportService {
    private static final java.time.format.DateTimeFormatter DT = java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
    private static final String STATUS_CHARGEABLE = "01";
    private static final String REPORT_TYPE_SUMMARY = "RATE_NOTICE";
    private static final java.nio.charset.Charset REPORT_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    private RateNoticeReportService() {
    }

    public static void main(String[] a) {
        new RateNoticeReportService().execute(java.time.LocalDate.of(2026, 6, 28));
    }

    private void execute(java.time.LocalDate businessDate) {
        assertSharedModelLinked();

        java.util.List<NoticeRecord> notices = loadPmnotf();
        java.util.List<RatePlanRecord> ratePlans = loadPmratf();
        java.util.Map<String, MerchantRecord> merchants = indexMerchants(loadPfmerf());
        java.util.Map<String, CategoryRecord> categories = indexCategories(loadPmcatf());

        java.util.Map<String, RatePlanRecord> approvedRatePlanByNotice = approvedRatePlanByNotice(ratePlans);
        java.util.List<ReportLine> lines = new java.util.ArrayList<>();
        java.util.Map<AggregateKey, NoticeSummary> summary = new java.util.TreeMap<>();

        for (NoticeRecord notice : notices) {
            MerchantRecord merchant = merchants.get(notice.merchantCode);
            CategoryRecord category = categories.get(notice.categoryCode);
            RatePlanRecord ratePlan = approvedRatePlanByNotice.get(notice.noticeId);

            java.util.List<String> errors = validateNotice(notice, merchant, category, ratePlan);
            if (!errors.isEmpty()) {
                lines.add(ReportLine.error(notice, merchant, category, errors));
            }

            if (merchant != null && category != null) {
                AggregateKey key = new AggregateKey(
                        notice.merchantCode,
                        merchant.merchantCategory,
                        notice.categoryCode,
                        category.categoryName,
                        notice.effectiveDate,
                        notice.channel);
                summary.computeIfAbsent(key, k -> new NoticeSummary()).add(notice.sendStatus, errors.isEmpty());
            }
        }

        for (java.util.Map.Entry<AggregateKey, NoticeSummary> entry : summary.entrySet()) {
            lines.add(ReportLine.summary(entry.getKey(), entry.getValue()));
        }

        lines.sort(java.util.Comparator
                .comparing((ReportLine l) -> l.kind)
                .thenComparing(l -> l.merchantCode)
                .thenComparing(l -> l.categoryCode)
                .thenComparing(l -> l.channel)
                .thenComparing(l -> l.noticeId));

        java.nio.file.Path outputPath = reportPath(businessDate);
        String reportId = "RNR" + businessDate.format(DT);
        ReportWriteStatus status = writePrrptf(reportId, businessDate, outputPath, lines);
        System.out.println(status.toOperatorMessage());
    }

    private void assertSharedModelLinked() {
        String linkedClassName = FeeModel.class.getName();
        if (linkedClassName == null || linkedClassName.trim().isEmpty()) {
            throw new IllegalStateException("共通モデル参照エラー");
        }
    }

    private java.util.List<String> validateNotice(
            NoticeRecord notice,
            MerchantRecord merchant,
            CategoryRecord category,
            RatePlanRecord ratePlan) {
        java.util.List<String> errors = new java.util.ArrayList<>();

        if (merchant == null) {
            errors.add("加盟店未登録");
        } else {
            if (!STATUS_CHARGEABLE.equals(merchant.merchantStatus)) {
                errors.add("請求対象外加盟店");
            }
            if (!merchant.merchantCategory.equals(categoryCodeToMerchantCategory(notice.categoryCode))) {
                errors.add("業種区分不一致");
            }
        }

        if (category == null) {
            errors.add("カテゴリ未登録");
        } else if (!category.activeFlag) {
            errors.add("カテゴリ無効");
        }

        if (ratePlan == null) {
            errors.add("承認済改定未登録");
        } else {
            if (!ratePlan.categoryCode.equals(notice.categoryCode)) {
                errors.add("改定カテゴリ不一致");
            }
            if (!ratePlan.effectiveDate.equals(notice.effectiveDate)) {
                errors.add("有効日不一致");
            }
        }

        if ("未送信".equals(sendStatusName(notice.sendStatus)) || "失敗".equals(sendStatusName(notice.sendStatus))) {
            errors.add("通知未達");
        }

        return errors;
    }

    private java.util.Map<String, MerchantRecord> indexMerchants(java.util.List<MerchantRecord> merchants) {
        java.util.Map<String, MerchantRecord> index = new java.util.HashMap<>();
        for (MerchantRecord merchant : merchants) {
            index.put(merchant.merchantCode, merchant);
        }
        return index;
    }

    private java.util.Map<String, CategoryRecord> indexCategories(java.util.List<CategoryRecord> categories) {
        java.util.Map<String, CategoryRecord> index = new java.util.HashMap<>();
        for (CategoryRecord category : categories) {
            index.put(category.categoryCode, category);
        }
        return index;
    }

    private java.util.Map<String, RatePlanRecord> approvedRatePlanByNotice(java.util.List<RatePlanRecord> ratePlans) {
        java.util.Map<String, RatePlanRecord> index = new java.util.HashMap<>();
        for (RatePlanRecord plan : ratePlans) {
            if ("承認済".equals(plan.approvalStatus)) {
                index.put(plan.noticeId, plan);
            }
        }
        return index;
    }

    private ReportWriteStatus writePrrptf(
            String reportId,
            java.time.LocalDate businessDate,
            java.nio.file.Path outputPath,
            java.util.List<ReportLine> lines) {
        java.util.List<String> rows = new java.util.ArrayList<>();
        rows.add("区分,通知ID,加盟店コード,加盟店名,業種区分,カテゴリコード,カテゴリ名,有効日,通知チャネル,送信済,未送信,失敗,検証結果");
        for (ReportLine line : lines) {
            rows.add(line.toCsvRow());
        }

        try {
            java.nio.file.Files.createDirectories(outputPath.getParent());
            java.nio.file.Files.write(outputPath, rows, REPORT_CHARSET);
            return new ReportWriteStatus(reportId, REPORT_TYPE_SUMMARY, businessDate, "", outputPath.toString(), "正常");
        } catch (java.io.IOException e) {
            return new ReportWriteStatus(reportId, REPORT_TYPE_SUMMARY, businessDate, "", outputPath.toString(), "異常");
        }
    }

    private java.nio.file.Path reportPath(java.time.LocalDate businessDate) {
        return java.nio.file.Paths.get(
                System.getProperty("java.io.tmpdir"),
                "PRRPTF",
                "RATE_NOTICE_" + businessDate.format(DT) + ".csv");
    }

    private java.util.List<NoticeRecord> loadPmnotf() {
        return java.util.Arrays.asList(
                new NoticeRecord("N20260601001", "M00010001", "C1", java.time.LocalDate.of(2026, 7, 1), "郵送", "S"),
                new NoticeRecord("N20260601002", "M00010002", "C2", java.time.LocalDate.of(2026, 7, 1), "メール", "S"),
                new NoticeRecord("N20260601003", "M00010003", "C3", java.time.LocalDate.of(2026, 7, 1), "メール", "N"),
                new NoticeRecord("N20260601004", "M00010004", "C4", java.time.LocalDate.of(2026, 7, 1), "管理画面", "F"),
                new NoticeRecord("N20260601005", "M00010005", "C5", java.time.LocalDate.of(2026, 7, 1), "郵送", "S"),
                new NoticeRecord("N20260601006", "M00010006", "C2", java.time.LocalDate.of(2026, 7, 1), "メール", "S"),
                new NoticeRecord("N20260601007", "M00010007", "C4", java.time.LocalDate.of(2026, 7, 1), "管理画面", "N"),
                new NoticeRecord("N20260601008", "M00010008", "C1", java.time.LocalDate.of(2026, 7, 1), "メール", "F"),
                new NoticeRecord("N20260601009", "M00010009", "C3", java.time.LocalDate.of(2026, 8, 1), "郵送", "S"),
                new NoticeRecord("N20260601010", "M00010010", "C5", java.time.LocalDate.of(2026, 8, 1), "メール", "N"));
    }

    private java.util.List<RatePlanRecord> loadPmratf() {
        return java.util.Arrays.asList(
                new RatePlanRecord("RP202607C1", "C1", java.time.LocalDate.of(2026, 7, 1), "N20260601001", "承認済", "8F92A1"),
                new RatePlanRecord("RP202607C2", "C2", java.time.LocalDate.of(2026, 7, 1), "N20260601002", "承認済", "0AC731"),
                new RatePlanRecord("RP202607C3", "C3", java.time.LocalDate.of(2026, 7, 1), "N20260601003", "承認済", "DD7A20"),
                new RatePlanRecord("RP202607C4", "C4", java.time.LocalDate.of(2026, 7, 1), "N20260601004", "承認済", "619BC4"),
                new RatePlanRecord("RP202607C5", "C5", java.time.LocalDate.of(2026, 7, 1), "N20260601005", "審査中", "44A09C"),
                new RatePlanRecord("RP202608C3", "C3", java.time.LocalDate.of(2026, 8, 1), "N20260601009", "承認済", "F19D61"),
                new RatePlanRecord("RP202608C5", "C5", java.time.LocalDate.of(2026, 8, 1), "N20260601010", "承認済", "315E2A"));
    }

    private java.util.List<MerchantRecord> loadPfmerf() {
        return java.util.Arrays.asList(
                new MerchantRecord("M00010001", "青葉文具店", "C1", "01"),
                new MerchantRecord("M00010002", "銀座食堂", "C2", "01"),
                new MerchantRecord("M00010003", "北町水道局", "C3", "01"),
                new MerchantRecord("M00010004", "東都通販", "C4", "01"),
                new MerchantRecord("M00010005", "みらい娯楽", "C5", "02"),
                new MerchantRecord("M00010006", "桜カフェ", "C2", "01"),
                new MerchantRecord("M00010007", "港オンライン", "C4", "09"),
                new MerchantRecord("M00010008", "中央雑貨", "C1", "01"),
                new MerchantRecord("M00010009", "南町税務課", "C3", "01"),
                new MerchantRecord("M00010010", "新宿チケット", "C5", "01"));
    }

    private java.util.List<CategoryRecord> loadPmcatf() {
        return java.util.Arrays.asList(
                new CategoryRecord("C1", "一般物販", "低", true, true, java.time.LocalDate.of(2026, 5, 20)),
                new CategoryRecord("C2", "飲食", "中", true, true, java.time.LocalDate.of(2026, 5, 21)),
                new CategoryRecord("C3", "公共・公金", "低", false, true, java.time.LocalDate.of(2026, 5, 22)),
                new CategoryRecord("C4", "EC・通信販売", "中", true, true, java.time.LocalDate.of(2026, 5, 23)),
                new CategoryRecord("C5", "高リスク業種", "高", true, true, java.time.LocalDate.of(2026, 5, 24)));
    }

    private String categoryCodeToMerchantCategory(String categoryCode) {
        return categoryCode;
    }

    private static String sendStatusName(String status) {
        if ("S".equals(status)) {
            return "送信済";
        }
        if ("N".equals(status)) {
            return "未送信";
        }
        if ("F".equals(status)) {
            return "失敗";
        }
        return "不明";
    }

    private static String escape(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0) {
            return '"' + v.replace("\"", "\"\"") + '"';
        }
        return v;
    }

    private static final class NoticeRecord {
        final String noticeId;
        final String merchantCode;
        final String categoryCode;
        final java.time.LocalDate effectiveDate;
        final String channel;
        final String sendStatus;

        NoticeRecord(String noticeId, String merchantCode, String categoryCode,
                     java.time.LocalDate effectiveDate, String channel, String sendStatus) {
            this.noticeId = noticeId;
            this.merchantCode = merchantCode;
            this.categoryCode = categoryCode;
            this.effectiveDate = effectiveDate;
            this.channel = channel;
            this.sendStatus = sendStatus;
        }
    }

    private static final class RatePlanRecord {
        final String ratePlanId;
        final String categoryCode;
        final java.time.LocalDate effectiveDate;
        final String noticeId;
        final String approvalStatus;
        final String ruleHash;

        RatePlanRecord(String ratePlanId, String categoryCode, java.time.LocalDate effectiveDate,
                       String noticeId, String approvalStatus, String ruleHash) {
            this.ratePlanId = ratePlanId;
            this.categoryCode = categoryCode;
            this.effectiveDate = effectiveDate;
            this.noticeId = noticeId;
            this.approvalStatus = approvalStatus;
            this.ruleHash = ruleHash;
        }
    }

    private static final class MerchantRecord {
        final String merchantCode;
        final String merchantName;
        final String merchantCategory;
        final String merchantStatus;

        MerchantRecord(String merchantCode, String merchantName, String merchantCategory, String merchantStatus) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merchantCategory = merchantCategory;
            this.merchantStatus = merchantStatus;
        }
    }

    private static final class CategoryRecord {
        final String categoryCode;
        final String categoryName;
        final String riskRank;
        final boolean taxableFlag;
        final boolean activeFlag;
        final java.time.LocalDate lastUpdateDate;

        CategoryRecord(String categoryCode, String categoryName, String riskRank,
                       boolean taxableFlag, boolean activeFlag, java.time.LocalDate lastUpdateDate) {
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDate = lastUpdateDate;
        }
    }

    private static final class AggregateKey implements Comparable<AggregateKey> {
        final String merchantCode;
        final String merchantCategory;
        final String categoryCode;
        final String categoryName;
        final java.time.LocalDate effectiveDate;
        final String channel;

        AggregateKey(String merchantCode, String merchantCategory, String categoryCode,
                     String categoryName, java.time.LocalDate effectiveDate, String channel) {
            this.merchantCode = merchantCode;
            this.merchantCategory = merchantCategory;
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.effectiveDate = effectiveDate;
            this.channel = channel;
        }

        public int compareTo(AggregateKey other) {
            int c = merchantCode.compareTo(other.merchantCode);
            if (c != 0) {
                return c;
            }
            c = categoryCode.compareTo(other.categoryCode);
            if (c != 0) {
                return c;
            }
            c = effectiveDate.compareTo(other.effectiveDate);
            if (c != 0) {
                return c;
            }
            return channel.compareTo(other.channel);
        }
    }

    private static final class NoticeSummary {
        int sent;
        int unsent;
        int failed;
        int valid;

        void add(String sendStatus, boolean validationPassed) {
            if ("S".equals(sendStatus)) {
                sent++;
            } else if ("N".equals(sendStatus)) {
                unsent++;
            } else if ("F".equals(sendStatus)) {
                failed++;
            }
            if (validationPassed) {
                valid++;
            }
        }
    }

    private static final class ReportLine {
        final String kind;
        final String noticeId;
        final String merchantCode;
        final String merchantName;
        final String merchantCategory;
        final String categoryCode;
        final String categoryName;
        final java.time.LocalDate effectiveDate;
        final String channel;
        final int sent;
        final int unsent;
        final int failed;
        final String result;

        private ReportLine(String kind, String noticeId, String merchantCode, String merchantName,
                           String merchantCategory, String categoryCode, String categoryName,
                           java.time.LocalDate effectiveDate, String channel, int sent, int unsent,
                           int failed, String result) {
            this.kind = kind;
            this.noticeId = noticeId;
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merchantCategory = merchantCategory;
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.effectiveDate = effectiveDate;
            this.channel = channel;
            this.sent = sent;
            this.unsent = unsent;
            this.failed = failed;
            this.result = result;
        }

        static ReportLine error(
                NoticeRecord notice,
                MerchantRecord merchant,
                CategoryRecord category,
                java.util.List<String> errors) {
            return new ReportLine(
                    "未達明細",
                    notice.noticeId,
                    notice.merchantCode,
                    merchant == null ? "" : merchant.merchantName,
                    merchant == null ? "" : merchant.merchantCategory,
                    notice.categoryCode,
                    category == null ? "" : category.categoryName,
                    notice.effectiveDate,
                    notice.channel,
                    "S".equals(notice.sendStatus) ? 1 : 0,
                    "N".equals(notice.sendStatus) ? 1 : 0,
                    "F".equals(notice.sendStatus) ? 1 : 0,
                    String.join("・", errors));
        }

        static ReportLine summary(AggregateKey key, NoticeSummary summary) {
            return new ReportLine(
                    "集計",
                    "",
                    key.merchantCode,
                    "",
                    key.merchantCategory,
                    key.categoryCode,
                    key.categoryName,
                    key.effectiveDate,
                    key.channel,
                    summary.sent,
                    summary.unsent,
                    summary.failed,
                    "検証正常=" + summary.valid);
        }

        String toCsvRow() {
            return escape(kind) + ','
                    + escape(noticeId) + ','
                    + escape(merchantCode) + ','
                    + escape(merchantName) + ','
                    + escape(merchantCategory) + ','
                    + escape(categoryCode) + ','
                    + escape(categoryName) + ','
                    + effectiveDate.format(DT) + ','
                    + escape(channel) + ','
                    + sent + ','
                    + unsent + ','
                    + failed + ','
                    + escape(result);
        }
    }

    private static final class ReportWriteStatus {
        final String reportId;
        final String reportType;
        final java.time.LocalDate businessDate;
        final String merchantCode;
        final String outputPath;
        final String status;

        ReportWriteStatus(String reportId, String reportType, java.time.LocalDate businessDate,
                          String merchantCode, String outputPath, String status) {
            this.reportId = reportId;
            this.reportType = reportType;
            this.businessDate = businessDate;
            this.merchantCode = merchantCode;
            this.outputPath = outputPath;
            this.status = status;
        }

        String toOperatorMessage() {
            return "PRRPTF 書込結果:"
                    + " 帳票ID=" + reportId
                    + " 帳票種別=" + reportType
                    + " 業務日=" + businessDate.format(DT)
                    + " 加盟店コード=" + merchantCode
                    + " 出力先=" + outputPath
                    + " 状態=" + status;
        }
    }
}
