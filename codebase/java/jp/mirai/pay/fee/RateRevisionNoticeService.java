package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日        担当      概要
 * 1.00    2026-03-18    決済基盤    初版作成
 * 1.01    2026-04-09    業務運用    承認済み改定予定の抽出条件を追加
 */
public class RateRevisionNoticeService {
    private static final String STATUS_ACTIVE = "01";
    private static final String APPROVAL_APPROVED = "承認済";
    private static final String SEND_STATUS_CREATED = "作成済";
    private static final String CHANNEL_POST = "郵送";
    private static final String CHANNEL_MAIL = "メール";

    public static void main(String[] a) {
        java.time.LocalDate today = a != null && a.length > 0 ? parseBusinessDate(a[0]) : java.time.LocalDate.now();

        java.util.List<Merchant> merchants = loadPfmerf();
        java.util.Map<String, Category> categories = indexCategories(loadPmcatf());
        java.util.List<RatePlan> plans = loadPmratf();

        java.util.List<Notice> notices = createNotices(today, merchants, categories, plans);
        writePmnotf(notices);

        System.out.println("処理日=" + today);
        System.out.println("加盟店件数=" + merchants.size());
        System.out.println("改定予定件数=" + plans.size());
        System.out.println("通知作成件数=" + notices.size());
    }

    private static java.time.LocalDate parseBusinessDate(String value) {
        try {
            return java.time.LocalDate.parse(value, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        } catch (RuntimeException ex) {
            throw new IllegalArgumentException("処理日の形式が不正です。YYYYMMDDで指定してください: " + value, ex);
        }
    }

    private static java.util.List<Notice> createNotices(
            java.time.LocalDate today,
            java.util.List<Merchant> merchants,
            java.util.Map<String, Category> categories,
            java.util.List<RatePlan> plans) {

        java.util.List<Notice> notices = new java.util.ArrayList<>();
        java.util.Set<String> createdKeys = new java.util.HashSet<>();

        for (RatePlan plan : plans) {
            validatePlan(plan);

            if (!APPROVAL_APPROVED.equals(plan.approvalStatus)) {
                continue;
            }
            if (plan.effectiveDate.isBefore(today)) {
                continue;
            }

            Category category = categories.get(plan.categoryCode);
            if (category == null) {
                System.out.println("警告: 業種マスタ未登録の改定予定を除外しました。通知ID=" + plan.noticeId
                        + ", 業種=" + plan.categoryCode);
                continue;
            }
            if (!category.activeFlag) {
                continue;
            }

            for (Merchant merchant : merchants) {
                if (!STATUS_ACTIVE.equals(merchant.status)) {
                    continue;
                }
                if (!plan.categoryCode.equals(merchant.categoryCode)) {
                    continue;
                }

                String key = plan.noticeId + "|" + merchant.code + "|" + plan.categoryCode + "|" + plan.effectiveDate;
                if (!createdKeys.add(key)) {
                    continue;
                }

                String channel = selectChannel(category);
                notices.add(new Notice(
                        plan.noticeId,
                        merchant.code,
                        plan.categoryCode,
                        plan.effectiveDate,
                        channel,
                        SEND_STATUS_CREATED));
            }
        }

        return notices;
    }

    private static String selectChannel(Category category) {
        if ("C5".equals(category.code) || "A".equals(category.riskRank)) {
            return CHANNEL_POST;
        }
        return CHANNEL_MAIL;
    }

    private static void validatePlan(RatePlan plan) {
        require(plan.ratePlanId, "RATE-PLAN-ID");
        require(plan.categoryCode, "CATEGORY-CODE");
        require(plan.noticeId, "NOTICE-ID");
        require(plan.approvalStatus, "APPROVAL-STATUS");
        require(plan.ruleHash, "RULE-HASH");

        if (!isCanonicalCategory(plan.categoryCode)) {
            throw new IllegalArgumentException("業種区分が規定外です: " + plan.categoryCode);
        }
        if (plan.effectiveDate == null) {
            throw new IllegalArgumentException("EFFECTIVE-DTが未設定です: " + plan.noticeId);
        }
    }

    private static java.util.Map<String, Category> indexCategories(java.util.List<Category> categories) {
        java.util.Map<String, Category> indexed = new java.util.LinkedHashMap<>();
        for (Category category : categories) {
            require(category.code, "CATEGORY-CODE");
            require(category.name, "CATEGORY-NAME");
            require(category.riskRank, "RISK-RANK");

            if (!isCanonicalCategory(category.code)) {
                throw new IllegalArgumentException("業種区分が規定外です: " + category.code);
            }
            if (indexed.put(category.code, category) != null) {
                throw new IllegalArgumentException("業種マスタが重複しています: " + category.code);
            }
        }
        return indexed;
    }

    private static boolean isCanonicalCategory(String categoryCode) {
        return "C1".equals(categoryCode)
                || "C2".equals(categoryCode)
                || "C3".equals(categoryCode)
                || "C4".equals(categoryCode)
                || "C5".equals(categoryCode);
    }

    private static void require(String value, String fieldName) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(fieldName + "が未設定です");
        }
    }

    private static void writePmnotf(java.util.List<Notice> notices) {
        java.util.Map<String, Integer> byCategory = new java.util.TreeMap<>();
        for (Notice notice : notices) {
            byCategory.merge(notice.categoryCode, 1, Integer::sum);
            System.out.println(toEsdsLine(notice));
        }

        for (java.util.Map.Entry<String, Integer> entry : byCategory.entrySet()) {
            System.out.println("業種別通知件数 業種=" + entry.getKey() + ", 件数=" + entry.getValue());
        }
    }

    private static String toEsdsLine(Notice notice) {
        return "PMNOTF{"
                + "NOTICE-ID=" + notice.noticeId
                + ", MERCHANT-CODE=" + notice.merchantCode
                + ", CATEGORY-CODE=" + notice.categoryCode
                + ", EFFECTIVE-DT=" + notice.effectiveDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + ", CHANNEL=" + notice.channel
                + ", SEND-STATUS=" + notice.sendStatus
                + '}';
    }

    private static java.util.List<Merchant> loadPfmerf() {
        java.util.List<Merchant> rows = new java.util.ArrayList<>();
        rows.add(new Merchant("M0001001", "未来商店銀座", "C1", "01"));
        rows.add(new Merchant("M0001002", "東都食堂", "C2", "01"));
        rows.add(new Merchant("M0001003", "港区水道料金", "C3", "01"));
        rows.add(new Merchant("M0001004", "青空オンライン", "C4", "01"));
        rows.add(new Merchant("M0001005", "新宿ナイトサービス", "C5", "01"));
        rows.add(new Merchant("M0001006", "北町雑貨", "C1", "02"));
        rows.add(new Merchant("M0001007", "西町カフェ", "C2", "09"));
        rows.add(new Merchant("M0001008", "関西通販センター", "C4", "01"));
        return rows;
    }

    private static java.util.List<Category> loadPmcatf() {
        java.util.List<Category> rows = new java.util.ArrayList<>();
        rows.add(new Category("C1", "一般物販", "C", true, true, java.time.LocalDate.of(2026, 2, 1)));
        rows.add(new Category("C2", "飲食", "B", true, true, java.time.LocalDate.of(2026, 2, 1)));
        rows.add(new Category("C3", "公共・公金", "D", false, true, java.time.LocalDate.of(2026, 2, 1)));
        rows.add(new Category("C4", "EC・通信販売", "B", true, true, java.time.LocalDate.of(2026, 2, 1)));
        rows.add(new Category("C5", "高リスク業種", "A", true, true, java.time.LocalDate.of(2026, 2, 1)));
        return rows;
    }

    private static java.util.List<RatePlan> loadPmratf() {
        java.util.List<RatePlan> rows = new java.util.ArrayList<>();
        rows.add(new RatePlan("RP202607-C1", "C1", java.time.LocalDate.of(2026, 7, 1),
                "N20260701001", "承認済", "8d7c3b9a"));
        rows.add(new RatePlan("RP202607-C2", "C2", java.time.LocalDate.of(2026, 7, 1),
                "N20260701002", "承認済", "b44e12f0"));
        rows.add(new RatePlan("RP202608-C4", "C4", java.time.LocalDate.of(2026, 8, 1),
                "N20260801004", "承認済", "41bc77aa"));
        rows.add(new RatePlan("RP202608-C5", "C5", java.time.LocalDate.of(2026, 8, 1),
                "N20260801005", "承認済", "df9013ab"));
        rows.add(new RatePlan("RP202607-C3", "C3", java.time.LocalDate.of(2026, 7, 1),
                "N20260701003", "審査中", "21c0e8dd"));
        return rows;
    }

    private static final class Merchant {
        private final String code;
        private final String name;
        private final String categoryCode;
        private final String status;

        private Merchant(String code, String name, String categoryCode, String status) {
            this.code = code;
            this.name = name;
            this.categoryCode = categoryCode;
            this.status = status;
        }
    }

    private static final class Category {
        private final String code;
        private final String name;
        private final String riskRank;
        private final boolean taxableFlag;
        private final boolean activeFlag;
        private final java.time.LocalDate lastUpdateDate;

        private Category(String code, String name, String riskRank, boolean taxableFlag,
                         boolean activeFlag, java.time.LocalDate lastUpdateDate) {
            this.code = code;
            this.name = name;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDate = lastUpdateDate;
        }
    }

    private static final class RatePlan {
        private final String ratePlanId;
        private final String categoryCode;
        private final java.time.LocalDate effectiveDate;
        private final String noticeId;
        private final String approvalStatus;
        private final String ruleHash;

        private RatePlan(String ratePlanId, String categoryCode, java.time.LocalDate effectiveDate,
                         String noticeId, String approvalStatus, String ruleHash) {
            this.ratePlanId = ratePlanId;
            this.categoryCode = categoryCode;
            this.effectiveDate = effectiveDate;
            this.noticeId = noticeId;
            this.approvalStatus = approvalStatus;
            this.ruleHash = ruleHash;
        }
    }

    private static final class Notice {
        private final String noticeId;
        private final String merchantCode;
        private final String categoryCode;
        private final java.time.LocalDate effectiveDate;
        private final String channel;
        private final String sendStatus;

        private Notice(String noticeId, String merchantCode, String categoryCode,
                       java.time.LocalDate effectiveDate, String channel, String sendStatus) {
            this.noticeId = noticeId;
            this.merchantCode = merchantCode;
            this.categoryCode = categoryCode;
            this.effectiveDate = effectiveDate;
            this.channel = channel;
            this.sendStatus = sendStatus;
        }
    }
}
