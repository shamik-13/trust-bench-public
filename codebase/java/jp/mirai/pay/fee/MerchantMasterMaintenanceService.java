package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日       担当        概要
 * 1.00    2026-02-10   基盤開発部  初版作成
 * 1.01    2026-04-18   業務開発部  業種変更時のカテゴリ有効性確認を追加
 */
public class MerchantMasterMaintenanceService {
    private static final String STATUS_ACTIVE = "01";
    private static final String STATUS_SUSPENDED = "02";
    private static final String STATUS_TERMINATED = "09";

    public static void main(String[] a) {
        MerchantMasterMaintenanceService service = new MerchantMasterMaintenanceService();
        MaintenanceResult result = service.process(sampleMerchants(), sampleCategories(), sampleRequests());

        System.out.println("処理件数=" + result.totalCount);
        System.out.println("更新件数=" + result.updatedCount);
        System.out.println("監査件数=" + result.auditEntries.size());
        for (String message : result.messages) {
            System.out.println(message);
        }
    }

    private MaintenanceResult process(
            java.util.Map<String, MerchantRecord> pfmerf,
            java.util.Map<String, CategoryRecord> pmcatf,
            java.util.List<MaintenanceRequest> requests) {

        MaintenanceResult result = new MaintenanceResult();

        for (MaintenanceRequest request : requests) {
            result.totalCount++;

            java.util.List<String> errors = validateRequest(request);
            MerchantRecord current = pfmerf.get(request.merchantCode);
            if (current == null) {
                errors.add("加盟店未登録");
            }

            CategoryRecord nextCategory = null;
            boolean categoryChanged = false;
            if (current != null) {
                categoryChanged = !current.category.equals(request.category);
                if (categoryChanged) {
                    nextCategory = pmcatf.get(request.category);
                    if (nextCategory == null) {
                        errors.add("業種区分未登録");
                    } else {
                        if (!"1".equals(nextCategory.activeFlag)) {
                            errors.add("業種区分が無効");
                        }
                        if ("5".equals(nextCategory.riskRank) && !STATUS_ACTIVE.equals(request.status)) {
                            errors.add("高リスク業種は有効状態のみ指定可");
                        }
                    }
                }
            }

            if (!errors.isEmpty()) {
                result.messages.add("更新拒否 加盟店=" + request.merchantCode + " 理由=" + join(errors));
                continue;
            }

            MerchantRecord before = current;
            MerchantRecord after = new MerchantRecord(
                    before.merchantCode,
                    normalizeName(request.merchantName),
                    request.category,
                    request.status);

            if (categoryChanged && STATUS_ACTIVE.equals(before.status)) {
                CategoryRecord beforeCategory = pmcatf.get(before.category);
                String beforeName = beforeCategory == null ? "名称不明" : beforeCategory.categoryName;
                String afterName = nextCategory == null ? "名称不明" : nextCategory.categoryName;
                result.auditEntries.add(new AuditEntry(
                        before.merchantCode,
                        before.category,
                        beforeName,
                        after.category,
                        afterName));
            }

            pfmerf.put(after.merchantCode, after);
            result.updatedCount++;
            result.messages.add("更新完了 加盟店=" + after.merchantCode);
        }

        return result;
    }

    private java.util.List<String> validateRequest(MaintenanceRequest request) {
        java.util.List<String> errors = new java.util.ArrayList<>();

        if (isBlank(request.merchantCode)) {
            errors.add("加盟店コード未設定");
        } else if (!request.merchantCode.matches("[0-9]{10}")) {
            errors.add("加盟店コード形式不正");
        }

        if (isBlank(request.merchantName)) {
            errors.add("加盟店名称未設定");
        } else if (normalizeName(request.merchantName).length() > 40) {
            errors.add("加盟店名称桁数超過");
        }

        if (!isValidCategory(request.category)) {
            errors.add("業種区分不正");
        }

        if (!STATUS_ACTIVE.equals(request.status)
                && !STATUS_SUSPENDED.equals(request.status)
                && !STATUS_TERMINATED.equals(request.status)) {
            errors.add("稼働状態不正");
        }

        return errors;
    }

    private static boolean isValidCategory(String category) {
        return "C1".equals(category)
                || "C2".equals(category)
                || "C3".equals(category)
                || "C4".equals(category)
                || "C5".equals(category);
    }

    private static String normalizeName(String value) {
        return value == null ? "" : value.trim().replaceAll("[ 　]+", " ");
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static String join(java.util.List<String> values) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < values.size(); i++) {
            if (i > 0) {
                builder.append("、");
            }
            builder.append(values.get(i));
        }
        return builder.toString();
    }

    private static java.util.Map<String, MerchantRecord> sampleMerchants() {
        java.util.Map<String, MerchantRecord> records = new java.util.LinkedHashMap<>();
        records.put("1000000001", new MerchantRecord("1000000001", "みらい青果店", "C1", "01"));
        records.put("1000000002", new MerchantRecord("1000000002", "東都食堂", "C2", "01"));
        records.put("1000000003", new MerchantRecord("1000000003", "さくら水道局", "C3", "02"));
        return records;
    }

    private static java.util.Map<String, CategoryRecord> sampleCategories() {
        java.util.Map<String, CategoryRecord> records = new java.util.LinkedHashMap<>();
        records.put("C1", new CategoryRecord("C1", "一般物販", "1", "1", "1", "20260401"));
        records.put("C2", new CategoryRecord("C2", "飲食", "2", "1", "1", "20260401"));
        records.put("C3", new CategoryRecord("C3", "公共・公金", "1", "0", "1", "20260401"));
        records.put("C4", new CategoryRecord("C4", "EC・通信販売", "3", "1", "1", "20260401"));
        records.put("C5", new CategoryRecord("C5", "高リスク業種", "5", "1", "1", "20260401"));
        return records;
    }

    private static java.util.List<MaintenanceRequest> sampleRequests() {
        java.util.List<MaintenanceRequest> requests = new java.util.ArrayList<>();
        requests.add(new MaintenanceRequest("1000000001", "みらい青果店 本店", "C4", "01"));
        requests.add(new MaintenanceRequest("1000000002", "東都食堂", "C5", "02"));
        requests.add(new MaintenanceRequest("1000000003", "さくら水道局", "C3", "01"));
        requests.add(new MaintenanceRequest("9999999999", "未登録加盟店", "C1", "01"));
        return requests;
    }

    private static final class MerchantRecord {
        private final String merchantCode;
        private final String merchantName;
        private final String category;
        private final String status;

        private MerchantRecord(String merchantCode, String merchantName, String category, String status) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.category = category;
            this.status = status;
        }
    }

    private static final class CategoryRecord {
        private final String categoryCode;
        private final String categoryName;
        private final String riskRank;
        private final String taxableFlag;
        private final String activeFlag;
        private final String lastUpdateDate;

        private CategoryRecord(
                String categoryCode,
                String categoryName,
                String riskRank,
                String taxableFlag,
                String activeFlag,
                String lastUpdateDate) {
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDate = lastUpdateDate;
        }
    }

    private static final class MaintenanceRequest {
        private final String merchantCode;
        private final String merchantName;
        private final String category;
        private final String status;

        private MaintenanceRequest(String merchantCode, String merchantName, String category, String status) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.category = category;
            this.status = status;
        }
    }

    private static final class AuditEntry {
        private final String merchantCode;
        private final String beforeCategory;
        private final String beforeCategoryName;
        private final String afterCategory;
        private final String afterCategoryName;

        private AuditEntry(
                String merchantCode,
                String beforeCategory,
                String beforeCategoryName,
                String afterCategory,
                String afterCategoryName) {
            this.merchantCode = merchantCode;
            this.beforeCategory = beforeCategory;
            this.beforeCategoryName = beforeCategoryName;
            this.afterCategory = afterCategory;
            this.afterCategoryName = afterCategoryName;
        }
    }

    private static final class MaintenanceResult {
        private int totalCount;
        private int updatedCount;
        private final java.util.List<String> messages = new java.util.ArrayList<>();
        private final java.util.List<AuditEntry> auditEntries = new java.util.ArrayList<>();
    }
}
