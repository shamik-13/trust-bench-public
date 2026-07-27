package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/04/01  業務開発  初版作成
 * 1.01    2024/07/18  業務開発  リスクランクと課税対象フラグの検証追加
 * 1.02    2025/01/10  保守担当  参照中カテゴリの削除時非活性化対応
 */
public class CategoryMaintenanceService {

    private static final String 有効 = "1";
    private static final String 無効 = "0";

    public static void main(String[] a) {
        CategoryMaintenanceService service = new CategoryMaintenanceService();
        PmcatfStore pmcatf = new PmcatfStore();

        pmcatf.put(new CategoryRecord("C010", "飲食加盟店", "B", "1", 有効, "20250531"));
        pmcatf.put(new CategoryRecord("C020", "医療加盟店", "A", "0", 有効, "20250531"));
        pmcatf.put(new CategoryRecord("C030", "換金性商品", "D", "1", 有効, "20250531"));

        ReferenceCounter references = new ReferenceCounter();
        references.add("C010", 1842);
        references.add("C020", 319);
        references.add("C030", 27);

        FeeRateKeyCatalog feeKeys = new FeeRateKeyCatalog();
        feeKeys.add("C010", "B", "1");
        feeKeys.add("C020", "A", "0");
        feeKeys.add("C031", "C", "1");

        MaintenanceResult result = service.execute(pmcatf, references, feeKeys, new MaintenanceRequest[] {
                MaintenanceRequest.rename("C010", "飲食・喫茶加盟店", "20250601"),
                MaintenanceRequest.register("C031", "高額物販加盟店", "C", "1", "20250601"),
                MaintenanceRequest.suspend("C020", "20250601"),
                MaintenanceRequest.delete("C030", "20250601"),
                MaintenanceRequest.register("C099", "臨時手数料検証", "D", "0", "20250601")
        });

        System.out.println("処理件数=" + result.processed);
        System.out.println("更新件数=" + result.updated);
        System.out.println("否認件数=" + result.rejected);
        System.out.println("非活性件数=" + result.inactivated);
        for (String message : result.messages) {
            System.out.println(message);
        }
    }

    MaintenanceResult execute(PmcatfStore pmcatf,
                              ReferenceCounter references,
                              FeeRateKeyCatalog feeKeys,
                              MaintenanceRequest[] requests) {
        MaintenanceResult result = new MaintenanceResult(requests.length);

        for (MaintenanceRequest request : requests) {
            String error = validateRequest(request, pmcatf, feeKeys);
            if (error != null) {
                result.reject(request.categoryCode, error);
                continue;
            }

            if ("登録".equals(request.operation)) {
                CategoryRecord record = new CategoryRecord(
                        request.categoryCode,
                        request.categoryName,
                        request.riskRank,
                        request.taxableFlag,
                        有効,
                        request.updateDate);
                pmcatf.put(record);
                result.update(request.categoryCode, "登録完了");
            } else if ("名称変更".equals(request.operation)) {
                CategoryRecord current = pmcatf.get(request.categoryCode);
                CategoryRecord changed = current.withName(request.categoryName, request.updateDate);
                pmcatf.put(changed);
                result.update(request.categoryCode, "名称変更完了");
            } else if ("利用停止".equals(request.operation)) {
                CategoryRecord current = pmcatf.get(request.categoryCode);
                CategoryRecord changed = current.withActiveFlag(無効, request.updateDate);
                pmcatf.put(changed);
                result.inactivate(request.categoryCode, "利用停止完了");
            } else if ("削除".equals(request.operation)) {
                int used = references.count(request.categoryCode);
                if (used > 0) {
                    CategoryRecord current = pmcatf.get(request.categoryCode);
                    CategoryRecord changed = current.withActiveFlag(無効, request.updateDate);
                    pmcatf.put(changed);
                    result.inactivate(request.categoryCode, "参照中のため非活性化 件数=" + used);
                } else {
                    pmcatf.remove(request.categoryCode);
                    result.update(request.categoryCode, "削除完了");
                }
            }
        }
        return result;
    }

    private String validateRequest(MaintenanceRequest request, PmcatfStore pmcatf, FeeRateKeyCatalog feeKeys) {
        if (isBlank(request.categoryCode) || request.categoryCode.length() != 4) {
            return "カテゴリコード不正";
        }
        if (!request.categoryCode.startsWith("C")) {
            return "カテゴリコード体系不正";
        }
        if (isBlank(request.updateDate) || request.updateDate.length() != 8) {
            return "更新日不正";
        }

        CategoryRecord current = pmcatf.get(request.categoryCode);

        if ("登録".equals(request.operation)) {
            if (current != null) {
                return "既存カテゴリ";
            }
            String common = validateAttributes(request.categoryName, request.riskRank, request.taxableFlag);
            if (common != null) {
                return common;
            }
            if (!feeKeys.exists(request.categoryCode, request.riskRank, request.taxableFlag)) {
                return "手数料規程キー未登録";
            }
            return null;
        }

        if (current == null) {
            return "対象カテゴリなし";
        }

        if ("名称変更".equals(request.operation)) {
            if (isBlank(request.categoryName)) {
                return "カテゴリ名称未設定";
            }
            if (request.categoryName.length() > 24) {
                return "カテゴリ名称桁超過";
            }
            if (無効.equals(current.activeFlag)) {
                return "非活性カテゴリ変更不可";
            }
            return null;
        }

        if ("利用停止".equals(request.operation) || "削除".equals(request.operation)) {
            if (無効.equals(current.activeFlag)) {
                return "既に非活性";
            }
            return null;
        }

        return "処理区分不正";
    }

    private String validateAttributes(String name, String riskRank, String taxableFlag) {
        if (isBlank(name)) {
            return "カテゴリ名称未設定";
        }
        if (name.length() > 24) {
            return "カテゴリ名称桁超過";
        }
        if (!("A".equals(riskRank) || "B".equals(riskRank) || "C".equals(riskRank) || "D".equals(riskRank))) {
            return "リスクランク不正";
        }
        if (!(有効.equals(taxableFlag) || 無効.equals(taxableFlag))) {
            return "課税対象フラグ不正";
        }
        if ("D".equals(riskRank) && 無効.equals(taxableFlag)) {
            return "高リスク非課税の組合せ不可";
        }
        return null;
    }

    private boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    static final class CategoryRecord {
        final String categoryCode;
        final String categoryName;
        final String riskRank;
        final String taxableFlag;
        final String activeFlag;
        final String lastUpdateDt;

        CategoryRecord(String categoryCode,
                       String categoryName,
                       String riskRank,
                       String taxableFlag,
                       String activeFlag,
                       String lastUpdateDt) {
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDt = lastUpdateDt;
        }

        CategoryRecord withName(String newName, String updateDate) {
            return new CategoryRecord(categoryCode, newName, riskRank, taxableFlag, activeFlag, updateDate);
        }

        CategoryRecord withActiveFlag(String newActiveFlag, String updateDate) {
            return new CategoryRecord(categoryCode, categoryName, riskRank, taxableFlag, newActiveFlag, updateDate);
        }
    }

    static final class MaintenanceRequest {
        final String operation;
        final String categoryCode;
        final String categoryName;
        final String riskRank;
        final String taxableFlag;
        final String updateDate;

        private MaintenanceRequest(String operation,
                                   String categoryCode,
                                   String categoryName,
                                   String riskRank,
                                   String taxableFlag,
                                   String updateDate) {
            this.operation = operation;
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.updateDate = updateDate;
        }

        static MaintenanceRequest register(String code, String name, String riskRank, String taxableFlag, String date) {
            return new MaintenanceRequest("登録", code, name, riskRank, taxableFlag, date);
        }

        static MaintenanceRequest rename(String code, String name, String date) {
            return new MaintenanceRequest("名称変更", code, name, null, null, date);
        }

        static MaintenanceRequest suspend(String code, String date) {
            return new MaintenanceRequest("利用停止", code, null, null, null, date);
        }

        static MaintenanceRequest delete(String code, String date) {
            return new MaintenanceRequest("削除", code, null, null, null, date);
        }
    }

    static final class PmcatfStore {
        private final java.util.Map<String, CategoryRecord> rows = new java.util.TreeMap<String, CategoryRecord>();

        CategoryRecord get(String categoryCode) {
            return rows.get(categoryCode);
        }

        void put(CategoryRecord record) {
            rows.put(record.categoryCode, record);
        }

        void remove(String categoryCode) {
            rows.remove(categoryCode);
        }
    }

    static final class ReferenceCounter {
        private final java.util.Map<String, Integer> counts = new java.util.HashMap<String, Integer>();

        void add(String categoryCode, int count) {
            counts.put(categoryCode, count);
        }

        int count(String categoryCode) {
            Integer count = counts.get(categoryCode);
            return count == null ? 0 : count.intValue();
        }
    }

    static final class FeeRateKeyCatalog {
        private final java.util.Set<String> keys = new java.util.HashSet<String>();

        void add(String categoryCode, String riskRank, String taxableFlag) {
            keys.add(key(categoryCode, riskRank, taxableFlag));
        }

        boolean exists(String categoryCode, String riskRank, String taxableFlag) {
            return keys.contains(key(categoryCode, riskRank, taxableFlag));
        }

        private String key(String categoryCode, String riskRank, String taxableFlag) {
            return categoryCode + "|" + riskRank + "|" + taxableFlag;
        }
    }

    static final class MaintenanceResult {
        final int processed;
        int updated;
        int rejected;
        int inactivated;
        final java.util.List<String> messages = new java.util.ArrayList<String>();

        MaintenanceResult(int processed) {
            this.processed = processed;
        }

        void update(String categoryCode, String detail) {
            updated++;
            messages.add("正常 " + categoryCode + " " + detail);
        }

        void inactivate(String categoryCode, String detail) {
            updated++;
            inactivated++;
            messages.add("非活性 " + categoryCode + " " + detail);
        }

        void reject(String categoryCode, String reason) {
            rejected++;
            messages.add("否認 " + categoryCode + " " + reason);
        }
    }
}
