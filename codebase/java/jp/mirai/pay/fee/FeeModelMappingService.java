package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/11/06  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class FeeModelMappingService {
    private static final String CHARGEABLE_STATUS = "01";

    public static void main(String[] a) {
        java.util.List<TransactionRecord> transactions = java.util.Arrays.asList(
                new TransactionRecord("T202606280001", "M000001", new java.math.BigDecimal("12800"), java.time.LocalDate.of(2026, 6, 28)),
                new TransactionRecord("T202606280002", "M000002", new java.math.BigDecimal("5400"), java.time.LocalDate.of(2026, 6, 28)),
                new TransactionRecord("T202606280003", "M000003", new java.math.BigDecimal("33000"), java.time.LocalDate.of(2026, 6, 28)),
                new TransactionRecord("T202606280004", "M000004", new java.math.BigDecimal("9800"), java.time.LocalDate.of(2026, 6, 28)),
                new TransactionRecord("T202606280005", "M000005", new java.math.BigDecimal("125000"), java.time.LocalDate.of(2026, 6, 28)),
                new TransactionRecord("T202606280006", "M000006", new java.math.BigDecimal("4700"), java.time.LocalDate.of(2026, 6, 28)),
                new TransactionRecord("T202606280007", "M999999", new java.math.BigDecimal("2100"), java.time.LocalDate.of(2026, 6, 28)),
                new TransactionRecord("T202606280008", "M000001", new java.math.BigDecimal("-500"), java.time.LocalDate.of(2026, 6, 28))
        );

        java.util.Map<String, MerchantRecord> merchants = indexMerchants(java.util.Arrays.asList(
                new MerchantRecord("M000001", "未来百貨店日本橋", "C1", "01"),
                new MerchantRecord("M000002", "銀座食堂", "C2", "01"),
                new MerchantRecord("M000003", "首都圏水道収納", "C3", "01"),
                new MerchantRecord("M000004", "未来オンライン", "C4", "02"),
                new MerchantRecord("M000005", "湾岸チケット販売", "C5", "01"),
                new MerchantRecord("M000006", "旧加盟店サンプル", "C9", "01")
        ));

        java.util.Map<String, CategoryRecord> categories = indexCategories(java.util.Arrays.asList(
                new CategoryRecord("C1", "一般物販", "A", "1", "1", java.time.LocalDate.of(2026, 4, 1)),
                new CategoryRecord("C2", "飲食", "B", "1", "1", java.time.LocalDate.of(2026, 4, 1)),
                new CategoryRecord("C3", "公共・公金", "A", "0", "1", java.time.LocalDate.of(2026, 4, 1)),
                new CategoryRecord("C4", "EC・通信販売", "B", "1", "1", java.time.LocalDate.of(2026, 4, 1)),
                new CategoryRecord("C5", "高リスク業種", "D", "1", "1", java.time.LocalDate.of(2026, 4, 1))
        ));

        java.util.List<Object> feeModels = new java.util.ArrayList<>();
        java.util.Map<String, Summary> summaries = new java.util.TreeMap<>();

        for (TransactionRecord txn : transactions) {
            MappingResult result = mapTransaction(txn, merchants, categories);
            if (result.accepted) {
                feeModels.add(result.feeModel);
                Summary summary = summaries.computeIfAbsent(result.categoryCode, k -> new Summary());
                summary.count++;
                summary.amount = summary.amount.add(txn.amount);
                System.out.println("変換完了 取引ID=" + txn.transactionId + " 加盟店=" + txn.merchantCode + " 業種=" + result.categoryCode);
            } else {
                System.out.println("変換除外 取引ID=" + txn.transactionId + " 理由=" + result.reason);
            }
        }

        System.out.println("変換件数=" + feeModels.size());
        for (java.util.Map.Entry<String, Summary> entry : summaries.entrySet()) {
            System.out.println("業種別集計 業種=" + entry.getKey() + " 件数=" + entry.getValue().count + " 金額=" + entry.getValue().amount);
        }
    }

    private static MappingResult mapTransaction(
            TransactionRecord txn,
            java.util.Map<String, MerchantRecord> merchants,
            java.util.Map<String, CategoryRecord> categories) {
        if (txn.transactionId == null || txn.transactionId.trim().isEmpty()) {
            return MappingResult.rejected("取引ID未設定");
        }
        if (txn.merchantCode == null || txn.merchantCode.trim().isEmpty()) {
            return MappingResult.rejected("加盟店コード未設定");
        }
        if (txn.amount == null || txn.amount.signum() <= 0) {
            return MappingResult.rejected("取引金額不正");
        }
        if (txn.transactionDate == null) {
            return MappingResult.rejected("取引日未設定");
        }

        MerchantRecord merchant = merchants.get(txn.merchantCode);
        if (merchant == null) {
            return MappingResult.rejected("加盟店マスタ未登録");
        }
        if (!CHARGEABLE_STATUS.equals(merchant.status)) {
            return MappingResult.rejected("加盟店状態対象外");
        }

        CategoryRecord category = categories.get(merchant.category);
        if (category == null) {
            return MappingResult.rejected("業種マスタ未登録");
        }
        if (!"1".equals(category.activeFlag)) {
            return MappingResult.rejected("業種マスタ無効");
        }

        java.math.BigDecimal roundedAmount = roundAmount(txn.amount, category.riskRank);
        Object feeModel = newFeeModel();
        setFeeModelValue(feeModel, "merchantCode", merchant.merchantCode);
        setFeeModelValue(feeModel, "merchantCategory", merchant.category);
        setFeeModelValue(feeModel, "transactionAmount", roundedAmount);
        setFeeModelValue(feeModel, "transactionDate", txn.transactionDate);
        setFeeModelValue(feeModel, "taxableFlag", category.taxableFlag);
        setFeeModelValue(feeModel, "roundingUnit", roundingUnit(category.riskRank));

        return MappingResult.accepted(feeModel, merchant.category);
    }

    private static java.util.Map<String, MerchantRecord> indexMerchants(java.util.List<MerchantRecord> records) {
        java.util.Map<String, MerchantRecord> index = new java.util.HashMap<>();
        for (MerchantRecord record : records) {
            index.put(record.merchantCode, record);
        }
        return index;
    }

    private static java.util.Map<String, CategoryRecord> indexCategories(java.util.List<CategoryRecord> records) {
        java.util.Map<String, CategoryRecord> index = new java.util.HashMap<>();
        for (CategoryRecord record : records) {
            index.put(record.categoryCode, record);
        }
        return index;
    }

    private static java.math.BigDecimal roundAmount(java.math.BigDecimal amount, String riskRank) {
        java.math.BigDecimal unit = new java.math.BigDecimal(roundingUnit(riskRank));
        return amount.divide(unit, 0, java.math.RoundingMode.DOWN).multiply(unit);
    }

    private static int roundingUnit(String riskRank) {
        if ("D".equals(riskRank)) {
            return 100;
        }
        if ("C".equals(riskRank)) {
            return 10;
        }
        return 1;
    }

    private static Object newFeeModel() {
        try {
            return Class.forName("jp.mirai.pay.fee.FeeModel").getDeclaredConstructor().newInstance();
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("FeeModel生成失敗", e);
        }
    }

    private static void setFeeModelValue(Object target, String property, Object value) {
        String setterName = "set" + Character.toUpperCase(property.charAt(0)) + property.substring(1);
        for (java.lang.reflect.Method method : target.getClass().getMethods()) {
            if (method.getName().equals(setterName) && method.getParameterCount() == 1) {
                try {
                    method.invoke(target, value);
                    return;
                } catch (ReflectiveOperationException e) {
                    throw new IllegalStateException("FeeModel設定失敗 項目=" + property, e);
                }
            }
        }
        try {
            java.lang.reflect.Field field = target.getClass().getDeclaredField(property);
            field.setAccessible(true);
            field.set(target, value);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("FeeModel項目未定義 項目=" + property, e);
        }
    }

    private static final class TransactionRecord {
        private final String transactionId;
        private final String merchantCode;
        private final java.math.BigDecimal amount;
        private final java.time.LocalDate transactionDate;

        private TransactionRecord(String transactionId, String merchantCode, java.math.BigDecimal amount, java.time.LocalDate transactionDate) {
            this.transactionId = transactionId;
            this.merchantCode = merchantCode;
            this.amount = amount;
            this.transactionDate = transactionDate;
        }
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
        private final java.time.LocalDate lastUpdateDate;

        private CategoryRecord(String categoryCode, String categoryName, String riskRank, String taxableFlag, String activeFlag, java.time.LocalDate lastUpdateDate) {
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDate = lastUpdateDate;
        }
    }

    private static final class Summary {
        private int count;
        private java.math.BigDecimal amount = java.math.BigDecimal.ZERO;
    }

    private static final class MappingResult {
        private final boolean accepted;
        private final Object feeModel;
        private final String categoryCode;
        private final String reason;

        private MappingResult(boolean accepted, Object feeModel, String categoryCode, String reason) {
            this.accepted = accepted;
            this.feeModel = feeModel;
            this.categoryCode = categoryCode;
            this.reason = reason;
        }

        private static MappingResult accepted(Object feeModel, String categoryCode) {
            return new MappingResult(true, feeModel, categoryCode, "");
        }

        private static MappingResult rejected(String reason) {
            return new MappingResult(false, null, "", reason);
        }
    }
}
