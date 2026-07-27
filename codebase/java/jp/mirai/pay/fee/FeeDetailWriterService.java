package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2024/06/24  みらいペイ システム部 加盟店・手数料チーム  初版作成。手数料モデル結果のFE明細変換とFEE-ID重複検査を実装。
 */
public class FeeDetailWriterService {

    private static final String STATUS_CHARGEABLE = "01";
    private static final java.nio.charset.Charset FILE_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    public static void main(String[] a) {
        try {
            java.nio.file.Path work = java.nio.file.Files.createTempDirectory("pffeef-fe-");
            java.nio.file.Path pffeef = work.resolve("PFFEEF.dat");

            // FE-MDR-RATE/FE-FEE-AMT は規程に基づき MdrFeeEngine が算定済みの値を受け取る。
            // 当サービスは料率値を保持・再計算せず、明細への変換と FEE-ID 重複検査のみを担う。
            java.util.List<FeeRecord> existing = java.util.Arrays.asList(
                    new FeeRecord("FE20240624000001", "M000000101", 125000L, rateRef("C1"), 3500L),
                    new FeeRecord("FE20240624000002", "M000000207", 86000L, rateRef("C2"), 2666L),
                    new FeeRecord("FE20240624000003", "M000000305", 42000L, rateRef("C3"), 504L)
            );
            writeAll(pffeef, existing);

            java.util.List<ModelResult> modelResults = java.util.Arrays.asList(
                    new ModelResult("FE20240624000004", "M000000410", "C4", "01", 99000L, 3564L),
                    new ModelResult("FE20240624000002", "M000000207", "C2", "01", 86000L, 2666L),
                    new ModelResult("FE20240624000005", "M000000512", "C5", "02", 301000L, 14448L),
                    new ModelResult("FE20240624000006", "M000000118", "C1", "01", 15400L, 500L)
            );

            WriteResult result = appendFeeDetails(pffeef, modelResults);
            System.out.println(result.operatorMessage());
        } catch (Exception e) {
            System.err.println("手数料明細書込サービス異常終了: " + e.getMessage());
            System.exit(1);
        }
    }

    private static WriteResult appendFeeDetails(java.nio.file.Path pffeef, java.util.List<ModelResult> models)
            throws java.io.IOException {
        java.util.Map<String, FeeRecord> existing = readExisting(pffeef);
        java.util.List<FeeRecord> appendTargets = new java.util.ArrayList<>();
        java.util.Map<String, MerchantAggregate> aggregateByMerchant = new java.util.LinkedHashMap<>();

        for (ModelResult model : models) {
            validateModel(model);
            if (!STATUS_CHARGEABLE.equals(model.merchantStatus)) {
                continue;
            }

            FeeRecord record = toFeeRecord(model);
            FeeRecord prior = existing.get(record.feeId);
            if (prior != null && isSameTransaction(prior, record)) {
                return WriteResult.duplicate(record.feeId, record.merchantCode);
            }
            if (prior != null) {
                throw new IllegalStateException("FEE-ID重複: " + record.feeId);
            }

            appendTargets.add(record);
            aggregateByMerchant.computeIfAbsent(record.merchantCode, MerchantAggregate::new).add(record);
        }

        appendAll(pffeef, appendTargets);
        return WriteResult.success(appendTargets.size(), aggregateByMerchant);
    }

    @SuppressWarnings("unused")
    private static FeeRecord fromFeeModel(FeeModel model) {
        Object source = model;
        return toFeeRecord(new ModelResult(
                textValue(source, "feeId", "getFeeId"),
                textValue(source, "merchantCode", "getMerchantCode"),
                textValue(source, "merchantCategory", "getMerchantCategory"),
                textValue(source, "merchantStatus", "getMerchantStatus"),
                longValue(source, "transactionAmount", "txnAmount", "getTransactionAmount", "getTxnAmount"),
                longValue(source, "feeAmount", "feeAmt", "getFeeAmount", "getFeeAmt")
        ));
    }

    private static FeeRecord toFeeRecord(ModelResult model) {
        // 手数料額は MdrFeeEngine が規程料率で算定した値(FE-FEE-AMT)をそのまま採用する。
        // 当サービスでは料率値を保持せず、業種区分由来の料率参照のみを記録する。
        return new FeeRecord(model.feeId, model.merchantCode, model.transactionAmount,
                rateRef(model.merchantCategory), model.feeAmount);
    }

    private static String rateRef(String category) {
        // 料率の数値は持たない。業種区分と料率出所(規程/MdrFeeEngine)のみを表す参照値。
        return "規程:" + category + ":MdrFeeEngine";
    }

    private static void validateModel(ModelResult model) {
        requireText(model.feeId, "FEE-ID");
        requireText(model.merchantCode, "MERCHANT-CODE");
        if (!isCanonicalCategory(model.merchantCategory)) {
            throw new IllegalArgumentException("業種区分不正: " + model.merchantCategory);
        }
        if (!("01".equals(model.merchantStatus) || "02".equals(model.merchantStatus) || "09".equals(model.merchantStatus))) {
            throw new IllegalArgumentException("加盟店状態不正: " + model.merchantStatus);
        }
        if (model.transactionAmount <= 0L) {
            throw new IllegalArgumentException("取引金額不正: " + model.transactionAmount);
        }
        if (model.feeAmount < 0L || model.feeAmount > model.transactionAmount) {
            throw new IllegalArgumentException("手数料金額不正: " + model.feeAmount);
        }
    }

    private static boolean isCanonicalCategory(String category) {
        return "C1".equals(category) || "C2".equals(category) || "C3".equals(category)
                || "C4".equals(category) || "C5".equals(category);
    }

    private static boolean isSameTransaction(FeeRecord left, FeeRecord right) {
        return left.merchantCode.equals(right.merchantCode)
                && left.transactionAmount == right.transactionAmount
                && left.mdrRate.equals(right.mdrRate)
                && left.feeAmount == right.feeAmount;
    }

    private static java.util.Map<String, FeeRecord> readExisting(java.nio.file.Path pffeef) throws java.io.IOException {
        java.util.Map<String, FeeRecord> records = new java.util.LinkedHashMap<>();
        if (!java.nio.file.Files.exists(pffeef)) {
            return records;
        }
        for (String line : java.nio.file.Files.readAllLines(pffeef, FILE_CHARSET)) {
            if (line.trim().isEmpty()) {
                continue;
            }
            FeeRecord record = FeeRecord.parse(line);
            FeeRecord duplicate = records.putIfAbsent(record.feeId, record);
            if (duplicate != null) {
                throw new IllegalStateException("既存順編成内FEE-ID重複: " + record.feeId);
            }
        }
        return records;
    }

    private static void writeAll(java.nio.file.Path pffeef, java.util.List<FeeRecord> records) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        for (FeeRecord record : records) {
            lines.add(record.format());
        }
        java.nio.file.Files.write(pffeef, lines, FILE_CHARSET,
                java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING);
    }

    private static void appendAll(java.nio.file.Path pffeef, java.util.List<FeeRecord> records) throws java.io.IOException {
        if (records.isEmpty()) {
            return;
        }
        java.util.List<String> lines = new java.util.ArrayList<>();
        for (FeeRecord record : records) {
            lines.add(record.format());
        }
        java.nio.file.Files.write(pffeef, lines, FILE_CHARSET,
                java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND);
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "未設定");
        }
    }

    private static String textValue(Object source, String fieldName, String getterName) {
        Object value = reflectiveValue(source, fieldName, getterName);
        return value == null ? null : String.valueOf(value);
    }

    private static long longValue(Object source, String fieldName, String alternateFieldName, String getterName, String alternateGetterName) {
        Object value = reflectiveValue(source, fieldName, getterName);
        if (value == null) {
            value = reflectiveValue(source, alternateFieldName, alternateGetterName);
        }
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        return Long.parseLong(String.valueOf(value));
    }

    private static Object reflectiveValue(Object source, String fieldName, String getterName) {
        Class<?> type = source.getClass();
        try {
            java.lang.reflect.Method method = type.getMethod(getterName);
            return method.invoke(source);
        } catch (ReflectiveOperationException ignored) {
            try {
                java.lang.reflect.Field field = type.getDeclaredField(fieldName);
                field.setAccessible(true);
                return field.get(source);
            } catch (ReflectiveOperationException e) {
                throw new IllegalArgumentException("FeeModel項目取得不可: " + fieldName);
            }
        }
    }

    private static final class ModelResult {
        private final String feeId;
        private final String merchantCode;
        private final String merchantCategory;
        private final String merchantStatus;
        private final long transactionAmount;
        private final long feeAmount;

        private ModelResult(String feeId, String merchantCode, String merchantCategory, String merchantStatus,
                            long transactionAmount, long feeAmount) {
            this.feeId = feeId;
            this.merchantCode = merchantCode;
            this.merchantCategory = merchantCategory;
            this.merchantStatus = merchantStatus;
            this.transactionAmount = transactionAmount;
            this.feeAmount = feeAmount;
        }
    }

    private static final class FeeRecord {
        private final String feeId;
        private final String merchantCode;
        private final long transactionAmount;
        private final String mdrRate;
        private final long feeAmount;

        private FeeRecord(String feeId, String merchantCode, long transactionAmount,
                          String mdrRate, long feeAmount) {
            this.feeId = feeId;
            this.merchantCode = merchantCode;
            this.transactionAmount = transactionAmount;
            this.mdrRate = mdrRate;
            this.feeAmount = feeAmount;
        }

        private static FeeRecord parse(String line) {
            String[] columns = line.split(",", -1);
            if (columns.length != 5) {
                throw new IllegalArgumentException("PFFEEF項目数不正: " + line);
            }
            return new FeeRecord(
                    columns[0],
                    columns[1],
                    Long.parseLong(columns[2]),
                    columns[3],
                    Long.parseLong(columns[4])
            );
        }

        private String format() {
            return feeId + "," + merchantCode + "," + transactionAmount + ","
                    + mdrRate + "," + feeAmount;
        }
    }

    private static final class MerchantAggregate {
        private final String merchantCode;
        private int count;
        private long transactionTotal;
        private long feeTotal;

        private MerchantAggregate(String merchantCode) {
            this.merchantCode = merchantCode;
        }

        private void add(FeeRecord record) {
            count++;
            transactionTotal += record.transactionAmount;
            feeTotal += record.feeAmount;
        }

        private String summary() {
            return merchantCode + ":件数=" + count + ",取引金額=" + transactionTotal + ",手数料=" + feeTotal;
        }
    }

    private static final class WriteResult {
        private final boolean duplicate;
        private final String message;

        private WriteResult(boolean duplicate, String message) {
            this.duplicate = duplicate;
            this.message = message;
        }

        private static WriteResult duplicate(String feeId, String merchantCode) {
            return new WriteResult(true, "重複エラー: FEE-ID=" + feeId + ",加盟店=" + merchantCode + ",差分書込停止");
        }

        private static WriteResult success(int writtenCount, java.util.Map<String, MerchantAggregate> aggregateByMerchant) {
            StringBuilder builder = new StringBuilder();
            builder.append("正常終了: 追記件数=").append(writtenCount);
            for (MerchantAggregate aggregate : aggregateByMerchant.values()) {
                builder.append(" / ").append(aggregate.summary());
            }
            return new WriteResult(false, builder.toString());
        }

        private String operatorMessage() {
            return message;
        }
    }
}
