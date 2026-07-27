public class MerchantRiskService {
    /**
     * 変更履歴
     * 版数 / 年月日 / 担当 / 概要
     * 1.0 / 2024-04-01 / 売買管理基盤 / 初版作成
     * 1.1 / 2024-09-12 / リスク管理部 / 未登録加盟店の高リスク既定応答を追加
     * 1.2 / 2025-02-18 / 取引統制運用 / 停止区分参照時の更新時刻確認を追加
     */

    private static final String DEFAULT_MCC = "5999";
    private static final String DEFAULT_COUNTRY_CD = "JP";
    private static final String DEFAULT_RISK_RANK = "Z";
    private static final String DEFAULT_STATUS = "停止";
    private static final long CACHE_TTL_MILLIS = 300_000L;

    private final java.util.Map<String, CdmerfRecord> cdmerf;
    private final java.util.Map<String, CacheEntry> cache = new java.util.HashMap<>();

    public MerchantRiskService(java.util.Collection<CdmerfRecord> records) {
        if (records == null) {
            throw new IllegalArgumentException("加盟店マスタが未指定です");
        }
        this.cdmerf = new java.util.HashMap<>();
        for (CdmerfRecord record : records) {
            validateRecord(record);
            this.cdmerf.put(record.merchantCode, record);
        }
    }

    public static void main(String[] a) {
        java.util.List<CdmerfRecord> seed = java.util.Arrays.asList(
                new CdmerfRecord("10000001", "トウキョウショウケン", "6211", "A", "有効", "JP", 1_716_192_000_000L),
                new CdmerfRecord("10000002", "オオサカブツリュウ", "4215", "B", "監視", "JP", 1_716_278_400_000L),
                new CdmerfRecord("10000003", "ナゴヤデンシ", "5732", "C", "有効", "JP", 1_716_364_800_000L),
                new CdmerfRecord("90000001", "カイガイキンセン", "6012", "E", "停止", "SG", 1_716_451_200_000L)
        );

        MerchantRiskService service = new MerchantRiskService(seed);
        MerchantRiskResult result = service.lookup("10000002");
        System.out.println(result.toOperatorLine());
    }

    public MerchantRiskResult lookup(String merchantCode) {
        String normalizedCode = normalizeMerchantCode(merchantCode);
        long now = System.currentTimeMillis();

        CacheEntry cached = cache.get(normalizedCode);
        CdmerfRecord latest = cdmerf.get(normalizedCode);

        if (cached != null && now - cached.cachedAtMillis <= CACHE_TTL_MILLIS) {
            if (latest != null && "停止".equals(latest.status)
                    && latest.updatedAtMillis > cached.recordUpdatedAtMillis) {
                cache.remove(normalizedCode);
            } else {
                return cached.result;
            }
        }

        if (latest == null) {
            MerchantRiskResult defaultResult = new MerchantRiskResult(
                    normalizedCode,
                    "未登録加盟店",
                    DEFAULT_MCC,
                    DEFAULT_COUNTRY_CD,
                    DEFAULT_RISK_RANK,
                    DEFAULT_STATUS,
                    true,
                    now
            );
            cache.put(normalizedCode, new CacheEntry(defaultResult, now, now));
            return defaultResult;
        }

        MerchantRiskResult result = new MerchantRiskResult(
                latest.merchantCode,
                latest.merchantNameKana,
                latest.mcc,
                latest.countryCd,
                latest.riskRank,
                latest.status,
                false,
                latest.updatedAtMillis
        );
        cache.put(normalizedCode, new CacheEntry(result, now, latest.updatedAtMillis));
        return result;
    }

    public void upsertCdmerf(String merchantCode,
                            String merchantNameKana,
                            String mcc,
                            String riskRank,
                            String status,
                            String countryCd,
                            long updatedAtMillis) {
        CdmerfRecord record = new CdmerfRecord(
                normalizeMerchantCode(merchantCode),
                requireText(merchantNameKana, "加盟店カナ名"),
                requireDigits(mcc, "ＭＣＣ", 4),
                normalizeRank(riskRank),
                normalizeStatus(status),
                requireCountry(countryCd),
                updatedAtMillis
        );
        validateRecord(record);
        cdmerf.put(record.merchantCode, record);
    }

    private static String normalizeMerchantCode(String merchantCode) {
        return requireDigits(merchantCode, "加盟店コード", 8);
    }

    private static String normalizeRank(String riskRank) {
        String value = requireText(riskRank, "リスクランク").trim().toUpperCase(java.util.Locale.ROOT);
        if (!java.util.Arrays.asList("A", "B", "C", "D", "E", "Z").contains(value)) {
            throw new IllegalArgumentException("リスクランクが不正です: " + value);
        }
        return value;
    }

    private static String normalizeStatus(String status) {
        String value = requireText(status, "状態").trim();
        if (!java.util.Arrays.asList("有効", "監視", "停止").contains(value)) {
            throw new IllegalArgumentException("状態が不正です: " + value);
        }
        return value;
    }

    private static String requireCountry(String countryCd) {
        String value = requireText(countryCd, "国コード").trim().toUpperCase(java.util.Locale.ROOT);
        if (!value.matches("[A-Z]{2}")) {
            throw new IllegalArgumentException("国コードが不正です: " + value);
        }
        return value;
    }

    private static String requireDigits(String value, String itemName, int length) {
        String normalized = requireText(value, itemName).trim();
        if (!normalized.matches("\\d{" + length + "}")) {
            throw new IllegalArgumentException(itemName + "が不正です: " + normalized);
        }
        return normalized;
    }

    private static String requireText(String value, String itemName) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(itemName + "が未設定です");
        }
        return value;
    }

    private static void validateRecord(CdmerfRecord record) {
        if (record == null) {
            throw new IllegalArgumentException("加盟店レコードが未指定です");
        }
        normalizeMerchantCode(record.merchantCode);
        requireText(record.merchantNameKana, "加盟店カナ名");
        requireDigits(record.mcc, "ＭＣＣ", 4);
        normalizeRank(record.riskRank);
        normalizeStatus(record.status);
        requireCountry(record.countryCd);
        if (record.updatedAtMillis <= 0L) {
            throw new IllegalArgumentException("更新時刻が不正です");
        }
    }

    private static final class CdmerfRecord {
        private final String merchantCode;
        private final String merchantNameKana;
        private final String mcc;
        private final String riskRank;
        private final String status;
        private final String countryCd;
        private final long updatedAtMillis;

        private CdmerfRecord(String merchantCode,
                             String merchantNameKana,
                             String mcc,
                             String riskRank,
                             String status,
                             String countryCd,
                             long updatedAtMillis) {
            this.merchantCode = merchantCode;
            this.merchantNameKana = merchantNameKana;
            this.mcc = mcc;
            this.riskRank = riskRank;
            this.status = status;
            this.countryCd = countryCd;
            this.updatedAtMillis = updatedAtMillis;
        }
    }

    private static final class CacheEntry {
        private final MerchantRiskResult result;
        private final long cachedAtMillis;
        private final long recordUpdatedAtMillis;

        private CacheEntry(MerchantRiskResult result, long cachedAtMillis, long recordUpdatedAtMillis) {
            this.result = result;
            this.cachedAtMillis = cachedAtMillis;
            this.recordUpdatedAtMillis = recordUpdatedAtMillis;
        }
    }

    public static final class MerchantRiskResult {
        private final String merchantCode;
        private final String merchantNameKana;
        private final String mcc;
        private final String countryCd;
        private final String riskRank;
        private final String status;
        private final boolean defaulted;
        private final long referencedAtMillis;

        private MerchantRiskResult(String merchantCode,
                                   String merchantNameKana,
                                   String mcc,
                                   String countryCd,
                                   String riskRank,
                                   String status,
                                   boolean defaulted,
                                   long referencedAtMillis) {
            this.merchantCode = merchantCode;
            this.merchantNameKana = merchantNameKana;
            this.mcc = mcc;
            this.countryCd = countryCd;
            this.riskRank = riskRank;
            this.status = status;
            this.defaulted = defaulted;
            this.referencedAtMillis = referencedAtMillis;
        }

        public String merchantCode() {
            return merchantCode;
        }

        public String merchantNameKana() {
            return merchantNameKana;
        }

        public String mcc() {
            return mcc;
        }

        public String countryCd() {
            return countryCd;
        }

        public String riskRank() {
            return riskRank;
        }

        public String status() {
            return status;
        }

        public boolean defaulted() {
            return defaulted;
        }

        public long referencedAtMillis() {
            return referencedAtMillis;
        }

        public String toOperatorLine() {
            return "加盟店=" + merchantCode
                    + ", 名称=" + merchantNameKana
                    + ", ＭＣＣ=" + mcc
                    + ", 国=" + countryCd
                    + ", ランク=" + riskRank
                    + ", 状態=" + status
                    + ", 既定=" + (defaulted ? "はい" : "いいえ");
        }
    }
}
