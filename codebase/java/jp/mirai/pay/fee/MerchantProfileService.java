package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/04/01  決済基盤    初版作成
 */
public class MerchantProfileService {

    private static final String STATUS_CHARGEABLE = "01";
    private static final String STATUS_STOPPED = "02";
    private static final String STATUS_CANCELED = "09";

    private static final java.util.Map<String, PfmerfRecord> PFMERF = createMerchantFile();
    private static final java.util.Map<String, PmcatfRecord> PMCATF = createCategoryFile();

    public static void main(String[] a) {
        MerchantProfileService service = new MerchantProfileService();
        String merchantCode = a.length == 0 ? "M1000001" : a[0];

        try {
            MerchantProfileDto dto = service照会(service, merchantCode);
            System.out.println("加盟店コード=" + dto.merchantCode);
            System.out.println("加盟店名=" + dto.merchantName);
            System.out.println("業種区分=" + dto.categoryCode);
            System.out.println("業種名=" + dto.categoryName);
            System.out.println("課税対象区分=" + dto.taxableFlag);
            System.out.println("リスクランク=" + dto.riskRank);
            System.out.println("制限フラグ=" + dto.restrictedFlag);
        } catch (IllegalArgumentException e) {
            System.err.println("照会エラー: " + e.getMessage());
            System.exit(8);
        }
    }

    private static MerchantProfileDto service照会(MerchantProfileService service, String merchantCode) {
        return service.findByMerchantCode(merchantCode);
    }

    private MerchantProfileDto findByMerchantCode(String merchantCode) {
        String normalizedCode = normalizeMerchantCode(merchantCode);

        PfmerfRecord merchant = PFMERF.get(normalizedCode);
        if (merchant == null) {
            throw new IllegalArgumentException("加盟店が存在しません。加盟店コード=" + normalizedCode);
        }

        PmcatfRecord category = PMCATF.get(merchant.merCategory);
        if (category == null) {
            throw new IllegalArgumentException("業種マスタが存在しません。業種区分=" + merchant.merCategory);
        }
        if (!"1".equals(category.activeFlag)) {
            throw new IllegalArgumentException("業種マスタが無効です。業種区分=" + merchant.merCategory);
        }

        boolean restricted = STATUS_STOPPED.equals(merchant.merStatus)
                || STATUS_CANCELED.equals(merchant.merStatus);
        boolean chargeable = STATUS_CHARGEABLE.equals(merchant.merStatus);

        return new MerchantProfileDto(
                merchant.merchantCode,
                merchant.merchantName,
                merchant.merCategory,
                category.categoryName,
                merchant.merStatus,
                category.taxableFlag,
                category.riskRank,
                restricted,
                chargeable,
                category.lastUpdateDt
        );
    }

    private String normalizeMerchantCode(String merchantCode) {
        if (merchantCode == null) {
            throw new IllegalArgumentException("加盟店コードが未指定です。");
        }

        String normalized = merchantCode.trim();
        if (normalized.isEmpty()) {
            throw new IllegalArgumentException("加盟店コードが空です。");
        }
        if (normalized.length() > 12) {
            throw new IllegalArgumentException("加盟店コードの桁数が不正です。");
        }
        for (int i = 0; i < normalized.length(); i++) {
            char c = normalized.charAt(i);
            if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z'))) {
                throw new IllegalArgumentException("加盟店コードに使用できない文字があります。");
            }
        }

        return normalized;
    }

    private static java.util.Map<String, PfmerfRecord> createMerchantFile() {
        java.util.Map<String, PfmerfRecord> map = new java.util.LinkedHashMap<String, PfmerfRecord>();
        putMerchant(map, "M1000001", "東京中央ストア", "C1", "01");
        putMerchant(map, "M1000002", "青山食堂", "C2", "01");
        putMerchant(map, "M1000003", "横浜市納付窓口", "C3", "01");
        putMerchant(map, "M1000004", "ミライ通販", "C4", "02");
        putMerchant(map, "M1000005", "新宿チケットセンター", "C5", "09");
        return java.util.Collections.unmodifiableMap(map);
    }

    private static void putMerchant(java.util.Map<String, PfmerfRecord> map,
                                    String merchantCode,
                                    String merchantName,
                                    String merCategory,
                                    String merStatus) {
        map.put(merchantCode, new PfmerfRecord(merchantCode, merchantName, merCategory, merStatus));
    }

    private static java.util.Map<String, PmcatfRecord> createCategoryFile() {
        java.util.Map<String, PmcatfRecord> map = new java.util.LinkedHashMap<String, PmcatfRecord>();
        putCategory(map, "C1", "一般物販", "L", "1", "1", "20240301");
        putCategory(map, "C2", "飲食", "M", "1", "1", "20240301");
        putCategory(map, "C3", "公共・公金", "L", "0", "1", "20240301");
        putCategory(map, "C4", "EC・通信販売", "M", "1", "1", "20240301");
        putCategory(map, "C5", "高リスク業種", "H", "1", "1", "20240301");
        return java.util.Collections.unmodifiableMap(map);
    }

    private static void putCategory(java.util.Map<String, PmcatfRecord> map,
                                    String categoryCode,
                                    String categoryName,
                                    String riskRank,
                                    String taxableFlag,
                                    String activeFlag,
                                    String lastUpdateDt) {
        map.put(categoryCode, new PmcatfRecord(
                categoryCode,
                categoryName,
                riskRank,
                taxableFlag,
                activeFlag,
                lastUpdateDt
        ));
    }

    private static final class PfmerfRecord {
        private final String merchantCode;
        private final String merchantName;
        private final String merCategory;
        private final String merStatus;

        private PfmerfRecord(String merchantCode, String merchantName, String merCategory, String merStatus) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merCategory = merCategory;
            this.merStatus = merStatus;
        }
    }

    private static final class PmcatfRecord {
        private final String categoryCode;
        private final String categoryName;
        private final String riskRank;
        private final String taxableFlag;
        private final String activeFlag;
        private final String lastUpdateDt;

        private PmcatfRecord(String categoryCode,
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
    }

    private static final class MerchantProfileDto {
        private final String merchantCode;
        private final String merchantName;
        private final String categoryCode;
        private final String categoryName;
        private final String merchantStatus;
        private final String taxableFlag;
        private final String riskRank;
        private final boolean restrictedFlag;
        private final boolean chargeable;
        private final String categoryLastUpdateDt;

        private MerchantProfileDto(String merchantCode,
                                   String merchantName,
                                   String categoryCode,
                                   String categoryName,
                                   String merchantStatus,
                                   String taxableFlag,
                                   String riskRank,
                                   boolean restrictedFlag,
                                   boolean chargeable,
                                   String categoryLastUpdateDt) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.merchantStatus = merchantStatus;
            this.taxableFlag = taxableFlag;
            this.riskRank = riskRank;
            this.restrictedFlag = restrictedFlag;
            this.chargeable = chargeable;
            this.categoryLastUpdateDt = categoryLastUpdateDt;
        }
    }
}
