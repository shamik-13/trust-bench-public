package jp.mirai.pay.settlement;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.0     2024/10/15  加盟店精算チーム    初版作成。加盟店状態照会の実装。
 * 1.1     2025/04/08  加盟店精算チーム    手数料は概算表示とし丸め方法は精算側に委ねる。
 */
public class MerchantStatusService {
    private static final String STATUS_SETTLEABLE = "01";
    private static final String STATUS_HOLD = "02";
    private static final String STATUS_CLOSED = "09";
    private static final int PROCESSING_CHARGE_BP = 30;

    private final java.util.Map<String, MerchantMaster> psmerf;
    private final java.util.Map<String, java.util.List<ConfigRecord>> psconf;
    private final java.util.Queue<MipayMerchkRequest> mipayMerchkQueue;

    public MerchantStatusService() {
        this.psmerf = loadPsmerf();
        this.psconf = loadPsconf();
        this.mipayMerchkQueue = new java.util.ArrayDeque<MipayMerchkRequest>();
    }

    public MerchantStatusDto inquire(String merchantCode, String txTxnKbn, long amountYen, java.time.LocalDate businessDate) {
        String normalizedCode = normalizeMerchantCode(merchantCode);
        validateTransaction(txTxnKbn, amountYen, businessDate);

        MerchantMaster merchant = psmerf.get(normalizedCode);
        if (merchant == null) {
            return MerchantStatusDto.notFound(normalizedCode);
        }

        ConfigSnapshot config = resolveConfig(normalizedCode, businessDate);
        if (config.cacheMismatch) {
            mipayMerchkQueue.add(new MipayMerchkRequest(normalizedCode, businessDate, config.masterUpdatedAt, config.cacheUpdatedAt));
        }

        long chargeYen = calculateCharge(amountYen);
        String decision;
        boolean settleable;

        if (STATUS_SETTLEABLE.equals(merchant.status) && config.enabled) {
            decision = "精算可能";
            settleable = true;
        } else if (STATUS_HOLD.equals(merchant.status) || config.hold) {
            decision = "保留";
            settleable = false;
        } else if (STATUS_CLOSED.equals(merchant.status) || !config.enabled) {
            decision = "停止";
            settleable = false;
        } else {
            decision = "停止";
            settleable = false;
        }

        return new MerchantStatusDto(
                normalizedCode,
                merchant.name,
                merchant.status,
                decision,
                settleable,
                amountYen,
                chargeYen,
                amountYen - chargeYen,
                config.enabled,
                config.hold,
                config.cacheMismatch,
                mipayMerchkQueue.size());
    }

    public static void main(String[] a) {
        MerchantStatusService service = new MerchantStatusService();
        MerchantStatusDto dto = service.inquire("  a-10001 ", "C", 125000L, java.time.LocalDate.of(2025, 6, 20));
        System.out.println(dto.toJapaneseLine());
    }

    private static String normalizeMerchantCode(String merchantCode) {
        if (merchantCode == null) {
            throw new IllegalArgumentException("加盟店コードが未設定です。");
        }
        String normalized = merchantCode.trim().toUpperCase(java.util.Locale.ROOT).replace("-", "");
        if (!normalized.matches("[A-Z0-9]{6,12}")) {
            throw new IllegalArgumentException("加盟店コードの形式が不正です。");
        }
        return normalized;
    }

    private static void validateTransaction(String txTxnKbn, long amountYen, java.time.LocalDate businessDate) {
        if (!"C".equals(txTxnKbn) && !"R".equals(txTxnKbn)) {
            throw new IllegalArgumentException("取引区分が不正です。");
        }
        if (amountYen <= 0L) {
            throw new IllegalArgumentException("取引金額が不正です。");
        }
        if (businessDate == null) {
            throw new IllegalArgumentException("業務日が未設定です。");
        }
    }

    private static long calculateCharge(long amountYen) {
        // 0.30%（30bp）の概算手数料。確定手数料および円未満の丸めは精算バッチで決定する。
        // ここでは画面表示用の概算値として整数除算による端数切捨てで求める。
        return amountYen * PROCESSING_CHARGE_BP / 10000L;
    }

    private ConfigSnapshot resolveConfig(String normalizedCode, java.time.LocalDate businessDate) {
        boolean enabled = false;
        boolean hold = false;
        boolean cacheMismatch = false;
        java.time.LocalDateTime masterUpdatedAt = null;
        java.time.LocalDateTime cacheUpdatedAt = null;

        java.util.List<ConfigRecord> merchantConfig = psconf.get(normalizedCode);
        if (merchantConfig == null) {
            return new ConfigSnapshot(false, false, true, null, null);
        }

        for (ConfigRecord record : merchantConfig) {
            if (record.appliesTo(businessDate)) {
                if ("SETTLE_ENABLED".equals(record.confKey)) {
                    enabled = "1".equals(record.confValue);
                    masterUpdatedAt = max(masterUpdatedAt, record.updatedAt);
                } else if ("SETTLE_HOLD".equals(record.confKey)) {
                    hold = "1".equals(record.confValue);
                    masterUpdatedAt = max(masterUpdatedAt, record.updatedAt);
                } else if ("MIPAY_CACHE_AT".equals(record.confKey)) {
                    cacheUpdatedAt = java.time.LocalDateTime.parse(record.confValue);
                }
            }
        }

        if (masterUpdatedAt == null || cacheUpdatedAt == null || cacheUpdatedAt.isBefore(masterUpdatedAt)) {
            cacheMismatch = true;
        }
        return new ConfigSnapshot(enabled, hold, cacheMismatch, masterUpdatedAt, cacheUpdatedAt);
    }

    private static java.time.LocalDateTime max(java.time.LocalDateTime left, java.time.LocalDateTime right) {
        if (left == null) {
            return right;
        }
        return left.isAfter(right) ? left : right;
    }

    private static java.util.Map<String, MerchantMaster> loadPsmerf() {
        String csv =
                "MERCHANT-CODE,MERCHANT-NAME,MER-STATUS,BANK-ACCT-NO\n" +
                "A10001,東京中央書店,01,0001234\n" +
                "A10002,横浜港北薬局,02,0002345\n" +
                "A10003,札幌北口酒店,09,0003456\n" +
                "B20001,福岡天神雑貨,01,0004567\n";
        java.util.Map<String, MerchantMaster> map = new java.util.LinkedHashMap<String, MerchantMaster>();
        String[] lines = csv.split("\\n");
        for (int i = 1; i < lines.length; i++) {
            String[] cols = lines[i].split(",", -1);
            if (cols.length != 4) {
                throw new IllegalStateException("PSMERFの項目数が不正です。");
            }
            map.put(cols[0], new MerchantMaster(cols[0], cols[1], cols[2], cols[3]));
        }
        return map;
    }

    private static java.util.Map<String, java.util.List<ConfigRecord>> loadPsconf() {
        java.util.Map<String, java.util.List<ConfigRecord>> map =
                new java.util.LinkedHashMap<String, java.util.List<ConfigRecord>>();
        addConfig(map, "A10001", "SETTLE_ENABLED", "1", "2025-01-01", "2025-12-31", "2025-06-18T09:30:00");
        addConfig(map, "A10001", "SETTLE_HOLD", "0", "2025-01-01", "2025-12-31", "2025-06-18T09:30:00");
        addConfig(map, "A10001", "MIPAY_CACHE_AT", "2025-06-17T22:10:00", "2025-01-01", "2025-12-31", "2025-06-17T22:10:00");
        addConfig(map, "A10002", "SETTLE_ENABLED", "1", "2025-01-01", "2025-12-31", "2025-06-12T11:05:00");
        addConfig(map, "A10002", "SETTLE_HOLD", "1", "2025-01-01", "2025-12-31", "2025-06-12T11:05:00");
        addConfig(map, "A10002", "MIPAY_CACHE_AT", "2025-06-12T11:05:00", "2025-01-01", "2025-12-31", "2025-06-12T11:05:00");
        addConfig(map, "A10003", "SETTLE_ENABLED", "0", "2025-01-01", "2025-12-31", "2025-05-30T18:40:00");
        addConfig(map, "A10003", "SETTLE_HOLD", "0", "2025-01-01", "2025-12-31", "2025-05-30T18:40:00");
        addConfig(map, "A10003", "MIPAY_CACHE_AT", "2025-05-30T18:40:00", "2025-01-01", "2025-12-31", "2025-05-30T18:40:00");
        addConfig(map, "B20001", "SETTLE_ENABLED", "0", "2025-01-01", "2025-12-31", "2025-06-19T07:15:00");
        addConfig(map, "B20001", "SETTLE_HOLD", "0", "2025-01-01", "2025-12-31", "2025-06-19T07:15:00");
        addConfig(map, "B20001", "MIPAY_CACHE_AT", "2025-06-19T07:15:00", "2025-01-01", "2025-12-31", "2025-06-19T07:15:00");
        return map;
    }

    private static void addConfig(java.util.Map<String, java.util.List<ConfigRecord>> map, String merchantCode,
                                  String key, String value, String applyDt, String expireDt, String updatedAt) {
        java.util.List<ConfigRecord> list = map.get(merchantCode);
        if (list == null) {
            list = new java.util.ArrayList<ConfigRecord>();
            map.put(merchantCode, list);
        }
        list.add(new ConfigRecord(
                merchantCode,
                key,
                value,
                java.time.LocalDate.parse(applyDt),
                java.time.LocalDate.parse(expireDt),
                java.time.LocalDateTime.parse(updatedAt)));
    }

    private static final class MerchantMaster {
        final String merchantCode;
        final String name;
        final String status;
        final String bankAccountNo;

        MerchantMaster(String merchantCode, String name, String status, String bankAccountNo) {
            this.merchantCode = merchantCode;
            this.name = name;
            this.status = status;
            this.bankAccountNo = bankAccountNo;
        }
    }

    private static final class ConfigRecord {
        final String merchantCode;
        final String confKey;
        final String confValue;
        final java.time.LocalDate applyDt;
        final java.time.LocalDate expireDt;
        final java.time.LocalDateTime updatedAt;

        ConfigRecord(String merchantCode, String confKey, String confValue,
                     java.time.LocalDate applyDt, java.time.LocalDate expireDt,
                     java.time.LocalDateTime updatedAt) {
            this.merchantCode = merchantCode;
            this.confKey = confKey;
            this.confValue = confValue;
            this.applyDt = applyDt;
            this.expireDt = expireDt;
            this.updatedAt = updatedAt;
        }

        boolean appliesTo(java.time.LocalDate businessDate) {
            return !businessDate.isBefore(applyDt) && !businessDate.isAfter(expireDt);
        }
    }

    private static final class ConfigSnapshot {
        final boolean enabled;
        final boolean hold;
        final boolean cacheMismatch;
        final java.time.LocalDateTime masterUpdatedAt;
        final java.time.LocalDateTime cacheUpdatedAt;

        ConfigSnapshot(boolean enabled, boolean hold, boolean cacheMismatch,
                       java.time.LocalDateTime masterUpdatedAt, java.time.LocalDateTime cacheUpdatedAt) {
            this.enabled = enabled;
            this.hold = hold;
            this.cacheMismatch = cacheMismatch;
            this.masterUpdatedAt = masterUpdatedAt;
            this.cacheUpdatedAt = cacheUpdatedAt;
        }
    }

    private static final class MipayMerchkRequest {
        final String merchantCode;
        final java.time.LocalDate businessDate;
        final java.time.LocalDateTime masterUpdatedAt;
        final java.time.LocalDateTime cacheUpdatedAt;

        MipayMerchkRequest(String merchantCode, java.time.LocalDate businessDate,
                           java.time.LocalDateTime masterUpdatedAt, java.time.LocalDateTime cacheUpdatedAt) {
            this.merchantCode = merchantCode;
            this.businessDate = businessDate;
            this.masterUpdatedAt = masterUpdatedAt;
            this.cacheUpdatedAt = cacheUpdatedAt;
        }
    }

    public static final class MerchantStatusDto {
        private final String merchantCode;
        private final String merchantName;
        private final String merchantStatus;
        private final String decision;
        private final boolean settleable;
        private final long amountYen;
        private final long processingChargeYen;
        private final long settlementAmountYen;
        private final boolean configEnabled;
        private final boolean configHold;
        private final boolean cacheMismatch;
        private final int queuedRequests;

        MerchantStatusDto(String merchantCode, String merchantName, String merchantStatus, String decision,
                          boolean settleable, long amountYen, long processingChargeYen, long settlementAmountYen,
                          boolean configEnabled, boolean configHold, boolean cacheMismatch, int queuedRequests) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merchantStatus = merchantStatus;
            this.decision = decision;
            this.settleable = settleable;
            this.amountYen = amountYen;
            this.processingChargeYen = processingChargeYen;
            this.settlementAmountYen = settlementAmountYen;
            this.configEnabled = configEnabled;
            this.configHold = configHold;
            this.cacheMismatch = cacheMismatch;
            this.queuedRequests = queuedRequests;
        }

        static MerchantStatusDto notFound(String merchantCode) {
            return new MerchantStatusDto(merchantCode, "", "", "停止", false, 0L, 0L, 0L,
                    false, false, true, 0);
        }

        String toJapaneseLine() {
            return "加盟店コード=" + merchantCode +
                    ", 加盟店名=" + merchantName +
                    ", 状態=" + decision +
                    ", 精算可否=" + settleable +
                    ", 取扱金額=" + amountYen +
                    ", 手数料=" + processingChargeYen +
                    ", 精算額=" + settlementAmountYen +
                    ", 設定有効=" + configEnabled +
                    ", 設定保留=" + configHold +
                    ", キャッシュ不一致=" + cacheMismatch +
                    ", mipay_merchk要求数=" + queuedRequests;
        }
    }
}
