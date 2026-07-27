package jp.mirai.pay.authorization;

public class MerchantProfileLookupService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2024-07-16  みらいペイ システム部  初版作成
     */

    private static final int MERCHANT_CODE_LENGTH = 10;
    private static final java.math.BigDecimal ZERO = java.math.BigDecimal.ZERO;

    private static final java.util.Map<String, PymerfRow> PYMERF = createPymerf();

    public static void main(String[] a) {
        if (a == null || a.length != 1) {
            System.err.println("引数誤り: 加盟店コードを1件指定してください");
            System.exit(2);
        }

        try {
            LookupResult result = lookup(a[0]);
            System.out.println(result.toCacheLine());
        } catch (ServiceException e) {
            System.err.println(e.code + ":" + e.getMessage());
            System.exit(e.exitCode);
        }
    }

    private static LookupResult lookup(String rawMerchantCode) {
        String merchantCode = normalizeMerchantCode(rawMerchantCode);
        PymerfRow row = PYMERF.get(merchantCode);

        if (row == null) {
            throw new ServiceException("E-MER-404", "加盟店未登録: " + merchantCode, 10);
        }

        validateMasterRow(row);

        if (!"1".equals(row.merchantStatus)) {
            throw new ServiceException("E-MER-403", "加盟店停止中: " + merchantCode, 11);
        }

        int cacheSeconds = decideCacheSeconds(row);
        boolean highRisk = "D".equals(row.riskRank) || "E".equals(row.riskRank);

        return new LookupResult(
                row.merchantCode,
                row.merchantStatus,
                row.mcc,
                row.dailyLimitAmount,
                row.riskRank,
                row.settleCycleKbn,
                highRisk,
                cacheSeconds);
    }

    private static String normalizeMerchantCode(String rawMerchantCode) {
        if (rawMerchantCode == null) {
            throw new ServiceException("E-MER-001", "加盟店コード未設定", 2);
        }

        String merchantCode = rawMerchantCode.trim();
        if (merchantCode.length() != MERCHANT_CODE_LENGTH) {
            throw new ServiceException("E-MER-002", "加盟店コード桁数誤り: " + merchantCode, 2);
        }

        for (int i = 0; i < merchantCode.length(); i++) {
            char c = merchantCode.charAt(i);
            if (c < '0' || c > '9') {
                throw new ServiceException("E-MER-003", "加盟店コード文字種誤り: " + merchantCode, 2);
            }
        }
        return merchantCode;
    }

    private static void validateMasterRow(PymerfRow row) {
        if (!isStatus(row.merchantStatus)) {
            throw new ServiceException("E-MER-901", "加盟店状態区分不正: " + row.merchantCode, 20);
        }
        if (!isMcc(row.mcc)) {
            throw new ServiceException("E-MER-902", "MCC不正: " + row.merchantCode, 20);
        }
        if (row.dailyLimitAmount == null || row.dailyLimitAmount.compareTo(ZERO) < 0) {
            throw new ServiceException("E-MER-903", "日次上限金額不正: " + row.merchantCode, 20);
        }
        if (!isRiskRank(row.riskRank)) {
            throw new ServiceException("E-MER-904", "リスクランク不正: " + row.merchantCode, 20);
        }
        if (!isSettleCycle(row.settleCycleKbn)) {
            throw new ServiceException("E-MER-905", "精算サイクル区分不正: " + row.merchantCode, 20);
        }
    }

    private static boolean isStatus(String status) {
        return "1".equals(status) || "2".equals(status) || "9".equals(status);
    }

    private static boolean isMcc(String mcc) {
        if (mcc == null || mcc.length() != 4) {
            return false;
        }
        for (int i = 0; i < mcc.length(); i++) {
            char c = mcc.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    private static boolean isRiskRank(String riskRank) {
        return "A".equals(riskRank)
                || "B".equals(riskRank)
                || "C".equals(riskRank)
                || "D".equals(riskRank)
                || "E".equals(riskRank);
    }

    private static boolean isSettleCycle(String settleCycleKbn) {
        return "D1".equals(settleCycleKbn)
                || "D2".equals(settleCycleKbn)
                || "W1".equals(settleCycleKbn)
                || "M1".equals(settleCycleKbn);
    }

    private static int decideCacheSeconds(PymerfRow row) {
        if ("D".equals(row.riskRank) || "E".equals(row.riskRank)) {
            return 60;
        }
        if (row.dailyLimitAmount.compareTo(new java.math.BigDecimal("10000000")) >= 0) {
            return 120;
        }
        if ("M1".equals(row.settleCycleKbn)) {
            return 600;
        }
        return 300;
    }

    private static java.util.Map<String, PymerfRow> createPymerf() {
        java.util.Map<String, PymerfRow> map = new java.util.LinkedHashMap<>();

        put(map, "1000000001", "1", "6211", "50000000", "A", "D1");
        put(map, "1000000002", "1", "5812", "1200000", "B", "D2");
        put(map, "1000000003", "1", "5732", "2500000", "B", "W1");
        put(map, "1000000004", "2", "5999", "300000", "C", "D2");
        put(map, "1000000005", "1", "4112", "900000", "C", "W1");
        put(map, "1000000006", "1", "6012", "8000000", "D", "D1");
        put(map, "1000000007", "9", "5944", "100000", "E", "M1");
        put(map, "1000000008", "1", "4814", "1500000", "C", "D2");
        put(map, "1000000009", "1", "7011", "4500000", "B", "W1");
        put(map, "1000000010", "1", "7538", "700000", "D", "D2");
        put(map, "1000000011", "1", "5411", "3500000", "A", "D1");
        put(map, "1000000012", "2", "5968", "200000", "E", "M1");

        return java.util.Collections.unmodifiableMap(map);
    }

    private static void put(
            java.util.Map<String, PymerfRow> map,
            String merchantCode,
            String merchantStatus,
            String mcc,
            String dailyLimitAmount,
            String riskRank,
            String settleCycleKbn) {
        map.put(merchantCode, new PymerfRow(
                merchantCode,
                merchantStatus,
                mcc,
                new java.math.BigDecimal(dailyLimitAmount),
                riskRank,
                settleCycleKbn));
    }

    private static final class PymerfRow {
        private final String merchantCode;
        private final String merchantStatus;
        private final String mcc;
        private final java.math.BigDecimal dailyLimitAmount;
        private final String riskRank;
        private final String settleCycleKbn;

        private PymerfRow(
                String merchantCode,
                String merchantStatus,
                String mcc,
                java.math.BigDecimal dailyLimitAmount,
                String riskRank,
                String settleCycleKbn) {
            this.merchantCode = merchantCode;
            this.merchantStatus = merchantStatus;
            this.mcc = mcc;
            this.dailyLimitAmount = dailyLimitAmount;
            this.riskRank = riskRank;
            this.settleCycleKbn = settleCycleKbn;
        }
    }

    private static final class LookupResult {
        private final String merchantCode;
        private final String merchantStatus;
        private final String mcc;
        private final java.math.BigDecimal dailyLimitAmount;
        private final String riskRank;
        private final String settleCycleKbn;
        private final boolean highRisk;
        private final int cacheSeconds;

        private LookupResult(
                String merchantCode,
                String merchantStatus,
                String mcc,
                java.math.BigDecimal dailyLimitAmount,
                String riskRank,
                String settleCycleKbn,
                boolean highRisk,
                int cacheSeconds) {
            this.merchantCode = merchantCode;
            this.merchantStatus = merchantStatus;
            this.mcc = mcc;
            this.dailyLimitAmount = dailyLimitAmount;
            this.riskRank = riskRank;
            this.settleCycleKbn = settleCycleKbn;
            this.highRisk = highRisk;
            this.cacheSeconds = cacheSeconds;
        }

        private String toCacheLine() {
            return "加盟店コード=" + merchantCode
                    + ",状態=" + merchantStatus
                    + ",MCC=" + mcc
                    + ",日次上限=" + dailyLimitAmount.toPlainString()
                    + ",リスクランク=" + riskRank
                    + ",精算サイクル=" + settleCycleKbn
                    + ",高リスク=" + (highRisk ? "1" : "0")
                    + ",キャッシュ秒=" + cacheSeconds;
        }
    }

    private static final class ServiceException extends RuntimeException {
        private final String code;
        private final int exitCode;

        private ServiceException(String code, String message, int exitCode) {
            super(message);
            this.code = code;
            this.exitCode = exitCode;
        }
    }
}
