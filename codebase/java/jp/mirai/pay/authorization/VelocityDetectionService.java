package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-09-11  みらいペイ システム部  初版作成
 */
public class VelocityDetectionService {
    private static final String 決定_承認 = "A";
    private static final String 決定_否認 = "D";
    private static final String 理由_残高不足 = "LIM";
    private static final String 理由_状態不正 = "STS";
    private static final String 理由_通貨対象外 = "CUR";

    private static final long 窓幅ミリ秒 = 10L * 60L * 1000L;

    private VelocityDetectionService() {
    }

    public static void main(String[] a) {
        System.out.println("速度違反検知サービス: 入出力は呼出元制御");
    }

    public static java.util.List<Object> updateVelocity(
            java.util.List<?> pyvelf,
            java.util.List<?> pytxnf,
            java.util.List<?> pyarspf) {
        java.util.Map<String, VelocityRow> 速度表 = new java.util.LinkedHashMap<String, VelocityRow>();
        for (Object 行 : nullToEmpty(pyvelf)) {
            VelocityRow 速度 = VelocityRow.from(行);
            速度表.put(速度.key(), 速度);
        }

        java.util.Map<String, AuthResponseRow> 応答表 = new java.util.HashMap<String, AuthResponseRow>();
        for (Object 行 : nullToEmpty(pyarspf)) {
            AuthResponseRow 応答 = AuthResponseRow.from(行);
            応答表.put(応答.reqId, 応答);
        }

        for (Object 行 : nullToEmpty(pytxnf)) {
            TxnRow 取引 = TxnRow.from(行);
            if (取引.walletId.length() == 0 || 取引.reqId.length() == 0) {
                continue;
            }

            long 要求時刻 = 取引.authDt > 0L ? 取引.authDt : 取引.captureDt;
            long 窓開始 = floorWindow(要求時刻);
            String キー = 取引.walletId + "|" + 窓開始;

            VelocityRow 速度 = 速度表.get(キー);
            if (速度 == null) {
                速度 = new VelocityRow(取引.walletId, 窓開始, 0, java.math.BigDecimal.ZERO, 0, 0L);
                速度表.put(キー, 速度);
            }

            AuthResponseRow 応答 = 応答表.get(取引.reqId);
            if (応答 != null && 決定_否認.equals(応答.decisionKbn) && isVelocityCounterReason(応答.declineReason)) {
                速度.denyCount++;
            } else if (isApprovedTxn(取引.txnStatus, 応答)) {
                速度.authCount++;
                速度.authSumAmt = 速度.authSumAmt.add(取引.reqAmt);
            }

            if (要求時刻 > 速度.lastReqTs) {
                速度.lastReqTs = 要求時刻;
            }

            速度.velocityViolation = violatesVelocity(速度, 取引.merchantCode);
        }

        java.util.List<Object> 出力 = new java.util.ArrayList<Object>();
        for (VelocityRow 速度 : 速度表.values()) {
            出力.add(速度.toMap());
        }
        return 出力;
    }

    private static boolean isVelocityCounterReason(String declineReason) {
        return 理由_残高不足.equals(declineReason)
                || 理由_状態不正.equals(declineReason)
                || 理由_通貨対象外.equals(declineReason);
    }

    private static boolean isApprovedTxn(String txnStatus, AuthResponseRow 応答) {
        if (応答 != null) {
            return 決定_承認.equals(応答.decisionKbn);
        }
        return "00".equals(txnStatus) || "10".equals(txnStatus) || "30".equals(txnStatus) || "A".equals(txnStatus);
    }

    private static boolean violatesVelocity(VelocityRow 速度, String merchantCode) {
        RiskLimit 上限 = RiskLimit.of(merchantCode);
        return 速度.authCount > 上限.countLimit
                || 速度.authSumAmt.compareTo(上限.amountLimit) > 0
                || 速度.denyCount > 上限.denyLimit;
    }

    private static long floorWindow(long ts) {
        if (ts <= 0L) {
            return 0L;
        }
        return (ts / 窓幅ミリ秒) * 窓幅ミリ秒;
    }

    private static java.util.List<?> nullToEmpty(java.util.List<?> list) {
        return list == null ? java.util.Collections.emptyList() : list;
    }

    private static String text(Object row, String... names) {
        Object value = value(row, names);
        return value == null ? "" : String.valueOf(value).trim();
    }

    private static int integer(Object row, String... names) {
        Object value = value(row, names);
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        if (value == null || String.valueOf(value).trim().length() == 0) {
            return 0;
        }
        return Integer.parseInt(String.valueOf(value).trim());
    }

    private static long longValue(Object row, String... names) {
        Object value = value(row, names);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        if (value == null || String.valueOf(value).trim().length() == 0) {
            return 0L;
        }
        return Long.parseLong(String.valueOf(value).trim());
    }

    private static java.math.BigDecimal decimal(Object row, String... names) {
        Object value = value(row, names);
        if (value instanceof java.math.BigDecimal) {
            return (java.math.BigDecimal) value;
        }
        if (value instanceof Number) {
            return new java.math.BigDecimal(String.valueOf(value));
        }
        if (value == null || String.valueOf(value).trim().length() == 0) {
            return java.math.BigDecimal.ZERO;
        }
        return new java.math.BigDecimal(String.valueOf(value).trim());
    }

    private static Object value(Object row, String... names) {
        if (row == null) {
            return null;
        }
        if (row instanceof java.util.Map<?, ?>) {
            java.util.Map<?, ?> map = (java.util.Map<?, ?>) row;
            for (String name : names) {
                if (map.containsKey(name)) {
                    return map.get(name);
                }
            }
        }
        Class<?> type = row.getClass();
        for (String name : names) {
            try {
                java.lang.reflect.Method method = type.getMethod(name);
                return method.invoke(row);
            } catch (ReflectiveOperationException ignored) {
                Object none = null;
            }
        }
        return null;
    }

    private static final class VelocityRow {
        final String walletId;
        final long windowStartTs;
        int authCount;
        java.math.BigDecimal authSumAmt;
        int denyCount;
        long lastReqTs;
        boolean velocityViolation;

        VelocityRow(String walletId, long windowStartTs, int authCount,
                    java.math.BigDecimal authSumAmt, int denyCount, long lastReqTs) {
            this.walletId = walletId;
            this.windowStartTs = windowStartTs;
            this.authCount = authCount;
            this.authSumAmt = authSumAmt;
            this.denyCount = denyCount;
            this.lastReqTs = lastReqTs;
        }

        static VelocityRow from(Object row) {
            return new VelocityRow(
                    text(row, "walletId", "WALLET_ID", "wallet_id"),
                    longValue(row, "windowStartTs", "WINDOW_START_TS", "window_start_ts"),
                    integer(row, "authCount", "AUTH_COUNT", "auth_count"),
                    decimal(row, "authSumAmt", "AUTH_SUM_AMT", "auth_sum_amt"),
                    integer(row, "denyCount", "DENY_COUNT", "deny_count"),
                    longValue(row, "lastReqTs", "LAST_REQ_TS", "last_req_ts"));
        }

        String key() {
            return walletId + "|" + windowStartTs;
        }

        java.util.Map<String, Object> toMap() {
            java.util.Map<String, Object> map = new java.util.LinkedHashMap<String, Object>();
            map.put("WALLET_ID", walletId);
            map.put("WINDOW_START_TS", windowStartTs);
            map.put("AUTH_COUNT", authCount);
            map.put("AUTH_SUM_AMT", authSumAmt);
            map.put("DENY_COUNT", denyCount);
            map.put("LAST_REQ_TS", lastReqTs);
            map.put("VELOCITY_VIOLATION", velocityViolation);
            return map;
        }
    }

    private static final class TxnRow {
        final String reqId;
        final String walletId;
        final String merchantCode;
        final java.math.BigDecimal reqAmt;
        final String txnStatus;
        final long authDt;
        final long captureDt;

        TxnRow(String reqId, String walletId, String merchantCode, java.math.BigDecimal reqAmt,
               String txnStatus, long authDt, long captureDt) {
            this.reqId = reqId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.reqAmt = reqAmt;
            this.txnStatus = txnStatus;
            this.authDt = authDt;
            this.captureDt = captureDt;
        }

        static TxnRow from(Object row) {
            return new TxnRow(
                    text(row, "reqId", "REQ_ID", "req_id"),
                    text(row, "walletId", "WALLET_ID", "wallet_id"),
                    text(row, "merchantCode", "MERCHANT_CODE", "merchant_code"),
                    decimal(row, "reqAmt", "REQ_AMT", "req_amt"),
                    text(row, "txnStatus", "TXN_STATUS", "txn_status"),
                    longValue(row, "authDt", "AUTH_DT", "auth_dt"),
                    longValue(row, "captureDt", "CAPTURE_DT", "capture_dt"));
        }
    }

    private static final class AuthResponseRow {
        final String reqId;
        final String walletId;
        final String decisionKbn;
        final java.math.BigDecimal availAmt;
        final java.math.BigDecimal reqAmt;
        final String declineReason;

        AuthResponseRow(String reqId, String walletId, String decisionKbn,
                        java.math.BigDecimal availAmt, java.math.BigDecimal reqAmt, String declineReason) {
            this.reqId = reqId;
            this.walletId = walletId;
            this.decisionKbn = decisionKbn;
            this.availAmt = availAmt;
            this.reqAmt = reqAmt;
            this.declineReason = declineReason;
        }

        static AuthResponseRow from(Object row) {
            return new AuthResponseRow(
                    text(row, "reqId", "REQ_ID", "req_id"),
                    text(row, "walletId", "WALLET_ID", "wallet_id"),
                    text(row, "decisionKbn", "DECISION_KBN", "decision_kbn"),
                    decimal(row, "availAmt", "AVAIL_AMT", "avail_amt"),
                    decimal(row, "reqAmt", "REQ_AMT", "req_amt"),
                    text(row, "declineReason", "DECLINE_REASON", "decline_reason"));
        }
    }

    private static final class RiskLimit {
        final int countLimit;
        final java.math.BigDecimal amountLimit;
        final int denyLimit;

        RiskLimit(int countLimit, String amountLimit, int denyLimit) {
            this.countLimit = countLimit;
            this.amountLimit = new java.math.BigDecimal(amountLimit);
            this.denyLimit = denyLimit;
        }

        static RiskLimit of(String merchantCode) {
            if (merchantCode != null && merchantCode.startsWith("9")) {
                return new RiskLimit(3, "50000", 1);
            }
            if (merchantCode != null && merchantCode.startsWith("7")) {
                return new RiskLimit(8, "200000", 3);
            }
            return new RiskLimit(20, "1000000", 5);
        }
    }
}
