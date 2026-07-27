package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.0     2024-07-30  みらいペイ システム部    不正スコアリングサービス初版
 *
 * 取引・ウォレット・加盟店・速度情報から不正リスクスコア (PYSCOF) を算出する。
 * 利用可否の判定は行わず、スコアと理由のみを返す。
 */
public class FraudScoringService {
    private static final String WALLET_ACTIVE = "01";
    private static final String WALLET_STOPPED = "02";
    private static final String WALLET_CLOSED = "03";
    private static final String WALLET_RESTRICTED = "09";

    private static final String TXN_APPROVED = "A";
    private static final String TXN_DENIED = "D";

    private static final String DECLINE_LIMIT = "LIM";
    private static final String DECLINE_STATUS = "STS";
    private static final String DECLINE_CURRENCY = "CUR";

    private static final String BASE_CURRENCY = "JPY";

    /**
     * 取引一覧に対して PYSCOF 相当のスコアレコードを生成する。
     */
    public java.util.List<java.util.Map<String, String>> scoreTransactions(
            java.util.List<java.util.Map<String, String>> pytxnf,
            java.util.List<java.util.Map<String, String>> pywalf,
            java.util.List<java.util.Map<String, String>> pymerf,
            java.util.List<java.util.Map<String, String>> pyvelf) {
        java.util.Map<String, java.util.Map<String, String>> walletsById = indexBy(pywalf, "WALLET-ID");
        java.util.Map<String, java.util.Map<String, String>> merchantsByCode = indexBy(pymerf, "MERCHANT-CODE");
        java.util.Map<String, java.util.Map<String, String>> velocitiesByWallet = indexBy(pyvelf, "WALLET-ID");

        java.util.List<java.util.Map<String, String>> pyscof = new java.util.ArrayList<java.util.Map<String, String>>();
        for (java.util.Map<String, String> txn : pytxnf) {
            java.util.Map<String, String> wallet = walletsById.get(txn.get("WALLET-ID"));
            java.util.Map<String, String> merchant = merchantsByCode.get(txn.get("MERCHANT-CODE"));
            java.util.Map<String, String> velocity = velocitiesByWallet.get(txn.get("WALLET-ID"));
            pyscof.add(scoreTransaction(txn, wallet, merchant, velocity));
        }
        return pyscof;
    }

    private java.util.Map<String, String> scoreTransaction(
            java.util.Map<String, String> txn,
            java.util.Map<String, String> wallet,
            java.util.Map<String, String> merchant,
            java.util.Map<String, String> velocity) {
        int score = 80;
        java.util.List<String> reasons = new java.util.ArrayList<String>();

        if (wallet == null) {
            score += 380;
            reasons.add("ウォレット未登録");
        } else {
            String status = wallet.get("WALLET-STATUS");
            if (!WALLET_ACTIVE.equals(status)) {
                score += walletStatusScore(status);
                reasons.add("ウォレット状態=" + status);
            }

            String tier = wallet.get("WALLET-TIER");
            if ("1".equals(tier)) {
                score += 50;
                reasons.add("低ティア");
            } else if ("4".equals(tier)) {
                score -= 20;
                reasons.add("高ティア");
            }
        }

        if (merchant == null) {
            score += 300;
            reasons.add("加盟店未登録");
        } else {
            String merchantStatus = merchant.get("MERCHANT-STATUS");
            if (!"01".equals(merchantStatus)) {
                score += 260;
                reasons.add("加盟店状態=" + merchantStatus);
            }

            int riskRank = parseInt(merchant.get("RISK-RANK"));
            score += riskRank * 32;
            if (riskRank >= 7) {
                reasons.add("高リスク加盟店");
            }

            String mcc = merchant.get("MCC");
            if ("5999".equals(mcc) || "7995".equals(mcc) || "6051".equals(mcc)) {
                score += 85;
                reasons.add("注意MCC=" + mcc);
            }

            long amount = parseLong(txn.get("REQ-AMT"));
            long dailyLimit = parseLong(merchant.get("DAILY-LIMIT-AMT"));
            if (dailyLimit > 0 && amount > dailyLimit) {
                score += 180;
                reasons.add("加盟店日次上限超過");
            } else if (dailyLimit > 0 && amount * 100 >= dailyLimit * 80) {
                score += 70;
                reasons.add("加盟店日次上限接近");
            }

            if ("9".equals(merchant.get("SETTLE-CYCLE-KBN"))) {
                score += 30;
                reasons.add("特殊精算");
            }
        }

        if (velocity == null) {
            score += 40;
            reasons.add("速度情報なし");
        } else {
            int authCount = parseInt(velocity.get("AUTH-COUNT"));
            long authSum = parseLong(velocity.get("AUTH-SUM-AMT"));
            int denyCount = parseInt(velocity.get("DENY-COUNT"));
            long amount = parseLong(txn.get("REQ-AMT"));

            if (authCount >= 12) {
                score += 170;
                reasons.add("短時間承認回数過多");
            } else if (authCount >= 6) {
                score += 80;
                reasons.add("短時間承認回数増加");
            }

            if (denyCount >= 4) {
                score += 160;
                reasons.add("短時間否認回数過多");
            } else if (denyCount >= 2) {
                score += 70;
                reasons.add("短時間否認あり");
            }

            if (amount >= 100000L) {
                score += 90;
                reasons.add("高額要求");
            }

            if (authSum >= 300000L) {
                score += 120;
                reasons.add("短時間累計高額");
            }

            if (amount > 0 && authSum / amount >= 5) {
                score += 45;
                reasons.add("同額帯連続利用");
            }
        }

        String status = txn.get("TXN-STATUS");
        if (TXN_DENIED.equals(status)) {
            score += 130;
            reasons.add("過去否認取引");
        } else if (TXN_APPROVED.equals(status)) {
            score -= 25;
            reasons.add("過去承認取引");
        } else if (DECLINE_LIMIT.equals(status)) {
            score += 70;
            reasons.add("残高不足履歴");
        } else if (DECLINE_STATUS.equals(status)) {
            score += 160;
            reasons.add("状態不正履歴");
        } else if (DECLINE_CURRENCY.equals(status)) {
            score += 100;
            reasons.add("通貨対象外履歴");
        }

        if (!BASE_CURRENCY.equals(txn.get("CCY"))) {
            score += 95;
            reasons.add("基準通貨外");
        }

        score = Math.max(0, Math.min(999, score));

        java.util.Map<String, String> out = new java.util.LinkedHashMap<String, String>();
        out.put("SCORE-ID", "SC" + txn.get("REQ-ID"));
        out.put("WALLET-ID", txn.get("WALLET-ID"));
        out.put("MERCHANT-CODE", txn.get("MERCHANT-CODE"));
        out.put("RISK-SCORE", String.valueOf(score));
        out.put("SCORE-REASON", joinReasons(reasons));
        out.put("SCORE-AS-OF-TS", nowTimestamp());
        return out;
    }

    private static int walletStatusScore(String status) {
        if (WALLET_STOPPED.equals(status)) {
            return 300;
        }
        if (WALLET_CLOSED.equals(status)) {
            return 420;
        }
        if (WALLET_RESTRICTED.equals(status)) {
            return 240;
        }
        return 180;
    }

    private static java.util.Map<String, java.util.Map<String, String>> indexBy(
            java.util.List<java.util.Map<String, String>> records,
            String keyName) {
        java.util.Map<String, java.util.Map<String, String>> index =
                new java.util.LinkedHashMap<String, java.util.Map<String, String>>();
        if (records == null) {
            return index;
        }
        for (java.util.Map<String, String> record : records) {
            index.put(record.get(keyName), record);
        }
        return index;
    }

    private static String joinReasons(java.util.List<String> reasons) {
        if (reasons.isEmpty()) {
            return "通常範囲";
        }
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < reasons.size(); i++) {
            if (i > 0) {
                sb.append(";");
            }
            sb.append(reasons.get(i));
        }
        return sb.toString();
    }

    private static int parseInt(String value) {
        if (value == null || value.trim().isEmpty()) {
            return 0;
        }
        return Integer.parseInt(value);
    }

    private static long parseLong(String value) {
        if (value == null || value.trim().isEmpty()) {
            return 0L;
        }
        return Long.parseLong(value);
    }

    private static String nowTimestamp() {
        java.time.format.DateTimeFormatter formatter =
                java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        return java.time.LocalDateTime.now(java.time.ZoneId.of("Asia/Tokyo")).format(formatter);
    }
}
