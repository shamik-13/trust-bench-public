package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-07-16  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundBalanceAdjustmentService {
    private static final java.time.LocalDate 処理日 = java.time.LocalDate.of(2026, 6, 28);

    public static void main(String[] a) {
        java.util.List<承認応答> prrspf = java.util.Arrays.asList(
                new 承認応答("RQ202606280001", "TXN202606010011", "A", "", 金額("4200")),
                new 承認応答("RQ202606280002", "TXN202606010012", "A", "", 金額("1800")),
                new 承認応答("RQ202606280003", "TXN202605150019", "D", "WIN", 金額("0")),
                new 承認応答("RQ202606280004", "TXN202606020041", "A", "", 金額("12000")),
                new 承認応答("RQ202606280005", "TXN202606030017", "D", "AMT", 金額("0")),
                new 承認応答("RQ202606280006", "TXN202606040022", "A", "", 金額("980")),
                new 承認応答("RQ202606280007", "TXN202606040022", "A", "", 金額("980")),
                new 承認応答("RQ202606280008", "TXN202606050033", "A", "", 金額("-300")),
                new 承認応答("RQ202606280009", "TXN202606060044", "D", "TXN", 金額("0"))
        );

        java.util.Map<String, 残高行> prbalf = new java.util.LinkedHashMap<String, 残高行>();
        prbalf.put("WLT-10001", new 残高行("WLT-10001", 金額("53000"), 金額("800"), java.time.LocalDate.of(2026, 6, 27)));
        prbalf.put("WLT-10002", new 残高行("WLT-10002", 金額("1200"), 金額("500"), java.time.LocalDate.of(2026, 6, 24)));
        prbalf.put("WLT-10003", new 残高行("WLT-10003", 金額("20000"), 金額("0"), java.time.LocalDate.of(2026, 6, 20)));
        prbalf.put("WLT-10004", new 残高行("WLT-10004", 金額("3000"), 金額("980"), java.time.LocalDate.of(2026, 6, 26)));

        java.util.Map<String, 原取引> 原取引表 = new java.util.LinkedHashMap<String, 原取引>();
        原取引表.put("TXN202606010011", new 原取引("TXN202606010011", "WLT-10001", "10", 金額("4200"), 金額("0")));
        原取引表.put("TXN202606010012", new 原取引("TXN202606010012", "WLT-10002", "20", 金額("2500"), 金額("700")));
        原取引表.put("TXN202606020041", new 原取引("TXN202606020041", "WLT-10002", "30", 金額("12000"), 金額("0")));
        原取引表.put("TXN202606040022", new 原取引("TXN202606040022", "WLT-10004", "20", 金額("980"), 金額("0")));

        java.util.Map<String, java.math.BigDecimal> 取引別承認済額 = new java.util.LinkedHashMap<String, java.math.BigDecimal>();
        java.util.List<String> 監査例外 = new java.util.ArrayList<String>();

        for (承認応答 応答 : prrspf) {
            if (!妥当な判定区分(応答.decisionKbn)) {
                監査例外.add("REQ-ID=" + 応答.reqId + " 判定区分不正=" + 応答.decisionKbn);
                continue;
            }

            if ("D".equals(応答.decisionKbn)) {
                if (!妥当な否認理由(応答.declineReason)) {
                    監査例外.add("REQ-ID=" + 応答.reqId + " 否認理由不正=" + 応答.declineReason);
                }
                continue;
            }

            if (応答.eligibleAmt.compareTo(java.math.BigDecimal.ZERO) <= 0) {
                監査例外.add("REQ-ID=" + 応答.reqId + " 承認金額不正=" + 応答.eligibleAmt);
                continue;
            }

            原取引 txn = 原取引表.get(応答.origTxnId);
            if (txn == null) {
                監査例外.add("REQ-ID=" + 応答.reqId + " 原取引なし ORIG-TXN-ID=" + 応答.origTxnId);
                continue;
            }

            if (!妥当な依頼理由(txn.reqReason)) {
                監査例外.add("REQ-ID=" + 応答.reqId + " 依頼理由不正=" + txn.reqReason);
                continue;
            }

            残高行 現残高 = prbalf.get(txn.walletId);
            if (現残高 == null) {
                監査例外.add("REQ-ID=" + 応答.reqId + " 残高行なし WALLET-ID=" + txn.walletId);
                continue;
            }

            java.math.BigDecimal 累計承認済 = 取引別承認済額.containsKey(txn.origTxnId)
                    ? 取引別承認済額.get(txn.origTxnId)
                    : java.math.BigDecimal.ZERO;
            java.math.BigDecimal 取引返金後 = txn.refundedAmt.add(累計承認済).add(応答.eligibleAmt);

            if (取引返金後.compareTo(txn.originalAmt) > 0) {
                監査例外.add("REQ-ID=" + 応答.reqId + " 二重計上疑義 ORIG-TXN-ID=" + txn.origTxnId);
                continue;
            }

            java.math.BigDecimal 調整後保留 = 現残高.pendingRefundAmt.add(応答.eligibleAmt);
            java.math.BigDecimal 利用可能控除後 = 現残高.availableBal.subtract(応答.eligibleAmt);

            if (利用可能控除後.compareTo(java.math.BigDecimal.ZERO) < 0) {
                監査例外.add("REQ-ID=" + 応答.reqId + " 負残高化 WALLET-ID=" + txn.walletId);
                continue;
            }

            prbalf.put(txn.walletId, new 残高行(txn.walletId, 利用可能控除後, 調整後保留, 処理日));
            取引別承認済額.put(txn.origTxnId, 累計承認済.add(応答.eligibleAmt));
        }

        for (残高行 行 : prbalf.values()) {
            System.out.println("PRBALF更新 WALLET-ID=" + 行.walletId
                    + " AVAILABLE-BAL=" + 行.availableBal
                    + " PENDING-REFUND-AMT=" + 行.pendingRefundAmt
                    + " LAST-ADJ-DT=" + 行.lastAdjDt);
        }

        for (String 例外 : 監査例外) {
            System.out.println("監査例外 " + 例外);
        }
    }

    private static java.math.BigDecimal 金額(String 値) {
        return new java.math.BigDecimal(値).setScale(0);
    }

    private static boolean 妥当な判定区分(String 値) {
        return "A".equals(値) || "D".equals(値);
    }

    private static boolean 妥当な依頼理由(String 値) {
        return "10".equals(値) || "20".equals(値) || "30".equals(値);
    }

    private static boolean 妥当な否認理由(String 値) {
        return "WIN".equals(値) || "AMT".equals(値) || "TXN".equals(値);
    }

    private static final class 承認応答 {
        private final String reqId;
        private final String origTxnId;
        private final String decisionKbn;
        private final String declineReason;
        private final java.math.BigDecimal eligibleAmt;

        private 承認応答(String reqId, String origTxnId, String decisionKbn, String declineReason,
                    java.math.BigDecimal eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class 残高行 {
        private final String walletId;
        private final java.math.BigDecimal availableBal;
        private final java.math.BigDecimal pendingRefundAmt;
        private final java.time.LocalDate lastAdjDt;

        private 残高行(String walletId, java.math.BigDecimal availableBal,
                  java.math.BigDecimal pendingRefundAmt, java.time.LocalDate lastAdjDt) {
            this.walletId = walletId;
            this.availableBal = availableBal;
            this.pendingRefundAmt = pendingRefundAmt;
            this.lastAdjDt = lastAdjDt;
        }
    }

    private static final class 原取引 {
        private final String origTxnId;
        private final String walletId;
        private final String reqReason;
        private final java.math.BigDecimal originalAmt;
        private final java.math.BigDecimal refundedAmt;

        private 原取引(String origTxnId, String walletId, String reqReason,
                  java.math.BigDecimal originalAmt, java.math.BigDecimal refundedAmt) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.reqReason = reqReason;
            this.originalAmt = originalAmt;
            this.refundedAmt = refundedAmt;
        }
    }
}
