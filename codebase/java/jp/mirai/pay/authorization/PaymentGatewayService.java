package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-09-04  みらいペイ システム部  決済受付ゲートウェイサービス初版
 */
public class PaymentGatewayService {
    private static final String WALLET_STATUS_ACTIVE = "01";
    private static final String MERCHANT_STATUS_ACTIVE = "01";
    private static final String QR_STATUS_ACTIVE = "01";
    private static final String DECISION_APPROVE = "A";
    private static final String DECISION_DECLINE = "D";
    private static final String DECLINE_LIMIT = "LIM";
    private static final String DECLINE_STATUS = "STS";
    private static final String CURRENCY_JPY = "JPY";

    public static void main(String[] a) {
        PaymentGatewayService service = new PaymentGatewayService();
        service.runBenchmark();
    }

    private final java.util.Map<String, QrConfig> pyqrcf = new java.util.LinkedHashMap<>();
    private final java.util.Map<String, Wallet> pywalf = new java.util.LinkedHashMap<>();
    private final java.util.Map<String, Merchant> pymerf = new java.util.LinkedHashMap<>();
    private final java.util.Map<String, Score> pyscof = new java.util.LinkedHashMap<>();
    private final java.util.Set<String> acceptedReqIds = new java.util.HashSet<>();
    private final java.util.Map<String, Long> merchantAcceptedAmount = new java.util.HashMap<>();
    private final java.util.List<Txn> pytxnf = new java.util.ArrayList<>();

    private void runBenchmark() {
        loadSyntheticBenchmarkData();

        java.util.List<GatewayRequest> requests = java.util.Arrays.asList(
                new GatewayRequest("REQ-20260628-000001", "QR-10000001", CURRENCY_JPY, 20260628101005L),
                new GatewayRequest("REQ-20260628-000002", "QR-10000002", CURRENCY_JPY, 20260628101012L),
                new GatewayRequest("REQ-20260628-000003", "QR-10000003", CURRENCY_JPY, 20260628101019L),
                new GatewayRequest("REQ-20260628-000004", "QR-10000004", CURRENCY_JPY, 20260628101025L),
                new GatewayRequest("REQ-20260628-000005", "QR-10000005", CURRENCY_JPY, 20260628101031L),
                new GatewayRequest("REQ-20260628-000001", "QR-10000006", CURRENCY_JPY, 20260628101044L),
                new GatewayRequest("REQ-20260628-000007", "QR-10000007", "USD", 20260628101102L),
                new GatewayRequest("REQ-20260628-000008", "QR-10000008", CURRENCY_JPY, 20260628101118L)
        );

        for (GatewayRequest request : requests) {
            accept(request);
        }

        for (Txn txn : pytxnf) {
            System.out.println(txn.toOperatorLine());
        }
        System.out.println("処理件数=" + requests.size() + " 保存件数=" + pytxnf.size());
    }

    private void loadSyntheticBenchmarkData() {
        pywalf.put("WL-1001", new Wallet("WL-1001", "US-810001", "01", "GOLD", "ヤマダタロウ"));
        pywalf.put("WL-1002", new Wallet("WL-1002", "US-810002", "09", "STD", "サトウハナコ"));
        pywalf.put("WL-1003", new Wallet("WL-1003", "US-810003", "01", "STD", "スズキイチロウ"));
        pywalf.put("WL-1004", new Wallet("WL-1004", "US-810004", "01", "VIP", "タナカミカ"));
        pywalf.put("WL-1005", new Wallet("WL-1005", "US-810005", "02", "STD", "イトウケン"));

        pymerf.put("MC-7001", new Merchant("MC-7001", "01", "5812", 900000L, "B", "D1"));
        pymerf.put("MC-7002", new Merchant("MC-7002", "09", "5999", 300000L, "C", "D1"));
        pymerf.put("MC-7003", new Merchant("MC-7003", "01", "6211", 120000L, "A", "D0"));
        pymerf.put("MC-7004", new Merchant("MC-7004", "01", "5732", 700000L, "D", "D1"));

        pyqrcf.put("QR-10000001", new QrConfig("QR-10000001", "WL-1001", "MC-7001", 12500L, "01", 20260628103000L));
        pyqrcf.put("QR-10000002", new QrConfig("QR-10000002", "WL-1002", "MC-7001", 9200L, "01", 20260628103000L));
        pyqrcf.put("QR-10000003", new QrConfig("QR-10000003", "WL-1001", "MC-7002", 18000L, "01", 20260628103000L));
        pyqrcf.put("QR-10000004", new QrConfig("QR-10000004", "WL-1003", "MC-7003", 130000L, "01", 20260628103000L));
        pyqrcf.put("QR-10000005", new QrConfig("QR-10000005", "WL-1004", "MC-7004", 62000L, "01", 20260628095959L));
        pyqrcf.put("QR-10000006", new QrConfig("QR-10000006", "WL-1001", "MC-7001", 3400L, "01", 20260628103000L));
        pyqrcf.put("QR-10000007", new QrConfig("QR-10000007", "WL-1003", "MC-7001", 8200L, "01", 20260628103000L));
        pyqrcf.put("QR-10000008", new QrConfig("QR-10000008", "WL-1005", "MC-7001", 4400L, "01", 20260628103000L));

        pyscof.put(scoreKey("WL-1001", "MC-7001"), new Score("SC-900001", "WL-1001", "MC-7001", 24, "通常購買", 20260628095500L));
        pyscof.put(scoreKey("WL-1002", "MC-7001"), new Score("SC-900002", "WL-1002", "MC-7001", 33, "端末変更", 20260628095600L));
        pyscof.put(scoreKey("WL-1001", "MC-7002"), new Score("SC-900003", "WL-1001", "MC-7002", 41, "加盟店監視", 20260628095700L));
        pyscof.put(scoreKey("WL-1003", "MC-7003"), new Score("SC-900004", "WL-1003", "MC-7003", 18, "少額継続", 20260628095800L));
        pyscof.put(scoreKey("WL-1004", "MC-7004"), new Score("SC-900005", "WL-1004", "MC-7004", 82, "高リスク端末", 20260628095900L));
        pyscof.put(scoreKey("WL-1005", "MC-7001"), new Score("SC-900006", "WL-1005", "MC-7001", 29, "停止会員", 20260628100000L));
    }

    private void accept(GatewayRequest request) {
        QrConfig qr = pyqrcf.get(request.qrId);
        if (qr == null) {
            saveDeclined(request, null, null, 0L, DECLINE_STATUS, "ＱＲ未登録");
            return;
        }

        Wallet wallet = pywalf.get(qr.walletId);
        Merchant merchant = pymerf.get(qr.merchantCode);
        Score score = pyscof.get(scoreKey(qr.walletId, qr.merchantCode));

        String declineReason = validateAtGateway(request, qr, wallet, merchant, score);
        if (declineReason != null) {
            saveDeclined(request, qr.walletId, qr.merchantCode, qr.reqAmt, declineReason, "受付否認");
            return;
        }

        acceptedReqIds.add(request.reqId);
        merchantAcceptedAmount.put(qr.merchantCode,
                merchantAcceptedAmount.getOrDefault(qr.merchantCode, 0L) + qr.reqAmt);

        AuthResult authResult = GatewayAuthClient.authorize(qr, wallet, merchant, score);
        String txnStatus = DECISION_APPROVE.equals(authResult.decision) ? "20" : "80";
        if (DECISION_APPROVE.equals(authResult.decision)) {
            notifyDownstream(request.reqId, qr, score);
        }
        pytxnf.add(new Txn(nextTxnId(), request.reqId, qr.walletId, qr.merchantCode,
                qr.reqAmt, txnStatus, request.receivedTs, 0L, authResult.declineReason));
    }

    private String validateAtGateway(GatewayRequest request, QrConfig qr, Wallet wallet, Merchant merchant, Score score) {
        if (acceptedReqIds.contains(request.reqId)) {
            return "DUP";
        }
        if (!CURRENCY_JPY.equals(request.currency)) {
            return "CUR";
        }
        if (!QR_STATUS_ACTIVE.equals(qr.qrStatus) || qr.expireTs < request.receivedTs) {
            return "EXP";
        }
        if (wallet == null || !WALLET_STATUS_ACTIVE.equals(wallet.walletStatus)) {
            return DECLINE_STATUS;
        }
        if (merchant == null || !MERCHANT_STATUS_ACTIVE.equals(merchant.merchantStatus)) {
            return "MST";
        }
        long accepted = merchantAcceptedAmount.getOrDefault(merchant.merchantCode, 0L);
        if (accepted + qr.reqAmt > merchant.dailyLimitAmt) {
            return DECLINE_LIMIT;
        }
        if (score == null || score.riskScore >= riskThreshold(merchant.riskRank)) {
            return "RSK";
        }
        return null;
    }

    private int riskThreshold(String riskRank) {
        if ("A".equals(riskRank)) {
            return 95;
        }
        if ("B".equals(riskRank)) {
            return 85;
        }
        if ("C".equals(riskRank)) {
            return 75;
        }
        return 65;
    }

    private void saveDeclined(GatewayRequest request, String walletId, String merchantCode,
                              long amount, String reason, String statusText) {
        pytxnf.add(new Txn(nextTxnId(), request.reqId,
                walletId == null ? "" : walletId,
                merchantCode == null ? "" : merchantCode,
                amount, "90", request.receivedTs, 0L, reason));
        System.out.println(statusText + " REQ-ID=" + request.reqId + " 理由=" + reason);
    }

    private void notifyDownstream(String reqId, QrConfig qr, Score score) {
        System.out.println("通知送信 REQ-ID=" + reqId + " 加盟店=" + qr.merchantCode);
        System.out.println("速度検知投入 REQ-ID=" + reqId + " ウォレット=" + qr.walletId);
        System.out.println("売上確定連携待機 REQ-ID=" + reqId + " スコア=" + score.riskScore);
    }

    private String nextTxnId() {
        return String.format("TXN-%08d", pytxnf.size() + 1);
    }

    private String scoreKey(String walletId, String merchantCode) {
        return walletId + "|" + merchantCode;
    }

    private static final class GatewayAuthClient {
        private static AuthResult authorize(QrConfig qr, Wallet wallet, Merchant merchant, Score score) {
            long tierLimit = tierLimit(wallet.walletTier);
            if (qr.reqAmt > tierLimit) {
                return new AuthResult(DECISION_DECLINE, DECLINE_LIMIT);
            }
            if (!"5812".equals(merchant.mcc) && score.riskScore >= 70) {
                return new AuthResult(DECISION_DECLINE, "RSK");
            }
            return new AuthResult(DECISION_APPROVE, "");
        }

        private static long tierLimit(String tier) {
            if ("VIP".equals(tier)) {
                return 500000L;
            }
            if ("GOLD".equals(tier)) {
                return 200000L;
            }
            return 100000L;
        }
    }

    private static final class GatewayRequest {
        private final String reqId;
        private final String qrId;
        private final String currency;
        private final long receivedTs;

        private GatewayRequest(String reqId, String qrId, String currency, long receivedTs) {
            this.reqId = reqId;
            this.qrId = qrId;
            this.currency = currency;
            this.receivedTs = receivedTs;
        }
    }

    private static final class QrConfig {
        private final String qrId;
        private final String walletId;
        private final String merchantCode;
        private final long reqAmt;
        private final String qrStatus;
        private final long expireTs;

        private QrConfig(String qrId, String walletId, String merchantCode, long reqAmt,
                         String qrStatus, long expireTs) {
            this.qrId = qrId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.reqAmt = reqAmt;
            this.qrStatus = qrStatus;
            this.expireTs = expireTs;
        }
    }

    private static final class Wallet {
        private final String walletId;
        private final String userId;
        private final String walletStatus;
        private final String walletTier;
        private final String userNameKana;

        private Wallet(String walletId, String userId, String walletStatus,
                       String walletTier, String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    private static final class Merchant {
        private final String merchantCode;
        private final String merchantStatus;
        private final String mcc;
        private final long dailyLimitAmt;
        private final String riskRank;
        private final String settleCycleKbn;

        private Merchant(String merchantCode, String merchantStatus, String mcc,
                         long dailyLimitAmt, String riskRank, String settleCycleKbn) {
            this.merchantCode = merchantCode;
            this.merchantStatus = merchantStatus;
            this.mcc = mcc;
            this.dailyLimitAmt = dailyLimitAmt;
            this.riskRank = riskRank;
            this.settleCycleKbn = settleCycleKbn;
        }
    }

    private static final class Score {
        private final String scoreId;
        private final String walletId;
        private final String merchantCode;
        private final int riskScore;
        private final String scoreReason;
        private final long scoreAsOfTs;

        private Score(String scoreId, String walletId, String merchantCode,
                      int riskScore, String scoreReason, long scoreAsOfTs) {
            this.scoreId = scoreId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.riskScore = riskScore;
            this.scoreReason = scoreReason;
            this.scoreAsOfTs = scoreAsOfTs;
        }
    }

    private static final class AuthResult {
        private final String decision;
        private final String declineReason;

        private AuthResult(String decision, String declineReason) {
            this.decision = decision;
            this.declineReason = declineReason;
        }
    }

    private static final class Txn {
        private final String txnId;
        private final String reqId;
        private final String walletId;
        private final String merchantCode;
        private final long reqAmt;
        private final String txnStatus;
        private final long authDt;
        private final long captureDt;
        private final String declineReason;

        private Txn(String txnId, String reqId, String walletId, String merchantCode,
                    long reqAmt, String txnStatus, long authDt, long captureDt,
                    String declineReason) {
            this.txnId = txnId;
            this.reqId = reqId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.reqAmt = reqAmt;
            this.txnStatus = txnStatus;
            this.authDt = authDt;
            this.captureDt = captureDt;
            this.declineReason = declineReason;
        }

        private String toOperatorLine() {
            return "ＰＹＴＸＮＦ保存 TXN-ID=" + txnId
                    + " REQ-ID=" + reqId
                    + " WALLET-ID=" + walletId
                    + " MERCHANT-CODE=" + merchantCode
                    + " REQ-AMT=" + reqAmt
                    + " TXN-STATUS=" + txnStatus
                    + " AUTH-DT=" + authDt
                    + " CAPTURE-DT=" + captureDt
                    + " DECLINE-REASON=" + declineReason;
        }
    }
}
