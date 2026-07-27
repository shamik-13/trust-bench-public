package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-11-19  みらいペイ システム部  初版作成。QRコード生成サービスの事前判定版。
 */
public class QrCodeGenerationService {
    private static final String WALLET_STATUS_ACTIVE = "01";
    private static final String DECISION_APPROVE = "A";
    private static final String DECISION_DENY = "D";
    private static final String DECLINE_STATUS = "STS";
    private static final String DECLINE_LIMIT = "LIM";
    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] a) {
        java.time.Clock clock = java.time.Clock.system(java.time.ZoneId.of("Asia/Tokyo"));

        java.util.Map<String, WalletRow> pywalf = new java.util.LinkedHashMap<>();
        pywalf.put("WLT000000001", new WalletRow("WLT000000001", "USR000001", "01", "T2", "ヤマダタロウ"));
        pywalf.put("WLT000000002", new WalletRow("WLT000000002", "USR000002", "09", "T1", "サトウハナコ"));
        pywalf.put("WLT000000003", new WalletRow("WLT000000003", "USR000003", "01", "T3", "タナカイチロウ"));

        java.util.Map<String, MerchantRow> pymerf = new java.util.LinkedHashMap<>();
        pymerf.put("MRC000001", new MerchantRow("MRC000001", "01", "5812", 800000L, "B", "D"));
        pymerf.put("MRC000002", new MerchantRow("MRC000002", "02", "5999", 300000L, "C", "W"));
        pymerf.put("MRC000003", new MerchantRow("MRC000003", "01", "6211", 5000000L, "A", "D"));

        java.util.Map<String, LimitRow> pylmtf = new java.util.LinkedHashMap<>();
        pylmtf.put("T1", new LimitRow("T1", 30000L, 100000L, 500000L, 80000L));
        pylmtf.put("T2", new LimitRow("T2", 100000L, 300000L, 1500000L, 250000L));
        pylmtf.put("T3", new LimitRow("T3", 500000L, 2000000L, 10000000L, 1500000L));

        java.util.Map<String, QrRow> pyqrcf = new java.util.LinkedHashMap<>();

        java.util.List<QrRequest> requests = java.util.Arrays.asList(
                new QrRequest("WLT000000001", "MRC000001", 12500L, BASE_CURRENCY, false),
                new QrRequest("WLT000000002", "MRC000001", 1000L, BASE_CURRENCY, false),
                new QrRequest("WLT000000003", "MRC000003", 750000L, BASE_CURRENCY, false),
                new QrRequest("WLT000000001", "MRC000001", 0L, BASE_CURRENCY, true)
        );

        for (QrRequest request : requests) {
            Decision decision = decide(request, pywalf, pymerf, pylmtf);
            if (DECISION_APPROVE.equals(decision.decisionKbn)) {
                QrRow qr = createQr(request, clock);
                pyqrcf.put(qr.qrId, qr);
                System.out.println("処理結果=承認 QR-ID=" + qr.qrId + " 署名対象=" + canonical(qr));
            } else {
                System.out.println("処理結果=否認 理由=" + decision.reasonCode
                        + " WALLET-ID=" + request.walletId
                        + " MERCHANT-CODE=" + request.merchantCode);
            }
        }
    }

    private static Decision decide(
            QrRequest request,
            java.util.Map<String, WalletRow> pywalf,
            java.util.Map<String, MerchantRow> pymerf,
            java.util.Map<String, LimitRow> pylmtf) {

        WalletRow wallet = pywalf.get(request.walletId);
        if (wallet == null || !WALLET_STATUS_ACTIVE.equals(wallet.walletStatus)) {
            return new Decision(DECISION_DENY, DECLINE_STATUS);
        }

        MerchantRow merchant = pymerf.get(request.merchantCode);
        if (merchant == null || !WALLET_STATUS_ACTIVE.equals(merchant.merchantStatus)) {
            return new Decision(DECISION_DENY, DECLINE_STATUS);
        }

        if (!BASE_CURRENCY.equals(request.currencyCode)) {
            return new Decision(DECISION_DENY, "CUR");
        }

        LimitRow limit = pylmtf.get(wallet.walletTier);
        if (limit == null) {
            return new Decision(DECISION_DENY, DECLINE_LIMIT);
        }

        long amountForLimit = request.variableAmount ? 1L : request.reqAmt;
        if (amountForLimit <= 0L) {
            return new Decision(DECISION_DENY, DECLINE_LIMIT);
        }

        long effectivePerTxnLimit = Math.min(limit.perTxnLimitAmt, merchant.dailyLimitAmt);
        if (amountForLimit > effectivePerTxnLimit) {
            return new Decision(DECISION_DENY, DECLINE_LIMIT);
        }

        if ("6211".equals(merchant.mcc) && amountForLimit > 300000L) {
            return new Decision(DECISION_DENY, DECLINE_LIMIT);
        }

        return new Decision(DECISION_APPROVE, "");
    }

    private static QrRow createQr(QrRequest request, java.time.Clock clock) {
        java.time.ZonedDateTime now = java.time.ZonedDateTime.now(clock);
        java.time.ZonedDateTime expires = now.plusMinutes(request.variableAmount ? 5L : 15L);
        String qrId = "QR" + now.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
                + String.format("%06d", Math.abs(java.util.Objects.hash(
                request.walletId, request.merchantCode, request.reqAmt, now.toInstant().toEpochMilli())) % 1000000);

        long storedAmount = request.variableAmount ? 0L : request.reqAmt;
        return new QrRow(
                qrId,
                request.walletId,
                request.merchantCode,
                storedAmount,
                "10",
                expires.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        );
    }

    private static String canonical(QrRow qr) {
        return "QR-ID=" + qr.qrId
                + "|WALLET-ID=" + qr.walletId
                + "|MERCHANT-CODE=" + qr.merchantCode
                + "|REQ-AMT=" + qr.reqAmt
                + "|QR-STATUS=" + qr.qrStatus
                + "|EXPIRE-TS=" + qr.expireTs;
    }

    private static final class QrRequest {
        private final String walletId;
        private final String merchantCode;
        private final long reqAmt;
        private final String currencyCode;
        private final boolean variableAmount;

        private QrRequest(String walletId, String merchantCode, long reqAmt, String currencyCode, boolean variableAmount) {
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.reqAmt = reqAmt;
            this.currencyCode = currencyCode;
            this.variableAmount = variableAmount;
        }
    }

    private static final class WalletRow {
        private final String walletId;
        private final String userId;
        private final String walletStatus;
        private final String walletTier;
        private final String userNameKana;

        private WalletRow(String walletId, String userId, String walletStatus, String walletTier, String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    private static final class MerchantRow {
        private final String merchantCode;
        private final String merchantStatus;
        private final String mcc;
        private final long dailyLimitAmt;
        private final String riskRank;
        private final String settleCycleKbn;

        private MerchantRow(String merchantCode, String merchantStatus, String mcc,
                            long dailyLimitAmt, String riskRank, String settleCycleKbn) {
            this.merchantCode = merchantCode;
            this.merchantStatus = merchantStatus;
            this.mcc = mcc;
            this.dailyLimitAmt = dailyLimitAmt;
            this.riskRank = riskRank;
            this.settleCycleKbn = settleCycleKbn;
        }
    }

    private static final class LimitRow {
        private final String tierCode;
        private final long perTxnLimitAmt;
        private final long dailyLimitAmt;
        private final long monthlyLimitAmt;
        private final long alertThresholdAmt;

        private LimitRow(String tierCode, long perTxnLimitAmt, long dailyLimitAmt,
                         long monthlyLimitAmt, long alertThresholdAmt) {
            this.tierCode = tierCode;
            this.perTxnLimitAmt = perTxnLimitAmt;
            this.dailyLimitAmt = dailyLimitAmt;
            this.monthlyLimitAmt = monthlyLimitAmt;
            this.alertThresholdAmt = alertThresholdAmt;
        }
    }

    private static final class QrRow {
        private final String qrId;
        private final String walletId;
        private final String merchantCode;
        private final long reqAmt;
        private final String qrStatus;
        private final String expireTs;

        private QrRow(String qrId, String walletId, String merchantCode,
                      long reqAmt, String qrStatus, String expireTs) {
            this.qrId = qrId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.reqAmt = reqAmt;
            this.qrStatus = qrStatus;
            this.expireTs = expireTs;
        }
    }

    private static final class Decision {
        private final String decisionKbn;
        private final String reasonCode;

        private Decision(String decisionKbn, String reasonCode) {
            this.decisionKbn = decisionKbn;
            this.reasonCode = reasonCode;
        }
    }
}
