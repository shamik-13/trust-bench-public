package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20240902  みらいペイ システム部  初版作成
 *
 * 外部ゲートウェイからのオーソリ要求を整形し、オーソリ判定 (AuthEngine) に橋渡しする。
 * 利用可能残高・承認可否は判定エンジンが確定させた応答 (PYARSPF) を転記するだけで、
 * 当ブリッジでは残高計算・承認判定を行わない。要求の書式検証と前段の状態/通貨確認のみ実施する。
 */
public class GatewayAuthBridgeService {
    private static final String STATUS_AUTH = "01";
    private static final String BASE_CURRENCY = "JPY";
    private static final java.util.regex.Pattern REQ_ID_PATTERN =
            java.util.regex.Pattern.compile("REQ-[0-9]{8}-[0-9]{6}");

    public static void main(String[] a) {
        java.util.List<WalletRow> wallets = java.util.Arrays.asList(
                new WalletRow("WLT000001", "USR100001", "01", "03", "ヤマダタロウ"),
                new WalletRow("WLT000002", "USR100002", "09", "02", "サトウハナコ"),
                new WalletRow("WLT000003", "USR100003", "01", "01", "タナカイチロウ")
        );

        java.util.List<RequestRow> requests = java.util.Arrays.asList(
                new RequestRow("REQ-20260628-000001", "WLT000001", 900000L, "JPY", 20260628),
                new RequestRow("REQ-20260628-000002", "WLT000002", 150000L, "JPY", 20260628),
                new RequestRow("REQ-20260628-000003", "WLT000003", 50000L, "USD", 20260628),
                new RequestRow("REQ-20260628-000004", "WLT000003", 1000000000000L, "JPY", 20260628)
        );

        // オーソリ判定 (AuthEngine) が確定させた応答 (PYARSPF) を REQ-ID で索引する。
        java.util.List<AuthResponseRow> authResponses = java.util.Arrays.asList(
                new AuthResponseRow("REQ-20260628-000001", "WLT000001", "A", 1480000L, 900000L, ""),
                new AuthResponseRow("REQ-20260628-000002", "WLT000002", "D", 0L, 150000L, "STS"),
                new AuthResponseRow("REQ-20260628-000004", "WLT000003", "D", 225000L, 1000000000000L, "LIM")
        );

        java.util.List<ResponseRow> responses = new java.util.ArrayList<>();
        for (RequestRow request : requests) {
            responses.add(process(request, wallets, authResponses));
        }

        for (ResponseRow response : responses) {
            System.out.println(response.reqId + "," + response.walletId + "," + response.decisionKbn
                    + "," + response.availAmt + "," + response.reqAmt + "," + response.declineReason);
        }
    }

    /**
     * 要求の前段検証を行い、問題がなければオーソリ判定の確定応答 (PYARSPF) を転記する。
     * 利用可能残高 (AVAIL-AMT) と承認可否は応答からそのまま受け取り、当ブリッジでは算出しない。
     */
    private static ResponseRow process(
            RequestRow request,
            java.util.List<WalletRow> wallets,
            java.util.List<AuthResponseRow> authResponses) {
        WalletRow wallet = findWallet(wallets, request.walletId);

        if (!REQ_ID_PATTERN.matcher(request.reqId).matches()) {
            return new ResponseRow(request.reqId, request.walletId, "D", 0L, request.reqAmt, "LIM");
        }
        if (!BASE_CURRENCY.equals(request.currencyCd)) {
            return new ResponseRow(request.reqId, request.walletId, "D", 0L, request.reqAmt, "CUR");
        }
        if (!validAmountDigits(request.reqAmt)) {
            return new ResponseRow(request.reqId, request.walletId, "D", 0L, request.reqAmt, "LIM");
        }
        if (wallet == null || !STATUS_AUTH.equals(wallet.walletStatus)) {
            return new ResponseRow(request.reqId, request.walletId, "D", 0L, request.reqAmt, "STS");
        }

        AuthResponseRow response = findResponse(authResponses, request.reqId);
        if (response == null) {
            // 判定エンジンの応答が未着の場合は受付保留として扱う。
            return new ResponseRow(request.reqId, request.walletId, "P", 0L, request.reqAmt, "WAIT");
        }
        return new ResponseRow(request.reqId, request.walletId, response.decisionKbn,
                response.availAmt, request.reqAmt, response.declineReason);
    }

    private static WalletRow findWallet(java.util.List<WalletRow> rows, String walletId) {
        for (WalletRow row : rows) {
            if (row.walletId.equals(walletId)) {
                return row;
            }
        }
        return null;
    }

    private static AuthResponseRow findResponse(java.util.List<AuthResponseRow> rows, String reqId) {
        for (AuthResponseRow row : rows) {
            if (row.reqId.equals(reqId)) {
                return row;
            }
        }
        return null;
    }

    private static boolean validAmountDigits(long amount) {
        return amount > 0L && amount <= 999999999999L;
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

    private static final class RequestRow {
        private final String reqId;
        private final String walletId;
        private final long reqAmt;
        private final String currencyCd;
        private final int requestDt;

        private RequestRow(String reqId, String walletId, long reqAmt, String currencyCd, int requestDt) {
            this.reqId = reqId;
            this.walletId = walletId;
            this.reqAmt = reqAmt;
            this.currencyCd = currencyCd;
            this.requestDt = requestDt;
        }
    }

    private static final class AuthResponseRow {
        private final String reqId;
        private final String walletId;
        private final String decisionKbn;
        private final long availAmt;
        private final long reqAmt;
        private final String declineReason;

        private AuthResponseRow(String reqId, String walletId, String decisionKbn,
                                long availAmt, long reqAmt, String declineReason) {
            this.reqId = reqId;
            this.walletId = walletId;
            this.decisionKbn = decisionKbn;
            this.availAmt = availAmt;
            this.reqAmt = reqAmt;
            this.declineReason = declineReason;
        }
    }

    private static final class ResponseRow {
        private final String reqId;
        private final String walletId;
        private final String decisionKbn;
        private final long availAmt;
        private final long reqAmt;
        private final String declineReason;

        private ResponseRow(String reqId, String walletId, String decisionKbn,
                            long availAmt, long reqAmt, String declineReason) {
            this.reqId = reqId;
            this.walletId = walletId;
            this.decisionKbn = decisionKbn;
            this.availAmt = availAmt;
            this.reqAmt = reqAmt;
            this.declineReason = declineReason;
        }
    }
}
