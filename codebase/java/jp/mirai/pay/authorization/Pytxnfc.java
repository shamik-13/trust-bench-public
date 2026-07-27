package jp.mirai.pay.authorization;

/** PYTXNFC -- PYTXNF record layout (shared/pinned). org VSAM-ESDS. */
public record Pytxnfc(String txTxnId, String txReqId, String txWalletId, String txMerchantCode, long txReqAmt, String txTxnStatus, int txAuthDt, int txCaptureDt) {}
