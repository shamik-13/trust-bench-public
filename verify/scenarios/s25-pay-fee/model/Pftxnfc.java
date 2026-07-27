package jp.mirai.pay.fee;

/** PFTXNFC -- PFTXNF record layout (shared/pinned). org 順編成. */
public record Pftxnfc(String txTxnId, String txMerchantCode, long txTxnAmt, int txTxnDt) {}
