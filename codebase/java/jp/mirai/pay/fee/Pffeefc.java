package jp.mirai.pay.fee;

/** PFFEEFC -- PFFEEF record layout (shared/pinned). org 順編成. */
public record Pffeefc(String feFeeId, String feMerchantCode, long feTxnAmt, double feMdrRate, long feFeeAmt) {}
