package jp.mirai.pay.trace;

/** PTHOLDFC -- PTHOLDF record layout (shared/pinned). org 順編成. */
public record Ptholdfc(String hdHoldId, String hdWalletId, String hdMerchantCode, long hdHoldAmt, String hdHoldStatus) {}
