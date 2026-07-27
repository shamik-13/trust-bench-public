package jp.mirai.pay.authorization;

/** PYHOLDFC -- PYHOLDF record layout (shared/pinned). org 順編成. */
public record Pyholdfc(String hdHoldId, String hdWalletId, long hdHoldAmt, String hdHoldResult, String hdMerchantCode, String hdCurrencyCd, int hdHoldExpDt) {}
