package jp.mirai.pay.trace;

/** PTSETFC -- PTSETF record layout (shared/pinned). org CSV. */
public record Ptsetfc(String stSettleTxnId, String stMerchantCode, long stTxnAmt, String stSettleKbn) {}
