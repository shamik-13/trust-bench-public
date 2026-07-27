package jp.mirai.pay.trace;

/** PCDTLFC -- PCDTLF record layout (shared/pinned). org VSAM-ESDS. */
public record Pcdtlfc(String pdDetailId, String pdSettleTxnId, String pdMerchantCode, long pdTxnAmt, String pdSettleKbn, String pdOutputStatus) {}
