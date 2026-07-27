package jp.mirai.pay.settlement;

/** PJCANFC -- PJCANF record layout (shared/pinned). org VSAM-ESDS. */
public record Pjcanfc(String canCancelId, String canTxnId, String canMerchantCode, long canRefundAmt, int canRefundDt, String canLinkStatus) {}
