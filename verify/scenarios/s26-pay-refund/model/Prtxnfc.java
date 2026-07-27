package jp.mirai.pay.refund;

/** PRTXNFC -- PRTXNF record layout (shared/pinned). org VSAM-KSDS. */
public record Prtxnfc(String otOrigTxnId, String otWalletId, String otMerchantCode, long otOrigTxnAmt, int otOrigTxnDt) {}
