package jp.mirai.pay.fee;

/** PFBILFC -- PFBILF record layout (shared/pinned). org VSAM-KSDS. */
public record Pfbilfc(String bilBillId, String bilMerchantCode, String bilBillingMonth, long bilFeeTotalAmt, long bilTaxAmt, String bilStatus, int bilDueDt) {}
