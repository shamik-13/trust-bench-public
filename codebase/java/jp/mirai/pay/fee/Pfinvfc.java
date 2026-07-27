package jp.mirai.pay.fee;

/** PFINVFC -- PFINVF record layout (shared/pinned). org VSAM-KSDS. */
public record Pfinvfc(String invInvoiceId, String invBillId, String invMerchantCode, String invQualifiedInvoiceNo, int invIssueDt, String invTaxBreakdown) {}
