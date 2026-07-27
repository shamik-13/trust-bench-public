package jp.mirai.pay.trace;

/** PCSUMFC -- PCSUMF record layout (shared/pinned). org VSAM-KSDS. */
public record Pcsumfc(String psMerchantCode, String psSettleDate, String psSettleKbn, int psTxnCount, long psTotalAmt, long psCarryAmt) {}
