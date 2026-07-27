package jp.mirai.pay.fee;

/** PFSUMFC -- PFSUMF record layout (shared/pinned). org VSAM-KSDS. */
public record Pfsumfc(String sumSummaryId, String sumMerchantCode, String sumSettleMonth, int sumTxnCount, long sumTxnTotalAmt, long sumFeeTotalAmt, long sumNetSettleAmt) {}
