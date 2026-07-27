package jp.mirai.pay.refund;

/** PRCBFC -- PRCBF record layout (shared/pinned). org VSAM-KSDS. */
public record Prcbfc(String cbCaseId, String cbOrigTxnId, String cbCardScheme, long cbDisputeAmt, int cbDisputeDt, String cbStatusKbn) {}
