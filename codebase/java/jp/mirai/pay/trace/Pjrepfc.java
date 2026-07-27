package jp.mirai.pay.trace;

/** PJREPFC -- PJREPF record layout (shared/pinned). org 順編成. */
public record Pjrepfc(String rpReportId, String rpMerchantCode, String rpSettleDate, long rpGrossAmt, long rpFeeAmt, long rpNetAmt, String rpReportStatus) {}
