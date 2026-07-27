package jp.mirai.pay.refund;

/** PRRPTF2C -- PRRPTF2 record layout (shared/pinned). org 順編成. */
public record Prrptf2c(String rpReportId, int rpReportDt, String rpMerchantCode, int rpRefundCnt, long rpRefundAmt, int rpDeclineCnt) {}
