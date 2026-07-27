package jp.mirai.pay.refund;

/** PRREQFC -- PRREQF record layout (shared/pinned). org 順編成. */
public record Prreqfc(String rqReqId, String rqOrigTxnId, long rqRefundAmt, int rqReqDt, String rqReqReason) {}
