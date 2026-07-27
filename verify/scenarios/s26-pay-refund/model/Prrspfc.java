package jp.mirai.pay.refund;

/** PRRSPFC -- PRRSPF record layout (shared/pinned). org 順編成. */
public record Prrspfc(String rsReqId, String rsOrigTxnId, String rsDecisionKbn, String rsDeclineReason, long rsEligibleAmt) {}
