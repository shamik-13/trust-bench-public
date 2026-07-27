package jp.mirai.pay.refund;

/** PRCANFC -- PRCANF record layout (shared/pinned). org 順編成. */
public record Prcanfc(String cnCancelId, String cnReqId, String cnCancelReason, int cnCancelDt, String cnOperatorId) {}
