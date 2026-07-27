package jp.mirai.pay.trace;

/** PTCANFC -- PTCANF record layout (shared/pinned). org 順編成. */
public record Ptcanfc(String cnCancelId, String cnCapId, String cnHoldId, String cnMerchantCode, long cnCancelAmt, String cnCancelStatus) {}
