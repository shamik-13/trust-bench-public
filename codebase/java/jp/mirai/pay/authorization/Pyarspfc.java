package jp.mirai.pay.authorization;

/** PYARSPFC -- PYARSPF record layout (shared/pinned). org 順編成. */
public record Pyarspfc(String arReqId, String arWalletId, String arDecisionKbn, long arAvailAmt, long arReqAmt, String arDeclineReason) {}
