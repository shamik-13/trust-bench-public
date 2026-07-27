package jp.mirai.pay.authorization;

/** PYPENDFC -- PYPENDF record layout (shared/pinned). org 順編成. */
public record Pypendfc(String pnPendId, String pnWalletId, long pnPendAmt, String pnPendStatus, int pnCaptureDt) {}
