package jp.mirai.pay.refund;

/** PRBALC -- PRBALF record layout (shared/pinned). org VSAM-KSDS. */
public record Prbalc(String rbWalletId, long rbAvailableBal, long rbPendingRefundAmt, int rbLastAdjDt) {}
