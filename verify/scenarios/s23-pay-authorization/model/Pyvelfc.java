package jp.mirai.pay.authorization;

/** PYVELFC -- PYVELF record layout (shared/pinned). org VSAM-KSDS. */
public record Pyvelfc(String vlWalletId, String vlWindowStartTs, int vlAuthCount, long vlAuthSumAmt, int vlDenyCount, String vlLastReqTs) {}
