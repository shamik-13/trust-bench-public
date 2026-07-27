package jp.mirai.sec.position;

/** SCPNLC -- SCPNLF record layout (shared/pinned). org VSAM-KSDS. */
public record Scpnlc(String pnCifNo, String pnInstrCode, int pnSessDt, long pnRlzdAmt, long pnUnrlzdAmt, long pnFeeAmt, String pnCalcTs) {}
