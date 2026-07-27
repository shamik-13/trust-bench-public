package jp.mirai.sec.position;

/** SCM2MC -- SCM2MF record layout (shared/pinned). org VSAM-KSDS. */
public record Scm2mc(String m2CifNo, String m2InstrCode, int m2SessDt, int m2NetQty, long m2MarkAmt, long m2MarkNotionalAmt, long m2UnrlzdAmt) {}
