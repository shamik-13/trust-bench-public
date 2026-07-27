package jp.mirai.sec.position;

/** SCEXPC -- SCEXPF record layout (shared/pinned). org VSAM-KSDS. */
public record Scexpc(String xpCifNo, int xpSessDt, long xpGrossLongAmt, long xpGrossShortAmt, long xpNetExposureAmt, String xpLimitUtilPct) {}
