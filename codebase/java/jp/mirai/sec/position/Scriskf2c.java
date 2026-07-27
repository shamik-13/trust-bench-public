package jp.mirai.sec.position;

/** SCRISKF2C -- SCRISKF2 record layout (shared/pinned). org VSAM-ESDS. */
public record Scriskf2c(String rkRiskEventId, String rkCifNo, String rkInstrCode, String rkEventTs, long rkLimitAmt, long rkUsedAmt, String rkDecisionKbn) {}
