package jp.mirai.sec.grouprisk;

/** HFRISKCC -- HFRISKC record layout (shared/pinned). org VSAM-KSDS. */
public record Hfriskcc(String hrcCifNo, String hrcInstrCode, long hrcOpenNotionalAmt, int hrcRejectCnt, String hrcLastUpdTs) {}
