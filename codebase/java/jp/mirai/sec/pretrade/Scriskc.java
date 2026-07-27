package jp.mirai.sec.pretrade;

/** SCRISKC -- SCRISK record layout (shared/pinned). org VSAM-ESDS. */
public record Scriskc(String rkEventId, String rkOrderId, String rkCifNo, String rkInstrCode, String rkRiskCd, String rkSeverityKbn, long rkObservedAmt, long rkThresholdAmt, String rkEventTs) {}
