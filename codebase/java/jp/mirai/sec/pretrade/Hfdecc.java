package jp.mirai.sec.pretrade;

/** HFDECC -- HFDEC record layout (shared/pinned). org VSAM-ESDS. */
public record Hfdecc(String hdDecisionId, String hdOrderId, String hdCifNo, String hdInstrCode, String hdDecisionCd, String hdReasonCd, long hdNotionalAmt, long hdLimitUsedAmt, String hdDecisionTs) {}
