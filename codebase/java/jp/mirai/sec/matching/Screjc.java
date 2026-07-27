package jp.mirai.sec.matching;

/** SCREJC -- SCREJ record layout (shared/pinned). org 順編成. */
public record Screjc(String rjRejectId, String rjOrderId, String rjCifNo, String rjInstrCode, String rjRejectCd, String rjRejectTs) {}
