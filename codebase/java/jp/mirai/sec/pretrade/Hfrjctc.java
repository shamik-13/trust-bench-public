package jp.mirai.sec.pretrade;

/** HFRJCTC -- HFRJCT record layout (shared/pinned). org VSAM-ESDS. */
public record Hfrjctc(String rjRejectId, String rjOrderId, String rjCifNo, String rjInstrCode, String rjRejectCd, String rjDetailCd, String rjRejectTs) {}
