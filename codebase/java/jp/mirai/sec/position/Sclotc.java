package jp.mirai.sec.position;

/** SCLOTC -- SCLOT record layout (shared/pinned). org VSAM-KSDS. */
public record Sclotc(String ltLotId, String ltCifNo, String ltInstrCode, int ltOpenQty, long ltOpenAmt, String ltAcqTs, String ltSrcExecId) {}
