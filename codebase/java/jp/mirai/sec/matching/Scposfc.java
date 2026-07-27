package jp.mirai.sec.matching;

/** SCPOSFC -- SCPOSF record layout (shared/pinned). org CSV. */
public record Scposfc(String psCifNo, String psInstrCode, int psNetQty, long psAvgAmt, long psRlzdAmt) {}
