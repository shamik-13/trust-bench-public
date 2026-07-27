package jp.mirai.sec.pretrade;

/** SCPOSFC -- SCPOSF record layout (shared/pinned). org CSV. */
public record Scposfc(String psCifNo, String psInstrCode, int psNetQty, long psAvgAmt, long psRlzdAmt) {}
