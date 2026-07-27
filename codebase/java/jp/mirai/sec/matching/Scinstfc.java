package jp.mirai.sec.matching;

/** SCINSTFC -- SCINSTF record layout (shared/pinned). org CSV. */
public record Scinstfc(String inInstrCode, String inInstrName, String inInstrTier, long inTickAmt, int inLotQty, String inBoardCode) {}
