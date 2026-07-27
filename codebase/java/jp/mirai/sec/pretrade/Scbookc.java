package jp.mirai.sec.pretrade;

/** SCBOOKC -- SCBOOK record layout (shared/pinned). org CSV. */
public record Scbookc(String bkInstrCode, String bkSideKbn, int bkLevelCnt, long bkPriceAmt, int bkBookQty, int bkOrderCnt, String bkEntryTs) {}
