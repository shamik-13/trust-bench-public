package jp.mirai.sec.pretrade;

/** SCEXPRC -- SCEXPR record layout (shared/pinned). org VSAM-KSDS. */
public record Scexprc(String xpCifNo, String xpInstrCode, long xpNetNotionalAmt, long xpBuyOpenAmt, long xpSellOpenAmt, String xpUpdatedTs) {}
