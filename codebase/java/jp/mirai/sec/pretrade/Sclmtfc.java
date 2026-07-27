package jp.mirai.sec.pretrade;

/** SCLMTFC -- SCLMTF record layout (shared/pinned). org VSAM-KSDS. */
public record Sclmtfc(String lmCifNo, String lmInstrTier, long lmMaxNotionalAmt, int lmMaxOrderQty, double lmMaxRateCnt, String lmUpdatedTs) {}
