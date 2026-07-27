package jp.mirai.sec.orderbook;

/** SCRISK2C -- SCRISK2 record layout (shared/pinned). org VSAM-KSDS. */
public record Scrisk2c(String rkCifNo, String rkInstrTier, long rkMaxNotionalAmt, int rkMaxQty, String rkKillSwKbn) {}
