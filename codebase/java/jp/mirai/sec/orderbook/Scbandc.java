package jp.mirai.sec.orderbook;

/** SCBANDC -- SCBAND record layout (shared/pinned). org VSAM-KSDS. */
public record Scbandc(String bdInstrCode, long bdLowerAmt, long bdUpperAmt, String bdBandTs, String bdSourceKbn) {}
