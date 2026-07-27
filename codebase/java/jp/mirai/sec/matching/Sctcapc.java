package jp.mirai.sec.matching;

/** SCTCAPC -- SCTCAP record layout (shared/pinned). org VSAM-KSDS. */
public record Sctcapc(String tcTradeId, String tcExecId, String tcOrderId, String tcInstrCode, String tcCifNo, int tcTradeQty, long tcTradeAmt, String tcCaptureTs) {}
