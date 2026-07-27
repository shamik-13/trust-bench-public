package jp.mirai.sec.matching;

/** SCORDSC -- SCORDS record layout (shared/pinned). org VSAM-KSDS. */
public record Scordsc(String osOrderId, String osCifNo, String osInstrCode, String osStateKbn, int osLeavesQty, int osCumQty, long osAvgFillAmt, String osLastUpdTs) {}
