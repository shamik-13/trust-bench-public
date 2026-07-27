package jp.mirai.sec.position;

/** SCHLDC -- SCHLDF record layout (shared/pinned). org VSAM-KSDS. */
public record Schldc(String hdCifNo, String hdInstrCode, int hdAsofDt, int hdSettledQty, int hdTradeQty, int hdRestrictedQty) {}
