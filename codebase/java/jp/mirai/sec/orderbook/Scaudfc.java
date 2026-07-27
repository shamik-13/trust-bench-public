package jp.mirai.sec.orderbook;

/** SCAUDFC -- SCAUDF record layout (shared/pinned). org VSAM-ESDS. */
public record Scaudfc(String auAuditId, String auOrderId, String auEventKbn, String auCifNo, String auInstrCode, String auEventTs, String auDetailCd) {}
