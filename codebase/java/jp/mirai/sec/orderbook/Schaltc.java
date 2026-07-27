package jp.mirai.sec.orderbook;

/** SCHALTC -- SCHALT record layout (shared/pinned). org VSAM-ESDS. */
public record Schaltc(String haAlertId, String haInstrCode, String haAlertKbn, String haSeverityCd, long haObservedAmt, long haLimitAmt, String haEventTs) {}
