package jp.mirai.sec.grouprisk;

/** HFDROPQC -- HFDROPQ record layout (shared/pinned). org VSAM-ESDS. */
public record Hfdropqc(String hdqDropId, String hdqExecId, String hdqOrderId, String hdqInstrCode, int hdqFillQty, long hdqFillAmt, String hdqCaptureTs) {}
