package jp.mirai.sec.matching;

/** SCDROPC -- SCDROP record layout (shared/pinned). org 順編成. */
public record Scdropc(String dcDropId, String dcExecId, String dcOrderId, String dcCifNo, String dcInstrCode, String dcSideKbn, int dcFillQty, long dcFillAmt, String dcSendTs) {}
