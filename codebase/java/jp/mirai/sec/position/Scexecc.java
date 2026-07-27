package jp.mirai.sec.position;

/** SCEXECC -- SCEXEC record layout (shared/pinned). org CSV. */
public record Scexecc(String exExecId, String exOrderId, String exInstrCode, String exSideKbn, int exFillQty, long exFillAmt, String exExecTs) {}
