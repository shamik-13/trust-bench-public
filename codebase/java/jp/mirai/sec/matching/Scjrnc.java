package jp.mirai.sec.matching;

/** SCJRNC -- SCJRNF record layout (shared/pinned). org VSAM-ESDS. */
public record Scjrnc(String jrSeqNo, String jrEventTs, String jrEventKbn, String jrOrderId, String jrInstrCode, String jrPayloadHash) {}
