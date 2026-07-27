package jp.mirai.sec.grouprisk;

/** HFDECLOGC -- HFDECLOG record layout (shared/pinned). org VSAM-ESDS. */
public record Hfdeclogc(String hdlDecisionId, String hdlOrderId, String hdlInstrCode, String hdlActionCode, String hdlReasonCode, String hdlDecisionTs) {}
