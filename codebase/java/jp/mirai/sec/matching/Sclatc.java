package jp.mirai.sec.matching;

/** SCLATC -- SCLATF record layout (shared/pinned). org 順編成. */
public record Sclatc(String ltSampleId, String ltOrderId, String ltStageKbn, String ltStartTs, String ltEndTs, String ltLatencyNs) {}
