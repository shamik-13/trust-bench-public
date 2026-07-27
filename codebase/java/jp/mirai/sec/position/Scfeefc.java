package jp.mirai.sec.position;

/** SCFEEFC -- SCFEEF record layout (shared/pinned). org CSV. */
public record Scfeefc(String feBoardCode, double feFeeRate, long feMinFeeAmt) {}
