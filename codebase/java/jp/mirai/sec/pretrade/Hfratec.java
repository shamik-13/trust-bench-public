package jp.mirai.sec.pretrade;

/** HFRATEC -- HFRATE record layout (shared/pinned). org VSAM-KSDS. */
public record Hfratec(String hrBucketKey, String hrWindowTs, int hrOrderCnt, long hrNotionalAmt, int hrDropCnt) {}
