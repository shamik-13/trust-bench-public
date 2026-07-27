package jp.mirai.sec.position;

/** SCCACC -- SCCACT record layout (shared/pinned). org VSAM-KSDS. */
public record Sccacc(String acActionId, String acInstrCode, int acExDt, String acActionKbn, String acRatioNum, String acRatioDen, long acCashAmt) {}
