package jp.mirai.sec.orderbook;

/** SCCUSTC -- SCCUST record layout (shared/pinned). org CSV. */
public record Sccustc(String cuCifNo, long cuGroupLimit, long cuGroupUsedAmt, long cuAcctUsedAmt) {}
