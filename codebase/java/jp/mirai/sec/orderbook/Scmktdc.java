package jp.mirai.sec.orderbook;

/** SCMKTDC -- SCMKTD record layout (shared/pinned). org CSV. */
public record Scmktdc(String mdInstrCode, long mdBidAmt, long mdAskAmt, long mdLastAmt, int mdVolQty, String mdTickTs) {}
