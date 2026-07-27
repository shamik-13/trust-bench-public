package jp.mirai.sec.matching;

/** SCAUCTC -- SCAUCT record layout (shared/pinned). org VSAM-KSDS. */
public record Scauctc(String axInstrCode, String axAuctionKbn, long axCrossAmt, int axImbalQty, int axMatchQty, String axCalcTs) {}
