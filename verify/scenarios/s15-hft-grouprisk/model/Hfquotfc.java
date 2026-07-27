package jp.mirai.sec.grouprisk;

/** HFQUOTFC -- HFQUOTF record layout (shared/pinned). org VSAM-KSDS. */
public record Hfquotfc(String hqtInstrCode, long hqtBidAmt, long hqtAskAmt, long hqtMidAmt, long hqtSpreadAmt, String hqtQuoteTs) {}
