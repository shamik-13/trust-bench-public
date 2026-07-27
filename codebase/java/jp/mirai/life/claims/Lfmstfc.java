package jp.mirai.life.claims;

/** LFMSTFC -- LFMSTF record layout (shared/pinned). org 順編成. */
public record Lfmstfc(String msYearMonth, String msCategoryKbn, String msCount, long msTotalGrossAmt, long msTotalPayoutAmt, double msAvgReductionRate) {}
