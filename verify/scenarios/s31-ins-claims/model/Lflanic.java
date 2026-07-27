package jp.mirai.life.claims;

/** LFLANIC -- LFLANF record layout (shared/pinned). org VSAM-KSDS. */
public record Lflanic(String lnPolNo, long lnLoanAmt, long lnInterestAmt, String lnTotalBalance, int lnLastUpdateDt) {}
