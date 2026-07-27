package jp.mirai.life.claims;

/** LFCLMFC -- LFCLMF record layout (shared/pinned). org 順編成. */
public record Lfclmfc(String clClaimId, String clPolNo, long clSumAssuredAmt, long clLoanBalanceAmt, int clRespStartDt, int clEventDt, String clClaimStatusKbn) {}
