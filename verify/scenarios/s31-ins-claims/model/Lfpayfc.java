package jp.mirai.life.claims;

/** LFPAYFC -- LFPAYF record layout (shared/pinned). org 順編成. */
public record Lfpayfc(String pyPayId, String pyClaimId, long pyGrossAmt, double pyReductionRate, long pyPayoutAmt) {}
