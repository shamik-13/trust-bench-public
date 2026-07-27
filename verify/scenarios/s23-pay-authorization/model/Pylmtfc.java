package jp.mirai.pay.authorization;

/** PYLMTFC -- PYLMTF record layout (shared/pinned). org VSAM-KSDS. */
public record Pylmtfc(String lmTierCode, long lmPerTxnLimitAmt, long lmDailyLimitAmt, long lmMonthlyLimitAmt, long lmAlertThresholdAmt) {}
