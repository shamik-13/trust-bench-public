package jp.mirai.pay.authorization;

/** PYMERFC -- PYMERF record layout (shared/pinned). org VSAM-KSDS. */
public record Pymerfc(String mrMerchantCode, String mrMerchantStatus, String mrMcc, long mrDailyLimitAmt, String mrRiskRank, String mrSettleCycleKbn) {}
