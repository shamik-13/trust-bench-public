package jp.mirai.pay.authorization;

/** PYSCOFC -- PYSCOF record layout (shared/pinned). org VSAM-KSDS. */
public record Pyscofc(String scScoreId, String scWalletId, String scMerchantCode, String scRiskScore, String scScoreReason, String scScoreAsOfTs) {}
