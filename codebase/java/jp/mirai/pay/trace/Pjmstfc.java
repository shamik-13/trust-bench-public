package jp.mirai.pay.trace;

/** PJMSTFC -- PJMSTF record layout (shared/pinned). org VSAM-KSDS. */
public record Pjmstfc(String msMerchantCode, String msMerchantName, String msBankCode, String msAccountNo, String msActiveFlag, String msRiskRank) {}
