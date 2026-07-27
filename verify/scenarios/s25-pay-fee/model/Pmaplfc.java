package jp.mirai.pay.fee;

/** PMAPLFC -- PMAPLF record layout (shared/pinned). org VSAM-KSDS. */
public record Pmaplfc(String aplApplicationId, String aplMerchantCode, int aplApplyDt, String aplScreeningStatus, String aplRiskScore, String aplReviewerId) {}
