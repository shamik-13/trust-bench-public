package jp.mirai.pay.fee;

/** PMRATFC -- PMRATF record layout (shared/pinned). org VSAM-KSDS. */
public record Pmratfc(double rtRatePlanId, String rtCategoryCode, int rtEffectiveDt, String rtNoticeId, String rtApprovalStatus, String rtRuleHash) {}
