package jp.mirai.pay.settlement;

/** PSADJFC -- PSADJF record layout (shared/pinned). org VSAM-KSDS. */
public record Psadjfc(String adjAdjustId, String adjMerchantCode, String adjAdjustKbn, long adjAdjustAmt, String adjReasonCd, int adjApplyDt, String adjApprovalStatus) {}
