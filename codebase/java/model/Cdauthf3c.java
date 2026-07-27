/** CDAUTHF3C -- CDAUTHF3 record layout (shared/pinned). org VSAM-KSDS. */
public record Cdauthf3c(String auAuthNo, String auCardNo, int auAuthDt, String auMerchantId, long auAuthAmt, String auAuthStatus, String auRevUseFlg, String auApprovalCd) {}
