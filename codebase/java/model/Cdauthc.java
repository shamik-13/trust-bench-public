/** CDAUTHC -- CDAUTHF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdauthc(String auAuthId, String auCardNo, String auMerchantId, int auAuthDt, String auAuthTm, long auAuthAmt, String auCurrencyCd, String auAuthResult, String auApprovalCd, int auHoldExpDt) {}
