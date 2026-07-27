/** CDAUTHF2C -- CDAUTHF2 record layout (shared/pinned). org VSAM-KSDS. */
public record Cdauthf2c(String auAuthId, String auCardNo, long auAuthAmt, String auCurrencyCd, int auAuthDt, String auAuthStatus, String auMerchantCode) {}
