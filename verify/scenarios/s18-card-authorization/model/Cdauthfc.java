/** CDAUTHFC -- CDAUTHF record layout (shared/pinned). org 順編成. */
public record Cdauthfc(String auAuthId, String auCardNo, long auAuthAmt, String auAuthResult, String auMerchantCode, String auCurrencyCd, String auAuthTs, int auHoldExpDt) {}
