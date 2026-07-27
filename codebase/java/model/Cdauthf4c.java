/** CDAUTHF4C -- CDAUTHF4 record layout (shared/pinned). org VSAM-ESDS. */
public record Cdauthf4c(String auAuthId, String auCardNo, String auTxnId, String auAuthKbn, long auReqAmt, long auApprovedAmt, String auReasonCd, int auAuthDt) {}
