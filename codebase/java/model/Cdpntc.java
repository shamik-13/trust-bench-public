/** CDPNTC -- CDPOINTF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdpntc(String ptMemberId, long ptPointBal, int ptLastEarnDt, int ptLastRedeemDt, String ptPointStatus) {}
