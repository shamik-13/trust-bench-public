/** CDPTC -- CDPTF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdptc(String ptMemberId, long ptPointBal, String ptEarnedPoint, String ptUsedPoint, int ptLastEarnDt, String ptPointStatus) {}
