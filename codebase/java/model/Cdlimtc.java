/** CDLIMTC -- CDLIMTF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdlimtc(String lmCardNo, long lmTotalLimitAmt, long lmRevLimitAmt, long lmUsedAmt, long lmTempLimitAmt, String lmLimitStatus) {}
