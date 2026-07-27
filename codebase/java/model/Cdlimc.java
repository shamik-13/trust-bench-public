/** CDLIMC -- CDLIMF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdlimc(String lmCardNo, long lmTempLimitAmt, int lmStartDt, int lmEndDt, String lmApprovalId, String lmStatus) {}
