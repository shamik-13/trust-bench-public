/** CDCANC -- CDCANF record layout (shared/pinned). org VSAM-ESDS. */
public record Cdcanc(String canCancelId, String canPayId, String canCardNo, long canCancelAmt, String canCancelReason, String canRequestUser, int canCancelDt) {}
