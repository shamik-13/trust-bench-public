/** CDRTRYC -- CDRTRYF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdrtryc(String rtyRetryId, String rtyCardNo, String rtyOriginalRequestId, int rtyRetryCount, int rtyNextRequestDt, long rtyRetryAmt, String rtyRetryStatus) {}
