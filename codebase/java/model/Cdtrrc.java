/** CDTRRC -- CDTRRF record layout (shared/pinned). org 順編成. */
public record Cdtrrc(String trrResultId, String trrRequestId, String trrCardNo, String trrResultCd, long trrSettledAmt, String trrReturnReason, int trrResultDt) {}
