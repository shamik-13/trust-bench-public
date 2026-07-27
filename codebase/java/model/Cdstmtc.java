/** CDSTMTC -- CDSTMTF record layout (shared/pinned). org 順編成. */
public record Cdstmtc(String mtStatementId, String mtMemberId, String mtCardNo, String mtSaleId, long mtBilledAmt, long mtFeeAmt, int mtPostingDt) {}
