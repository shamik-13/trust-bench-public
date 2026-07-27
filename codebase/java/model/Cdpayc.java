/** CDPAYC -- CDPAYF record layout (shared/pinned). org 順編成. */
public record Cdpayc(String pyPaymentId, String pyCardNo, int pyReceivedDt, long pyPayAmt, String pyPayMethod, String pyAllocStatus, String pyBankRefNo) {}
