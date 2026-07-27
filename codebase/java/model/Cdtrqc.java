/** CDTRQC -- CDTRQF record layout (shared/pinned). org 順編成. */
public record Cdtrqc(String trqRequestId, String trqCardNo, int trqBillingCycleDt, long trqRequestAmt, int trqDueDt, String trqBankCd, String trqAccountNo, String trqRequestStatus) {}
