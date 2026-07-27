/** CDRTNC -- CDRTNF record layout (shared/pinned). org VSAM-ESDS. */
public record Cdrtnc(String rtReturnId, String rtSaleId, String rtCardNo, long rtReturnAmt, int rtReturnDt, String rtReturnReason, String rtApprovalStatus) {}
