/** CDCBKPC -- CDCBKPF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdcbkpc(String cbkChargebackId, String cbkSaleId, String cbkCardNo, String cbkMerchantCode, long cbkClaimAmt, String cbkClaimReason, String cbkCaseStatus) {}
