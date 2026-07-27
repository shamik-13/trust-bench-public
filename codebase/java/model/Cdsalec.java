/** CDSALEC -- CDSALESF record layout (shared/pinned). org VSAM-ESDS. */
public record Cdsalec(String slSalesId, String slAuthId, String slCardNo, String slMerchantId, int slSalesDt, int slPostingDt, long slSalesAmt, long slTaxAmt, String slCaptureStatus) {}
