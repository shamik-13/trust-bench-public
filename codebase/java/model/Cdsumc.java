/** CDSUMC -- CDSUMF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdsumc(String smSummaryKey, int smSummaryDt, String smMerchantCode, String smCurrencyCd, int smSaleCount, long smSaleAmt, long smReturnAmt) {}
