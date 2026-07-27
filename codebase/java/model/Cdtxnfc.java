/** CDTXNFC -- CDTXNF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdtxnfc(String txTxnId, String txCardNo, String txTxnKbn, String txChannelKbn, String txMerchantKbn, long txTxnAmt, int txTxnDt, String txAuthCd) {}
