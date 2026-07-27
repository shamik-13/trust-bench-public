package jp.mirai.pay.settlement;

/** PSTXNFC -- PSTXNF record layout (shared/pinned). org CSV. */
public record Pstxnfc(String txTxnId, String txMerchantCode, String txTxnKbn, long txTxnAmt, int txTxnDt) {}
