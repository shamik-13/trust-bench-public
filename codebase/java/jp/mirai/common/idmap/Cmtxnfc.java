package jp.mirai.common.idmap;

/** CMTXNFC -- CMTXNF record layout (shared/pinned). org 順編成. */
public record Cmtxnfc(String txTxnId, String txCompanyCode, String txLocalTxnNo, long txTxnAmt, String txTxnStatusKbn) {}
