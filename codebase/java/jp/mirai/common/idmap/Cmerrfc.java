package jp.mirai.common.idmap;

/** CMERRFC -- CMERRF record layout (shared/pinned). org VSAM-KSDS. */
public record Cmerrfc(String erErrorId, String erImportBatchId, String erCompanyCode, String erLocalTxnNo, String erErrorCode, String erErrorStatusKbn) {}
