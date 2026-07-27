package jp.mirai.common.idmap;

/** CAMONFC -- CAMONF record layout (shared/pinned). org VSAM-KSDS. */
public record Camonfc(String moSummaryMonth, String moCompanyCode, int moTxnCount, int moAuditCount, int moMismatchCount, String moSummaryStatusKbn) {}
