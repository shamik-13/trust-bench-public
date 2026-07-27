package jp.mirai.common.idmap;

/** CMINBFC -- CMINBF record layout (shared/pinned). org 順編成. */
public record Cminbfc(String ibImportBatchId, String ibSourceSystemId, String ibReceivedAt, int ibRecordCount, String ibImportStatusKbn) {}
