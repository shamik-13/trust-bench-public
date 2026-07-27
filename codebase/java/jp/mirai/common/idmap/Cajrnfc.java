package jp.mirai.common.idmap;

/** CAJRNFC -- CAJRNF record layout (shared/pinned). org VSAM-ESDS. */
public record Cajrnfc(String jrJournalSeq, String jrAuditId, String jrGroupRefNo, String jrEventTypeKbn, String jrJournalStatusKbn) {}
