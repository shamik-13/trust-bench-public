package jp.mirai.sec.grouprisk;

/** SCAUDF2C -- SCAUDF2 record layout (shared/pinned). org VSAM-ESDS. */
public record Scaudf2c(String audAuditId, String audActorId, String audActionKbn, String audObjectId, String audResultCode, String audAuditTs) {}
