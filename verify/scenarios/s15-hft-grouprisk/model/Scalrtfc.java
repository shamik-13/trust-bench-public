package jp.mirai.sec.grouprisk;

/** SCALRTFC -- SCALRTF record layout (shared/pinned). org VSAM-ESDS. */
public record Scalrtfc(String alAlertId, String alAlertKbn, String alSeverityCode, String alSubjectId, String alDetailCode, String alRaisedTs) {}
