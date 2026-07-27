package jp.mirai.common.idmap;

/** CMRPTFC -- CMRPTF record layout (shared/pinned). org 順編成. */
public record Cmrptfc(String rpReportId, String rpReportTypeKbn, String rpTargetMonth, String rpOutputStatusKbn, String rpCreatedAt) {}
