package jp.mirai.life.claims;

/** LFREPMFC -- LFREPMF record layout (shared/pinned). org 順編成. */
public record Lfrepmfc(String rpReportId, String rpReportTypeKbn, int rpOutputDt, String rpPageNo, String rpLineData) {}
