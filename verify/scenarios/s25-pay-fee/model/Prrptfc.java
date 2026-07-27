package jp.mirai.pay.fee;

/** PRRPTFC -- PRRPTF record layout (shared/pinned). org 順編成. */
public record Prrptfc(String rptReportId, String rptReportType, int rptBusinessDt, String rptMerchantCode, String rptOutputPath, String rptStatus) {}
