package jp.mirai.pay.settlement;

/** PSRPTFC -- PSRPTF record layout (shared/pinned). org 順編成. */
public record Psrptfc(String rptReportId, String rptMerchantCode, String rptReportKbn, String rptPeriodFrom, String rptPeriodTo, String rptOutputPath, String rptCreateStatus) {}
