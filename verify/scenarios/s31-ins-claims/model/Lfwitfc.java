package jp.mirai.life.claims;

/** LFWITFC -- LFWITF record layout (shared/pinned). org 順編成. */
public record Lfwitfc(String wtReportId, String wtPayId, String wtBeneficiaryId, long wtTaxableAmt, long wtTaxAmt, String wtTaxYear, String wtTaxExemptFlg) {}
