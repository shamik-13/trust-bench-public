package jp.mirai.pay.fee;

/** PMCATFC -- PMCATF record layout (shared/pinned). org VSAM-KSDS. */
public record Pmcatfc(String catCategoryCode, String catCategoryName, String catRiskRank, String catTaxableFlag, String catActiveFlag, int catLastUpdateDt) {}
