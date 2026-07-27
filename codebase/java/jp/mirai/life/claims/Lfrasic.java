package jp.mirai.life.claims;

/** LFRASIC -- LFRASF record layout (shared/pinned). org VSAM-KSDS. */
public record Lfrasic(String raAssessId, String raClaimId, int raAssessDt, String raCategoryKbn, String raAuthLevelKbn, String raResultKbn, String raAssessorId) {}
