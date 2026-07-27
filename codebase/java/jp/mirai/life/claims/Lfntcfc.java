package jp.mirai.life.claims;

/** LFNTCFC -- LFNTCF record layout (shared/pinned). org 順編成. */
public record Lfntcfc(String ntNoticeId, String ntClaimId, String ntBeneficiaryId, int ntNoticeDt, String ntNoticeTypeKbn, String ntStatusKbn) {}
