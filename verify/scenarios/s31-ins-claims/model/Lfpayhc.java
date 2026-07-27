package jp.mirai.life.claims;

/** LFPAYHC -- LFPAYH record layout (shared/pinned). org 順編成. */
public record Lfpayhc(String phSeqNo, String phClaimId, String phStatusFrom, String phStatusTo, int phChangeDt, String phOperatorId) {}
