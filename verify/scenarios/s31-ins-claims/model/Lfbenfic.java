package jp.mirai.life.claims;

/** LFBENFIC -- LFBENF record layout (shared/pinned). org VSAM-KSDS. */
public record Lfbenfic(String bnPolNo, String bnBeneficiaryId, String bnNameKana, String bnRelationshipKbn, String bnBankCd, String bnBranchCd, String bnAcctNo, String bnPaymentPriority) {}
