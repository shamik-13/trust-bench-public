package jp.mirai.pay.refund;

/** PDFRDC -- PDFRDF record layout (shared/pinned). org VSAM-KSDS. */
public record Pdfrdc(String fdFraudId, String fdReqId, String fdWalletId, String fdScore, String fdRuleHitCd, int fdJudgeDt) {}
