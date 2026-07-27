package jp.mirai.pay.refund;

/** PRRSNC -- PRRSNF record layout (shared/pinned). org VSAM-KSDS. */
public record Prrsnc(String rrReasonCode, String rrReasonGroup, String rrRiskWeight, String rrAutoReviewKbn) {}
