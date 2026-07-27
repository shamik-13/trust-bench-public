package jp.mirai.pay.trace;

/** PCCARFC -- PCCARF record layout (shared/pinned). org VSAM-KSDS. */
public record Pccarfc(String crCarryId, String crMerchantCode, String crSettleKbn, long crCarryAmt, String crCarryReason, String crNextSettleDate) {}
