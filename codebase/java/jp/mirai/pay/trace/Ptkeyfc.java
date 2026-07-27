package jp.mirai.pay.trace;

/** PTKEYFC -- PTKEYF record layout (shared/pinned). org VSAM-KSDS. */
public record Ptkeyfc(String tkTraceKey, String tkHoldId, String tkCapId, String tkSettleTxnId, String tkMerchantCode, String tkCheckResult) {}
