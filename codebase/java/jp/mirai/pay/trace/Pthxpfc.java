package jp.mirai.pay.trace;

/** PTHXPFC -- PTHXPF record layout (shared/pinned). org VSAM-KSDS. */
public record Pthxpfc(String hxHoldId, String hxWalletId, String hxMerchantCode, String hxExpireAt, String hxReasonCode, String hxExpireStatus) {}
