package jp.mirai.pay.settlement;

/** PSNTFFC -- PSNTFF record layout (shared/pinned). org VSAM-ESDS. */
public record Psntffc(String ntfNoticeId, String ntfMerchantCode, String ntfNoticeKbn, String ntfSettleId, String ntfSendStatus, String ntfSendAt) {}
