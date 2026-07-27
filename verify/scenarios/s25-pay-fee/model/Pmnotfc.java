package jp.mirai.pay.fee;

/** PMNOTFC -- PMNOTF record layout (shared/pinned). org VSAM-ESDS. */
public record Pmnotfc(String ntfNoticeId, String ntfMerchantCode, String ntfCategoryCode, int ntfEffectiveDt, String ntfChannel, String ntfSendStatus) {}
