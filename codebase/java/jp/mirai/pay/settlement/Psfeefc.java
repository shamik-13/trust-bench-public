package jp.mirai.pay.settlement;

/** PSFEEFC -- PSFEEF record layout (shared/pinned). org VSAM-KSDS. */
public record Psfeefc(String feFeePlanId, String feMerchantCode, double feRateKbn, double feRateValue, long feMinFeeAmt, long feMaxFeeAmt, int feApplyDt) {}
