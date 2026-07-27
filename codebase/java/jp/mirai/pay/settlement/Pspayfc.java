package jp.mirai.pay.settlement;

/** PSPAYFC -- PSPAYF record layout (shared/pinned). org 順編成. */
public record Pspayfc(String pyPayoutId, String pyMerchantCode, String pyBankAcctNo, long pyPayoutAmt, int pyPayoutDt, String pyBankResultCd) {}
