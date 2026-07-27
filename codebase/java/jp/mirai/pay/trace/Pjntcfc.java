package jp.mirai.pay.trace;

/** PJNTCFC -- PJNTCF record layout (shared/pinned). org 順編成. */
public record Pjntcfc(String ntNoticeId, String ntMerchantCode, String ntSettleDate, long ntPaymentAmt, String ntBankRefNo, String ntNoticeStatus) {}
