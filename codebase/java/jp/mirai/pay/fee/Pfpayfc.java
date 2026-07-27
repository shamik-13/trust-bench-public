package jp.mirai.pay.fee;

/** PFPAYFC -- PFPAYF record layout (shared/pinned). org 順編成. */
public record Pfpayfc(String payPaymentId, String payMerchantCode, int payPaymentDt, long payPaymentAmt, String payBankRefNo, String payMatchStatus) {}
