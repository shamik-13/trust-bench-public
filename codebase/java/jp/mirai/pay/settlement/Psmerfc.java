package jp.mirai.pay.settlement;

/** PSMERFC -- PSMERF record layout (shared/pinned). org CSV. */
public record Psmerfc(String mrMerchantCode, String mrMerchantName, String mrMerStatus, String mrBankAcctNo) {}
