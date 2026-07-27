package jp.mirai.pay.fee;

/** PFMERFC -- PFMERF record layout (shared/pinned). org VSAM-KSDS. */
public record Pfmerfc(String mrMerchantCode, String mrMerchantName, String mrMerCategory, String mrMerStatus) {}
