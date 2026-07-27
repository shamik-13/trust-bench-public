package jp.mirai.pay.settlement;

/** PSRCVFC -- PSRCVF record layout (shared/pinned). org VSAM-KSDS. */
public record Psrcvfc(String rcvReceiptId, String rcvMerchantCode, long rcvReceiptAmt, int rcvReceiptDt, String rcvMatchStatus, String rcvSettleId) {}
