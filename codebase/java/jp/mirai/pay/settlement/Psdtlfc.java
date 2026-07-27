package jp.mirai.pay.settlement;

/** PSDTLFC -- PSDTLF record layout (shared/pinned). org VSAM-ESDS. */
public record Psdtlfc(String dtlDetailId, String dtlSettleId, String dtlMerchantCode, String dtlTxnId, long dtlTxnAmt, long dtlChargeAmt, String dtlLineKbn) {}
