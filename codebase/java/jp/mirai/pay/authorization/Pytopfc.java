package jp.mirai.pay.authorization;

/** PYTOPFC -- PYTOPF record layout (shared/pinned). org VSAM-ESDS. */
public record Pytopfc(String tpTopupId, String tpWalletId, long tpTopupAmt, String tpPaymentMethod, String tpTopupStatus, String tpRequestTs) {}
