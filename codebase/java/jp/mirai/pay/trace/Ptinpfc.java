package jp.mirai.pay.trace;

/** PTINPFC -- PTINPF record layout (shared/pinned). org 順編成. */
public record Ptinpfc(String piImportBatchId, String piCapId, String piHoldId, String piMerchantCode, long piCapAmt, String piImportStatus) {}
