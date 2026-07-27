package jp.mirai.pay.trace;

/** PTCAPFC -- PTCAPF record layout (shared/pinned). org 順編成. */
public record Ptcapfc(String cpCapId, String cpHoldId, String cpSettleTxnId, String cpMerchantCode, String cpSettleKbn, long cpCapAmt) {}
