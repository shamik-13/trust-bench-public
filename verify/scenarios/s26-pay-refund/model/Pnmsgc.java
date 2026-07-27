package jp.mirai.pay.refund;

/** PNMSGC -- PNMSGF record layout (shared/pinned). org 順編成. */
public record Pnmsgc(String msMessageId, String msReqId, String msWalletId, String msChannelKbn, String msMessageBody, String msDeliveryKbn) {}
