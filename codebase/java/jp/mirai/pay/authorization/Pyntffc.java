package jp.mirai.pay.authorization;

/** PYNTFFC -- PYNTFF record layout (shared/pinned). org 順編成. */
public record Pyntffc(String nfNoticeId, String nfWalletId, String nfNoticeKbn, String nfNoticeText, String nfSendStatus, String nfCreateTs) {}
