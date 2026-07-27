package jp.mirai.pay.refund;

/** PRNTFC -- PRNTF record layout (shared/pinned). org 順編成. */
public record Prntfc(String ntNoticeId, String ntReqId, String ntDestKbn, String ntTemplateId, String ntSendStatus, int ntSendDt) {}
