package jp.mirai.pay.refund;

/** PRQUEC -- PRQUEF record layout (shared/pinned). org VSAM-ESDS. */
public record Prquec(String quQueueId, String quReqId, String quQueueKbn, String quPriority, int quEnqueueDt, String quLockOwner) {}
