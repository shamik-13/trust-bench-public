package jp.mirai.sec.pretrade;

/** HFKILLC -- HFKILL record layout (shared/pinned). org VSAM-KSDS. */
public record Hfkillc(String hkScopeKey, String hkKillFlg, String hkReasonCd, String hkUpdatedTs, String hkUpdatedBy) {}
