package jp.mirai.pay.settlement;

/** PSCONFC -- PSCONF record layout (shared/pinned). org VSAM-KSDS. */
public record Psconfc(String cfConfKey, String cfConfValue, int cfApplyDt, int cfExpireDt, String cfUpdatedAt) {}
