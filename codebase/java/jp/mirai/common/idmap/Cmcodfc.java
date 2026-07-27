package jp.mirai.common.idmap;

/** CMCODFC -- CMCODF record layout (shared/pinned). org VSAM-KSDS. */
public record Cmcodfc(String cdCodeKey, String cdCodeType, String cdCodeValue, String cdValidFrom, String cdValidTo, String cdCodeStatusKbn) {}
