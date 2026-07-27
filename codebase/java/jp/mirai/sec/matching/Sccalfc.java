package jp.mirai.sec.matching;

/** SCCALFC -- SCCALF record layout (shared/pinned). org CSV. */
public record Sccalfc(int caSessDt, String caSessKbn, String caOpenTs, String caCloseTs) {}
