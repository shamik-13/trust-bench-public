package jp.mirai.life.claims;

/** LFXFRFC -- LFXFRF record layout (shared/pinned). org 順編成. */
public record Lfxfrfc(String xfTransferId, String xfPayId, String xfBankCd, String xfBranchCd, String xfAcctNo, String xfAcctHolderKna, String xfAmount, int xfTransferDt) {}
