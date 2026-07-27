package jp.mirai.pay.authorization;

/** PYBALFC -- PYBALF record layout (shared/pinned). org 順編成. */
public record Pybalfc(String blWalletId, long blLedgerBalAmt, long blLastTopupAmt, int blBalAsOfDt) {}
