package jp.mirai.pay.trace;

/** PCKBNFC -- PCKBNF record layout (shared/pinned). org VSAM-KSDS. */
public record Pckbnfc(String kbSettleKbn, String kbKbnName, String kbNettableFlag, double kbFeeRate, String kbValidFrom, String kbValidTo) {}
