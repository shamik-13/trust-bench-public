package jp.mirai.sec.position;

/** SCSETC -- SCSETF record layout (shared/pinned). org VSAM-KSDS. */
public record Scsetc(String stSettleId, String stCifNo, String stInstrCode, int stSettleDt, int stNetQty, long stNetCashAmt, String stStatusKbn) {}
