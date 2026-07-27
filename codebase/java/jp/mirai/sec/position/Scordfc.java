package jp.mirai.sec.position;

/** SCORDFC -- SCORDF record layout (shared/pinned). org CSV. */
public record Scordfc(String orOrderId, String orCifNo, String orInstrCode, String orSideKbn, String orOrdType, String orTifCode, int orOrdQty, long orPriceAmt, String orInstrTier) {}
