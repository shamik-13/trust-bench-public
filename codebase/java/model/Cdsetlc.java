/** CDSETLC -- CDSETLF record layout (shared/pinned). org 順編成. */
public record Cdsetlc(String stSettlementId, String stMerchantCode, int stSettleDt, long stGrossAmt, long stNetAmt, long stAdjAmt, String stSettleStatus) {}
