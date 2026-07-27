package jp.mirai.pay.settlement;

/** PSSETFC -- PSSETF record layout (shared/pinned). org CSV. */
public record Pssetfc(String stSettleId, String stMerchantCode, long stNetAmt, long stChargeAmt, long stPayoutAmt, int stSettleDt) {}
