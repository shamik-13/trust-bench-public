/** CDPAYF2C -- CDPAYF2 record layout (shared/pinned). org 順編成. */
public record Cdpayf2c(String pyPaymentId, String pyCardNo, int pyPaymentDt, long pyPaymentAmt, String pyPaymentChannel, long pyAppliedAmt, long pyUnappliedAmt) {}
