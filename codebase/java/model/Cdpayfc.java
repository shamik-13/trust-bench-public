/** CDPAYFC -- CDPAYF record layout (shared/pinned). org 順編成. */
public record Cdpayfc(String pyPayId, String pyCardNo, long pyPayAmt, int pyPayDt, String pyPayMethod) {}
