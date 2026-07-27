/** CDPYMFC -- CDPYMF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdpymfc(String pyPaymentId, String pyCardNo, long pyPayAmt, int pyPayDt, String pyAllocKbn, long pyUnappliedAmt) {}
