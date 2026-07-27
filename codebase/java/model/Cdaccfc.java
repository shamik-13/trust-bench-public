/** CDACCFC -- CDACCF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdaccfc(String acCardNo, String acMemberId, String acStatusKbn, long acCreditLimit, long acCashLimit, long acUsedAmt, String acDelayKbn, int acLastUpdDt) {}
