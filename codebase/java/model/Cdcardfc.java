/** CDCARDFC -- CDCARDF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdcardfc(String cfCardNo, String cfMemberId, String cfCardStatus, long cfCreditLimit, String cfMemberNameKana) {}
