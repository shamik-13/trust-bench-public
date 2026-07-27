/** CDCARDFC -- CDCARDF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdcardfc2(String cfCardNo, String cfMemberId, String cfCardStatus, long cfCreditLimit, String cfBillCycleCd, String cfMemberNameKana, int cfOpenDt) {}
