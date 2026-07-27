/** CDMSTC -- CDMEMSTATF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdmstc(String msMemberId, String msStatusCd, String msStatusReason, int msEffectiveDt, String msLastUpdatedTs) {}
