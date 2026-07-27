/** CDNOTIC -- CDNOTIF record layout (shared/pinned). org VSAM-ESDS. */
public record Cdnotic(String ntNoticeId, String ntCardNo, int ntNoticeDt, String ntNoticeType, long ntNoticeAmt, String ntNoticeStatus) {}
