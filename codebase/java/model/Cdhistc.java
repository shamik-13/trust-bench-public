/** CDHISTC -- CDHISTF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdhistc(String hisCardNo, String hisPayId, String hisEventSeq, String hisEventType, long hisEventAmt, int hisEventDt, String hisSourceProgram) {}
