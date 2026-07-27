/** CDSIDXC -- CDSTMTIDXF record layout (shared/pinned). org VSAM-KSDS. */
public record Cdsidxc(String siCardNo, int siCycleDt, String siStatementId, long siBillAmt, int siDueDt, String siPublishStatus) {}
