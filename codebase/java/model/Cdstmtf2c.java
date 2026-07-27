/** CDSTMTF2C -- CDSTMTF2 record layout (shared/pinned). org VSAM-KSDS. */
public record Cdstmtf2c(String stCardNo, int stCycleDt, long stBillAmt, long stMinPayAmt, int stDueDt, String stStmtStatus, String stDelinqDays) {}
