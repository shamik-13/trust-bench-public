/** CDBILLFC -- CDBILLF record layout (shared/pinned). org 順編成. */
public record Cdbillfc(String biCardNo, int biCycleDt, long biBillAmt, long biMinPayAmt, int biDueDt, String biBillStatus, String biProgramId) {}
