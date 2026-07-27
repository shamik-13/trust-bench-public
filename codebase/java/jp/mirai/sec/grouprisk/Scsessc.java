/** SCSESSC -- SCSESSF record layout (shared/pinned). org VSAM-KSDS. */

package jp.mirai.sec.grouprisk;

public record Scsessc(String ssSessKey, int ssSessDt, String ssBoardCode, String ssStateKbn, String ssLastSeqNo, String ssUpdatedTs) {}
