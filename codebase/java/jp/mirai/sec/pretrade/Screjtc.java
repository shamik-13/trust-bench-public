/** SCREJTC -- SCREJTF record layout (shared/pinned). org VSAM-ESDS. */

package jp.mirai.sec.pretrade;

public record Screjtc(String rjRejectId, String rjOrderId, String rjCifNo, String rjInstrCode, String rjRejectCode, String rjRejectTs) {}
