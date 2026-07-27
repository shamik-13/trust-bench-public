/** SCROUTC -- SCROUTEF record layout (shared/pinned). org VSAM-KSDS. */

package jp.mirai.sec.grouprisk;

public record Scroutc(String rtRouteKey, String rtInstrCode, String rtBoardCode, String rtVenueKbn, String rtPriorityNo, int rtMaxSliceQty, String rtEnabledFlg) {}
