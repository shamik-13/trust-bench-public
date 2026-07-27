package jp.mirai.sec.orderbook;

/** SCROUTC -- SCROUT record layout (shared/pinned). org VSAM-ESDS. */
public record Scroutc(String rtRouteId, String rtOrderId, String rtVenueCode, int rtChildQty, long rtLimitAmt, String rtRouteTs) {}
