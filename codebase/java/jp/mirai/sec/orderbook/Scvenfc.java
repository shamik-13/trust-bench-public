package jp.mirai.sec.orderbook;

/** SCVENFC -- SCVENF record layout (shared/pinned). org VSAM-KSDS. */
public record Scvenfc(String vnVenueCode, String vnBoardCode, String vnLatencyUs, String vnFeeBps, String vnEnabledKbn, int vnCapacityQty) {}
