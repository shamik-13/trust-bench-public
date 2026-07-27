package jp.mirai.pay.settlement;

/** PSFXRFC -- PSFXRF record layout (shared/pinned). org VSAM-KSDS. */
public record Psfxrfc(String fxCcyPair, double fxRateDt, double fxTtmRate, String fxSourceCd, String fxLoadStatus) {}
