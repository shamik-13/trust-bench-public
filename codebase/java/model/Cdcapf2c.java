/** CDCAPF2C -- CDCAPF2 record layout (shared/pinned). org 順編成. */
public record Cdcapf2c(String cpCaptureId, String cpTxnId, String cpCardNo, long cpCaptureAmt, String cpBrandKbn, int cpCaptureDt, String cpMatchKbn) {}
