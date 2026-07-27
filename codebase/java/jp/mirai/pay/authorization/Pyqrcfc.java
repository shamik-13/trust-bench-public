package jp.mirai.pay.authorization;

/** PYQRCFC -- PYQRCF record layout (shared/pinned). org VSAM-KSDS. */
public record Pyqrcfc(String qrQrId, String qrWalletId, String qrMerchantCode, long qrReqAmt, String qrQrStatus, String qrExpireTs) {}
