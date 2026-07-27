package jp.mirai.pay.authorization;

/** PYWALFC -- PYWALF record layout (shared/pinned). org VSAM-KSDS. */
public record Pywalfc(String wlWalletId, String wlUserId, String wlWalletStatus, String wlWalletTier, String wlUserNameKana) {}
