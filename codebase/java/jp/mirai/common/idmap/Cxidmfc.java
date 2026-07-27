package jp.mirai.common.idmap;

/** CXIDMFC -- CXIDMF record layout (shared/pinned). org VSAM-KSDS. */
public record Cxidmfc(String imIdmapKey, String imCompanyCode, String imLocalTxnNo, String imCustomerAliasId, String imLinkStatusKbn) {}
