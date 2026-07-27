/** SCAUDTC -- SCAUDTF record layout (shared/pinned). org VSAM-ESDS. */

package jp.mirai.sec.grouprisk;

public record Scaudtc(String adAuditId, String adEventTs, String adServiceId, String adObjectId, String adEventKbn, String adDetailCode) {}
