/** SCKILLC -- SCKILLF record layout (shared/pinned). org VSAM-KSDS. */

package jp.mirai.sec.pretrade;

public record Sckillc(String klKillKey, String klScopeKbn, String klInstrCode, String klCifNo, String klActiveFlg, String klReasonCode, String klUpdatedTs) {}
