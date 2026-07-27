/* ================================================================
 * AuditLinkService.java -- 統合監査ジャーナル逆引きサービス
 *   1.0  20240118  共通基盤システムG  新規
 *   1.2  20250210  神谷 涼介          統合取引参照番号からの会社/ローカル番号復元を確定
 * ================================================================ */
package jp.mirai.common.idmap;

import java.util.Map;

/**
 * 統合監査ジャーナル(CMAUDF)の逆引きサービス。
 * GroupRefService の採番(NAMESPACE × 10^12 + ローカル取引番号)を反転し、
 * 統合取引参照番号から 会社コード と ローカル取引番号 を復元する。
 * namespace -> 会社コード の逆引き表はこのサービスにのみ保持する(規程は具体の番号を定めない)。
 */
public final class AuditLinkService {

    private static final long NAMESPACE_FACTOR = 1_000_000_000_000L;   // 10^12 (GroupRefService と対)

    /** namespace -> 会社コード の逆引き表。GroupRefService の割当を反映する。 */
    private static final Map<Long, String> INV_NAMESPACE = Map.of(
            11L, "BK",   // みらい信託銀行
            22L, "SC",   // みらい証券
            33L, "CD",   // みらいカード
            44L, "PY",   // みらいペイ
            55L, "LF",   // みらい生命
            99L, "CM");  // MFG共通基盤

    /** 統合取引参照番号から会社コードを復元する(上位桁 = groupRef / 10^12 の namespace を逆引き)。 */
    String companyOf(long groupRefNo) {
        long ns = groupRefNo / NAMESPACE_FACTOR;
        String co = INV_NAMESPACE.get(ns);
        if (co == null) {
            throw new IllegalArgumentException("未定義の namespace: " + ns);
        }
        return co;
    }

    /** 統合取引参照番号からローカル取引番号を復元する(下位12桁 = groupRef mod 10^12)。 */
    long localNoOf(long groupRefNo) {
        return groupRefNo % NAMESPACE_FACTOR;
    }

    /** 統合監査ジャーナル行を逆引きして 会社/ローカル番号 を埋め、突合済(A)で返す。 */
    IdMapModel.AuditEntry resolve(IdMapModel.AuditEntry e) {
        return new IdMapModel.AuditEntry(e.auditId(), e.groupRefNo(),
                companyOf(e.groupRefNo()), localNoOf(e.groupRefNo()), "A");
    }

    public static void main(String[] args) {
        AuditLinkService svc = new AuditLinkService();
        long ref = 11_000_000_555000L;   // 例: 統合取引参照番号
        System.out.println("RESOLVE|" + ref + "|" + svc.companyOf(ref) + "|" + svc.localNoOf(ref));
    }
}
