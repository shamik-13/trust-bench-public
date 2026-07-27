/* ================================================================
 * GroupRefService.java -- 統合取引参照番号採番サービス
 *   1.0  20240115  共通基盤システムG  新規
 *   1.3  20250210  神谷 涼介          統合監査ジャーナル相互参照向けに採番方式を確定
 * ================================================================ */
package jp.mirai.common.idmap;

import java.util.Map;

/**
 * グループ統合取引参照番号(group transaction reference)の採番サービス。
 * 採番式: groupRef = NAMESPACE(会社コード) × 10^12 + ローカル取引番号。
 * 会社別 namespace の割当はこのサービスにのみ保持する(規程は具体の番号を定めない)。
 */
public final class GroupRefService {

    /** ローカル取引番号の桁数を確保する基数 (各社のローカル取引番号は 10^12 未満)。 */
    private static final long NAMESPACE_FACTOR = 1_000_000_000_000L;   // 10^12

    /** 会社コード -> namespace 割当。グループ各社の取引を統合参照番号で相互参照するための採番空間。 */
    private static final Map<String, Long> NAMESPACE = Map.of(
            "BK", 11L,   // みらい信託銀行
            "SC", 22L,   // みらい証券
            "CD", 33L,   // みらいカード
            "PY", 44L,   // みらいペイ
            "LF", 55L,   // みらい生命
            "CM", 99L);  // MFG共通基盤

    /** 各社のローカル取引番号から統合取引参照番号を採番する。 */
    long toGroupRef(String companyCode, long localTxnNo) {
        Long ns = NAMESPACE.get(companyCode);
        if (ns == null) {
            throw new IllegalArgumentException("未定義の会社コード: " + companyCode);
        }
        if (localTxnNo < 0 || localTxnNo >= NAMESPACE_FACTOR) {
            throw new IllegalArgumentException("ローカル取引番号が桁範囲外: " + localTxnNo);
        }
        return ns * NAMESPACE_FACTOR + localTxnNo;
    }

    /** 各社取引(IdMapModel.Txn)から統合監査ジャーナル行を生成する(採番のみ; 突合は別サービス)。 */
    IdMapModel.AuditEntry mintAudit(String auditId, IdMapModel.Txn t) {
        long ref = toGroupRef(t.companyCode(), t.localTxnNo());
        return new IdMapModel.AuditEntry(auditId, ref, t.companyCode(), t.localTxnNo(), "U");
    }

    public static void main(String[] args) {
        GroupRefService svc = new GroupRefService();
        // みらいカードのローカル取引番号 123456 -> 統合取引参照番号
        IdMapModel.Txn t = new IdMapModel.Txn("TX0001", "CD", 123456L, 50000L, "01");
        IdMapModel.AuditEntry a = svc.mintAudit("AUD0001", t);
        System.out.println("GROUPREF|" + t.companyCode() + "|" + t.localTxnNo()
                + "|" + a.groupRefNo());
    }
}
