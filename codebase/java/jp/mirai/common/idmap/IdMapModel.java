package jp.mirai.common.idmap;

/* ================================================================
 * IdMapModel.java -- MFG共通基盤 取引ID相互参照 ドメインモデル (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20240115  共通基盤システムG  新規 (各社取引と統合監査ジャーナルの共通型)
 *   1.1  20250210  共通基盤システムG  監査ジャーナル相互参照サービス向けに型を共有
 * ================================================================ */

/** グループ取引ID相互参照ドメインの共通型。統合取引参照番号の採番規則・会社別 namespace は
 *  各サービス側(GroupRefService / AuditLinkService)に実装される。 */
public final class IdMapModel {
    private IdMapModel() {}

    /**
     * 各社取引。companyCode は TX-COMPANY-CODE (BK/SC/CD/PY/LF/CM),
     * localTxnNo は TX-LOCAL-TXN-NO (各社内のローカル取引番号),
     * amount は TX-TXN-AMT (円), status は TX-TXN-STATUS-KBN。
     */
    public record Txn(String txnId, String companyCode, long localTxnNo,
                      long amount, String status) {}

    /**
     * 統合監査ジャーナル行。groupRefNo は AU-GROUP-REF-NO (グループ統合取引参照番号),
     * companyCode/localTxnNo は逆引き結果, status は AU-AUDIT-STATUS-KBN。
     */
    public record AuditEntry(String auditId, long groupRefNo, String companyCode,
                             long localTxnNo, String status) {}
}
