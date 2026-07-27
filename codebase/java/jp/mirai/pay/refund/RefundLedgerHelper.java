/* ================================================================
 * RefundLedgerHelper.java -- 返金実績参照ヘルパ
 *   1.0  20240305  ペイ返金基盤  新規 (原取引別 返金実績集計)
 * ================================================================ */
package jp.mirai.pay.refund;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 原取引別に既受付の返金額を集計するヘルパ。RefundEngine が一部返金の妥当性検証で用いる。
 * 受付可能期間や受付可否の判定は行わない（期間・判定は規程に基づき RefundEngine に実装される）。
 */
public final class RefundLedgerHelper {

    /** 原取引ID別に、既に受付(A)された返金額の合計を返す。 */
    public Map<String, Long> refundedByOrig(List<RefundModel.Request> accepted) {
        Map<String, Long> sum = new HashMap<>();
        for (RefundModel.Request r : accepted) {
            sum.merge(r.origTxnId(), r.refundAmt(), Long::sum);
        }
        return sum;
    }

    /** 原取引に対する追加返金可能額 = 原取引額 − 既受付返金合計（負値は0）。 */
    public long remainingRefundable(RefundModel.OrigTxn orig, Map<String, Long> refunded) {
        long used = refunded.getOrDefault(orig.origTxnId(), 0L);
        long left = orig.amount() - used;
        return Math.max(left, 0L);
    }
}
