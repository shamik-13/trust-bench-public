/* ================================================================
 * AuthHoldAggregator.java -- ホールド集計ヘルパ
 *   1.0  20240310  ペイ基盤   新規 (ウォレット別 ホールド集計)
 * ================================================================ */
package jp.mirai.pay.authorization;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * PYHOLDF のホールド行をウォレット別に集計するヘルパ。AuthEngine が利用可能残高を算定する際の
 * 素の承認済ホールド一覧/合計を提供する。判定 (失効解放の適用やオーソリ承認可否) は行わない。
 */
public final class AuthHoldAggregator {

    /** ウォレット別に、承認済 (HD-HOLD-RESULT='00') かつ JPY のホールド合計を返す (失効は問わない素の合計)。 */
    public Map<String, Long> approvedJpyHoldByWallet(List<PayModel.Hold> holds) {
        Map<String, Long> sum = new HashMap<>();
        for (PayModel.Hold h : holds) {
            if (!"00".equals(h.result())) continue;
            if (!"JPY".equals(h.ccy())) continue;
            sum.merge(h.walletId(), h.amt(), Long::sum);
        }
        return sum;
    }

    /** 指定ウォレットの承認済 JPY ホールド件数。監視・通知向けの参考値。 */
    public int approvedHoldCount(List<PayModel.Hold> holds, String walletId) {
        int n = 0;
        for (PayModel.Hold h : holds) {
            if (h.walletId().equals(walletId) && "00".equals(h.result()) && "JPY".equals(h.ccy())) n++;
        }
        return n;
    }
}
