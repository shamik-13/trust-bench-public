/* ================================================================
 * FeeScheduleLoader.java -- 手数料スケジュール参照ヘルパ
 *   1.0  20240220  ペイ加盟店基盤  新規 (加盟店業種区分の参照・検証)
 * ================================================================ */
package jp.mirai.pay.fee;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 加盟店の業種区分 (MR-MER-CATEGORY) を参照・検証するヘルパ。MdrFeeEngine が手数料率を引く際の
 * 区分の取得に用いる。手数料率の値そのものは保持せず、料率算定も行わない（料率表は規程に基づき
 * MdrFeeEngine に実装される）。
 */
public final class FeeScheduleLoader {

    /** 正準の業種区分コード（規程の区分体系）。料率ではなく区分の妥当性検証用。 */
    private static final Set<String> CATEGORIES = Set.of("C1", "C2", "C3", "C4", "C5");

    /** 加盟店コードから業種区分を解決する。未登録なら null。 */
    public String categoryOf(Map<String, FeeModel.Merchant> merchants, String merchantCode) {
        FeeModel.Merchant m = merchants.get(merchantCode);
        return (m == null) ? null : m.category();
    }

    /** 区分コードが正準の体系に含まれるか。請求前の入力検証に使う。 */
    public boolean isValidCategory(String category) {
        return category != null && CATEGORIES.contains(category);
    }

    /** 請求対象（MR-MER-STATUS='01'）かつ区分が妥当な加盟店明細の件数を数える（監視用）。 */
    public int chargeableCount(List<FeeModel.Merchant> merchants) {
        int n = 0;
        for (FeeModel.Merchant m : merchants) {
            if ("01".equals(m.status()) && isValidCategory(m.category())) n++;
        }
        return n;
    }
}
