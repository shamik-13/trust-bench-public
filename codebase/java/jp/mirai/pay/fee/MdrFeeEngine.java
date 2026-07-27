/* ================================================================
 * MdrFeeEngine.java -- 加盟店手数料(MDR)算定エンジン
 *   1.0  20240220  ペイ加盟店基盤  新規
 * ================================================================ */
package jp.mirai.pay.fee;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class MdrFeeEngine {

    private static final String CHARGEABLE = "01";   // 有効

    /* 加盟店手数料率表 (業種区分 -> bp; 10000bp = 100%)。 */
    int mdrRateBp(String category) {
        switch (category) {
            case "C1": return 325;   // 一般物販      3.25%
            case "C2": return 325;   // 飲食          3.25%
            case "C3": return 150;   // 公共・公金     1.50%
            case "C4": return 360;   // EC・通信販売   3.60%
            case "C5": return 395;   // 高リスク業種   3.95%
            default:   return 0;     // 未定義区分は 0 (請求対象外)
        }
    }

    /** 1明細の MDR 手数料 = 取引額 × 料率(bp) / 10000、円未満切捨て。 */
    long feeFor(FeeModel.Txn t, String category) {
        return (t.amount() * mdrRateBp(category)) / 10000;
    }

    // --- fixture harness: read merchants.csv + transactions.csv, print "<txnId> <fee>" per
    //     transaction of a chargeable (MR-MER-STATUS='01') merchant. ---
    public static void main(String[] args) throws Exception {
        Map<String, FeeModel.Merchant> mers = new HashMap<>();
        for (String[] f : rows("merchants.csv")) {
            mers.put(f[0], new FeeModel.Merchant(f[0], f[1], f[2]));
        }
        List<FeeModel.Txn> txns = new ArrayList<>();
        for (String[] f : rows("transactions.csv")) {
            txns.add(new FeeModel.Txn(f[0], f[1], Long.parseLong(f[2]), Integer.parseInt(f[3])));
        }
        MdrFeeEngine eng = new MdrFeeEngine();
        for (FeeModel.Txn t : txns) {
            FeeModel.Merchant m = mers.get(t.merchantCode());
            if (m == null || !CHARGEABLE.equals(m.status())) continue;   // 請求対象のみ
            System.out.println(t.txnId() + " " + eng.feeFor(t, m.category()));
        }
    }

    private static List<String[]> rows(String name) throws Exception {
        List<String[]> out = new ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(java.nio.file.Path.of(name))) {
            String s = line.trim();
            if (s.isEmpty() || s.startsWith("#")) continue;
            String[] f = s.split(",");
            for (int i = 0; i < f.length; i++) f[i] = f[i].trim();
            out.add(f);
        }
        return out;
    }
}
