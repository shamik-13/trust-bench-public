/* ================================================================
 * RefundEngine.java -- 返金・チャージバック受付判定エンジン (D-2601 実装箇所)  [GOLDEN]
 *   1.0  20240305  ペイ返金基盤  新規
 *   2.0  20250418  富永 健       D-2601 適用: 返金受付可能期間を改定後の180日に変更
 * ================================================================ */
package jp.mirai.pay.refund;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class RefundEngine {

    /* D-2601: 返金受付可能期間 = 改定後 180日 (規程 CD-RULE-REFUND の決裁値)。第180日まで受付可。 */
    int refundWindowDays() {
        return 180;
    }

    private static LocalDate ymd(int d) {
        return LocalDate.of(d / 10000, (d / 100) % 100, d % 100);
    }

    /** 判定結果を "<decision> <reason>" で返す。reason は受付時 '-'。判定順: TXN -> WIN -> AMT。 */
    String decide(RefundModel.Request r, RefundModel.OrigTxn orig) {
        if (orig == null) return "D TXN";
        long days = ChronoUnit.DAYS.between(ymd(orig.txnDt()), ymd(r.reqDt()));
        if (days > refundWindowDays()) return "D WIN";
        if (r.refundAmt() > orig.amount()) return "D AMT";
        return "A -";
    }

    // --- fixture harness: read originals.csv + requests.csv, print "<reqId> <decision> <reason>" ---
    public static void main(String[] args) throws Exception {
        Map<String, RefundModel.OrigTxn> origs = new HashMap<>();
        for (String[] f : rows("originals.csv")) {
            origs.put(f[0], new RefundModel.OrigTxn(f[0], f[1], Long.parseLong(f[2]), Integer.parseInt(f[3])));
        }
        List<RefundModel.Request> reqs = new ArrayList<>();
        for (String[] f : rows("requests.csv")) {
            reqs.add(new RefundModel.Request(f[0], f[1], Long.parseLong(f[2]), Integer.parseInt(f[3]), f[4]));
        }
        RefundEngine eng = new RefundEngine();
        for (RefundModel.Request r : reqs) {
            String[] d = eng.decide(r, origs.get(r.origTxnId())).split(" ");
            System.out.println(r.reqId() + " " + d[0] + " " + d[1]);
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
