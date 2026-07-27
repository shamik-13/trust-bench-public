/* ================================================================
 * AuthEngine.java -- ウォレット与信オーソリ判定エンジン
 *   1.0  20240310  ペイ基盤   新規 (ウォレット与信オーソリ判定)
 * ================================================================ */
package jp.mirai.pay.authorization;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public final class AuthEngine {

    /** オーソリ判定の基準日 (バッチ実行日, YYYYMMDD)。 */
    private static final int AS_OF = 20260601;

    private static final String AUTHORIZABLE = "01";   // 有効
    private static final String BASE_CCY = "JPY";

    /* 利用可能残高 = 確定残高 − Σ(承認済ホールド) − Σ(未確定決済)。
     * 承認済ホールド = HD-HOLD-RESULT='00' かつ JPY のホールド。 */
    long availableBalance(PayModel.Wallet w, List<PayModel.Hold> holds,
                          List<PayModel.Pending> pend, int asOfDt) {
        long heldActive = 0;
        for (PayModel.Hold h : holds) {
            if (!h.walletId().equals(w.walletId())) continue;
            if (!"00".equals(h.result())) continue;          // 取消/売上確定済は対象外
            if (!BASE_CCY.equals(h.ccy())) continue;         // JPY ホールドのみ
            heldActive += h.amt();
        }
        long pendingTotal = 0;
        for (PayModel.Pending p : pend) {
            if (!p.walletId().equals(w.walletId())) continue;
            if (!"10".equals(p.status())) continue;          // 未確定 (清算待ち) のみ
            pendingTotal += p.amt();
        }
        return w.ledgerBal() - heldActive - pendingTotal;
    }

    /** 判定結果を "<decision> <reason>" で返す。reason は承認時 '-'。 */
    String decide(PayModel.Request r, long available, String walletStatus) {
        if (!AUTHORIZABLE.equals(walletStatus)) return "D STS";
        if (!BASE_CCY.equals(r.ccy())) return "D CUR";
        if (r.amt() <= available) return "A -";
        return "D LIM";
    }

    // --- fixture harness: read wallets/holds/pending/requests, print "<reqId> <decision> <avail> <reason>" ---
    public static void main(String[] args) throws Exception {
        Map<String, PayModel.Wallet> wallets = new LinkedHashMap<>();
        for (String[] f : rows("wallets.csv")) {
            wallets.put(f[0], new PayModel.Wallet(f[0], f[1], Long.parseLong(f[2])));
        }
        List<PayModel.Hold> holds = new ArrayList<>();
        for (String[] f : rows("holds.csv")) {
            holds.add(new PayModel.Hold(f[0], Long.parseLong(f[1]), f[2], Integer.parseInt(f[3]), f[4]));
        }
        List<PayModel.Pending> pend = new ArrayList<>();
        for (String[] f : rows("pending.csv")) {
            pend.add(new PayModel.Pending(f[0], Long.parseLong(f[1]), f[2]));
        }
        AuthEngine eng = new AuthEngine();
        for (String[] f : rows("requests.csv")) {
            PayModel.Request r = new PayModel.Request(f[0], f[1], Long.parseLong(f[2]), f[3]);
            PayModel.Wallet w = wallets.get(r.walletId());
            long avail = (w == null) ? 0 : eng.availableBalance(w, holds, pend, AS_OF);
            String status = (w == null) ? "99" : w.status();
            String[] d = eng.decide(r, avail, status).split(" ");
            System.out.println(r.reqId() + " " + d[0] + " " + avail + " " + d[1]);
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
