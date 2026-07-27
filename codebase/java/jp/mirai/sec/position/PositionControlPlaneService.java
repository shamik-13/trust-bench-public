/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2020-09-02  村上 健司 (E-301)   ポジション制御プレーン日次締め初版
 */

package jp.mirai.sec.position;

public class PositionControlPlaneService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    public static void main(String[] a) throws Exception {
        java.nio.file.Path dir = java.nio.file.Paths.get(a.length == 0 ? "." : a[0]);
        java.time.LocalDate sessDt = a.length >= 2 ? java.time.LocalDate.parse(a[1]) : java.time.LocalDate.now();

        java.util.Map<String, Instr> instrs = loadInstr(dir.resolve("SCINSTF.csv"));
        java.util.Map<String, Fee> fees = loadFee(dir.resolve("SCFEEF.csv"));
        java.util.Map<Key, Pos> basePos = loadPos(dir.resolve("SCPOSF.csv"));
        java.util.List<Exec> execs = loadExec(dir.resolve("SCEXEC.csv"));
        java.util.Map<String, Market> markets = loadMarket(dir.resolve("SCMKTD.csv"));
        java.util.List<Action> actions = loadAction(dir.resolve("SCCACT.csv"));
        java.util.Map<String, Customer> customers = loadCustomer(dir.resolve("SCCUST.csv"));
        java.util.Map<Key, Hold> holds = loadHold(dir.resolve("SCHLDF.csv"));

        java.time.LocalDateTime calcTs = java.time.LocalDateTime.now();
        java.util.Map<Key, Work> works = new java.util.TreeMap<Key, Work>();
        java.util.List<RiskEvent> risks = new java.util.ArrayList<RiskEvent>();
        java.util.List<Settle> settles = new java.util.ArrayList<Settle>();

        for (java.util.Map.Entry<Key, Pos> e : basePos.entrySet()) {
            Pos p = e.getValue();
            Work w = new Work(e.getKey(), p.netQty, p.avgAmt, p.rlzdAmt);
            works.put(e.getKey(), w);
        }

        for (Exec x : execs) {
            Instr ins = instrs.get(x.instrCode);
            if (ins == null) {
                risks.add(new RiskEvent("RISK-" + x.execId, "", x.instrCode, calcTs, 0, x.fillQty * x.fillAmt, 8));
                continue;
            }
            long notional = x.fillQty * x.fillAmt;
            if (notional > MIHFT_MAX_NOTIONAL) {
                risks.add(new RiskEvent("RISK-" + x.execId, "", x.instrCode, calcTs, MIHFT_MAX_NOTIONAL, notional, 8));
                continue;
            }
            if (x.fillAmt % ins.tickAmt != 0) {
                risks.add(new RiskEvent("RISK-" + x.execId, "", x.instrCode, calcTs, 0, x.fillAmt, 12));
                continue;
            }

            String cif = orderCustomer(x.orderId);
            Customer c = customers.get(cif);
            long margin = notional * tierMarginBp(ins.instrTier) / 10000L;
            if (c != null && c.groupUsedAmt + margin > c.groupLimit) {
                risks.add(new RiskEvent("RISK-" + x.execId, cif, x.instrCode, calcTs, c.groupLimit, c.groupUsedAmt + margin, 4));
                continue;
            }

            Key k = new Key(cif, x.instrCode);
            Work w = works.get(k);
            if (w == null) {
                w = new Work(k, 0, 0, 0);
                works.put(k, w);
            }
            Fee f = fees.get(ins.boardCode);
            long feeAmt = calcFee(notional, f);

            // AVG-AMT (平均取得単価) の算定は mihft_pos 本体に従う。当制御プレーンは
            // SCPOSF が確定した AVG-AMT を所与とし、数量更新と実現損益・建玉ゼロ時の
            // 初期化のみを行う。約定列からの取得単価の再算定は行わない。
            if ("B".equals(x.sideKbn)) {
                long newQty = w.netQty + x.fillQty;
                if (newQty == 0) {
                    w.avgAmt = 0;
                }
                w.netQty = newQty;
            } else {
                long closeQty = Math.min(Math.abs(w.netQty), x.fillQty);
                w.rlzdAmt += (x.fillAmt - w.avgAmt) * closeQty - feeAmt;
                w.netQty -= x.fillQty;
                if (w.netQty == 0) {
                    w.avgAmt = 0;
                } else if (w.netQty < 0) {
                    w.avgAmt = x.fillAmt;
                }
            }
            w.feeAmt += feeAmt;

            java.time.LocalDate settleDt = sessDt.plusDays(2);
            settles.add(new Settle("SET-" + x.execId, cif, x.instrCode, settleDt,
                    "B".equals(x.sideKbn) ? x.fillQty : -x.fillQty,
                    "B".equals(x.sideKbn) ? -(notional + feeAmt) : notional - feeAmt,
                    "0"));
        }

        for (Action ac : actions) {
            if (!sessDt.equals(ac.exDt)) {
                continue;
            }
            for (Work w : works.values()) {
                if (!w.key.instrCode.equals(ac.instrCode)) {
                    continue;
                }
                if ("SPLIT".equals(ac.actionKbn) && ac.ratioDen != 0) {
                    w.netQty = w.netQty * ac.ratioNum / ac.ratioDen;
                    w.avgAmt = w.avgAmt * ac.ratioDen / ac.ratioNum;
                } else if ("CASH".equals(ac.actionKbn)) {
                    w.rlzdAmt += w.netQty * ac.cashAmt;
                }
            }
        }

        java.util.Map<String, Exposure> exposures = new java.util.TreeMap<String, Exposure>();
        java.util.List<Pnl> pnls = new java.util.ArrayList<Pnl>();
        java.util.List<M2m> m2ms = new java.util.ArrayList<M2m>();
        java.util.List<Hold> outHolds = new java.util.ArrayList<Hold>();

        for (Work w : works.values()) {
            Market m = markets.get(w.key.instrCode);
            long mark = m == null ? w.avgAmt : m.lastAmt;
            long notional = w.netQty * mark;
            long unrlzd = (mark - w.avgAmt) * w.netQty;
            pnls.add(new Pnl(w.key.cifNo, w.key.instrCode, sessDt, w.rlzdAmt, unrlzd, w.feeAmt, calcTs));
            m2ms.add(new M2m(w.key.cifNo, w.key.instrCode, sessDt, w.netQty, mark, notional, unrlzd));

            Exposure ex = exposures.get(w.key.cifNo);
            if (ex == null) {
                ex = new Exposure(w.key.cifNo, sessDt);
                exposures.put(w.key.cifNo, ex);
            }
            if (notional >= 0) {
                ex.grossLongAmt += notional;
            } else {
                ex.grossShortAmt += -notional;
            }

            Hold h = holds.get(w.key);
            long settled = h == null ? 0 : h.settledQty;
            long restricted = h == null ? 0 : h.restrictedQty;
            outHolds.add(new Hold(w.key.cifNo, w.key.instrCode, sessDt, settled, w.netQty - settled, restricted));
        }

        for (Exposure ex : exposures.values()) {
            Customer c = customers.get(ex.cifNo);
            ex.netExposureAmt = ex.grossLongAmt - ex.grossShortAmt;
            long limit = c == null ? 0 : c.groupLimit;
            long used = Math.abs(ex.netExposureAmt);
            ex.limitUtilPct = limit == 0 ? 0 : used * 10000L / limit;
            if (limit > 0 && used > limit) {
                risks.add(new RiskEvent("RISK-EXP-" + ex.cifNo + "-" + sessDt, ex.cifNo, "", calcTs, limit, used, 4));
            }
        }

        writePnl(dir.resolve("SCPNLF.out"), pnls);
        writeM2m(dir.resolve("SCM2MF.out"), m2ms);
        writeExposure(dir.resolve("SCEXPF.out"), exposures.values());
        writeRisk(dir.resolve("SCRISKF2.out"), risks);
        writeSettle(dir.resolve("SCSETF.out"), settles);
        writeHold(dir.resolve("SCHLDF.out"), outHolds);
    }

    private static long tierMarginBp(int tier) {
        if (tier == 1) return 1000L;
        if (tier == 2) return 2000L;
        return 4000L;
    }

    private static long calcFee(long notional, Fee f) {
        if (f == null) return 0L;
        long fee = notional * f.feeRate / 1000000L;
        return Math.max(fee, f.minFeeAmt);
    }

    private static String orderCustomer(String orderId) {
        int p = orderId.indexOf('-');
        return p > 0 ? orderId.substring(0, p) : "";
    }

    private static java.util.List<String[]> csv(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<String[]> r = new java.util.ArrayList<String[]>();
        if (!java.nio.file.Files.exists(p)) return r;
        java.util.List<String> lines = java.nio.file.Files.readAllLines(p, java.nio.charset.StandardCharsets.UTF_8);
        for (int i = 1; i < lines.size(); i++) {
            String s = lines.get(i).trim();
            if (!s.isEmpty()) r.add(s.split(",", -1));
        }
        return r;
    }

    private static java.util.Map<String, Instr> loadInstr(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, Instr> m = new java.util.HashMap<String, Instr>();
        for (String[] v : csv(p)) m.put(v[0], new Instr(v[0], v[1], Integer.parseInt(v[2]), Long.parseLong(v[3]), Long.parseLong(v[4]), v[5]));
        return m;
    }

    private static java.util.Map<String, Fee> loadFee(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, Fee> m = new java.util.HashMap<String, Fee>();
        for (String[] v : csv(p)) m.put(v[0], new Fee(v[0], Long.parseLong(v[1]), Long.parseLong(v[2])));
        return m;
    }

    private static java.util.Map<Key, Pos> loadPos(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<Key, Pos> m = new java.util.HashMap<Key, Pos>();
        for (String[] v : csv(p)) m.put(new Key(v[0], v[1]), new Pos(v[0], v[1], Long.parseLong(v[2]), Long.parseLong(v[3]), Long.parseLong(v[4])));
        return m;
    }

    private static java.util.List<Exec> loadExec(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<Exec> r = new java.util.ArrayList<Exec>();
        for (String[] v : csv(p)) r.add(new Exec(v[0], v[1], v[2], v[3], Long.parseLong(v[4]), Long.parseLong(v[5]), java.time.LocalDateTime.parse(v[6])));
        r.sort(java.util.Comparator.comparing((Exec e) -> e.execTs).thenComparing(e -> e.execId));
        return r;
    }

    private static java.util.Map<String, Market> loadMarket(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, Market> m = new java.util.HashMap<String, Market>();
        for (String[] v : csv(p)) m.put(v[0], new Market(v[0], Long.parseLong(v[1]), Long.parseLong(v[2]), Long.parseLong(v[3]), Long.parseLong(v[4]), java.time.LocalDateTime.parse(v[5])));
        return m;
    }

    private static java.util.List<Action> loadAction(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<Action> r = new java.util.ArrayList<Action>();
        for (String[] v : csv(p)) r.add(new Action(v[0], v[1], java.time.LocalDate.parse(v[2]), v[3], Long.parseLong(v[4]), Long.parseLong(v[5]), Long.parseLong(v[6])));
        return r;
    }

    private static java.util.Map<String, Customer> loadCustomer(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, Customer> m = new java.util.HashMap<String, Customer>();
        for (String[] v : csv(p)) m.put(v[0], new Customer(v[0], Long.parseLong(v[1]), Long.parseLong(v[2]), Long.parseLong(v[3])));
        return m;
    }

    private static java.util.Map<Key, Hold> loadHold(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<Key, Hold> m = new java.util.HashMap<Key, Hold>();
        for (String[] v : csv(p)) m.put(new Key(v[0], v[1]), new Hold(v[0], v[1], java.time.LocalDate.parse(v[2]), Long.parseLong(v[3]), Long.parseLong(v[4]), Long.parseLong(v[5])));
        return m;
    }

    private static void writePnl(java.nio.file.Path p, java.util.List<Pnl> r) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<String>();
        out.add("CIF-NO,INSTR-CODE,SESS-DT,RLZD-AMT,UNRLZD-AMT,FEE-AMT,CALC-TS");
        for (Pnl x : r) out.add(x.cifNo + "," + x.instrCode + "," + x.sessDt + "," + x.rlzdAmt + "," + x.unrlzdAmt + "," + x.feeAmt + "," + x.calcTs);
        java.nio.file.Files.write(p, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeM2m(java.nio.file.Path p, java.util.List<M2m> r) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<String>();
        out.add("CIF-NO,INSTR-CODE,SESS-DT,NET-QTY,MARK-AMT,MARK-NOTIONAL-AMT,UNRLZD-AMT");
        for (M2m x : r) out.add(x.cifNo + "," + x.instrCode + "," + x.sessDt + "," + x.netQty + "," + x.markAmt + "," + x.markNotionalAmt + "," + x.unrlzdAmt);
        java.nio.file.Files.write(p, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeExposure(java.nio.file.Path p, java.util.Collection<Exposure> r) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<String>();
        out.add("CIF-NO,SESS-DT,GROSS-LONG-AMT,GROSS-SHORT-AMT,NET-EXPOSURE-AMT,LIMIT-UTIL-PCT");
        for (Exposure x : r) out.add(x.cifNo + "," + x.sessDt + "," + x.grossLongAmt + "," + x.grossShortAmt + "," + x.netExposureAmt + "," + x.limitUtilPct);
        java.nio.file.Files.write(p, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeRisk(java.nio.file.Path p, java.util.List<RiskEvent> r) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<String>();
        out.add("RISK-EVENT-ID,CIF-NO,INSTR-CODE,EVENT-TS,LIMIT-AMT,USED-AMT,DECISION-KBN");
        for (RiskEvent x : r) out.add(x.riskEventId + "," + x.cifNo + "," + x.instrCode + "," + x.eventTs + "," + x.limitAmt + "," + x.usedAmt + "," + x.decisionKbn);
        java.nio.file.Files.write(p, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeSettle(java.nio.file.Path p, java.util.List<Settle> r) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<String>();
        out.add("SETTLE-ID,CIF-NO,INSTR-CODE,SETTLE-DT,NET-QTY,NET-CASH-AMT,STATUS-KBN");
        for (Settle x : r) out.add(x.settleId + "," + x.cifNo + "," + x.instrCode + "," + x.settleDt + "," + x.netQty + "," + x.netCashAmt + "," + x.statusKbn);
        java.nio.file.Files.write(p, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeHold(java.nio.file.Path p, java.util.List<Hold> r) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<String>();
        out.add("CIF-NO,INSTR-CODE,ASOF-DT,SETTLED-QTY,TRADE-QTY,RESTRICTED-QTY");
        for (Hold x : r) out.add(x.cifNo + "," + x.instrCode + "," + x.asofDt + "," + x.settledQty + "," + x.tradeQty + "," + x.restrictedQty);
        java.nio.file.Files.write(p, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static final class Key implements Comparable<Key> {
        final String cifNo;
        final String instrCode;
        Key(String c, String i) { cifNo = c; instrCode = i; }
        public int compareTo(Key o) {
            int n = cifNo.compareTo(o.cifNo);
            return n != 0 ? n : instrCode.compareTo(o.instrCode);
        }
        public boolean equals(Object o) {
            if (!(o instanceof Key)) return false;
            Key k = (Key) o;
            return cifNo.equals(k.cifNo) && instrCode.equals(k.instrCode);
        }
        public int hashCode() { return cifNo.hashCode() * 31 + instrCode.hashCode(); }
    }

    private static final class Instr {
        final String instrCode, instrName, boardCode;
        final int instrTier;
        final long tickAmt, lotQty;
        Instr(String a, String b, int c, long d, long e, String f) { instrCode = a; instrName = b; instrTier = c; tickAmt = d; lotQty = e; boardCode = f; }
    }

    private static final class Fee {
        final String boardCode;
        final long feeRate, minFeeAmt;
        Fee(String a, long b, long c) { boardCode = a; feeRate = b; minFeeAmt = c; }
    }

    private static final class Pos {
        final String cifNo, instrCode;
        final long netQty, avgAmt, rlzdAmt;
        Pos(String a, String b, long c, long d, long e) { cifNo = a; instrCode = b; netQty = c; avgAmt = d; rlzdAmt = e; }
    }

    private static final class Exec {
        final String execId, orderId, instrCode, sideKbn;
        final long fillQty, fillAmt;
        final java.time.LocalDateTime execTs;
        Exec(String a, String b, String c, String d, long e, long f, java.time.LocalDateTime g) { execId = a; orderId = b; instrCode = c; sideKbn = d; fillQty = e; fillAmt = f; execTs = g; }
    }

    private static final class Market {
        final String instrCode;
        final long bidAmt, askAmt, lastAmt, volQty;
        final java.time.LocalDateTime tickTs;
        Market(String a, long b, long c, long d, long e, java.time.LocalDateTime f) { instrCode = a; bidAmt = b; askAmt = c; lastAmt = d; volQty = e; tickTs = f; }
    }

    private static final class Action {
        final String actionId, instrCode, actionKbn;
        final java.time.LocalDate exDt;
        final long ratioNum, ratioDen, cashAmt;
        Action(String a, String b, java.time.LocalDate c, String d, long e, long f, long g) { actionId = a; instrCode = b; exDt = c; actionKbn = d; ratioNum = e; ratioDen = f; cashAmt = g; }
    }

    private static final class Customer {
        final String cifNo;
        final long groupLimit, groupUsedAmt, acctUsedAmt;
        Customer(String a, long b, long c, long d) { cifNo = a; groupLimit = b; groupUsedAmt = c; acctUsedAmt = d; }
    }

    private static final class Work {
        final Key key;
        long netQty, avgAmt, rlzdAmt, feeAmt;
        Work(Key a, long b, long c, long d) { key = a; netQty = b; avgAmt = c; rlzdAmt = d; }
    }

    private static final class Pnl {
        final String cifNo, instrCode;
        final java.time.LocalDate sessDt;
        final long rlzdAmt, unrlzdAmt, feeAmt;
        final java.time.LocalDateTime calcTs;
        Pnl(String a, String b, java.time.LocalDate c, long d, long e, long f, java.time.LocalDateTime g) { cifNo = a; instrCode = b; sessDt = c; rlzdAmt = d; unrlzdAmt = e; feeAmt = f; calcTs = g; }
    }

    private static final class M2m {
        final String cifNo, instrCode;
        final java.time.LocalDate sessDt;
        final long netQty, markAmt, markNotionalAmt, unrlzdAmt;
        M2m(String a, String b, java.time.LocalDate c, long d, long e, long f, long g) { cifNo = a; instrCode = b; sessDt = c; netQty = d; markAmt = e; markNotionalAmt = f; unrlzdAmt = g; }
    }

    private static final class Exposure {
        final String cifNo;
        final java.time.LocalDate sessDt;
        long grossLongAmt, grossShortAmt, netExposureAmt, limitUtilPct;
        Exposure(String a, java.time.LocalDate b) { cifNo = a; sessDt = b; }
    }

    private static final class RiskEvent {
        final String riskEventId, cifNo, instrCode;
        final java.time.LocalDateTime eventTs;
        final long limitAmt, usedAmt;
        final int decisionKbn;
        RiskEvent(String a, String b, String c, java.time.LocalDateTime d, long e, long f, int g) { riskEventId = a; cifNo = b; instrCode = c; eventTs = d; limitAmt = e; usedAmt = f; decisionKbn = g; }
    }

    private static final class Settle {
        final String settleId, cifNo, instrCode, statusKbn;
        final java.time.LocalDate settleDt;
        final long netQty, netCashAmt;
        Settle(String a, String b, String c, java.time.LocalDate d, long e, long f, String g) { settleId = a; cifNo = b; instrCode = c; settleDt = d; netQty = e; netCashAmt = f; statusKbn = g; }
    }

    private static final class Hold {
        final String cifNo, instrCode;
        final java.time.LocalDate asofDt;
        final long settledQty, tradeQty, restrictedQty;
        Hold(String a, String b, java.time.LocalDate c, long d, long e, long f) { cifNo = a; instrCode = b; asofDt = c; settledQty = d; tradeQty = e; restrictedQty = f; }
    }
}
