/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2021/01/28  三宅 拓也 (E-241)  初版作成
 */

package jp.mirai.sec.matching;

public class OrderStateService {
    private static final String SERVICE_ID = "OrderStateService";
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    public static void main(String[] a) throws Exception {
        java.util.List<String> orderLines;
        java.util.List<String> execLines;
        java.util.List<String> rejectLines;
        java.io.PrintWriter out;

        if (a.length >= 4) {
            orderLines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(a[0]), java.nio.charset.StandardCharsets.UTF_8);
            execLines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(a[1]), java.nio.charset.StandardCharsets.UTF_8);
            rejectLines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(a[2]), java.nio.charset.StandardCharsets.UTF_8);
            out = new java.io.PrintWriter(java.nio.file.Files.newBufferedWriter(
                    java.nio.file.Paths.get(a[3]), java.nio.charset.StandardCharsets.UTF_8));
        } else {
            orderLines = java.util.Arrays.asList(
                    "ORDER-ID,CIF-NO,INSTR-CODE,SIDE-KBN,ORD-TYPE,TIF-CODE,ORD-QTY,PRICE-AMT,INSTR-TIER",
                    "O202501150001,C000001,7203,B,L,DAY,3000,312000,1",
                    "O202501150002,C000002,9984,S,L,IOC,1000,942000,1",
                    "O202501150003,C000003,4568,B,M,FOK,500,0,2",
                    "O202501150004,C000004,3911,B,L,DAY,800,151200,3",
                    "O202501150005,C000005,1306,B,L,DAY,1000000,200000,1",
                    "O202501150006,C000006,6758,B,L,FOK,1000,142000,1"
            );
            execLines = java.util.Arrays.asList(
                    "EXEC-ID,ORDER-ID,INSTR-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,EXEC-TS",
                    "E202501150001,O202501150001,7203,B,1000,312000000,2025-01-15T09:05:10",
                    "E202501150002,O202501150001,7203,B,2000,624000000,2025-01-15T09:08:20",
                    "E202501150003,O202501150002,9984,S,400,376800000,2025-01-15T09:10:00",
                    "E202501150004,O202501150006,6758,B,500,71000000,2025-01-15T09:12:00",
                    "E202501150005,O202501159999,7203,B,100,31200000,2025-01-15T09:13:00"
            );
            rejectLines = java.util.Arrays.asList(
                    "REJECT-ID,ORDER-ID,CIF-NO,INSTR-CODE,REJECT-CODE,REJECT-TS",
                    "R202501150001,O202501150004,C000004,3911,12,2025-01-15T09:01:00",
                    "R202501150002,O202501150001,C000001,7203,4,2025-01-15T09:09:00",
                    "R202501150003,O202501150888,C000888,8306,8,2025-01-15T09:11:00"
            );
            out = new java.io.PrintWriter(System.out);
        }

        try {
            new OrderStateService().run(orderLines, execLines, rejectLines, out);
        } finally {
            out.flush();
            if (a.length >= 4) {
                out.close();
            }
        }
    }

    private void run(java.util.List<String> orderLines,
                     java.util.List<String> execLines,
                     java.util.List<String> rejectLines,
                     java.io.PrintWriter out) {
        RefDataService refData = new RefDataService();
        java.util.Map<String, OrderRec> orders = new java.util.LinkedHashMap<String, OrderRec>();
        java.util.Map<String, FillAgg> fills = new java.util.LinkedHashMap<String, FillAgg>();
        java.util.List<RejectRec> rejects = new java.util.ArrayList<RejectRec>();
        java.util.List<AuditRec> audits = new java.util.ArrayList<AuditRec>();

        for (String[] c : parseCsv(orderLines, 9)) {
            OrderRec o = new OrderRec(c[0], c[1], c[2], c[3], c[4], c[5],
                    parseLong(c[6]), parseLong(c[7]), parseInt(c[8]));
            if (orders.containsKey(o.orderId)) {
                audits.add(audit(o.orderId, "矛盾", "注文重複"));
                continue;
            }
            orders.put(o.orderId, o);
            validateOrder(o, refData, audits);
        }

        for (String[] c : parseCsv(execLines, 7)) {
            ExecRec e = new ExecRec(c[0], c[1], c[2], c[3], parseLong(c[4]), parseLong(c[5]), c[6]);
            OrderRec o = orders.get(e.orderId);
            if (o == null) {
                audits.add(audit(e.orderId, "矛盾", "約定対象注文なし"));
                continue;
            }
            if (!o.instrCode.equals(e.instrCode) || !o.sideKbn.equals(e.sideKbn)) {
                audits.add(audit(e.orderId, "矛盾", "約定属性不一致"));
                continue;
            }
            if (!refData.isTradingDay(e.execTs.substring(0, 10)) || !refData.existsInstrument(e.instrCode)) {
                audits.add(audit(e.orderId, "矛盾", "約定日銘柄不正"));
                continue;
            }
            FillAgg agg = fills.get(e.orderId);
            if (agg == null) {
                agg = new FillAgg();
                fills.put(e.orderId, agg);
            }
            agg.qty += e.fillQty;
            agg.amt += e.fillAmt;
            if (agg.qty > o.ordQty) {
                audits.add(audit(e.orderId, "矛盾", "注文数量超過"));
            }
        }

        for (String[] c : parseCsv(rejectLines, 6)) {
            RejectRec r = new RejectRec(c[0], c[1], c[2], c[3], c[4], c[5]);
            rejects.add(r);
            OrderRec o = orders.get(r.orderId);
            FillAgg agg = fills.get(r.orderId);
            if (o == null) {
                if (!isRejectCode(r.rejectCode)) {
                    audits.add(audit(r.orderId, "矛盾", "拒否理由不正"));
                }
                if (!refData.existsInstrument(r.instrCode) || !refData.isTradingDay(r.rejectTs.substring(0, 10))) {
                    audits.add(audit(r.orderId, "矛盾", "拒否日銘柄不正"));
                }
                continue;
            }
            if (!o.cifNo.equals(r.cifNo) || !o.instrCode.equals(r.instrCode)) {
                audits.add(audit(r.orderId, "矛盾", "拒否属性不一致"));
            }
            if (agg != null && agg.qty > 0) {
                audits.add(audit(r.orderId, "矛盾", "約定後拒否"));
            } else {
                audits.add(audit(r.orderId, "矛盾", "受付済注文拒否"));
            }
        }

        for (OrderRec o : orders.values()) {
            FillAgg agg = fills.get(o.orderId);
            long filled = agg == null ? 0L : agg.qty;
            String state = decideState(o, filled);
            if ("部分約定".equals(state) && "FOK".equals(o.tifCode)) {
                audits.add(audit(o.orderId, "矛盾", "FOK部分約定"));
            }
            if ("受付済".equals(state) && "IOC".equals(o.tifCode)) {
                audits.add(audit(o.orderId, "失効", "IOC未約定失効"));
            }
            if ("部分約定".equals(state) && "IOC".equals(o.tifCode)) {
                audits.add(audit(o.orderId, "失効", "IOC残数量失効"));
            }
            audits.add(audit(o.orderId, state, "状態確定"));
        }

        out.println("AUDIT-ID,EVENT-TS,SERVICE-ID,OBJECT-ID,EVENT-KBN,DETAIL-CODE");
        for (int i = 0; i < audits.size(); i++) {
            AuditRec x = audits.get(i);
            out.println("A" + String.format("%012d", i + 1) + "," + x.eventTs + "," + x.serviceId + ","
                    + x.objectId + "," + x.eventKbn + "," + x.detailCode);
        }
    }

    private void validateOrder(OrderRec o, RefDataService refData, java.util.List<AuditRec> audits) {
        if (!("B".equals(o.sideKbn) || "S".equals(o.sideKbn))) {
            audits.add(audit(o.orderId, "矛盾", "売買区分不正"));
        }
        if (!("L".equals(o.ordType) || "M".equals(o.ordType))) {
            audits.add(audit(o.orderId, "矛盾", "注文種別不正"));
        }
        if (!("DAY".equals(o.tifCode) || "IOC".equals(o.tifCode) || "FOK".equals(o.tifCode))) {
            audits.add(audit(o.orderId, "矛盾", "有効期限不正"));
        }
        if (!refData.existsInstrument(o.instrCode)) {
            audits.add(audit(o.orderId, "矛盾", "銘柄なし"));
        }
        TierRule tier = tierRule(o.instrTier);
        if (tier == null) {
            audits.add(audit(o.orderId, "拒否", "12"));
            return;
        }
        if ("L".equals(o.ordType) && o.priceAmt % tier.tick != 0) {
            audits.add(audit(o.orderId, "拒否", "12"));
        }
        long basisPrice = "M".equals(o.ordType) ? refData.referencePrice(o.instrCode) : o.priceAmt;
        long notional = multiplyCap(o.ordQty, basisPrice);
        if (notional > MIHFT_MAX_NOTIONAL) {
            audits.add(audit(o.orderId, "拒否", "8"));
        }
        long margin = multiplyCap(notional, tier.rateBp) / 10000L;
        if (margin > refData.marginLimit(o.cifNo)) {
            audits.add(audit(o.orderId, "拒否", "4"));
        }
    }

    private String decideState(OrderRec o, long filled) {
        if (filled <= 0L) {
            return "受付済";
        }
        if (filled < o.ordQty) {
            return "部分約定";
        }
        if (filled == o.ordQty) {
            return "全約定";
        }
        return "矛盾";
    }

    private AuditRec audit(String objectId, String eventKbn, String detailCode) {
        return new AuditRec(nowText(), SERVICE_ID, objectId, eventKbn, detailCode);
    }

    private static java.util.List<String[]> parseCsv(java.util.List<String> lines, int width) {
        java.util.List<String[]> rows = new java.util.ArrayList<String[]>();
        for (int i = 0; i < lines.size(); i++) {
            String s = lines.get(i);
            if (s == null || s.trim().isEmpty()) {
                continue;
            }
            if (i == 0 && s.indexOf('-') >= 0) {
                continue;
            }
            String[] c = splitCsv(s);
            if (c.length != width) {
                throw new IllegalArgumentException("項目数不正:" + s);
            }
            rows.add(c);
        }
        return rows;
    }

    private static String[] splitCsv(String s) {
        java.util.List<String> r = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean q = false;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (ch == '"') {
                if (q && i + 1 < s.length() && s.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    q = !q;
                }
            } else if (ch == ',' && !q) {
                r.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        r.add(b.toString().trim());
        return r.toArray(new String[r.size()]);
    }

    private static long parseLong(String s) {
        return Long.parseLong(s.trim());
    }

    private static int parseInt(String s) {
        return Integer.parseInt(s.trim());
    }

    private static boolean isRejectCode(String s) {
        return "4".equals(s) || "8".equals(s) || "12".equals(s);
    }

    private static long multiplyCap(long a, long b) {
        if (a != 0L && b > Long.MAX_VALUE / a) {
            return Long.MAX_VALUE;
        }
        return a * b;
    }

    private static String nowText() {
        return java.time.LocalDateTime.now().withNano(0).toString();
    }

    private static TierRule tierRule(int tier) {
        if (tier == 1) {
            return new TierRule(1000, 100);
        }
        if (tier == 2) {
            return new TierRule(2000, 500);
        }
        if (tier == 3) {
            return new TierRule(4000, 1000);
        }
        return null;
    }

    private static final class RefDataService {
        private final java.util.Map<String, Long> prices = new java.util.HashMap<String, Long>();
        private final java.util.Map<String, String> boards = new java.util.HashMap<String, String>();

        RefDataService() {
            prices.put("7203", 312000L);
            prices.put("9984", 942000L);
            prices.put("4568", 601000L);
            prices.put("3911", 151000L);
            prices.put("1306", 200000L);
            prices.put("6758", 142000L);
            prices.put("8306", 158000L);

            boards.put("7203", "T1");
            boards.put("9984", "T1");
            boards.put("4568", "T1");
            boards.put("3911", "ST");
            boards.put("1306", "ETF");
            boards.put("6758", "T1");
            boards.put("8306", "T1");
        }

        boolean existsInstrument(String instrCode) {
            return prices.containsKey(instrCode) && boards.containsKey(instrCode);
        }

        long referencePrice(String instrCode) {
            Long p = prices.get(instrCode);
            return p == null ? 0L : p.longValue();
        }

        boolean isTradingDay(String yyyyMmDd) {
            java.time.LocalDate d = java.time.LocalDate.parse(yyyyMmDd);
            java.time.DayOfWeek w = d.getDayOfWeek();
            return w != java.time.DayOfWeek.SATURDAY && w != java.time.DayOfWeek.SUNDAY;
        }

        long marginLimit(String cifNo) {
            int h = Math.abs(cifNo.hashCode() % 5);
            return 30000000L + h * 20000000L;
        }
    }

    private static final class TierRule {
        final int rateBp;
        final long tick;

        TierRule(int rateBp, long tick) {
            this.rateBp = rateBp;
            this.tick = tick;
        }
    }

    private static final class OrderRec {
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final String ordType;
        final String tifCode;
        final long ordQty;
        final long priceAmt;
        final int instrTier;

        OrderRec(String orderId, String cifNo, String instrCode, String sideKbn, String ordType,
                 String tifCode, long ordQty, long priceAmt, int instrTier) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.ordType = ordType;
            this.tifCode = tifCode;
            this.ordQty = ordQty;
            this.priceAmt = priceAmt;
            this.instrTier = instrTier;
        }
    }

    private static final class ExecRec {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final String execTs;

        ExecRec(String execId, String orderId, String instrCode, String sideKbn,
                long fillQty, long fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class RejectRec {
        final String rejectId;
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String rejectCode;
        final String rejectTs;

        RejectRec(String rejectId, String orderId, String cifNo, String instrCode,
                  String rejectCode, String rejectTs) {
            this.rejectId = rejectId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.rejectCode = rejectCode;
            this.rejectTs = rejectTs;
        }
    }

    private static final class FillAgg {
        long qty;
        long amt;
    }

    private static final class AuditRec {
        final String eventTs;
        final String serviceId;
        final String objectId;
        final String eventKbn;
        final String detailCode;

        AuditRec(String eventTs, String serviceId, String objectId, String eventKbn, String detailCode) {
            this.eventTs = eventTs;
            this.serviceId = serviceId;
            this.objectId = objectId;
            this.eventKbn = eventKbn;
            this.detailCode = detailCode;
        }
    }
}
