/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025-06-03  市場基盤部  初版作成
 */

package jp.mirai.sec.grouprisk;

public class ReconService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final int CODE_ACCEPT = 0;
    private static final int CODE_REJECT_MARGIN = 4;
    private static final int CODE_REJECT_NOTIONAL = 8;
    private static final int CODE_REJECT_TICK = 12;

    private static final java.nio.charset.Charset CS = java.nio.charset.StandardCharsets.UTF_8;

    public static void main(String[] a) throws Exception {
        if (a.length < 2) {
            System.err.println("使用方法: java ReconService SCEXEC.csv SCPOSF.csv");
            System.exit(2);
        }

        java.util.Map<String, FillAgg> fills = readExec(java.nio.file.Paths.get(a[0]));
        java.util.Map<String, PosAgg> positions = readPos(java.nio.file.Paths.get(a[1]));

        java.util.TreeSet<String> keys = new java.util.TreeSet<String>();
        keys.addAll(fills.keySet());
        keys.addAll(positions.keySet());

        System.out.println("照合日,銘柄コード,約定数量,残高数量,数量差異,約定金額,評価平均,実現損益,判定コード,事由");
        String bizDate = java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo")).toString();

        int breakCount = 0;
        for (String instr : keys) {
            FillAgg f = fills.get(instr);
            PosAgg p = positions.get(instr);

            long fillQty = f == null ? 0L : f.netQty;
            long fillAmt = f == null ? 0L : f.netAmt;
            long posQty = p == null ? 0L : p.netQty;
            long avgAmt = p == null ? 0L : p.avgAmt;
            long rlzdAmt = p == null ? 0L : p.rlzdAmt;
            long diff = fillQty - posQty;

            Decision d = judge(instr, f, p, diff);
            if (diff != 0L || d.code != CODE_ACCEPT) {
                breakCount++;
                System.out.println(csv(bizDate) + "," + csv(instr) + "," + fillQty + "," + posQty + "," + diff
                        + "," + fillAmt + "," + avgAmt + "," + rlzdAmt + "," + d.code + "," + csv(d.reason));
            }
        }

        System.err.println("照合完了: 対象銘柄数=" + keys.size() + " ブレーク件数=" + breakCount);
    }

    private static java.util.Map<String, FillAgg> readExec(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, FillAgg> map = new java.util.HashMap<String, FillAgg>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CS);
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.trim().isEmpty()) {
                continue;
            }

            java.util.List<String> c = parseCsv(line);
            if (c.size() < 7) {
                throw new IllegalArgumentException("SCEXEC形式不正: 行=" + (i + 1));
            }

            String execId = c.get(0).trim();
            String orderId = c.get(1).trim();
            String instr = c.get(2).trim();
            String side = c.get(3).trim();
            long qty = parseLong(c.get(4), "FILL-QTY", i + 1);
            long amt = parseLong(c.get(5), "FILL-AMT", i + 1);
            java.time.LocalDateTime execTs = parseTs(c.get(6), i + 1);

            if (execId.isEmpty() || orderId.isEmpty() || instr.isEmpty()) {
                throw new IllegalArgumentException("SCEXEC必須項目未設定: 行=" + (i + 1));
            }
            if (!"B".equals(side) && !"S".equals(side)) {
                throw new IllegalArgumentException("SIDE-KBN不正: 行=" + (i + 1) + " 値=" + side);
            }
            if (qty <= 0L || amt <= 0L) {
                throw new IllegalArgumentException("約定数量または約定金額不正: 行=" + (i + 1));
            }

            FillAgg agg = map.get(instr);
            if (agg == null) {
                agg = new FillAgg(instr);
                map.put(instr, agg);
            }

            long signedQty = "B".equals(side) ? qty : -qty;
            long signedAmt = "B".equals(side) ? amt : -amt;
            agg.netQty += signedQty;
            agg.netAmt += signedAmt;
            agg.grossNotional += amt;
            agg.count++;
            if (agg.firstTs == null || execTs.isBefore(agg.firstTs)) {
                agg.firstTs = execTs;
            }
            if (agg.lastTs == null || execTs.isAfter(agg.lastTs)) {
                agg.lastTs = execTs;
            }
        }
        return map;
    }

    private static java.util.Map<String, PosAgg> readPos(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, PosAgg> map = new java.util.HashMap<String, PosAgg>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CS);
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.trim().isEmpty()) {
                continue;
            }

            java.util.List<String> c = parseCsv(line);
            if (c.size() < 5) {
                throw new IllegalArgumentException("SCPOSF形式不正: 行=" + (i + 1));
            }

            String cifNo = c.get(0).trim();
            String instr = c.get(1).trim();
            long qty = parseLong(c.get(2), "NET-QTY", i + 1);
            long avgAmt = parseLong(c.get(3), "AVG-AMT", i + 1);
            long rlzdAmt = parseLong(c.get(4), "RLZD-AMT", i + 1);

            if (cifNo.isEmpty() || instr.isEmpty()) {
                throw new IllegalArgumentException("SCPOSF必須項目未設定: 行=" + (i + 1));
            }

            PosAgg agg = map.get(instr);
            if (agg == null) {
                agg = new PosAgg(instr);
                map.put(instr, agg);
            }
            agg.netQty += qty;
            agg.avgAmt += avgAmt;
            agg.rlzdAmt += rlzdAmt;
            agg.accountCount++;
        }
        return map;
    }

    private static Decision judge(String instr, FillAgg f, PosAgg p, long diff) {
        if (f == null) {
            return new Decision(CODE_REJECT_NOTIONAL, "残高のみ存在");
        }
        if (p == null) {
            return new Decision(CODE_REJECT_NOTIONAL, "約定のみ存在");
        }
        if (diff != 0L) {
            return new Decision(CODE_REJECT_NOTIONAL, "数量差異");
        }

        Tier tier = tierOf(instr);
        if (Math.abs(f.grossNotional) > MIHFT_MAX_NOTIONAL) {
            return new Decision(CODE_REJECT_NOTIONAL, "想定元本上限超過");
        }
        if (!onTick(f, tier.tick)) {
            return new Decision(CODE_REJECT_TICK, "呼値単位不整合");
        }

        long margin = Math.abs(f.grossNotional) * tier.marginRateBp / 10000L;
        long realizedAbs = Math.abs(p.rlzdAmt);
        if (realizedAbs > 0L && margin > realizedAbs * 20L) {
            return new Decision(CODE_REJECT_MARGIN, "証拠金見合い不足");
        }

        return new Decision(CODE_ACCEPT, "一致");
    }

    private static boolean onTick(FillAgg f, int tick) {
        if (f == null || f.netQty == 0L) {
            return true;
        }
        long absQty = Math.abs(f.netQty);
        long absAmt = Math.abs(f.netAmt);
        if (absQty == 0L) {
            return true;
        }
        if (absAmt % absQty != 0L) {
            return false;
        }
        long px = absAmt / absQty;
        return px % tick == 0L;
    }

    private static Tier tierOf(String instr) {
        int h = Math.abs(instr.hashCode());
        int bucket = h % 10;
        if (bucket < 5) {
            return new Tier(1, 1000, 100);
        }
        if (bucket < 8) {
            return new Tier(2, 2000, 500);
        }
        return new Tier(3, 4000, 1000);
    }

    private static long parseLong(String v, String name, int line) {
        try {
            return Long.parseLong(v.trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + "数値不正: 行=" + line + " 値=" + v, e);
        }
    }

    private static java.time.LocalDateTime parseTs(String v, int line) {
        String s = v.trim();
        try {
            return java.time.LocalDateTime.parse(s);
        } catch (java.time.format.DateTimeParseException e) {
            try {
                return java.time.OffsetDateTime.parse(s).toLocalDateTime();
            } catch (java.time.format.DateTimeParseException e2) {
                throw new IllegalArgumentException("EXEC-TS日時不正: 行=" + line + " 値=" + v, e2);
            }
        }
    }

    private static java.util.List<String> parseCsv(String line) {
        java.util.ArrayList<String> out = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean quote = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quote && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    quote = !quote;
                }
            } else if (ch == ',' && !quote) {
                out.add(b.toString());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        out.add(b.toString());
        return out;
    }

    private static String csv(String s) {
        if (s == null) {
            return "";
        }
        boolean q = s.indexOf(',') >= 0 || s.indexOf('"') >= 0 || s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0;
        if (!q) {
            return s;
        }
        return "\"" + s.replace("\"", "\"\"") + "\"";
    }

    private static final class FillAgg {
        final String instr;
        long netQty;
        long netAmt;
        long grossNotional;
        long count;
        java.time.LocalDateTime firstTs;
        java.time.LocalDateTime lastTs;

        FillAgg(String instr) {
            this.instr = instr;
        }
    }

    private static final class PosAgg {
        final String instr;
        long netQty;
        long avgAmt;
        long rlzdAmt;
        long accountCount;

        PosAgg(String instr) {
            this.instr = instr;
        }
    }

    private static final class Tier {
        final int tier;
        final int marginRateBp;
        final int tick;

        Tier(int tier, int marginRateBp, int tick) {
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tick = tick;
        }
    }

    private static final class Decision {
        final int code;
        final String reason;

        Decision(int code, String reason) {
            this.code = code;
            this.reason = reason;
        }
    }
}
