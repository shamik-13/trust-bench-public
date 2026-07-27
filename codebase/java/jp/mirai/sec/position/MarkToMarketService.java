package jp.mirai.sec.position;

public class MarkToMarketService {

    /*
     * 変更履歴
     * 版数  年月日      担当        概要
     * 1.00  2020-03-10  西村 亮 (E-204)    SCM2MF時価評価サービス初版
     */

    private static final java.math.BigDecimal MIHFT_MAX_NOTIONAL = new java.math.BigDecimal("500000000");

    private static final String DEF_SCPOSF = "SCPOSF.csv";
    private static final String DEF_SCMKTD = "SCMKTD.csv";
    private static final String DEF_SCM2MF = "SCM2MF.csv";
    private static final String DEF_OUT = "SCM2MF.out.csv";

    public static void main(String[] a) throws Exception {
        String scposf = a.length > 0 ? a[0] : DEF_SCPOSF;
        String scmktd = a.length > 1 ? a[1] : DEF_SCMKTD;
        String scm2mf = a.length > 2 ? a[2] : DEF_SCM2MF;
        String out = a.length > 3 ? a[3] : DEF_OUT;
        String sessDt = a.length > 4 ? a[4] : java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo")).toString();

        java.util.Map<String, Position> positions = readPositions(java.nio.file.Paths.get(scposf));
        java.util.Map<String, Market> markets = readMarkets(java.nio.file.Paths.get(scmktd));
        java.util.Map<String, MarkRow> marks = readMarks(java.nio.file.Paths.get(scm2mf));

        int updated = 0;
        int supplemented = 0;
        int skipped = 0;

        for (Position p : positions.values()) {
            String key = key(p.cifNo, p.instrCode);
            MarkRow row = marks.get(key);
            if (row == null) {
                row = new MarkRow(p.cifNo, p.instrCode, sessDt, p.netQty,
                        java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO);
                marks.put(key, row);
                supplemented++;
            }

            Market m = markets.get(p.instrCode);
            if (m == null) {
                skipped++;
                continue;
            }

            java.math.BigDecimal markAmt = choosePrice(p.netQty, m);
            if (markAmt == null || markAmt.signum() <= 0) {
                skipped++;
                continue;
            }

            long tick = tickFor(p.instrCode);
            markAmt = alignTick(markAmt, tick, p.netQty);
            java.math.BigDecimal markNotional = markAmt.multiply(java.math.BigDecimal.valueOf(p.netQty));
            java.math.BigDecimal costNotional = p.avgAmt.multiply(java.math.BigDecimal.valueOf(p.netQty));
            java.math.BigDecimal unrealized = markNotional.subtract(costNotional);

            row.sessDt = sessDt;
            row.netQty = p.netQty;
            row.markAmt = markAmt;
            row.markNotionalAmt = markNotional;
            row.unrlzdAmt = unrealized;
            updated++;
        }

        writeMarks(java.nio.file.Paths.get(out), marks.values());
        System.out.println("処理終了 更新=" + updated + " 補完=" + supplemented + " 評価不能=" + skipped);
    }

    private static java.util.Map<String, Position> readPositions(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, Position> map = new java.util.LinkedHashMap<String, Position>();
        if (!java.nio.file.Files.exists(path)) {
            throw new java.io.FileNotFoundException("入力ファイルなし: " + path);
        }
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (isBlank(line) || isHeader(line, "CIF-NO")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            requireColumns(path, i + 1, c, 5);
            Position p = new Position(c.get(0), c.get(1), parseLong(c.get(2)), money(c.get(3)), money(c.get(4)));
            String key = key(p.cifNo, p.instrCode);
            Position old = map.get(key);
            if (old == null) {
                map.put(key, p);
            } else {
                long qty = old.netQty + p.netQty;
                java.math.BigDecimal avg = qty == 0
                        ? java.math.BigDecimal.ZERO
                        : old.avgAmt.multiply(java.math.BigDecimal.valueOf(old.netQty))
                        .add(p.avgAmt.multiply(java.math.BigDecimal.valueOf(p.netQty)))
                        .divide(java.math.BigDecimal.valueOf(qty), 0, java.math.RoundingMode.HALF_UP);
                map.put(key, new Position(p.cifNo, p.instrCode, qty, avg, old.rlzdAmt.add(p.rlzdAmt)));
            }
        }
        return map;
    }

    private static java.util.Map<String, Market> readMarkets(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, Market> map = new java.util.HashMap<String, Market>();
        if (!java.nio.file.Files.exists(path)) {
            throw new java.io.FileNotFoundException("入力ファイルなし: " + path);
        }
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (isBlank(line) || isHeader(line, "INSTR-CODE")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            requireColumns(path, i + 1, c, 6);
            Market m = new Market(c.get(0), moneyOrNull(c.get(1)), moneyOrNull(c.get(2)),
                    moneyOrNull(c.get(3)), parseLong(c.get(4)), c.get(5));
            Market old = map.get(m.instrCode);
            if (old == null || m.tickTs.compareTo(old.tickTs) >= 0) {
                map.put(m.instrCode, m);
            }
        }
        return map;
    }

    private static java.util.Map<String, MarkRow> readMarks(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, MarkRow> map = new java.util.LinkedHashMap<String, MarkRow>();
        if (!java.nio.file.Files.exists(path)) {
            return map;
        }
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (isBlank(line) || isHeader(line, "CIF-NO")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            requireColumns(path, i + 1, c, 7);
            MarkRow r = new MarkRow(c.get(0), c.get(1), c.get(2), parseLong(c.get(3)),
                    money(c.get(4)), money(c.get(5)), money(c.get(6)));
            map.put(key(r.cifNo, r.instrCode), r);
        }
        return map;
    }

    private static void writeMarks(java.nio.file.Path path, java.util.Collection<MarkRow> rows) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<String>();
        out.add("CIF-NO,INSTR-CODE,SESS-DT,NET-QTY,MARK-AMT,MARK-NOTIONAL-AMT,UNRLZD-AMT");
        for (MarkRow r : rows) {
            out.add(csv(r.cifNo) + "," + csv(r.instrCode) + "," + csv(r.sessDt) + "," + r.netQty + ","
                    + fmt(r.markAmt) + "," + fmt(r.markNotionalAmt) + "," + fmt(r.unrlzdAmt));
        }
        java.nio.file.Files.write(path, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static java.math.BigDecimal choosePrice(long netQty, Market m) {
        if (netQty > 0 && positive(m.bidAmt)) {
            return m.bidAmt;
        }
        if (netQty < 0 && positive(m.askAmt)) {
            return m.askAmt;
        }
        if (positive(m.lastAmt)) {
            return m.lastAmt;
        }
        if (positive(m.bidAmt) && positive(m.askAmt)) {
            return m.bidAmt.add(m.askAmt).divide(java.math.BigDecimal.valueOf(2), 0, java.math.RoundingMode.HALF_UP);
        }
        return positive(m.bidAmt) ? m.bidAmt : m.askAmt;
    }

    private static java.math.BigDecimal alignTick(java.math.BigDecimal price, long tick, long qty) {
        java.math.BigDecimal t = java.math.BigDecimal.valueOf(tick);
        java.math.BigDecimal[] div = price.divideAndRemainder(t);
        if (div[1].signum() == 0) {
            return price;
        }
        if (qty >= 0) {
            return div[0].multiply(t);
        }
        return div[0].add(java.math.BigDecimal.ONE).multiply(t);
    }

    private static long tickFor(String instrCode) {
        int tier = tierFor(instrCode);
        if (tier == 1) {
            return 100L;
        }
        if (tier == 2) {
            return 500L;
        }
        return 1000L;
    }

    private static int tierFor(String instrCode) {
        if (instrCode == null || instrCode.isEmpty()) {
            return 3;
        }
        char c = instrCode.charAt(instrCode.length() - 1);
        if (c >= '0' && c <= '3') {
            return 1;
        }
        if (c >= '4' && c <= '7') {
            return 2;
        }
        return 3;
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                out.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        out.add(b.toString().trim());
        return out;
    }

    private static String csv(String s) {
        if (s == null) {
            return "";
        }
        if (s.indexOf(',') < 0 && s.indexOf('"') < 0 && s.indexOf('\n') < 0 && s.indexOf('\r') < 0) {
            return s;
        }
        return "\"" + s.replace("\"", "\"\"") + "\"";
    }

    private static void requireColumns(java.nio.file.Path path, int line, java.util.List<String> c, int n) {
        if (c.size() < n) {
            throw new IllegalArgumentException("列数不正: " + path + ":" + line);
        }
    }

    private static boolean isHeader(String line, String firstName) {
        return !isBlank(line) && splitCsv(line).get(0).equalsIgnoreCase(firstName);
    }

    private static boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }

    private static long parseLong(String s) {
        if (isBlank(s)) {
            return 0L;
        }
        return Long.parseLong(s.replace(",", "").trim());
    }

    private static java.math.BigDecimal money(String s) {
        java.math.BigDecimal v = moneyOrNull(s);
        return v == null ? java.math.BigDecimal.ZERO : v;
    }

    private static java.math.BigDecimal moneyOrNull(String s) {
        if (isBlank(s)) {
            return null;
        }
        return new java.math.BigDecimal(s.replace(",", "").trim());
    }

    private static boolean positive(java.math.BigDecimal v) {
        return v != null && v.signum() > 0;
    }

    private static String fmt(java.math.BigDecimal v) {
        return v.setScale(0, java.math.RoundingMode.HALF_UP).toPlainString();
    }

    private static String key(String cifNo, String instrCode) {
        return cifNo + '\u0001' + instrCode;
    }

    private static final class Position {
        final String cifNo;
        final String instrCode;
        final long netQty;
        final java.math.BigDecimal avgAmt;
        final java.math.BigDecimal rlzdAmt;

        Position(String cifNo, String instrCode, long netQty, java.math.BigDecimal avgAmt, java.math.BigDecimal rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    private static final class Market {
        final String instrCode;
        final java.math.BigDecimal bidAmt;
        final java.math.BigDecimal askAmt;
        final java.math.BigDecimal lastAmt;
        final long volQty;
        final String tickTs;

        Market(String instrCode, java.math.BigDecimal bidAmt, java.math.BigDecimal askAmt,
               java.math.BigDecimal lastAmt, long volQty, String tickTs) {
            this.instrCode = instrCode;
            this.bidAmt = bidAmt;
            this.askAmt = askAmt;
            this.lastAmt = lastAmt;
            this.volQty = volQty;
            this.tickTs = tickTs;
        }
    }

    private static final class MarkRow {
        final String cifNo;
        final String instrCode;
        String sessDt;
        long netQty;
        java.math.BigDecimal markAmt;
        java.math.BigDecimal markNotionalAmt;
        java.math.BigDecimal unrlzdAmt;

        MarkRow(String cifNo, String instrCode, String sessDt, long netQty,
                java.math.BigDecimal markAmt, java.math.BigDecimal markNotionalAmt,
                java.math.BigDecimal unrlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sessDt = sessDt;
            this.netQty = netQty;
            this.markAmt = markAmt;
            this.markNotionalAmt = markNotionalAmt;
            this.unrlzdAmt = unrlzdAmt;
        }
    }
}
