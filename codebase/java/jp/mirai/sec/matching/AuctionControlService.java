package jp.mirai.sec.matching;

public class AuctionControlService {
    /**
     * 変更履歴
     * 版数  年月日      担当        概要
     * 1.00  2024-07-09  小林 直樹 (E-252)    SCAUCT候補のセッション・銘柄適格性確認を追加
     */

    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final String AUCTION_OPEN = "O";
    private static final String AUCTION_CLOSE = "C";
    private static final String SIDE_BUY = "B";
    private static final String SIDE_SELL = "S";

    public static void main(String[] a) throws Exception {
        if (a.length != 4) {
            System.err.println("引数不正: SCAUCT入力 SCBOOK入力 SCCALF入力 SCAUCT出力");
            System.exit(2);
        }

        java.util.Map<String, CalendarWindow> calendar = readCalendar(a[2]);
        java.util.Map<String, BookState> books = readBook(a[1]);
        java.util.List<AuctionCandidate> candidates = readAuction(a[0]);

        java.util.List<AuctionCandidate> usable = new java.util.ArrayList<AuctionCandidate>();
        for (AuctionCandidate c : candidates) {
            BookState book = books.get(c.instrCode);
            CalendarWindow window = calendar.get(c.auctionKbn);
            if (isUsable(c, book, window)) {
                usable.add(c);
            }
        }

        writeAuction(a[3], usable);
    }

    private static boolean isUsable(AuctionCandidate c, BookState book, CalendarWindow window) {
        if (c.instrCode.length() == 0 || c.crossAmt.signum() <= 0) {
            return false;
        }
        if (!AUCTION_OPEN.equals(c.auctionKbn) && !AUCTION_CLOSE.equals(c.auctionKbn)) {
            return false;
        }
        if (window == null || !window.contains(c.calcTs)) {
            return false;
        }
        if (book == null || !book.hasBothSides()) {
            return false;
        }
        if (c.matchQty <= 0 || c.imbalQty < 0) {
            return false;
        }

        TierRule tier = tierOf(c.instrCode);
        if (!isTickAligned(c.crossAmt, tier.tick)) {
            return false;
        }

        java.math.BigDecimal notional = c.crossAmt.multiply(java.math.BigDecimal.valueOf(c.matchQty));
        if (notional.compareTo(java.math.BigDecimal.valueOf(MIHFT_MAX_NOTIONAL)) > 0) {
            return false;
        }

        java.math.BigDecimal buyLimit = book.bestBuyPrice;
        java.math.BigDecimal sellLimit = book.bestSellPrice;
        if (buyLimit == null || sellLimit == null) {
            return false;
        }
        return c.crossAmt.compareTo(sellLimit) >= 0 && c.crossAmt.compareTo(buyLimit) <= 0;
    }

    private static java.util.List<AuctionCandidate> readAuction(String path) throws java.io.IOException {
        java.util.List<AuctionCandidate> rows = new java.util.ArrayList<AuctionCandidate>();
        java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        try {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().length() == 0) {
                    continue;
                }
                String[] f = splitCsv(line);
                if (first && isHeader(f, "INSTR-CODE")) {
                    first = false;
                    continue;
                }
                first = false;
                rows.add(new AuctionCandidate(
                        val(f, 0),
                        val(f, 1),
                        decimal(val(f, 2)),
                        longValue(val(f, 3)),
                        longValue(val(f, 4)),
                        time(val(f, 5))));
            }
        } finally {
            br.close();
        }
        return rows;
    }

    private static java.util.Map<String, BookState> readBook(String path) throws java.io.IOException {
        java.util.Map<String, BookState> map = new java.util.HashMap<String, BookState>();
        java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        try {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().length() == 0) {
                    continue;
                }
                String[] f = splitCsv(line);
                if (first && isHeader(f, "INSTR-CODE")) {
                    first = false;
                    continue;
                }
                first = false;

                String instr = val(f, 0);
                String side = val(f, 1);
                int level = intValue(val(f, 2));
                java.math.BigDecimal price = decimal(val(f, 3));
                long qty = longValue(val(f, 4));
                int orders = intValue(val(f, 5));

                if (level <= 0 || qty <= 0 || orders <= 0 || price.signum() <= 0) {
                    continue;
                }

                BookState b = map.get(instr);
                if (b == null) {
                    b = new BookState();
                    map.put(instr, b);
                }
                b.add(side, price, qty);
            }
        } finally {
            br.close();
        }
        return map;
    }

    private static java.util.Map<String, CalendarWindow> readCalendar(String path) throws java.io.IOException {
        java.util.Map<String, CalendarWindow> map = new java.util.HashMap<String, CalendarWindow>();
        java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        try {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().length() == 0) {
                    continue;
                }
                String[] f = splitCsv(line);
                if (first && isHeader(f, "SESS-DT")) {
                    first = false;
                    continue;
                }
                first = false;

                String sessKbn = val(f, 1);
                map.put(sessKbn, new CalendarWindow(time(val(f, 2)), time(val(f, 3))));
            }
        } finally {
            br.close();
        }
        return map;
    }

    private static void writeAuction(String path, java.util.List<AuctionCandidate> rows) throws java.io.IOException {
        java.io.BufferedWriter bw = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        try {
            bw.write("INSTR-CODE,AUCTION-KBN,CROSS-AMT,IMBAL-QTY,MATCH-QTY,CALC-TS");
            bw.newLine();
            for (AuctionCandidate r : rows) {
                bw.write(escape(r.instrCode));
                bw.write(',');
                bw.write(escape(r.auctionKbn));
                bw.write(',');
                bw.write(r.crossAmt.toPlainString());
                bw.write(',');
                bw.write(Long.toString(r.imbalQty));
                bw.write(',');
                bw.write(Long.toString(r.matchQty));
                bw.write(',');
                bw.write(r.calcTs.toString());
                bw.newLine();
            }
        } finally {
            bw.close();
        }
    }

    private static TierRule tierOf(String instrCode) {
        char c = instrCode.length() == 0 ? '9' : instrCode.charAt(instrCode.length() - 1);
        if (c >= '0' && c <= '3') {
            return new TierRule(1, 1000, 100);
        }
        if (c >= '4' && c <= '7') {
            return new TierRule(2, 2000, 500);
        }
        return new TierRule(3, 4000, 1000);
    }

    private static boolean isTickAligned(java.math.BigDecimal price, int tick) {
        java.math.BigDecimal[] div = price.remainder(java.math.BigDecimal.valueOf(tick))
                .divideAndRemainder(java.math.BigDecimal.ONE);
        return div[0].signum() == 0 && div[1].signum() == 0;
    }

    private static String[] splitCsv(String line) {
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
        return out.toArray(new String[out.size()]);
    }

    private static String escape(String s) {
        if (s.indexOf(',') < 0 && s.indexOf('"') < 0 && s.indexOf('\n') < 0 && s.indexOf('\r') < 0) {
            return s;
        }
        return "\"" + s.replace("\"", "\"\"") + "\"";
    }

    private static boolean isHeader(String[] f, String firstName) {
        return f.length > 0 && firstName.equalsIgnoreCase(val(f, 0));
    }

    private static String val(String[] f, int i) {
        return i < f.length ? f[i].trim() : "";
    }

    private static java.math.BigDecimal decimal(String s) {
        return new java.math.BigDecimal(s.length() == 0 ? "0" : s);
    }

    private static long longValue(String s) {
        return s.length() == 0 ? 0L : Long.parseLong(s);
    }

    private static int intValue(String s) {
        return s.length() == 0 ? 0 : Integer.parseInt(s);
    }

    private static java.time.LocalDateTime time(String s) {
        if (s.indexOf('T') >= 0) {
            return java.time.LocalDateTime.parse(s);
        }
        return java.time.LocalDateTime.parse(s, java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
    }

    private static final class AuctionCandidate {
        final String instrCode;
        final String auctionKbn;
        final java.math.BigDecimal crossAmt;
        final long imbalQty;
        final long matchQty;
        final java.time.LocalDateTime calcTs;

        AuctionCandidate(String instrCode, String auctionKbn, java.math.BigDecimal crossAmt,
                         long imbalQty, long matchQty, java.time.LocalDateTime calcTs) {
            this.instrCode = instrCode;
            this.auctionKbn = auctionKbn;
            this.crossAmt = crossAmt;
            this.imbalQty = imbalQty;
            this.matchQty = matchQty;
            this.calcTs = calcTs;
        }
    }

    private static final class BookState {
        java.math.BigDecimal bestBuyPrice;
        java.math.BigDecimal bestSellPrice;
        long buyQty;
        long sellQty;

        void add(String side, java.math.BigDecimal price, long qty) {
            if (SIDE_BUY.equals(side)) {
                buyQty += qty;
                if (bestBuyPrice == null || price.compareTo(bestBuyPrice) > 0) {
                    bestBuyPrice = price;
                }
            } else if (SIDE_SELL.equals(side)) {
                sellQty += qty;
                if (bestSellPrice == null || price.compareTo(bestSellPrice) < 0) {
                    bestSellPrice = price;
                }
            }
        }

        boolean hasBothSides() {
            return buyQty > 0 && sellQty > 0;
        }
    }

    private static final class CalendarWindow {
        final java.time.LocalDateTime openTs;
        final java.time.LocalDateTime closeTs;

        CalendarWindow(java.time.LocalDateTime openTs, java.time.LocalDateTime closeTs) {
            this.openTs = openTs;
            this.closeTs = closeTs;
        }

        boolean contains(java.time.LocalDateTime ts) {
            return !ts.isBefore(openTs) && !ts.isAfter(closeTs);
        }
    }

    private static final class TierRule {
        final int tier;
        final int marginRateBp;
        final int tick;

        TierRule(int tier, int marginRateBp, int tick) {
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tick = tick;
        }
    }
}
