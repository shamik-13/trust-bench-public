/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2022-02-22  今井 彩 (E-230)    初版作成
 */

package jp.mirai.sec.orderbook;

public class MarketDataReplayService {
    private static final java.nio.charset.Charset CSV_CHARSET = java.nio.charset.StandardCharsets.UTF_8;
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final String DETAIL_BAD_CSV = "MD001";
    private static final String DETAIL_NO_INST = "MD002";
    private static final String DETAIL_NO_SESSION = "MD003";
    private static final String DETAIL_TS_REVERSE = "MD004";

    private static long auditSeq = 0L;

    private MarketDataReplayService() {
    }

    public static void main(String[] a) throws Exception {
        if (a.length != 5) {
            System.err.println("使用方法: java MarketDataReplayService SCMKTD入力 SCINSTF入力 SCCALF入力 SCMKTD出力 SCAUDF出力");
            return;
        }

        java.nio.file.Path marketIn = java.nio.file.Paths.get(a[0]);
        java.nio.file.Path instIn = java.nio.file.Paths.get(a[1]);
        java.nio.file.Path calIn = java.nio.file.Paths.get(a[2]);
        java.nio.file.Path marketOut = java.nio.file.Paths.get(a[3]);
        java.nio.file.Path auditOut = java.nio.file.Paths.get(a[4]);

        java.util.Map<String, Instrument> instruments = readInstruments(instIn);
        java.util.List<SessionWindow> sessions = readSessions(calIn);
        java.util.List<MarketRow> rows = readMarketRows(marketIn);

        rows.sort(new java.util.Comparator<MarketRow>() {
            public int compare(MarketRow l, MarketRow r) {
                int ts = l.tickTs.compareTo(r.tickTs);
                if (ts != 0) {
                    return ts;
                }
                return Integer.compare(l.lineNo, r.lineNo);
            }
        });

        java.util.Map<String, java.time.LocalDateTime> lastTsByInstrument = new java.util.HashMap<>();
        int writeCount = 0;
        int skipCount = 0;

        try (java.io.BufferedWriter marketWriter = java.nio.file.Files.newBufferedWriter(marketOut, CSV_CHARSET);
             java.io.BufferedWriter auditWriter = java.nio.file.Files.newBufferedWriter(auditOut, CSV_CHARSET)) {
            marketWriter.write("INSTR-CODE,BID-AMT,ASK-AMT,LAST-AMT,VOL-QTY,TICK-TS");
            marketWriter.newLine();
            auditWriter.write("AUDIT-ID,ORDER-ID,EVENT-KBN,CIF-NO,INSTR-CODE,EVENT-TS,DETAIL-CD");
            auditWriter.newLine();

            for (MarketRow row : rows) {
                String detail = validate(row, instruments, sessions, lastTsByInstrument);
                if (detail != null) {
                    writeAudit(auditWriter, row, detail);
                    skipCount++;
                    continue;
                }

                lastTsByInstrument.put(row.instrCode, row.tickTs);
                marketWriter.write(row.toCsv());
                marketWriter.newLine();
                writeCount++;
            }
        }

        System.err.println("リプレイ完了: 出力件数=" + writeCount + " スキップ件数=" + skipCount);
    }

    private static java.util.Map<String, Instrument> readInstruments(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, Instrument> map = new java.util.HashMap<>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = parseCsvLine(lines.get(i));
            if (c.size() < 6) {
                continue;
            }
            String code = c.get(0).trim();
            if (code.isEmpty()) {
                continue;
            }
            int tier = parseInt(c.get(2).trim(), 0);
            long tick = parseLong(c.get(3).trim(), 0L);
            long lot = parseLong(c.get(4).trim(), 0L);
            String board = c.get(5).trim();
            if (isKnownTier(tier) && isKnownBoard(board) && tick > 0L && lot > 0L) {
                map.put(code, new Instrument(code, tier, tick, lot, board));
            }
        }
        return map;
    }

    private static java.util.List<SessionWindow> readSessions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<SessionWindow> list = new java.util.ArrayList<>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = parseCsvLine(lines.get(i));
            if (c.size() < 4) {
                continue;
            }
            try {
                java.time.LocalDate d = java.time.LocalDate.parse(c.get(0).trim());
                java.time.LocalDateTime open = parseSessionTs(d, c.get(2).trim());
                java.time.LocalDateTime close = parseSessionTs(d, c.get(3).trim());
                if (!close.isBefore(open)) {
                    list.add(new SessionWindow(c.get(1).trim(), open, close));
                }
            } catch (RuntimeException ignored) {
                System.err.println("立会日カレンダ読込スキップ: 行=" + (i + 1));
            }
        }
        list.sort(new java.util.Comparator<SessionWindow>() {
            public int compare(SessionWindow l, SessionWindow r) {
                return l.openTs.compareTo(r.openTs);
            }
        });
        return list;
    }

    private static java.util.List<MarketRow> readMarketRows(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<MarketRow> list = new java.util.ArrayList<>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = parseCsvLine(lines.get(i));
            if (c.size() < 6) {
                list.add(MarketRow.bad(i + 1, lines.get(i)));
                continue;
            }
            try {
                list.add(new MarketRow(
                        i + 1,
                        c.get(0).trim(),
                        new java.math.BigDecimal(c.get(1).trim()),
                        new java.math.BigDecimal(c.get(2).trim()),
                        new java.math.BigDecimal(c.get(3).trim()),
                        Long.parseLong(c.get(4).trim()),
                        parseTickTs(c.get(5).trim()),
                        c));
            } catch (RuntimeException ex) {
                list.add(MarketRow.bad(i + 1, lines.get(i)));
            }
        }
        return list;
    }

    private static String validate(MarketRow row,
                                   java.util.Map<String, Instrument> instruments,
                                   java.util.List<SessionWindow> sessions,
                                   java.util.Map<String, java.time.LocalDateTime> lastTsByInstrument) {
        if (row.badRecord) {
            return DETAIL_BAD_CSV;
        }

        Instrument inst = instruments.get(row.instrCode);
        if (inst == null) {
            return DETAIL_NO_INST;
        }

        if (!inSession(row.tickTs, sessions)) {
            return DETAIL_NO_SESSION;
        }

        java.time.LocalDateTime lastTs = lastTsByInstrument.get(row.instrCode);
        if (lastTs != null && row.tickTs.isBefore(lastTs)) {
            return DETAIL_TS_REVERSE;
        }

        return null;
    }

    private static boolean inSession(java.time.LocalDateTime ts, java.util.List<SessionWindow> sessions) {
        for (SessionWindow s : sessions) {
            if (!ts.isBefore(s.openTs) && !ts.isAfter(s.closeTs)) {
                return true;
            }
        }
        return false;
    }

    private static void writeAudit(java.io.BufferedWriter w, MarketRow row, String detail) throws java.io.IOException {
        auditSeq++;
        String eventTs = java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME);
        String instr = row.instrCode == null ? "" : row.instrCode;
        w.write(csv("AUD" + String.format("%012d", auditSeq)));
        w.write(",,");
        w.write(csv("MKT-SKIP"));
        w.write(",");
        w.write(",");
        w.write(csv(instr));
        w.write(",");
        w.write(csv(eventTs));
        w.write(",");
        w.write(csv(detail));
        w.newLine();
    }

    private static java.time.LocalDateTime parseTickTs(String s) {
        String v = s.trim();
        if (v.length() == 14 && isDigits(v)) {
            return java.time.LocalDateTime.parse(v, java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
        }
        return java.time.LocalDateTime.parse(v);
    }

    private static java.time.LocalDateTime parseSessionTs(java.time.LocalDate d, String s) {
        String v = s.trim();
        if (v.length() == 14 && isDigits(v)) {
            return java.time.LocalDateTime.parse(v, java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
        }
        if (v.length() == 8 && isDigits(v)) {
            return java.time.LocalDateTime.of(d, java.time.LocalTime.parse(v, java.time.format.DateTimeFormatter.ofPattern("HHmmss")));
        }
        if (v.length() == 5 || v.length() == 8) {
            return java.time.LocalDateTime.of(d, java.time.LocalTime.parse(v));
        }
        return java.time.LocalDateTime.parse(v);
    }

    private static boolean isKnownTier(int tier) {
        return tier == 1 || tier == 2 || tier == 3;
    }

    private static boolean isKnownBoard(String board) {
        return "T1".equals(board) || "ST".equals(board) || "ETF".equals(board);
    }

    private static int parseInt(String s, int fallback) {
        try {
            return Integer.parseInt(s);
        } catch (RuntimeException ex) {
            return fallback;
        }
    }

    private static long parseLong(String s, long fallback) {
        try {
            return Long.parseLong(s);
        } catch (RuntimeException ex) {
            return fallback;
        }
    }

    private static boolean isDigits(String v) {
        for (int i = 0; i < v.length(); i++) {
            if (!Character.isDigit(v.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
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

    private static String csv(String v) {
        if (v == null) {
            return "";
        }
        boolean quote = v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0;
        if (!quote) {
            return v;
        }
        return "\"" + v.replace("\"", "\"\"") + "\"";
    }

    private static final class Instrument {
        final String instrCode;
        final int tier;
        final long tickAmount;
        final long lotQty;
        final String boardCode;

        Instrument(String instrCode, int tier, long tickAmount, long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.tier = tier;
            this.tickAmount = tickAmount;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class SessionWindow {
        final String sessKbn;
        final java.time.LocalDateTime openTs;
        final java.time.LocalDateTime closeTs;

        SessionWindow(String sessKbn, java.time.LocalDateTime openTs, java.time.LocalDateTime closeTs) {
            this.sessKbn = sessKbn;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }

    private static final class MarketRow {
        final int lineNo;
        final String instrCode;
        final java.math.BigDecimal bidAmount;
        final java.math.BigDecimal askAmount;
        final java.math.BigDecimal lastAmount;
        final long volumeQty;
        final java.time.LocalDateTime tickTs;
        final java.util.List<String> original;
        final boolean badRecord;

        MarketRow(int lineNo,
                  String instrCode,
                  java.math.BigDecimal bidAmount,
                  java.math.BigDecimal askAmount,
                  java.math.BigDecimal lastAmount,
                  long volumeQty,
                  java.time.LocalDateTime tickTs,
                  java.util.List<String> original) {
            this.lineNo = lineNo;
            this.instrCode = instrCode;
            this.bidAmount = bidAmount;
            this.askAmount = askAmount;
            this.lastAmount = lastAmount;
            this.volumeQty = volumeQty;
            this.tickTs = tickTs;
            this.original = new java.util.ArrayList<>(original);
            this.badRecord = false;
        }

        static MarketRow bad(int lineNo, String raw) {
            java.util.List<String> original = new java.util.ArrayList<>();
            original.add(raw);
            return new MarketRow(lineNo, "", java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO,
                    java.math.BigDecimal.ZERO, 0L, java.time.LocalDateTime.MAX, original, true);
        }

        private MarketRow(int lineNo,
                          String instrCode,
                          java.math.BigDecimal bidAmount,
                          java.math.BigDecimal askAmount,
                          java.math.BigDecimal lastAmount,
                          long volumeQty,
                          java.time.LocalDateTime tickTs,
                          java.util.List<String> original,
                          boolean badRecord) {
            this.lineNo = lineNo;
            this.instrCode = instrCode;
            this.bidAmount = bidAmount;
            this.askAmount = askAmount;
            this.lastAmount = lastAmount;
            this.volumeQty = volumeQty;
            this.tickTs = tickTs;
            this.original = original;
            this.badRecord = badRecord;
        }

        String toCsv() {
            StringBuilder b = new StringBuilder();
            for (int i = 0; i < 6; i++) {
                if (i > 0) {
                    b.append(',');
                }
                b.append(csv(original.get(i)));
            }
            return b.toString();
        }
    }
}
