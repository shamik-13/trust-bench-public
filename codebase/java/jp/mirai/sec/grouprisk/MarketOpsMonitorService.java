/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025-06-03  岡本 涼 (E-294)  初版作成
 */

package jp.mirai.sec.grouprisk;

public class MarketOpsMonitorService {
    private static final String SERVICE_ID = "MIHFT-MON";
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final long MARKET_STALE_MILLIS = 3000L;
    private static final long BOOK_STALL_MILLIS = 5000L;
    private static final long SESSION_STALL_MILLIS = 8000L;
    private static final long CLOCK_DRIFT_MILLIS = 60000L;

    private static final java.time.ZoneId ZONE = java.time.ZoneId.of("Asia/Tokyo");
    private static final java.nio.charset.Charset CSV_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    public static void main(String[] a) throws Exception {
        java.nio.file.Path base = a.length == 0
                ? java.nio.file.Paths.get(".")
                : java.nio.file.Paths.get(a[0]);

        java.nio.file.Files.createDirectories(base);

        java.nio.file.Path marketPath = base.resolve("SCMKTD.csv");
        java.nio.file.Path bookPath = base.resolve("SCBOOK.csv");
        java.nio.file.Path sessionPath = base.resolve("SCSESSF.csv");
        java.nio.file.Path killPath = base.resolve("SCKILLF.csv");
        java.nio.file.Path auditPath = base.resolve("SCAUDTF.csv");

        if (!java.nio.file.Files.exists(marketPath)
                || !java.nio.file.Files.exists(bookPath)
                || !java.nio.file.Files.exists(sessionPath)
                || !java.nio.file.Files.exists(killPath)) {
            writeSyntheticBenchmark(base);
        }

        ClockSource clock = new ClockSource();
        RefDataService refData = new RefDataService();
        AuditSink audit = new AuditSink(auditPath);

        java.time.Instant now = clock.now();
        java.time.LocalDate businessDate = java.time.ZonedDateTime.ofInstant(now, ZONE).toLocalDate();

        java.util.Map<String, MarketTick> market = readMarket(marketPath);
        java.util.Map<String, BookSnapshot> books = readBooks(bookPath);
        java.util.List<SessionState> sessions = readSessions(sessionPath);
        java.util.List<KillSwitchState> kills = readKillSwitches(killPath);

        if (!refData.isTradingTime(businessDate, now)) {
            return;
        }

        java.util.Set<String> instruments = new java.util.TreeSet<String>();
        instruments.addAll(market.keySet());
        instruments.addAll(books.keySet());
        for (KillSwitchState k : kills) {
            if (k.instrCode.length() > 0) {
                instruments.add(k.instrCode);
            }
        }

        for (String instr : instruments) {
            MarketTick tick = market.get(instr);
            BookSnapshot book = books.get(instr);

            if (tick == null) {
                audit.write(now, instr, "MKT", "相場未受信");
            } else {
                long age = java.time.Duration.between(tick.tickTs, now).toMillis();
                if (age > MARKET_STALE_MILLIS) {
                    audit.write(now, instr, "MKT", "相場鮮度超過");
                }
                if (Math.abs(age) > CLOCK_DRIFT_MILLIS) {
                    audit.write(now, instr, "CLK", "相場時刻乖離");
                }
                if (tick.bidAmt <= 0 || tick.askAmt <= 0 || tick.lastAmt <= 0 || tick.volQty < 0) {
                    audit.write(now, instr, "MKT", "相場値不正");
                }
                if (tick.bidAmt > tick.askAmt) {
                    audit.write(now, instr, "MKT", "気配逆転");
                }
                if (tick.lastAmt * Math.max(1L, tick.volQty) > MIHFT_MAX_NOTIONAL) {
                    audit.write(now, instr, "MKT", "想定元本超過");
                }
                int tier = refData.instrumentTier(instr);
                long tickSize = refData.tickSize(tier);
                if (tick.lastAmt % tickSize != 0 || tick.bidAmt % tickSize != 0 || tick.askAmt % tickSize != 0) {
                    audit.write(now, instr, "MKT", "呼値不整合");
                }
            }

            if (book == null) {
                audit.write(now, instr, "BOK", "板未受信");
            } else {
                long age = java.time.Duration.between(book.latestEntryTs, now).toMillis();
                if (age > BOOK_STALL_MILLIS) {
                    audit.write(now, instr, "BOK", "板更新停滞");
                }
                if (book.bidQty <= 0 || book.askQty <= 0) {
                    audit.write(now, instr, "BOK", "片側板不足");
                }
                if (book.bidLevels <= 0 || book.askLevels <= 0) {
                    audit.write(now, instr, "BOK", "板階層不足");
                }
                if (book.bidOrders < 0 || book.askOrders < 0) {
                    audit.write(now, instr, "BOK", "注文件数不正");
                }
                if (book.bestBid > 0 && book.bestAsk > 0 && book.bestBid >= book.bestAsk) {
                    audit.write(now, instr, "BOK", "板価格交差");
                }
            }
        }

        for (SessionState s : sessions) {
            String objectId = s.sessKey;
            if (!refData.isBoardOpen(s.boardCode, businessDate, now)) {
                continue;
            }
            long age = java.time.Duration.between(s.updatedTs, now).toMillis();
            if (!"A".equals(s.stateKbn)) {
                audit.write(now, objectId, "SES", "セッション非稼働");
            }
            if (s.lastSeqNo <= 0) {
                audit.write(now, objectId, "SEQ", "シーケンス未進捗");
            }
            if (age > SESSION_STALL_MILLIS) {
                audit.write(now, objectId, "SEQ", "シーケンス停滞");
            }
            if (s.sessDt != null && !s.sessDt.equals(businessDate)) {
                audit.write(now, objectId, "SES", "営業日不一致");
            }
        }

        for (KillSwitchState k : kills) {
            if (!"Y".equals(k.activeFlg)) {
                continue;
            }
            String objectId = k.scopeKbn + ":" + (k.instrCode.length() == 0 ? k.cifNo : k.instrCode);
            if ("ALL".equals(k.scopeKbn) || "I".equals(k.scopeKbn) || "C".equals(k.scopeKbn)) {
                audit.write(now, objectId, "KIL", k.reasonCode.length() == 0 ? "キルスイッチ有効" : k.reasonCode);
            } else {
                audit.write(now, objectId, "KIL", "キル範囲不正");
            }
        }
    }

    private static java.util.Map<String, MarketTick> readMarket(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, MarketTick> out = new java.util.HashMap<String, MarketTick>();
        for (CsvRow row : readCsv(path)) {
            MarketTick tick = new MarketTick(
                    row.get("INSTR-CODE"),
                    parseLong(row.get("BID-AMT")),
                    parseLong(row.get("ASK-AMT")),
                    parseLong(row.get("LAST-AMT")),
                    parseLong(row.get("VOL-QTY")),
                    parseInstant(row.get("TICK-TS")));
            out.put(tick.instrCode, tick);
        }
        return out;
    }

    private static java.util.Map<String, BookSnapshot> readBooks(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, BookSnapshot> out = new java.util.HashMap<String, BookSnapshot>();
        for (CsvRow row : readCsv(path)) {
            String instr = row.get("INSTR-CODE");
            String side = row.get("SIDE-KBN");
            BookSnapshot b = out.get(instr);
            if (b == null) {
                b = new BookSnapshot(instr);
                out.put(instr, b);
            }

            int levels = (int) parseLong(row.get("LEVEL-CNT"));
            long price = parseLong(row.get("PRICE-AMT"));
            long qty = parseLong(row.get("BOOK-QTY"));
            long orders = parseLong(row.get("ORDER-CNT"));
            java.time.Instant entryTs = parseInstant(row.get("ENTRY-TS"));

            if ("B".equals(side)) {
                b.bidLevels += levels;
                b.bidQty += qty;
                b.bidOrders += orders;
                if (price > b.bestBid) {
                    b.bestBid = price;
                }
            } else if ("S".equals(side)) {
                b.askLevels += levels;
                b.askQty += qty;
                b.askOrders += orders;
                if (b.bestAsk == 0 || price < b.bestAsk) {
                    b.bestAsk = price;
                }
            }
            if (b.latestEntryTs == null || entryTs.isAfter(b.latestEntryTs)) {
                b.latestEntryTs = entryTs;
            }
        }
        return out;
    }

    private static java.util.List<SessionState> readSessions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<SessionState> out = new java.util.ArrayList<SessionState>();
        for (CsvRow row : readCsv(path)) {
            out.add(new SessionState(
                    row.get("SESS-KEY"),
                    parseDate(row.get("SESS-DT")),
                    row.get("BOARD-CODE"),
                    row.get("STATE-KBN"),
                    parseLong(row.get("LAST-SEQ-NO")),
                    parseInstant(row.get("UPDATED-TS"))));
        }
        return out;
    }

    private static java.util.List<KillSwitchState> readKillSwitches(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<KillSwitchState> out = new java.util.ArrayList<KillSwitchState>();
        for (CsvRow row : readCsv(path)) {
            out.add(new KillSwitchState(
                    row.get("KILL-KEY"),
                    row.get("SCOPE-KBN"),
                    row.get("INSTR-CODE"),
                    row.get("CIF-NO"),
                    row.get("ACTIVE-FLG"),
                    row.get("REASON-CODE"),
                    parseInstant(row.get("UPDATED-TS"))));
        }
        return out;
    }

    private static java.util.List<CsvRow> readCsv(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        java.util.List<CsvRow> rows = new java.util.ArrayList<CsvRow>();
        if (lines.isEmpty()) {
            return rows;
        }

        java.util.List<String> header = splitCsv(lines.get(0));
        for (int i = 1; i < lines.size(); i++) {
            if (lines.get(i).trim().isEmpty()) {
                continue;
            }
            java.util.List<String> values = splitCsv(lines.get(i));
            java.util.Map<String, String> map = new java.util.HashMap<String, String>();
            for (int c = 0; c < header.size(); c++) {
                map.put(header.get(c), c < values.size() ? values.get(c) : "");
            }
            rows.add(new CsvRow(map));
        }
        return rows;
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder cur = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cur.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                out.add(cur.toString());
                cur.setLength(0);
            } else {
                cur.append(ch);
            }
        }
        out.add(cur.toString());
        return out;
    }

    private static long parseLong(String s) {
        if (s == null || s.trim().isEmpty()) {
            return 0L;
        }
        return Long.parseLong(s.trim());
    }

    private static java.time.Instant parseInstant(String s) {
        if (s == null || s.trim().isEmpty()) {
            return java.time.Instant.EPOCH;
        }
        String v = s.trim();
        if (v.endsWith("Z") || v.indexOf('+', 10) >= 0) {
            return java.time.Instant.parse(v);
        }
        return java.time.LocalDateTime.parse(v).atZone(ZONE).toInstant();
    }

    private static java.time.LocalDate parseDate(String s) {
        if (s == null || s.trim().isEmpty()) {
            return null;
        }
        return java.time.LocalDate.parse(s.trim());
    }

    private static String esc(String s) {
        if (s == null) {
            return "";
        }
        if (s.indexOf(',') < 0 && s.indexOf('"') < 0 && s.indexOf('\n') < 0 && s.indexOf('\r') < 0) {
            return s;
        }
        return "\"" + s.replace("\"", "\"\"") + "\"";
    }

    private static void writeSyntheticBenchmark(java.nio.file.Path base) throws java.io.IOException {
        java.time.Instant now = java.time.Instant.now();
        java.time.LocalDate today = java.time.ZonedDateTime.ofInstant(now, ZONE).toLocalDate();

        java.nio.file.Files.write(base.resolve("SCMKTD.csv"), java.util.Arrays.asList(
                "INSTR-CODE,BID-AMT,ASK-AMT,LAST-AMT,VOL-QTY,TICK-TS",
                "7203,321000,321500,321000,84000," + now.minusMillis(1200L),
                "6758,1845500,1846000,1846000,52000," + now.minusMillis(7400L),
                "9984,995000,994000,995000,91000," + now.minusMillis(1800L),
                "1306,318900,319000,318900,140000," + now.minusMillis(2100L)
        ), CSV_CHARSET);

        java.nio.file.Files.write(base.resolve("SCBOOK.csv"), java.util.Arrays.asList(
                "INSTR-CODE,SIDE-KBN,LEVEL-CNT,PRICE-AMT,BOOK-QTY,ORDER-CNT,ENTRY-TS",
                "7203,B,5,321000,18000,42," + now.minusMillis(1000L),
                "7203,S,5,321500,15400,39," + now.minusMillis(1000L),
                "6758,B,4,1845500,6400,18," + now.minusMillis(9200L),
                "6758,S,4,1846000,7100,21," + now.minusMillis(9200L),
                "9984,B,5,995000,9300,31," + now.minusMillis(1700L),
                "9984,S,5,994000,8700,27," + now.minusMillis(1700L),
                "1306,B,3,318900,42000,55," + now.minusMillis(2500L)
        ), CSV_CHARSET);

        java.nio.file.Files.write(base.resolve("SCSESSF.csv"), java.util.Arrays.asList(
                "SESS-KEY,SESS-DT,BOARD-CODE,STATE-KBN,LAST-SEQ-NO,UPDATED-TS",
                "T1-AM," + today + ",T1,A,184201," + now.minusMillis(2200L),
                "ST-AM," + today + ",ST,A,0," + now.minusMillis(13000L),
                "ETF-AM," + today + ",ETF,H,88201," + now.minusMillis(3000L)
        ), CSV_CHARSET);

        java.nio.file.Files.write(base.resolve("SCKILLF.csv"), java.util.Arrays.asList(
                "KILL-KEY,SCOPE-KBN,INSTR-CODE,CIF-NO,ACTIVE-FLG,REASON-CODE,UPDATED-TS",
                "K00001,I,6758,,Y,価格急変," + now.minusMillis(6000L),
                "K00002,C,,C100482,N,顧客停止解除," + now.minusMillis(9000L),
                "K00003,ALL,,,N,全体停止解除," + now.minusMillis(15000L)
        ), CSV_CHARSET);

        if (!java.nio.file.Files.exists(base.resolve("SCAUDTF.csv"))) {
            java.nio.file.Files.write(base.resolve("SCAUDTF.csv"), java.util.Collections.singletonList(
                    "AUDIT-ID,EVENT-TS,SERVICE-ID,OBJECT-ID,EVENT-KBN,DETAIL-CODE"
            ), CSV_CHARSET);
        }
    }

    private static final class CsvRow {
        private final java.util.Map<String, String> values;

        CsvRow(java.util.Map<String, String> values) {
            this.values = values;
        }

        String get(String key) {
            String v = values.get(key);
            return v == null ? "" : v.trim();
        }
    }

    private static final class MarketTick {
        final String instrCode;
        final long bidAmt;
        final long askAmt;
        final long lastAmt;
        final long volQty;
        final java.time.Instant tickTs;

        MarketTick(String instrCode, long bidAmt, long askAmt, long lastAmt, long volQty, java.time.Instant tickTs) {
            this.instrCode = instrCode;
            this.bidAmt = bidAmt;
            this.askAmt = askAmt;
            this.lastAmt = lastAmt;
            this.volQty = volQty;
            this.tickTs = tickTs;
        }
    }

    private static final class BookSnapshot {
        final String instrCode;
        int bidLevels;
        int askLevels;
        long bestBid;
        long bestAsk;
        long bidQty;
        long askQty;
        long bidOrders;
        long askOrders;
        java.time.Instant latestEntryTs;

        BookSnapshot(String instrCode) {
            this.instrCode = instrCode;
            this.latestEntryTs = java.time.Instant.EPOCH;
        }
    }

    private static final class SessionState {
        final String sessKey;
        final java.time.LocalDate sessDt;
        final String boardCode;
        final String stateKbn;
        final long lastSeqNo;
        final java.time.Instant updatedTs;

        SessionState(String sessKey, java.time.LocalDate sessDt, String boardCode, String stateKbn,
                     long lastSeqNo, java.time.Instant updatedTs) {
            this.sessKey = sessKey;
            this.sessDt = sessDt;
            this.boardCode = boardCode;
            this.stateKbn = stateKbn;
            this.lastSeqNo = lastSeqNo;
            this.updatedTs = updatedTs;
        }
    }

    private static final class KillSwitchState {
        final String killKey;
        final String scopeKbn;
        final String instrCode;
        final String cifNo;
        final String activeFlg;
        final String reasonCode;
        final java.time.Instant updatedTs;

        KillSwitchState(String killKey, String scopeKbn, String instrCode, String cifNo,
                        String activeFlg, String reasonCode, java.time.Instant updatedTs) {
            this.killKey = killKey;
            this.scopeKbn = scopeKbn;
            this.instrCode = instrCode;
            this.cifNo = cifNo;
            this.activeFlg = activeFlg;
            this.reasonCode = reasonCode;
            this.updatedTs = updatedTs;
        }
    }

    private static final class AuditSink {
        private final java.nio.file.Path path;

        AuditSink(java.nio.file.Path path) throws java.io.IOException {
            this.path = path;
            if (!java.nio.file.Files.exists(path)) {
                java.nio.file.Files.write(path, java.util.Collections.singletonList(
                        "AUDIT-ID,EVENT-TS,SERVICE-ID,OBJECT-ID,EVENT-KBN,DETAIL-CODE"
                ), CSV_CHARSET);
            }
        }

        void write(java.time.Instant eventTs, String objectId, String eventKbn, String detailCode) throws java.io.IOException {
            String id = java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS")
                    .withZone(ZONE)
                    .format(eventTs) + "-" + Math.abs((objectId + eventKbn + detailCode).hashCode());
            String line = esc(id) + ","
                    + esc(eventTs.toString()) + ","
                    + esc(SERVICE_ID) + ","
                    + esc(objectId) + ","
                    + esc(eventKbn) + ","
                    + esc(detailCode);
            java.nio.file.Files.write(path, java.util.Collections.singletonList(line), CSV_CHARSET,
                    java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND);
        }
    }

    private static final class RefDataService {
        boolean isTradingTime(java.time.LocalDate date, java.time.Instant instant) {
            if (isHoliday(date)) {
                return false;
            }
            java.time.LocalTime t = java.time.ZonedDateTime.ofInstant(instant, ZONE).toLocalTime();
            return !t.isBefore(java.time.LocalTime.of(9, 0))
                    && t.isBefore(java.time.LocalTime.of(15, 30))
                    && (t.isBefore(java.time.LocalTime.of(11, 30)) || !t.isBefore(java.time.LocalTime.of(12, 30)));
        }

        boolean isBoardOpen(String boardCode, java.time.LocalDate date, java.time.Instant instant) {
            if (!"T1".equals(boardCode) && !"ST".equals(boardCode) && !"ETF".equals(boardCode)) {
                return false;
            }
            return isTradingTime(date, instant);
        }

        int instrumentTier(String instrCode) {
            if (instrCode == null || instrCode.length() == 0) {
                return 3;
            }
            if (instrCode.startsWith("13") || instrCode.startsWith("72")) {
                return 1;
            }
            if (instrCode.startsWith("67") || instrCode.startsWith("99")) {
                return 2;
            }
            return 3;
        }

        long tickSize(int tier) {
            if (tier == 1) {
                return 100L;
            }
            if (tier == 2) {
                return 500L;
            }
            return 1000L;
        }

        @SuppressWarnings("unused")
        int marginRateBp(int tier) {
            if (tier == 1) {
                return 1000;
            }
            if (tier == 2) {
                return 2000;
            }
            return 4000;
        }

        private boolean isHoliday(java.time.LocalDate date) {
            java.time.DayOfWeek d = date.getDayOfWeek();
            if (d == java.time.DayOfWeek.SATURDAY || d == java.time.DayOfWeek.SUNDAY) {
                return true;
            }
            java.util.Set<java.time.LocalDate> closed = new java.util.HashSet<java.time.LocalDate>();
            closed.add(java.time.LocalDate.of(date.getYear(), 1, 1));
            closed.add(java.time.LocalDate.of(date.getYear(), 1, 2));
            closed.add(java.time.LocalDate.of(date.getYear(), 1, 3));
            closed.add(java.time.LocalDate.of(date.getYear(), 12, 31));
            return closed.contains(date);
        }
    }

    private static final class ClockSource {
        java.time.Instant now() {
            return java.time.Instant.now();
        }
    }
}
