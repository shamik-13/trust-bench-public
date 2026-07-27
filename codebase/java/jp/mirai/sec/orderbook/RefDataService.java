/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2023/11/14  今井 彩 (E-230)    初版作成。参照データ統合サービスの事前判定用スナップショット生成。
 */

package jp.mirai.sec.orderbook;

public class RefDataService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final String SCINSTF =
            "INSTR-CODE,INSTR-NAME,INSTR-TIER,TICK-AMT,LOT-QTY,BOARD-CODE\n" +
            "13010,極東鉄鋼,1,100,100,T1\n" +
            "28020,東都食品,2,500,100,T1\n" +
            "43880,桜クラウド,3,1000,100,ST\n" +
            "72030,瑞穂自動車,1,100,100,T1\n" +
            "99840,日輪通信,1,100,100,T1\n" +
            "13450,日経高配当ETF,2,500,10,ETF\n";

    private static final String SCFEEF =
            "BOARD-CODE,FEE-RATE,MIN-FEE-AMT\n" +
            "T1,25,80\n" +
            "ST,35,120\n" +
            "ETF,12,50\n";

    private static final String SCCALF =
            "SESS-DT,SESS-KBN,OPEN-TS,CLOSE-TS\n" +
            "20250115,AM,2025-01-15T09:00:00+09:00,2025-01-15T11:30:00+09:00\n" +
            "20250115,PM,2025-01-15T12:30:00+09:00,2025-01-15T15:30:00+09:00\n" +
            "20250115,AM,2025-01-15T09:00:00+09:00,2025-01-15T11:30:00+09:00\n" +
            "20250115,PM,2025-01-15T12:30:00+09:00,2025-01-15T15:30:00+09:00\n";

    private static final String SCVENF =
            "VENUE-CODE,BOARD-CODE,LATENCY-US,FEE-BPS,ENABLED-KBN,CAPACITY-QTY\n" +
            "XTYO,T1,180,2,Y,2000000\n" +
            "XJNX,T1,240,3,Y,1500000\n" +
            "XMTH,ST,310,5,Y,500000\n" +
            "XETF,ETF,210,1,Y,750000\n" +
            "XDLY,T1,1500,9,N,100000\n";

    private static final String SCBAND =
            "INSTR-CODE,LOWER-AMT,UPPER-AMT,BAND-TS,SOURCE-KBN\n" +
            "13010,310000,460000,2025-01-15T16:45:00+09:00,T\n" +
            "28020,180000,260000,2025-01-15T16:45:00+09:00,T\n" +
            "43880,92000,138000,2025-01-15T16:45:00+09:00,S\n" +
            "72030,248000,372000,2025-01-15T16:45:00+09:00,T\n" +
            "99840,910000,1360000,2025-01-15T16:45:00+09:00,T\n" +
            "13450,201000,303000,2025-01-15T16:45:00+09:00,E\n";

    public static void main(String[] a) {
        Snapshot snapshot = loadSnapshot();
        long enabledCapacity = 0L;
        for (BoardSnapshot board : snapshot.boards.values()) {
            enabledCapacity += board.enabledCapacityQty;
        }
        System.out.println("参照データ読込完了 銘柄数=" + snapshot.instruments.size()
                + " ボード数=" + snapshot.boards.size()
                + " 有効会場容量=" + enabledCapacity
                + " 最大想定元本=" + MIHFT_MAX_NOTIONAL);
    }

    private static Snapshot loadSnapshot() {
        java.util.Map<String, Instrument> instruments = parseInstruments(SCINSTF);
        java.util.Map<String, FeeRule> fees = parseFees(SCFEEF);
        java.util.Map<String, java.util.List<SessionWindow>> calendar = parseCalendar(SCCALF);
        java.util.Map<String, java.util.List<Venue>> venues = parseVenues(SCVENF);
        java.util.Map<String, PriceBand> bands = parseBands(SCBAND);

        for (Instrument instrument : instruments.values()) {
            PriceBand band = bands.get(instrument.code);
            require(band != null, "値幅未設定 銘柄=" + instrument.code);
            require(fees.containsKey(instrument.boardCode), "手数料未設定 ボード=" + instrument.boardCode);
            require(calendar.size() > 0, "立会時刻未設定");
            require(band.lowerAmount % instrument.tickAmount == 0L, "下限値幅刻み不一致 銘柄=" + instrument.code);
            require(band.upperAmount % instrument.tickAmount == 0L, "上限値幅刻み不一致 銘柄=" + instrument.code);
            require(band.lowerAmount < band.upperAmount, "値幅上下不正 銘柄=" + instrument.code);
        }

        java.util.Map<String, BoardSnapshot> boards = new java.util.TreeMap<>();
        for (String boardCode : fees.keySet()) {
            java.util.List<Venue> boardVenues = venues.get(boardCode);
            require(boardVenues != null && !boardVenues.isEmpty(), "会場未設定 ボード=" + boardCode);
            long capacity = 0L;
            int minLatency = Integer.MAX_VALUE;
            for (Venue venue : boardVenues) {
                if (venue.enabled) {
                    capacity += venue.capacityQty;
                    minLatency = Math.min(minLatency, venue.latencyUs);
                }
            }
            require(capacity > 0L, "有効会場なし ボード=" + boardCode);
            boards.put(boardCode, new BoardSnapshot(boardCode, fees.get(boardCode),
                    immutableList(boardVenues), capacity, minLatency));
        }

        return new Snapshot(immutableMap(instruments), immutableMap(boards),
                immutableMap(calendar), immutableMap(bands));
    }

    private static java.util.Map<String, Instrument> parseInstruments(String text) {
        java.util.Map<String, Instrument> result = new java.util.TreeMap<>();
        for (String[] row : rows(text, 6)) {
            String code = row[0];
            int tier = parseInt(row[2], "銘柄階層");
            long tick = parseLong(row[3], "呼値");
            long lot = parseLong(row[4], "売買単位");
            String board = row[5];
            require(isBoard(board), "ボード区分不正 銘柄=" + code);
            require(tick == tierTick(tier), "階層別呼値不一致 銘柄=" + code);
            require(lot > 0L, "売買単位不正 銘柄=" + code);
            putUnique(result, code, new Instrument(code, row[1], tier, tierMarginBp(tier), tick, lot, board),
                    "銘柄重複 銘柄=" + code);
        }
        return result;
    }

    private static java.util.Map<String, FeeRule> parseFees(String text) {
        java.util.Map<String, FeeRule> result = new java.util.TreeMap<>();
        for (String[] row : rows(text, 3)) {
            String board = row[0];
            int feeRate = parseInt(row[1], "手数料率");
            long minFee = parseLong(row[2], "最低手数料");
            require(isBoard(board), "ボード区分不正 ボード=" + board);
            require(feeRate >= 0 && feeRate <= 1000, "手数料率不正 ボード=" + board);
            require(minFee >= 0L, "最低手数料不正 ボード=" + board);
            putUnique(result, board, new FeeRule(board, feeRate, minFee), "手数料重複 ボード=" + board);
        }
        return result;
    }

    private static java.util.Map<String, java.util.List<SessionWindow>> parseCalendar(String text) {
        java.util.Map<String, java.util.List<SessionWindow>> result = new java.util.TreeMap<>();
        for (String[] row : rows(text, 4)) {
            String date = row[0];
            String kbn = row[1];
            long open = parseEpochMillis(row[2], "開始時刻");
            long close = parseEpochMillis(row[3], "終了時刻");
            require("AM".equals(kbn) || "PM".equals(kbn), "立会区分不正 日付=" + date);
            require(open < close, "立会時刻逆転 日付=" + date + " 区分=" + kbn);
            result.computeIfAbsent(date, x -> new java.util.ArrayList<SessionWindow>())
                    .add(new SessionWindow(date, kbn, open, close));
        }
        for (java.util.List<SessionWindow> sessions : result.values()) {
            sessions.sort((x, y) -> Long.compare(x.openTsMillis, y.openTsMillis));
            for (int i = 1; i < sessions.size(); i++) {
                require(sessions.get(i - 1).closeTsMillis <= sessions.get(i).openTsMillis,
                        "立会時刻重複 日付=" + sessions.get(i).sessionDate);
            }
        }
        return result;
    }

    private static java.util.Map<String, java.util.List<Venue>> parseVenues(String text) {
        java.util.Map<String, java.util.List<Venue>> result = new java.util.TreeMap<>();
        java.util.Set<String> keys = new java.util.HashSet<>();
        for (String[] row : rows(text, 6)) {
            String venueCode = row[0];
            String board = row[1];
            int latency = parseInt(row[2], "遅延マイクロ秒");
            int feeBps = parseInt(row[3], "会場手数料");
            boolean enabled = parseEnabled(row[4]);
            long capacity = parseLong(row[5], "容量");
            require(isBoard(board), "ボード区分不正 会場=" + venueCode);
            require(latency > 0, "遅延不正 会場=" + venueCode);
            require(feeBps >= 0, "会場手数料不正 会場=" + venueCode);
            require(capacity > 0L, "容量不正 会場=" + venueCode);
            require(keys.add(venueCode + "|" + board), "会場重複 会場=" + venueCode + " ボード=" + board);
            result.computeIfAbsent(board, x -> new java.util.ArrayList<Venue>())
                    .add(new Venue(venueCode, board, latency, feeBps, enabled, capacity));
        }
        return result;
    }

    private static java.util.Map<String, PriceBand> parseBands(String text) {
        java.util.Map<String, PriceBand> result = new java.util.TreeMap<>();
        for (String[] row : rows(text, 5)) {
            String code = row[0];
            long lower = parseLong(row[1], "下限金額");
            long upper = parseLong(row[2], "上限金額");
            long ts = parseEpochMillis(row[3], "値幅時刻");
            String source = row[4];
            require(lower > 0L && upper > 0L, "値幅金額不正 銘柄=" + code);
            require("T".equals(source) || "S".equals(source) || "E".equals(source), "値幅ソース不正 銘柄=" + code);
            putUnique(result, code, new PriceBand(code, lower, upper, ts, source), "値幅重複 銘柄=" + code);
        }
        return result;
    }

    private static java.util.List<String[]> rows(String text, int width) {
        String[] lines = text.split("\\R");
        java.util.List<String[]> result = new java.util.ArrayList<>();
        for (int i = 1; i < lines.length; i++) {
            if (lines[i].trim().isEmpty()) {
                continue;
            }
            String[] cols = lines[i].split(",", -1);
            require(cols.length == width, "項目数不正 行=" + (i + 1));
            for (int j = 0; j < cols.length; j++) {
                cols[j] = cols[j].trim();
                require(!cols[j].isEmpty(), "空項目不正 行=" + (i + 1));
            }
            result.add(cols);
        }
        return result;
    }

    private static int tierMarginBp(int tier) {
        if (tier == 1) {
            return 1000;
        }
        if (tier == 2) {
            return 2000;
        }
        if (tier == 3) {
            return 4000;
        }
        throw new IllegalArgumentException("銘柄階層不正 階層=" + tier);
    }

    private static long tierTick(int tier) {
        if (tier == 1) {
            return 100L;
        }
        if (tier == 2) {
            return 500L;
        }
        if (tier == 3) {
            return 1000L;
        }
        throw new IllegalArgumentException("銘柄階層不正 階層=" + tier);
    }

    private static boolean isBoard(String board) {
        return "T1".equals(board) || "ST".equals(board) || "ETF".equals(board);
    }

    private static boolean parseEnabled(String value) {
        if ("Y".equals(value)) {
            return true;
        }
        if ("N".equals(value)) {
            return false;
        }
        throw new IllegalArgumentException("有効区分不正 区分=" + value);
    }

    private static int parseInt(String value, String name) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " 数値不正 値=" + value, e);
        }
    }

    private static long parseLong(String value, String name) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " 数値不正 値=" + value, e);
        }
    }

    private static long parseEpochMillis(String value, String name) {
        try {
            return java.time.OffsetDateTime.parse(value).toInstant().toEpochMilli();
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException(name + " 時刻形式不正 値=" + value, e);
        }
    }

    private static <K, V> void putUnique(java.util.Map<K, V> map, K key, V value, String message) {
        require(!map.containsKey(key), message);
        map.put(key, value);
    }

    private static <K, V> java.util.Map<K, V> immutableMap(java.util.Map<K, V> src) {
        java.util.Map<K, V> copy = new java.util.TreeMap<>(src);
        return java.util.Collections.unmodifiableMap(copy);
    }

    private static <T> java.util.List<T> immutableList(java.util.List<T> src) {
        return java.util.Collections.unmodifiableList(new java.util.ArrayList<T>(src));
    }

    private static void require(boolean ok, String message) {
        if (!ok) {
            throw new IllegalStateException(message);
        }
    }

    private static final class Snapshot {
        final java.util.Map<String, Instrument> instruments;
        final java.util.Map<String, BoardSnapshot> boards;
        final java.util.Map<String, java.util.List<SessionWindow>> calendar;
        final java.util.Map<String, PriceBand> priceBands;

        Snapshot(java.util.Map<String, Instrument> instruments,
                 java.util.Map<String, BoardSnapshot> boards,
                 java.util.Map<String, java.util.List<SessionWindow>> calendar,
                 java.util.Map<String, PriceBand> priceBands) {
            this.instruments = instruments;
            this.boards = boards;
            this.calendar = calendar;
            this.priceBands = priceBands;
        }
    }

    private static final class Instrument {
        final String code;
        final String name;
        final int tier;
        final int marginRateBp;
        final long tickAmount;
        final long lotQty;
        final String boardCode;

        Instrument(String code, String name, int tier, int marginRateBp,
                   long tickAmount, long lotQty, String boardCode) {
            this.code = code;
            this.name = name;
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tickAmount = tickAmount;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class FeeRule {
        final String boardCode;
        final int feeRate;
        final long minFeeAmount;

        FeeRule(String boardCode, int feeRate, long minFeeAmount) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmount = minFeeAmount;
        }
    }

    private static final class SessionWindow {
        final String sessionDate;
        final String sessionKbn;
        final long openTsMillis;
        final long closeTsMillis;

        SessionWindow(String sessionDate, String sessionKbn, long openTsMillis, long closeTsMillis) {
            this.sessionDate = sessionDate;
            this.sessionKbn = sessionKbn;
            this.openTsMillis = openTsMillis;
            this.closeTsMillis = closeTsMillis;
        }
    }

    private static final class Venue {
        final String venueCode;
        final String boardCode;
        final int latencyUs;
        final int feeBps;
        final boolean enabled;
        final long capacityQty;

        Venue(String venueCode, String boardCode, int latencyUs, int feeBps,
              boolean enabled, long capacityQty) {
            this.venueCode = venueCode;
            this.boardCode = boardCode;
            this.latencyUs = latencyUs;
            this.feeBps = feeBps;
            this.enabled = enabled;
            this.capacityQty = capacityQty;
        }
    }

    private static final class PriceBand {
        final String instrumentCode;
        final long lowerAmount;
        final long upperAmount;
        final long bandTsMillis;
        final String sourceKbn;

        PriceBand(String instrumentCode, long lowerAmount, long upperAmount,
                  long bandTsMillis, String sourceKbn) {
            this.instrumentCode = instrumentCode;
            this.lowerAmount = lowerAmount;
            this.upperAmount = upperAmount;
            this.bandTsMillis = bandTsMillis;
            this.sourceKbn = sourceKbn;
        }
    }

    private static final class BoardSnapshot {
        final String boardCode;
        final FeeRule feeRule;
        final java.util.List<Venue> venues;
        final long enabledCapacityQty;
        final int bestLatencyUs;

        BoardSnapshot(String boardCode, FeeRule feeRule, java.util.List<Venue> venues,
                      long enabledCapacityQty, int bestLatencyUs) {
            this.boardCode = boardCode;
            this.feeRule = feeRule;
            this.venues = venues;
            this.enabledCapacityQty = enabledCapacityQty;
            this.bestLatencyUs = bestLatencyUs;
        }
    }
}
