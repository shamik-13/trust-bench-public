/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2019-10-22  中川 美和 (E-283)      初版作成
 */

package jp.mirai.sec.matching;

public class RefDataService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final java.nio.charset.Charset CSV_CHARSET = java.nio.charset.StandardCharsets.UTF_8;
    private static final java.time.format.DateTimeFormatter DT_FMT = java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
    private static final java.time.format.DateTimeFormatter TS_FMT = java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

    public static void main(String[] a) throws Exception {
        if (a.length < 3) {
            System.err.println("引数不足: SCINSTF SCCALF SCFEEF [基準日yyyyMMdd]");
            System.exit(16);
        }

        java.time.LocalDate kijunDt = a.length >= 4
                ? java.time.LocalDate.parse(a[3], DT_FMT)
                : java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo"));

        RefCache cache = new RefCache();
        cache.loadInstrument(java.nio.file.Paths.get(a[0]));
        cache.loadCalendar(java.nio.file.Paths.get(a[1]));
        cache.loadFee(java.nio.file.Paths.get(a[2]));
        cache.close(kijunDt);

        SessionRow session = cache.session(kijunDt);
        System.out.println("参照データ読込完了"
                + ", 銘柄数=" + cache.instrumentCount()
                + ", 立会区分=" + session.sessKbn
                + ", 開始=" + session.openTs
                + ", 終了=" + session.closeTs
                + ", 手数料区分数=" + cache.feeCount());
    }

    private static final class RefCache {
        private final java.util.Map<String, InstrumentRow> instruments = new java.util.LinkedHashMap<String, InstrumentRow>();
        private final java.util.Map<java.time.LocalDate, SessionRow> sessions = new java.util.LinkedHashMap<java.time.LocalDate, SessionRow>();
        private final java.util.Map<String, FeeRow> fees = new java.util.LinkedHashMap<String, FeeRow>();
        private final java.util.Set<String> badInstrumentCodes = new java.util.HashSet<String>();
        private final java.util.Set<java.time.LocalDate> badSessionDates = new java.util.HashSet<java.time.LocalDate>();
        private final java.util.Set<String> badBoardCodes = new java.util.HashSet<String>();
        private boolean closed;

        void loadInstrument(java.nio.file.Path path) throws java.io.IOException {
            for (CsvRow r : readCsv(path)) {
                String code = required(r, "INSTR-CODE");
                String name = required(r, "INSTR-NAME");
                int tier = parseInt(required(r, "INSTR-TIER"), "INSTR-TIER", r.lineNo);
                long tickAmt = parseLong(required(r, "TICK-AMT"), "TICK-AMT", r.lineNo);
                long lotQty = parseLong(required(r, "LOT-QTY"), "LOT-QTY", r.lineNo);
                String board = required(r, "BOARD-CODE");

                if (!validBoard(board)) {
                    throw new IllegalArgumentException("板コード不正: 行=" + r.lineNo + ", 値=" + board);
                }
                if (lotQty <= 0L || tickAmt <= 0L) {
                    throw new IllegalArgumentException("刻み又は売買単位不正: 行=" + r.lineNo);
                }
                if (tickAmt != tierTick(tier)) {
                    throw new IllegalArgumentException("階層別刻み不整合: 行=" + r.lineNo + ", 銘柄=" + code);
                }

                InstrumentRow row = new InstrumentRow(code, name, tier, tickAmt, lotQty, board, marginRateBp(tier));
                if (instruments.putIfAbsent(code, row) != null) {
                    badInstrumentCodes.add(code);
                }
            }
        }

        void loadCalendar(java.nio.file.Path path) throws java.io.IOException {
            for (CsvRow r : readCsv(path)) {
                java.time.LocalDate dt = java.time.LocalDate.parse(required(r, "SESS-DT"), DT_FMT);
                String sessKbn = required(r, "SESS-KBN");
                java.time.LocalDateTime openTs = java.time.LocalDateTime.parse(required(r, "OPEN-TS"), TS_FMT);
                java.time.LocalDateTime closeTs = java.time.LocalDateTime.parse(required(r, "CLOSE-TS"), TS_FMT);

                if (!closeTs.isAfter(openTs)) {
                    throw new IllegalArgumentException("立会時刻不正: 行=" + r.lineNo + ", 日付=" + dt.format(DT_FMT));
                }

                SessionRow row = new SessionRow(dt, sessKbn, openTs, closeTs);
                if (sessions.putIfAbsent(dt, row) != null) {
                    badSessionDates.add(dt);
                }
            }
        }

        void loadFee(java.nio.file.Path path) throws java.io.IOException {
            for (CsvRow r : readCsv(path)) {
                String board = required(r, "BOARD-CODE");
                java.math.BigDecimal rate = new java.math.BigDecimal(required(r, "FEE-RATE"));
                long minFee = parseLong(required(r, "MIN-FEE-AMT"), "MIN-FEE-AMT", r.lineNo);

                if (!validBoard(board)) {
                    throw new IllegalArgumentException("手数料板コード不正: 行=" + r.lineNo + ", 値=" + board);
                }
                if (rate.signum() < 0 || minFee < 0L) {
                    throw new IllegalArgumentException("手数料不正: 行=" + r.lineNo);
                }

                FeeRow row = new FeeRow(board, rate, minFee);
                if (fees.putIfAbsent(board, row) != null) {
                    badBoardCodes.add(board);
                }
            }
        }

        void close(java.time.LocalDate kijunDt) {
            if (closed) {
                return;
            }
            for (String code : badInstrumentCodes) {
                instruments.remove(code);
            }
            for (java.time.LocalDate dt : badSessionDates) {
                sessions.remove(dt);
            }
            for (String board : badBoardCodes) {
                fees.remove(board);
            }

            java.time.LocalDateTime now = java.time.LocalDateTime.now(java.time.ZoneId.of("Asia/Tokyo"));
            java.util.Iterator<java.util.Map.Entry<java.time.LocalDate, SessionRow>> it = sessions.entrySet().iterator();
            while (it.hasNext()) {
                SessionRow row = it.next().getValue();
                if (row.sessDt.isBefore(kijunDt) && row.closeTs.isBefore(now)) {
                    it.remove();
                }
            }

            for (InstrumentRow row : instruments.values()) {
                if (!fees.containsKey(row.boardCode)) {
                    throw new IllegalStateException("手数料未定義: 銘柄=" + row.instrCode + ", 板=" + row.boardCode);
                }
            }
            if (!badInstrumentCodes.isEmpty()) {
                throw new IllegalStateException("銘柄マスタ重複: 件数=" + badInstrumentCodes.size());
            }
            if (!badSessionDates.isEmpty()) {
                throw new IllegalStateException("営業日カレンダ重複: 件数=" + badSessionDates.size());
            }
            if (!badBoardCodes.isEmpty()) {
                throw new IllegalStateException("手数料重複: 件数=" + badBoardCodes.size());
            }
            closed = true;
        }

        SessionRow session(java.time.LocalDate dt) {
            SessionRow row = sessions.get(dt);
            if (row == null) {
                throw new IllegalArgumentException("営業日なし: 日付=" + dt.format(DT_FMT));
            }
            return row;
        }

        int instrumentCount() {
            return instruments.size();
        }

        int feeCount() {
            return fees.size();
        }
    }

    private static final class InstrumentRow {
        final String instrCode;
        final String instrName;
        final int instrTier;
        final long tickAmt;
        final long lotQty;
        final String boardCode;
        final int marginRateBp;

        InstrumentRow(String instrCode, String instrName, int instrTier, long tickAmt, long lotQty, String boardCode, int marginRateBp) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
            this.marginRateBp = marginRateBp;
        }
    }

    private static final class SessionRow {
        final java.time.LocalDate sessDt;
        final String sessKbn;
        final java.time.LocalDateTime openTs;
        final java.time.LocalDateTime closeTs;

        SessionRow(java.time.LocalDate sessDt, String sessKbn, java.time.LocalDateTime openTs, java.time.LocalDateTime closeTs) {
            this.sessDt = sessDt;
            this.sessKbn = sessKbn;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }

    private static final class FeeRow {
        final String boardCode;
        final java.math.BigDecimal feeRate;
        final long minFeeAmt;

        FeeRow(String boardCode, java.math.BigDecimal feeRate, long minFeeAmt) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmt = minFeeAmt;
        }
    }

    private static final class CsvRow {
        final int lineNo;
        final java.util.Map<String, String> values;

        CsvRow(int lineNo, java.util.Map<String, String> values) {
            this.lineNo = lineNo;
            this.values = values;
        }
    }

    private static java.util.List<CsvRow> readCsv(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        java.util.List<CsvRow> rows = new java.util.ArrayList<CsvRow>();
        if (lines.isEmpty()) {
            return rows;
        }

        java.util.List<String> header = splitCsv(lines.get(0));
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.trim().isEmpty()) {
                continue;
            }
            java.util.List<String> cols = splitCsv(line);
            if (cols.size() != header.size()) {
                throw new IllegalArgumentException("CSV項目数不正: 行=" + (i + 1));
            }
            java.util.Map<String, String> m = new java.util.LinkedHashMap<String, String>();
            for (int c = 0; c < header.size(); c++) {
                m.put(header.get(c), cols.get(c));
            }
            rows.add(new CsvRow(i + 1, m));
        }
        return rows;
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean q = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (q && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    q = !q;
                }
            } else if (ch == ',' && !q) {
                out.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        if (q) {
            throw new IllegalArgumentException("CSV引用符不正");
        }
        out.add(b.toString().trim());
        return out;
    }

    private static String required(CsvRow r, String name) {
        String v = r.values.get(name);
        if (v == null || v.isEmpty()) {
            throw new IllegalArgumentException("必須項目なし: 行=" + r.lineNo + ", 項目=" + name);
        }
        return v;
    }

    private static int parseInt(String s, String name, int lineNo) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値不正: 行=" + lineNo + ", 項目=" + name, e);
        }
    }

    private static long parseLong(String s, String name, int lineNo) {
        try {
            return Long.parseLong(s);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値不正: 行=" + lineNo + ", 項目=" + name, e);
        }
    }

    private static boolean validBoard(String board) {
        return "T1".equals(board) || "ST".equals(board) || "ETF".equals(board);
    }

    private static int marginRateBp(int tier) {
        switch (tier) {
            case 1:
                return 1000;
            case 2:
                return 2000;
            case 3:
                return 4000;
            default:
                throw new IllegalArgumentException("銘柄階層不正: 階層=" + tier);
        }
    }

    private static long tierTick(int tier) {
        switch (tier) {
            case 1:
                return 100L;
            case 2:
                return 500L;
            case 3:
                return 1000L;
            default:
                throw new IllegalArgumentException("銘柄階層不正: 階層=" + tier);
        }
    }
}
