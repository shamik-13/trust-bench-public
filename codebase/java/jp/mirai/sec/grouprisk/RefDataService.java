package jp.mirai.sec.grouprisk;

public class RefDataService {
    /**
     * 変更履歴
     * 版数  年月日      担当    概要
     * 1.00  2021-07-15  西村 亮 (E-204)    初版作成
     */

    private static final long MIHFT_MAX_NOTIONAL = 500_000_000L;

    private static final String SCINSTF = "SCINSTF.csv";
    private static final String SCFEEF = "SCFEEF.csv";
    private static final String SCCALF = "SCCALF.csv";

    private static final java.nio.charset.Charset CSV_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    public static void main(String[] a) throws Exception {
        java.nio.file.Path base = a.length == 0 ? java.nio.file.Paths.get(".") : java.nio.file.Paths.get(a[0]);
        Store store = load(base);

        java.time.LocalDate today = java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo"));
        Session session = store.sessions.get(today);

        System.out.println("参照データサービス 起動");
        System.out.println("銘柄件数=" + store.instruments.size());
        System.out.println("手数料区分件数=" + store.fees.size());
        System.out.println("カレンダー件数=" + store.sessions.size());
        System.out.println("本日セッション=" + (session == null ? "未定義" : session.sessionKind));
        System.out.println("必要証拠金合計=" + store.totalMarginByBoard());
        System.out.println("最大想定代金=" + store.maxReferenceNotional());
    }

    private static Store load(java.nio.file.Path base) throws java.io.IOException {
        Store store = new Store();

        java.util.List<java.util.List<String>> instRows = readCsv(base.resolve(SCINSTF));
        for (int i = 1; i < instRows.size(); i++) {
            java.util.List<String> r = instRows.get(i);
            requireColumns(SCINSTF, i + 1, r, 6);
            Instrument inst = Instrument.from(r, i + 1);
            if (store.instruments.put(inst.code, inst) != null) {
                throw new IllegalArgumentException("銘柄重複 行=" + (i + 1) + " 銘柄=" + inst.code);
            }
        }

        java.util.List<java.util.List<String>> feeRows = readCsv(base.resolve(SCFEEF));
        for (int i = 1; i < feeRows.size(); i++) {
            java.util.List<String> r = feeRows.get(i);
            requireColumns(SCFEEF, i + 1, r, 3);
            Fee fee = Fee.from(r, i + 1);
            if (store.fees.put(fee.boardCode, fee) != null) {
                throw new IllegalArgumentException("手数料区分重複 行=" + (i + 1) + " 市場=" + fee.boardCode);
            }
        }

        java.util.List<java.util.List<String>> calRows = readCsv(base.resolve(SCCALF));
        for (int i = 1; i < calRows.size(); i++) {
            java.util.List<String> r = calRows.get(i);
            requireColumns(SCCALF, i + 1, r, 4);
            Session session = Session.from(r, i + 1);
            if (store.sessions.put(session.sessionDate, session) != null) {
                throw new IllegalArgumentException("営業日重複 行=" + (i + 1) + " 日付=" + session.sessionDate);
            }
        }

        store.validateCrossReference();
        return store;
    }

    private static java.util.List<java.util.List<String>> readCsv(java.nio.file.Path path) throws java.io.IOException {
        if (!java.nio.file.Files.exists(path)) {
            throw new java.io.FileNotFoundException("入力ファイルなし " + path);
        }

        java.util.List<java.util.List<String>> rows = new java.util.ArrayList<>();
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(path, CSV_CHARSET)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    rows.add(parseCsvLine(line));
                }
            }
        }
        if (rows.isEmpty()) {
            throw new IllegalArgumentException("入力ファイル空 " + path);
        }
        return rows;
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
        StringBuilder cell = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quoted) {
                if (c == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        cell.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    cell.append(c);
                }
            } else if (c == ',') {
                out.add(cell.toString().trim());
                cell.setLength(0);
            } else if (c == '"') {
                if (cell.length() != 0) {
                    throw new IllegalArgumentException("CSV引用符位置不正");
                }
                quoted = true;
            } else {
                cell.append(c);
            }
        }

        if (quoted) {
            throw new IllegalArgumentException("CSV引用符未終了");
        }
        out.add(cell.toString().trim());
        return out;
    }

    private static void requireColumns(String file, int row, java.util.List<String> r, int n) {
        if (r.size() != n) {
            throw new IllegalArgumentException("項目数不正 ファイル=" + file + " 行=" + row + " 件数=" + r.size());
        }
        for (int i = 0; i < r.size(); i++) {
            if (r.get(i).isEmpty()) {
                throw new IllegalArgumentException("必須項目空 ファイル=" + file + " 行=" + row + " 列=" + (i + 1));
            }
        }
    }

    private static int parseInt(String v, String name, int row) {
        try {
            return Integer.parseInt(v);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値不正 行=" + row + " 項目=" + name + " 値=" + v, e);
        }
    }

    private static long parseLong(String v, String name, int row) {
        try {
            return Long.parseLong(v);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値不正 行=" + row + " 項目=" + name + " 値=" + v, e);
        }
    }

    private static java.math.BigDecimal parseDecimal(String v, String name, int row) {
        try {
            return new java.math.BigDecimal(v);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("小数不正 行=" + row + " 項目=" + name + " 値=" + v, e);
        }
    }

    private static void requireBoard(String boardCode, int row) {
        if (!"T1".equals(boardCode) && !"ST".equals(boardCode) && !"ETF".equals(boardCode)) {
            throw new IllegalArgumentException("市場区分不正 行=" + row + " 市場=" + boardCode);
        }
    }

    private static Tier tierOf(int tier, int row) {
        if (tier == 1) {
            return new Tier(1, 1000, 100);
        }
        if (tier == 2) {
            return new Tier(2, 2000, 500);
        }
        if (tier == 3) {
            return new Tier(3, 4000, 1000);
        }
        throw new IllegalArgumentException("銘柄階層不正 行=" + row + " 階層=" + tier);
    }

    private static final class Store {
        private final java.util.Map<String, Instrument> instruments = new java.util.TreeMap<>();
        private final java.util.Map<String, Fee> fees = new java.util.TreeMap<>();
        private final java.util.Map<java.time.LocalDate, Session> sessions = new java.util.TreeMap<>();

        private void validateCrossReference() {
            for (Instrument inst : instruments.values()) {
                if (!fees.containsKey(inst.boardCode)) {
                    throw new IllegalArgumentException("手数料未定義 銘柄=" + inst.code + " 市場=" + inst.boardCode);
                }
            }
            if (sessions.isEmpty()) {
                throw new IllegalArgumentException("カレンダー未定義");
            }
        }

        private java.util.Map<String, Long> totalMarginByBoard() {
            java.util.Map<String, Long> totals = new java.util.TreeMap<>();
            for (Instrument inst : instruments.values()) {
                long notional = inst.referenceNotional();
                long margin = notional * inst.marginRateBp / 10_000L;
                totals.merge(inst.boardCode, margin, Long::sum);
            }
            return totals;
        }

        private long maxReferenceNotional() {
            long max = 0L;
            for (Instrument inst : instruments.values()) {
                long notional = inst.referenceNotional();
                if (notional > MIHFT_MAX_NOTIONAL) {
                    throw new IllegalArgumentException("想定代金上限超過 銘柄=" + inst.code + " 代金=" + notional);
                }
                max = Math.max(max, notional);
            }
            return max;
        }
    }

    private static final class Instrument {
        private final String code;
        private final String name;
        private final int tier;
        private final int marginRateBp;
        private final long tickAmount;
        private final long lotQty;
        private final String boardCode;

        private Instrument(String code, String name, int tier, int marginRateBp, long tickAmount, long lotQty, String boardCode) {
            this.code = code;
            this.name = name;
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tickAmount = tickAmount;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }

        private static Instrument from(java.util.List<String> r, int row) {
            String code = r.get(0);
            String name = r.get(1);
            int tierValue = parseInt(r.get(2), "INSTR-TIER", row);
            long tickAmount = parseLong(r.get(3), "TICK-AMT", row);
            long lotQty = parseLong(r.get(4), "LOT-QTY", row);
            String boardCode = r.get(5);

            Tier tier = tierOf(tierValue, row);
            requireBoard(boardCode, row);

            if (tickAmount != tier.tickAmount) {
                throw new IllegalArgumentException("呼値不整合 行=" + row + " 銘柄=" + code + " 呼値=" + tickAmount + " 期待=" + tier.tickAmount);
            }
            if (lotQty <= 0L) {
                throw new IllegalArgumentException("売買単位不正 行=" + row + " 銘柄=" + code);
            }
            if (!code.matches("[0-9A-Z]{4,12}")) {
                throw new IllegalArgumentException("銘柄コード不正 行=" + row + " 銘柄=" + code);
            }

            return new Instrument(code, name, tier.code, tier.marginRateBp, tickAmount, lotQty, boardCode);
        }

        private long referenceNotional() {
            return tickAmount * lotQty * 100L;
        }
    }

    private static final class Fee {
        private final String boardCode;
        private final java.math.BigDecimal feeRate;
        private final long minFeeAmount;

        private Fee(String boardCode, java.math.BigDecimal feeRate, long minFeeAmount) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmount = minFeeAmount;
        }

        private static Fee from(java.util.List<String> r, int row) {
            String boardCode = r.get(0);
            java.math.BigDecimal feeRate = parseDecimal(r.get(1), "FEE-RATE", row);
            long minFeeAmount = parseLong(r.get(2), "MIN-FEE-AMT", row);

            requireBoard(boardCode, row);
            if (feeRate.signum() < 0 || feeRate.compareTo(new java.math.BigDecimal("0.0100")) > 0) {
                throw new IllegalArgumentException("手数料率不正 行=" + row + " 市場=" + boardCode);
            }
            if (minFeeAmount < 0L) {
                throw new IllegalArgumentException("最低手数料不正 行=" + row + " 市場=" + boardCode);
            }

            return new Fee(boardCode, feeRate, minFeeAmount);
        }

        /*
         * 参照系では手数料の算定そのものは行わない。
         * 手数料の確定計算は mihft_fee 本体に従うこと。ここでは登録済みの率・最低額を保持するのみ。
         */
        @SuppressWarnings("unused")
        private long minimumFee() {
            return minFeeAmount;
        }
    }

    private static final class Session {
        private final java.time.LocalDate sessionDate;
        private final String sessionKind;
        private final java.time.LocalTime openTime;
        private final java.time.LocalTime closeTime;

        private Session(java.time.LocalDate sessionDate, String sessionKind, java.time.LocalTime openTime, java.time.LocalTime closeTime) {
            this.sessionDate = sessionDate;
            this.sessionKind = sessionKind;
            this.openTime = openTime;
            this.closeTime = closeTime;
        }

        private static Session from(java.util.List<String> r, int row) {
            java.time.LocalDate date;
            java.time.LocalTime open;
            java.time.LocalTime close;
            try {
                date = java.time.LocalDate.parse(r.get(0));
                open = java.time.LocalTime.parse(r.get(2));
                close = java.time.LocalTime.parse(r.get(3));
            } catch (java.time.DateTimeException e) {
                throw new IllegalArgumentException("日時不正 行=" + row, e);
            }

            String kind = r.get(1);
            if (!"0".equals(kind) && !"1".equals(kind) && !"2".equals(kind)) {
                throw new IllegalArgumentException("営業区分不正 行=" + row + " 区分=" + kind);
            }
            if ("0".equals(kind)) {
                if (!open.equals(close)) {
                    throw new IllegalArgumentException("休場時刻不正 行=" + row + " 日付=" + date);
                }
            } else if (!open.isBefore(close)) {
                throw new IllegalArgumentException("立会時刻不正 行=" + row + " 日付=" + date);
            }

            return new Session(date, kind, open, close);
        }

        @SuppressWarnings("unused")
        private boolean isOpenAt(java.time.LocalTime time) {
            return !"0".equals(sessionKind) && !time.isBefore(openTime) && time.isBefore(closeTime);
        }
    }

    private static final class Tier {
        private final int code;
        private final int marginRateBp;
        private final long tickAmount;

        private Tier(int code, int marginRateBp, long tickAmount) {
            this.code = code;
            this.marginRateBp = marginRateBp;
            this.tickAmount = tickAmount;
        }
    }
}
