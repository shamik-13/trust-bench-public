/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2023-04-18  村上 健司 (E-301)    初版作成
 */

package jp.mirai.sec.position;

public class RefDataService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L * 100L;

    public static void main(String[] a) throws Exception {
        if (a.length != 1) {
            System.err.println("使用法: java RefDataService 入力ディレクトリ");
            System.exit(2);
        }

        java.nio.file.Path dir = java.nio.file.Paths.get(a[0]);
        RefDataView view = load(dir);

        long sampleNotional = 0L;
        long sampleMargin = 0L;
        long sampleFee = 0L;
        for (Instrument inst : view.instrumentsByCode.values()) {
            long price = inst.tickAmt;
            long qty = inst.lotQty;
            long notional = price * qty;
            sampleNotional += notional;
            sampleMargin += notional * inst.marginRateBp / 10000L;
            FeeRule fee = view.feesByBoard.get(inst.boardCode);
            if (fee != null) {
                long feeAmt = notional * fee.feeRateBp / 10000L;
                sampleFee += Math.max(feeAmt, fee.minFeeAmt);
            }
        }

        int rejectNotional = sampleNotional > MIHFT_MAX_NOTIONAL ? 8 : 0;
        System.out.println("読込完了"
                + " バージョン=" + view.version
                + " 銘柄数=" + view.instrumentsByCode.size()
                + " 手数料数=" + view.feesByBoard.size()
                + " 営業日数=" + view.sessionsByDate.size()
                + " 想定元本=" + sampleNotional
                + " 必要証拠金=" + sampleMargin
                + " 概算手数料=" + sampleFee
                + " 判定=" + rejectNotional);
    }

    private static RefDataView load(java.nio.file.Path dir) throws java.io.IOException {
        java.util.Map<String, FeeRule> fees = readFees(dir.resolve("SCFEEF.csv"));
        java.util.Map<String, Session> sessions = readSessions(dir.resolve("SCCALF.csv"));
        java.util.Map<String, Instrument> instruments = readInstruments(dir.resolve("SCINSTF.csv"), fees);

        long version = 17L;
        version = 31L * version + checksumInstruments(instruments);
        version = 31L * version + checksumFees(fees);
        version = 31L * version + checksumSessions(sessions);

        return new RefDataView(
                version,
                java.util.Collections.unmodifiableMap(instruments),
                java.util.Collections.unmodifiableMap(fees),
                java.util.Collections.unmodifiableMap(sessions));
    }

    private static java.util.Map<String, Instrument> readInstruments(
            java.nio.file.Path file,
            java.util.Map<String, FeeRule> fees) throws java.io.IOException {
        java.util.List<java.util.List<String>> rows = readCsv(file);
        java.util.Map<String, Instrument> out = new java.util.LinkedHashMap<String, Instrument>();

        for (int i = 1; i < rows.size(); i++) {
            java.util.List<String> r = rows.get(i);
            requireColumns(file, i, r, 6);

            String code = requireText(file, i, "INSTR-CODE", r.get(0));
            String name = requireText(file, i, "INSTR-NAME", r.get(1));
            int tier = parseInt(file, i, "INSTR-TIER", r.get(2));
            long tickAmt = parseMoney100(file, i, "TICK-AMT", r.get(3));
            long lotQty = parseLong(file, i, "LOT-QTY", r.get(4));
            String boardCode = requireBoard(file, i, r.get(5));

            int marginRateBp = marginRateBp(file, i, tier);
            long canonicalTick = canonicalTick100(file, i, tier);
            if (tickAmt != canonicalTick) {
                throw new IllegalArgumentException(message(file, i, "TICK-AMT が tier 定義と不一致"));
            }
            if (lotQty <= 0L) {
                throw new IllegalArgumentException(message(file, i, "LOT-QTY が不正"));
            }
            if (!fees.containsKey(boardCode)) {
                throw new IllegalArgumentException(message(file, i, "BOARD-CODE に対応する手数料が未登録"));
            }
            if (out.put(code, new Instrument(code, name, tier, marginRateBp, tickAmt, lotQty, boardCode)) != null) {
                throw new IllegalArgumentException(message(file, i, "INSTR-CODE が重複"));
            }
        }

        if (out.isEmpty()) {
            throw new IllegalArgumentException(file + ": 銘柄が未登録");
        }
        return out;
    }

    private static java.util.Map<String, FeeRule> readFees(java.nio.file.Path file) throws java.io.IOException {
        java.util.List<java.util.List<String>> rows = readCsv(file);
        java.util.Map<String, FeeRule> out = new java.util.LinkedHashMap<String, FeeRule>();

        for (int i = 1; i < rows.size(); i++) {
            java.util.List<String> r = rows.get(i);
            requireColumns(file, i, r, 3);

            String boardCode = requireBoard(file, i, r.get(0));
            int feeRateBp = parseRateBp(file, i, "FEE-RATE", r.get(1));
            long minFeeAmt = parseMoney100(file, i, "MIN-FEE-AMT", r.get(2));

            if (out.put(boardCode, new FeeRule(boardCode, feeRateBp, minFeeAmt)) != null) {
                throw new IllegalArgumentException(message(file, i, "BOARD-CODE が重複"));
            }
        }

        return out;
    }

    private static java.util.Map<String, Session> readSessions(java.nio.file.Path file) throws java.io.IOException {
        java.util.List<java.util.List<String>> rows = readCsv(file);
        java.util.Map<String, Session> out = new java.util.LinkedHashMap<String, Session>();

        for (int i = 1; i < rows.size(); i++) {
            java.util.List<String> r = rows.get(i);
            requireColumns(file, i, r, 4);

            java.time.LocalDate date = parseDate(file, i, "SESS-DT", r.get(0));
            String sessKbn = requireText(file, i, "SESS-KBN", r.get(1));
            java.time.LocalTime open = parseTime(file, i, "OPEN-TS", r.get(2));
            java.time.LocalTime close = parseTime(file, i, "CLOSE-TS", r.get(3));

            if (!"0".equals(sessKbn) && !"1".equals(sessKbn)) {
                throw new IllegalArgumentException(message(file, i, "SESS-KBN が不正"));
            }
            if ("1".equals(sessKbn) && !open.isBefore(close)) {
                throw new IllegalArgumentException(message(file, i, "OPEN-TS と CLOSE-TS が不正"));
            }
            if (out.put(date.toString(), new Session(date.toString(), sessKbn, open.toString(), close.toString())) != null) {
                throw new IllegalArgumentException(message(file, i, "SESS-DT が重複"));
            }
        }

        return out;
    }

    private static java.util.List<java.util.List<String>> readCsv(java.nio.file.Path file) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(file, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<java.util.List<String>> rows = new java.util.ArrayList<java.util.List<String>>();
        for (String line : lines) {
            if (!line.trim().isEmpty()) {
                rows.add(parseCsvLine(line));
            }
        }
        if (rows.isEmpty()) {
            throw new IllegalArgumentException(file + ": 入力が空");
        }
        return rows;
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean quote = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quote) {
                if (c == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        b.append('"');
                        i++;
                    } else {
                        quote = false;
                    }
                } else {
                    b.append(c);
                }
            } else if (c == '"') {
                quote = true;
            } else if (c == ',') {
                out.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(c);
            }
        }

        if (quote) {
            throw new IllegalArgumentException("CSV 引用符が未終了");
        }
        out.add(b.toString().trim());
        return out;
    }

    private static void requireColumns(java.nio.file.Path file, int row, java.util.List<String> r, int n) {
        if (r.size() != n) {
            throw new IllegalArgumentException(message(file, row, "列数が不正"));
        }
    }

    private static String requireText(java.nio.file.Path file, int row, String name, String v) {
        String s = v == null ? "" : v.trim();
        if (s.isEmpty()) {
            throw new IllegalArgumentException(message(file, row, name + " が空"));
        }
        return s;
    }

    private static String requireBoard(java.nio.file.Path file, int row, String v) {
        String s = requireText(file, row, "BOARD-CODE", v);
        if (!"T1".equals(s) && !"ST".equals(s) && !"ETF".equals(s)) {
            throw new IllegalArgumentException(message(file, row, "BOARD-CODE が不正"));
        }
        return s;
    }

    private static int marginRateBp(java.nio.file.Path file, int row, int tier) {
        if (tier == 1) {
            return 1000;
        }
        if (tier == 2) {
            return 2000;
        }
        if (tier == 3) {
            return 4000;
        }
        throw new IllegalArgumentException(message(file, row, "INSTR-TIER が不正"));
    }

    private static long canonicalTick100(java.nio.file.Path file, int row, int tier) {
        if (tier == 1) {
            return 100L;
        }
        if (tier == 2) {
            return 500L;
        }
        if (tier == 3) {
            return 1000L;
        }
        throw new IllegalArgumentException(message(file, row, "INSTR-TIER が不正"));
    }

    private static int parseRateBp(java.nio.file.Path file, int row, String name, String v) {
        String s = requireText(file, row, name, v);
        int bp;
        if (s.indexOf('.') >= 0) {
            java.math.BigDecimal rate = new java.math.BigDecimal(s);
            bp = rate.multiply(new java.math.BigDecimal("10000")).setScale(0, java.math.RoundingMode.UNNECESSARY).intValueExact();
        } else {
            bp = Integer.parseInt(s);
        }
        if (bp < 0 || bp > 10000) {
            throw new IllegalArgumentException(message(file, row, name + " が範囲外"));
        }
        return bp;
    }

    private static long parseMoney100(java.nio.file.Path file, int row, String name, String v) {
        String s = requireText(file, row, name, v).replace(",", "");
        java.math.BigDecimal yen = new java.math.BigDecimal(s);
        return yen.multiply(new java.math.BigDecimal("100")).setScale(0, java.math.RoundingMode.UNNECESSARY).longValueExact();
    }

    private static int parseInt(java.nio.file.Path file, int row, String name, String v) {
        try {
            return Integer.parseInt(requireText(file, row, name, v));
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(message(file, row, name + " が数値でない"), e);
        }
    }

    private static long parseLong(java.nio.file.Path file, int row, String name, String v) {
        try {
            return Long.parseLong(requireText(file, row, name, v));
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(message(file, row, name + " が数値でない"), e);
        }
    }

    private static java.time.LocalDate parseDate(java.nio.file.Path file, int row, String name, String v) {
        try {
            return java.time.LocalDate.parse(requireText(file, row, name, v));
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(message(file, row, name + " が日付でない"), e);
        }
    }

    private static java.time.LocalTime parseTime(java.nio.file.Path file, int row, String name, String v) {
        try {
            return java.time.LocalTime.parse(requireText(file, row, name, v));
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(message(file, row, name + " が時刻でない"), e);
        }
    }

    private static String message(java.nio.file.Path file, int row, String text) {
        return file + ":" + (row + 1) + ": " + text;
    }

    private static long checksumInstruments(java.util.Map<String, Instrument> m) {
        long h = 1125899906842597L;
        for (Instrument v : m.values()) {
            h = 31L * h + v.code.hashCode();
            h = 31L * h + v.name.hashCode();
            h = 31L * h + v.tier;
            h = 31L * h + v.marginRateBp;
            h = 31L * h + v.tickAmt;
            h = 31L * h + v.lotQty;
            h = 31L * h + v.boardCode.hashCode();
        }
        return h;
    }

    private static long checksumFees(java.util.Map<String, FeeRule> m) {
        long h = 1469598103934665603L;
        for (FeeRule v : m.values()) {
            h = 31L * h + v.boardCode.hashCode();
            h = 31L * h + v.feeRateBp;
            h = 31L * h + v.minFeeAmt;
        }
        return h;
    }

    private static long checksumSessions(java.util.Map<String, Session> m) {
        long h = 1099511628211L;
        for (Session v : m.values()) {
            h = 31L * h + v.sessDt.hashCode();
            h = 31L * h + v.sessKbn.hashCode();
            h = 31L * h + v.openTs.hashCode();
            h = 31L * h + v.closeTs.hashCode();
        }
        return h;
    }

    private static final class RefDataView {
        final long version;
        final java.util.Map<String, Instrument> instrumentsByCode;
        final java.util.Map<String, FeeRule> feesByBoard;
        final java.util.Map<String, Session> sessionsByDate;

        RefDataView(
                long version,
                java.util.Map<String, Instrument> instrumentsByCode,
                java.util.Map<String, FeeRule> feesByBoard,
                java.util.Map<String, Session> sessionsByDate) {
            this.version = version;
            this.instrumentsByCode = instrumentsByCode;
            this.feesByBoard = feesByBoard;
            this.sessionsByDate = sessionsByDate;
        }
    }

    private static final class Instrument {
        final String code;
        final String name;
        final int tier;
        final int marginRateBp;
        final long tickAmt;
        final long lotQty;
        final String boardCode;

        Instrument(String code, String name, int tier, int marginRateBp, long tickAmt, long lotQty, String boardCode) {
            this.code = code;
            this.name = name;
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class FeeRule {
        final String boardCode;
        final int feeRateBp;
        final long minFeeAmt;

        FeeRule(String boardCode, int feeRateBp, long minFeeAmt) {
            this.boardCode = boardCode;
            this.feeRateBp = feeRateBp;
            this.minFeeAmt = minFeeAmt;
        }
    }

    private static final class Session {
        final String sessDt;
        final String sessKbn;
        final String openTs;
        final String closeTs;

        Session(String sessDt, String sessKbn, String openTs, String closeTs) {
            this.sessDt = sessDt;
            this.sessKbn = sessKbn;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }
}
