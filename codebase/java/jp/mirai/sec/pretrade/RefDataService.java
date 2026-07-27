/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2024/07/09  西村 亮 (E-204)  初版作成
 */

package jp.mirai.sec.pretrade;

public class RefDataService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final java.util.Map<String, TierRule> TIER_RULES = new java.util.HashMap<String, TierRule>();
    private static final java.util.Set<String> BOARD_CODES = new java.util.HashSet<String>();

    static {
        TIER_RULES.put("1", new TierRule(1000L, 100L));
        TIER_RULES.put("2", new TierRule(2000L, 500L));
        TIER_RULES.put("3", new TierRule(4000L, 1000L));

        BOARD_CODES.add("T1");
        BOARD_CODES.add("ST");
        BOARD_CODES.add("ETF");
    }

    public static void main(String[] a) {
        if (a == null || a.length != 3) {
            System.err.println("起動引数は銘柄、手数料、カレンダーの順で三件指定してください。");
            System.exit(2);
        }

        try {
            RefDataCache cache = load(a[0], a[1], a[2]);
            long tickTotal = 0L;
            long lotTotal = 0L;
            long feeRateTotal = 0L;
            long sessionCount = 0L;

            for (InstrumentRow row : cache.instruments.values()) {
                tickTotal += row.tickAmount;
                lotTotal += row.lotQuantity;
            }
            for (FeeRow row : cache.fees.values()) {
                feeRateTotal += row.feeRateBp;
            }
            sessionCount = cache.sessions.size();

            System.out.println("読込完了"
                    + ", 銘柄件数=" + cache.instruments.size()
                    + ", 手数料件数=" + cache.fees.size()
                    + ", セッション件数=" + sessionCount
                    + ", 呼値合計=" + tickTotal
                    + ", 売買単位合計=" + lotTotal
                    + ", 手数料率合計=" + feeRateTotal);
        } catch (ValidationException e) {
            System.err.println("検証例外: " + e.getMessage());
            System.exit(1);
        } catch (java.io.IOException e) {
            System.err.println("入出力例外: " + e.getMessage());
            System.exit(3);
        }
    }

    private static RefDataCache load(String instrumentPath, String feePath, String calendarPath)
            throws java.io.IOException {
        java.util.Map<String, InstrumentRow> instruments = readInstruments(instrumentPath);
        java.util.Map<String, FeeRow> fees = readFees(feePath);
        java.util.List<SessionRow> sessions = readSessions(calendarPath);

        for (InstrumentRow row : instruments.values()) {
            if (!fees.containsKey(row.boardCode)) {
                throw new ValidationException("銘柄の市場コードに対応する手数料がありません。銘柄コード=" + row.instrumentCode
                        + ", 市場コード=" + row.boardCode);
            }
        }

        return new RefDataCache(instruments, fees, sessions);
    }

    private static java.util.Map<String, InstrumentRow> readInstruments(String path) throws java.io.IOException {
        CsvTable table = CsvTable.read(path);
        java.util.Map<String, InstrumentRow> rows = new java.util.LinkedHashMap<String, InstrumentRow>();

        for (int i = 0; i < table.rows.size(); i++) {
            java.util.Map<String, String> row = table.rows.get(i);
            int lineNo = i + 2;

            String code = required(row, "INSTR-CODE", path, lineNo);
            String name = required(row, "INSTR-NAME", path, lineNo);
            String tier = required(row, "INSTR-TIER", path, lineNo);
            long csvTick = parseLong(required(row, "TICK-AMT", path, lineNo), "TICK-AMT", path, lineNo);
            long lot = parseLong(required(row, "LOT-QTY", path, lineNo), "LOT-QTY", path, lineNo);
            String board = required(row, "BOARD-CODE", path, lineNo);

            if (rows.containsKey(code)) {
                throw new ValidationException("銘柄コードが重複しています。ファイル=" + path + ", 行=" + lineNo
                        + ", 銘柄コード=" + code);
            }
            TierRule tierRule = TIER_RULES.get(tier);
            if (tierRule == null) {
                throw new ValidationException("銘柄階層が不正です。ファイル=" + path + ", 行=" + lineNo
                        + ", 銘柄階層=" + tier);
            }
            if (csvTick < 0L) {
                throw new ValidationException("負の呼値は使用できません。ファイル=" + path + ", 行=" + lineNo
                        + ", 銘柄コード=" + code);
            }
            if (csvTick != tierRule.tickAmount) {
                throw new ValidationException("銘柄階層と呼値が一致しません。ファイル=" + path + ", 行=" + lineNo
                        + ", 銘柄コード=" + code);
            }
            if (lot <= 0L) {
                throw new ValidationException("売買単位が不正です。ファイル=" + path + ", 行=" + lineNo
                        + ", 銘柄コード=" + code);
            }
            if (!BOARD_CODES.contains(board)) {
                throw new ValidationException("市場コードが不正です。ファイル=" + path + ", 行=" + lineNo
                        + ", 市場コード=" + board);
            }

            long sampleNotional = csvTick * lot;
            int decisionCode = sampleNotional > MIHFT_MAX_NOTIONAL ? 8 : 0;
            rows.put(code, new InstrumentRow(code, name, tier, tierRule.marginRateBp, csvTick, lot, board, decisionCode));
        }

        return rows;
    }

    private static java.util.Map<String, FeeRow> readFees(String path) throws java.io.IOException {
        CsvTable table = CsvTable.read(path);
        java.util.Map<String, FeeRow> rows = new java.util.LinkedHashMap<String, FeeRow>();

        for (int i = 0; i < table.rows.size(); i++) {
            java.util.Map<String, String> row = table.rows.get(i);
            int lineNo = i + 2;

            String board = required(row, "BOARD-CODE", path, lineNo);
            long feeRateBp = parseScaledBp(required(row, "FEE-RATE", path, lineNo), "FEE-RATE", path, lineNo);
            long minFeeAmount = parseLong(required(row, "MIN-FEE-AMT", path, lineNo), "MIN-FEE-AMT", path, lineNo);

            if (!BOARD_CODES.contains(board)) {
                throw new ValidationException("市場コードが不正です。ファイル=" + path + ", 行=" + lineNo
                        + ", 市場コード=" + board);
            }
            if (rows.containsKey(board)) {
                throw new ValidationException("市場コードが重複しています。ファイル=" + path + ", 行=" + lineNo
                        + ", 市場コード=" + board);
            }
            if (feeRateBp < 0L || minFeeAmount < 0L) {
                throw new ValidationException("手数料が不正です。ファイル=" + path + ", 行=" + lineNo
                        + ", 市場コード=" + board);
            }

            rows.put(board, new FeeRow(board, feeRateBp, minFeeAmount));
        }

        return rows;
    }

    private static java.util.List<SessionRow> readSessions(String path) throws java.io.IOException {
        CsvTable table = CsvTable.read(path);
        java.util.List<SessionRow> rows = new java.util.ArrayList<SessionRow>();
        java.util.Set<String> keys = new java.util.HashSet<String>();

        for (int i = 0; i < table.rows.size(); i++) {
            java.util.Map<String, String> row = table.rows.get(i);
            int lineNo = i + 2;

            String date = required(row, "SESS-DT", path, lineNo);
            String kubun = required(row, "SESS-KBN", path, lineNo);
            long openTs = parseTimestamp(required(row, "OPEN-TS", path, lineNo), "OPEN-TS", path, lineNo);
            long closeTs = parseTimestamp(required(row, "CLOSE-TS", path, lineNo), "CLOSE-TS", path, lineNo);
            String key = date + "\u0001" + kubun;

            if (!keys.add(key)) {
                throw new ValidationException("セッションキーが重複しています。ファイル=" + path + ", 行=" + lineNo
                        + ", 日付=" + date + ", 区分=" + kubun);
            }
            if (closeTs < openTs) {
                throw new ValidationException("セッション時刻が逆転しています。ファイル=" + path + ", 行=" + lineNo
                        + ", 日付=" + date + ", 区分=" + kubun);
            }

            rows.add(new SessionRow(date, kubun, openTs, closeTs));
        }

        return rows;
    }

    private static String required(java.util.Map<String, String> row, String name, String path, int lineNo) {
        String value = row.get(name);
        if (value == null || value.trim().isEmpty()) {
            throw new ValidationException("キー項目または必須項目が欠落しています。ファイル=" + path
                    + ", 行=" + lineNo + ", 項目=" + name);
        }
        return value.trim();
    }

    private static long parseLong(String value, String name, String path, int lineNo) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            throw new ValidationException("数値項目が不正です。ファイル=" + path + ", 行=" + lineNo
                    + ", 項目=" + name + ", 値=" + value);
        }
    }

    private static long parseScaledBp(String value, String name, String path, int lineNo) {
        try {
            java.math.BigDecimal decimal = new java.math.BigDecimal(value);
            return decimal.multiply(new java.math.BigDecimal("10000")).setScale(0, java.math.RoundingMode.HALF_UP).longValueExact();
        } catch (ArithmeticException e) {
            throw new ValidationException("手数料率の丸めに失敗しました。ファイル=" + path + ", 行=" + lineNo
                    + ", 項目=" + name + ", 値=" + value);
        } catch (NumberFormatException e) {
            throw new ValidationException("手数料率が不正です。ファイル=" + path + ", 行=" + lineNo
                    + ", 項目=" + name + ", 値=" + value);
        }
    }

    private static long parseTimestamp(String value, String name, String path, int lineNo) {
        String compact = value.replace(":", "").replace("-", "").replace("/", "").replace(" ", "").replace("T", "");
        if (compact.length() != 14 && compact.length() != 6) {
            throw new ValidationException("時刻形式が不正です。ファイル=" + path + ", 行=" + lineNo
                    + ", 項目=" + name + ", 値=" + value);
        }
        return parseLong(compact, name, path, lineNo);
    }

    private static final class CsvTable {
        private final java.util.List<String> headers;
        private final java.util.List<java.util.Map<String, String>> rows;

        private CsvTable(java.util.List<String> headers, java.util.List<java.util.Map<String, String>> rows) {
            this.headers = headers;
            this.rows = rows;
        }

        private static CsvTable read(String path) throws java.io.IOException {
            java.util.List<String> lines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
            if (lines.isEmpty()) {
                throw new ValidationException("ヘッダーがありません。ファイル=" + path);
            }

            java.util.List<String> headers = parseLine(lines.get(0));
            java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<java.util.Map<String, String>>();

            for (int i = 1; i < lines.size(); i++) {
                if (lines.get(i).trim().isEmpty()) {
                    continue;
                }
                java.util.List<String> values = parseLine(lines.get(i));
                if (values.size() != headers.size()) {
                    throw new ValidationException("列数が一致しません。ファイル=" + path + ", 行=" + (i + 1));
                }

                java.util.Map<String, String> row = new java.util.LinkedHashMap<String, String>();
                for (int j = 0; j < headers.size(); j++) {
                    row.put(headers.get(j), values.get(j));
                }
                rows.add(row);
            }

            return new CsvTable(headers, rows);
        }

        private static java.util.List<String> parseLine(String line) {
            java.util.List<String> values = new java.util.ArrayList<String>();
            StringBuilder cell = new StringBuilder();
            boolean quoted = false;

            for (int i = 0; i < line.length(); i++) {
                char ch = line.charAt(i);
                if (ch == '"') {
                    if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        cell.append('"');
                        i++;
                    } else {
                        quoted = !quoted;
                    }
                } else if (ch == ',' && !quoted) {
                    values.add(cell.toString().trim());
                    cell.setLength(0);
                } else {
                    cell.append(ch);
                }
            }
            values.add(cell.toString().trim());
            return values;
        }
    }

    private static final class RefDataCache {
        private final java.util.Map<String, InstrumentRow> instruments;
        private final java.util.Map<String, FeeRow> fees;
        private final java.util.List<SessionRow> sessions;

        private RefDataCache(java.util.Map<String, InstrumentRow> instruments,
                             java.util.Map<String, FeeRow> fees,
                             java.util.List<SessionRow> sessions) {
            this.instruments = instruments;
            this.fees = fees;
            this.sessions = sessions;
        }
    }

    private static final class InstrumentRow {
        private final String instrumentCode;
        private final String instrumentName;
        private final String instrumentTier;
        private final long marginRateBp;
        private final long tickAmount;
        private final long lotQuantity;
        private final String boardCode;
        private final int sampleDecisionCode;

        private InstrumentRow(String instrumentCode, String instrumentName, String instrumentTier,
                              long marginRateBp, long tickAmount, long lotQuantity,
                              String boardCode, int sampleDecisionCode) {
            this.instrumentCode = instrumentCode;
            this.instrumentName = instrumentName;
            this.instrumentTier = instrumentTier;
            this.marginRateBp = marginRateBp;
            this.tickAmount = tickAmount;
            this.lotQuantity = lotQuantity;
            this.boardCode = boardCode;
            this.sampleDecisionCode = sampleDecisionCode;
        }
    }

    private static final class FeeRow {
        private final String boardCode;
        private final long feeRateBp;
        private final long minFeeAmount;

        private FeeRow(String boardCode, long feeRateBp, long minFeeAmount) {
            this.boardCode = boardCode;
            this.feeRateBp = feeRateBp;
            this.minFeeAmount = minFeeAmount;
        }
    }

    private static final class SessionRow {
        private final String sessionDate;
        private final String sessionKubun;
        private final long openTs;
        private final long closeTs;

        private SessionRow(String sessionDate, String sessionKubun, long openTs, long closeTs) {
            this.sessionDate = sessionDate;
            this.sessionKubun = sessionKubun;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }

    private static final class TierRule {
        private final long marginRateBp;
        private final long tickAmount;

        private TierRule(long marginRateBp, long tickAmount) {
            this.marginRateBp = marginRateBp;
            this.tickAmount = tickAmount;
        }
    }

    private static final class ValidationException extends RuntimeException {
        private ValidationException(String message) {
            super(message);
        }
    }
}
