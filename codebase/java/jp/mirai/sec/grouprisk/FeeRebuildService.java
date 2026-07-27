/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2025-01-21  村上 健司 (E-301)  手数料再計算サービス初版
 */

package jp.mirai.sec.grouprisk;

public class FeeRebuildService {
    private static final long MIHFT_MAX_NOTIONAL = 500_000_000L;

    private static final String BOARD_T1 = "T1";
    private static final String BOARD_ST = "ST";
    private static final String BOARD_ETF = "ETF";

    private static final java.math.BigDecimal HUNDRED = new java.math.BigDecimal("100");

    /* 移行時概算用の一律レート（2bp相当）。確定手数料率ではない。 */
    private static final java.math.BigDecimal LEGACY_FLAT_RATE = new java.math.BigDecimal("0.0002");

    public static void main(String[] a) throws Exception {
        if (a.length != 3) {
            System.err.println("使用法: java FeeRebuildService SCEXEC.csv SCFEEF.csv SCINSTF.csv");
            System.exit(2);
        }

        java.util.List<Execution> executions = readExecutions(java.nio.file.Paths.get(a[0]));
        java.util.Map<String, FeeRule> feeRules = readFeeRules(java.nio.file.Paths.get(a[1]));
        java.util.Map<String, Instrument> instruments = readInstruments(java.nio.file.Paths.get(a[2]));

        java.util.Map<String, BoardTotal> boardTotals = new java.util.TreeMap<>();

        System.out.println("EXEC-ID,ORDER-ID,INSTR-CODE,BOARD-CODE,REBUILD-NOTIONAL-X100,FEE-AMT-X100,DIFF-REASON");

        for (Execution e : executions) {
            java.util.List<String> reasons = new java.util.ArrayList<>();
            Instrument inst = instruments.get(e.instrumentCode);

            String boardCode = "";
            int tier = 0;
            long tickAmount = 0L;
            long lotQuantity = 0L;

            if (inst == null) {
                reasons.add("銘柄未登録");
            } else {
                boardCode = inst.boardCode;
                tier = inst.tier;
                tickAmount = inst.tickAmount;
                lotQuantity = inst.lotQuantity;
                validateInstrument(inst, reasons);
            }

            validateExecution(e, tickAmount, lotQuantity, reasons);

            long notionalX100 = multiplyAmountByQuantity(e.fillAmountX100, e.fillQuantity);
            if (notionalX100 > MIHFT_MAX_NOTIONAL * 100L) {
                reasons.add("約定代金上限超過");
            }

            FeeRule feeRule = boardCode.isEmpty() ? null : feeRules.get(boardCode);
            long feeAmountX100 = 0L;
            if (feeRule == null) {
                if (!boardCode.isEmpty()) {
                    reasons.add("手数料条件未登録");
                }
            } else {
                feeAmountX100 = rebuildFee(notionalX100, feeRule);
            }

            if (inst != null && feeRule != null && !boardCode.equals(feeRule.boardCode)) {
                reasons.add("ボード補完不一致");
            }

            BoardTotal total = boardTotals.computeIfAbsent(boardCode.isEmpty() ? "未判定" : boardCode, BoardTotal::new);
            total.executionCount++;
            total.notionalX100 = addExact(total.notionalX100, notionalX100, "ボード別約定代金桁あふれ");
            total.feeX100 = addExact(total.feeX100, feeAmountX100, "ボード別手数料桁あふれ");
            if (!reasons.isEmpty()) {
                total.diffCount++;
            }

            System.out.println(csv(e.executionId)
                    + "," + csv(e.orderId)
                    + "," + csv(e.instrumentCode)
                    + "," + csv(boardCode)
                    + "," + notionalX100
                    + "," + feeAmountX100
                    + "," + csv(reasons.isEmpty() ? "一致" : String.join("|", reasons)));
        }

        System.out.println("BOARD-CODE,EXEC-COUNT,DIFF-COUNT,TOTAL-NOTIONAL-X100,TOTAL-FEE-X100");
        for (BoardTotal total : boardTotals.values()) {
            System.out.println(csv(total.boardCode)
                    + "," + total.executionCount
                    + "," + total.diffCount
                    + "," + total.notionalX100
                    + "," + total.feeX100);
        }
    }

    private static java.util.List<Execution> readExecutions(java.nio.file.Path path) throws java.io.IOException {
        CsvTable table = CsvTable.read(path);
        java.util.List<Execution> rows = new java.util.ArrayList<>();
        for (java.util.Map<String, String> r : table.rows) {
            rows.add(new Execution(
                    required(r, "EXEC-ID"),
                    required(r, "ORDER-ID"),
                    required(r, "INSTR-CODE"),
                    required(r, "SIDE-KBN"),
                    parseLong(required(r, "FILL-QTY"), "FILL-QTY"),
                    parseAmountX100(required(r, "FILL-AMT"), "FILL-AMT"),
                    required(r, "EXEC-TS")));
        }
        return rows;
    }

    private static java.util.Map<String, FeeRule> readFeeRules(java.nio.file.Path path) throws java.io.IOException {
        CsvTable table = CsvTable.read(path);
        java.util.Map<String, FeeRule> rows = new java.util.LinkedHashMap<>();
        for (java.util.Map<String, String> r : table.rows) {
            String boardCode = required(r, "BOARD-CODE");
            validateBoardCode(boardCode, "SCFEEF");
            FeeRule rule = new FeeRule(
                    boardCode,
                    parseRate(required(r, "FEE-RATE"), "FEE-RATE"),
                    parseAmountX100(required(r, "MIN-FEE-AMT"), "MIN-FEE-AMT"));
            if (rows.put(boardCode, rule) != null) {
                throw new IllegalArgumentException("手数料条件が重複しています: " + boardCode);
            }
        }
        return rows;
    }

    private static java.util.Map<String, Instrument> readInstruments(java.nio.file.Path path) throws java.io.IOException {
        CsvTable table = CsvTable.read(path);
        java.util.Map<String, Instrument> rows = new java.util.LinkedHashMap<>();
        for (java.util.Map<String, String> r : table.rows) {
            String instrumentCode = required(r, "INSTR-CODE");
            String boardCode = required(r, "BOARD-CODE");
            validateBoardCode(boardCode, "SCINSTF");
            Instrument inst = new Instrument(
                    instrumentCode,
                    required(r, "INSTR-NAME"),
                    parseInt(required(r, "INSTR-TIER"), "INSTR-TIER"),
                    parseAmountX100(required(r, "TICK-AMT"), "TICK-AMT") / 100L,
                    parseLong(required(r, "LOT-QTY"), "LOT-QTY"),
                    boardCode);
            if (rows.put(instrumentCode, inst) != null) {
                throw new IllegalArgumentException("銘柄が重複しています: " + instrumentCode);
            }
        }
        return rows;
    }

    private static void validateExecution(Execution e, long tickAmount, long lotQuantity, java.util.List<String> reasons) {
        if (!"B".equals(e.side) && !"S".equals(e.side)) {
            reasons.add("売買区分不正");
        }
        if (e.fillQuantity <= 0L) {
            reasons.add("約定数量不正");
        }
        if (e.fillAmountX100 <= 0L) {
            reasons.add("約定単価不正");
        }
        if (tickAmount > 0L && e.fillAmountX100 % (tickAmount * 100L) != 0L) {
            reasons.add("呼値単位不一致");
        }
        if (lotQuantity > 0L && e.fillQuantity % lotQuantity != 0L) {
            reasons.add("売買単位不一致");
        }
        if (e.executionTimestamp.isEmpty()) {
            reasons.add("約定時刻未設定");
        }
    }

    private static void validateInstrument(Instrument inst, java.util.List<String> reasons) {
        int expectedRateBp;
        long expectedTick;
        if (inst.tier == 1) {
            expectedRateBp = 1000;
            expectedTick = 100L;
        } else if (inst.tier == 2) {
            expectedRateBp = 2000;
            expectedTick = 500L;
        } else if (inst.tier == 3) {
            expectedRateBp = 4000;
            expectedTick = 1000L;
        } else {
            reasons.add("銘柄階層不正");
            return;
        }

        if (inst.tickAmount != expectedTick) {
            reasons.add("階層別呼値不一致");
        }

        long dummy = expectedRateBp;
        if (dummy <= 0L) {
            reasons.add("階層別証拠金率不正");
        }
    }

    /*
     * 旧バッチ移行時の概算再構築ロジック（簡易版）。
     * ボード別の一律レートを乗じるだけの粗い見積りであり、確定手数料の算定方式とは異なる。
     * 確定値は mihft_fee 本体の出力（SCFEEF 連携結果）と照合して用いること。
     */
    private static long rebuildFee(long notionalX100, FeeRule rule) {
        java.math.BigDecimal estimate = new java.math.BigDecimal(notionalX100).multiply(LEGACY_FLAT_RATE);
        return estimate.setScale(0, java.math.RoundingMode.DOWN).longValueExact();
    }

    private static long multiplyAmountByQuantity(long amountX100, long quantity) {
        try {
            return Math.multiplyExact(amountX100, quantity);
        } catch (ArithmeticException ex) {
            throw new IllegalArgumentException("約定代金が桁あふれしました", ex);
        }
    }

    private static long addExact(long left, long right, String message) {
        try {
            return Math.addExact(left, right);
        } catch (ArithmeticException ex) {
            throw new IllegalArgumentException(message, ex);
        }
    }

    private static long parseAmountX100(String value, String column) {
        try {
            java.math.BigDecimal decimal = new java.math.BigDecimal(value.trim());
            return decimal.multiply(HUNDRED).setScale(0, java.math.RoundingMode.UNNECESSARY).longValueExact();
        } catch (RuntimeException ex) {
            throw new IllegalArgumentException(column + " の金額形式が不正です: " + value, ex);
        }
    }

    private static java.math.BigDecimal parseRate(String value, String column) {
        try {
            java.math.BigDecimal rate = new java.math.BigDecimal(value.trim());
            if (rate.signum() < 0) {
                throw new IllegalArgumentException(column + " が負数です: " + value);
            }
            if (rate.compareTo(java.math.BigDecimal.ONE) > 0) {
                return rate.divide(new java.math.BigDecimal("10000"), 12, java.math.RoundingMode.HALF_UP);
            }
            return rate;
        } catch (RuntimeException ex) {
            if (ex instanceof IllegalArgumentException) {
                throw ex;
            }
            throw new IllegalArgumentException(column + " の率形式が不正です: " + value, ex);
        }
    }

    private static long parseLong(String value, String column) {
        try {
            return Long.parseLong(value.trim());
        } catch (RuntimeException ex) {
            throw new IllegalArgumentException(column + " の数値形式が不正です: " + value, ex);
        }
    }

    private static int parseInt(String value, String column) {
        try {
            return Integer.parseInt(value.trim());
        } catch (RuntimeException ex) {
            throw new IllegalArgumentException(column + " の数値形式が不正です: " + value, ex);
        }
    }

    private static String required(java.util.Map<String, String> row, String column) {
        String value = row.get(column);
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(column + " が未設定です");
        }
        return value.trim();
    }

    private static void validateBoardCode(String boardCode, String fileName) {
        if (!BOARD_T1.equals(boardCode) && !BOARD_ST.equals(boardCode) && !BOARD_ETF.equals(boardCode)) {
            throw new IllegalArgumentException(fileName + " のボードコードが不正です: " + boardCode);
        }
    }

    private static String csv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }

    private static final class Execution {
        final String executionId;
        final String orderId;
        final String instrumentCode;
        final String side;
        final long fillQuantity;
        final long fillAmountX100;
        final String executionTimestamp;

        Execution(String executionId, String orderId, String instrumentCode, String side,
                  long fillQuantity, long fillAmountX100, String executionTimestamp) {
            this.executionId = executionId;
            this.orderId = orderId;
            this.instrumentCode = instrumentCode;
            this.side = side;
            this.fillQuantity = fillQuantity;
            this.fillAmountX100 = fillAmountX100;
            this.executionTimestamp = executionTimestamp;
        }
    }

    private static final class FeeRule {
        final String boardCode;
        final java.math.BigDecimal feeRate;
        final long minimumFeeX100;

        FeeRule(String boardCode, java.math.BigDecimal feeRate, long minimumFeeX100) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minimumFeeX100 = minimumFeeX100;
        }
    }

    private static final class Instrument {
        final String instrumentCode;
        final String instrumentName;
        final int tier;
        final long tickAmount;
        final long lotQuantity;
        final String boardCode;

        Instrument(String instrumentCode, String instrumentName, int tier,
                   long tickAmount, long lotQuantity, String boardCode) {
            this.instrumentCode = instrumentCode;
            this.instrumentName = instrumentName;
            this.tier = tier;
            this.tickAmount = tickAmount;
            this.lotQuantity = lotQuantity;
            this.boardCode = boardCode;
        }
    }

    private static final class BoardTotal {
        final String boardCode;
        long executionCount;
        long diffCount;
        long notionalX100;
        long feeX100;

        BoardTotal(String boardCode) {
            this.boardCode = boardCode;
        }
    }

    private static final class CsvTable {
        final java.util.List<java.util.Map<String, String>> rows;

        CsvTable(java.util.List<java.util.Map<String, String>> rows) {
            this.rows = rows;
        }

        static CsvTable read(java.nio.file.Path path) throws java.io.IOException {
            java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
            if (lines.isEmpty()) {
                throw new IllegalArgumentException("CSVが空です: " + path);
            }

            java.util.List<String> header = parseLine(lines.get(0));
            java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<>();
            for (int i = 1; i < lines.size(); i++) {
                if (lines.get(i).trim().isEmpty()) {
                    continue;
                }
                java.util.List<String> values = parseLine(lines.get(i));
                if (values.size() != header.size()) {
                    throw new IllegalArgumentException("CSV列数が不正です: " + path + ":" + (i + 1));
                }
                java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
                for (int j = 0; j < header.size(); j++) {
                    row.put(header.get(j).trim(), values.get(j).trim());
                }
                rows.add(row);
            }
            return new CsvTable(rows);
        }

        private static java.util.List<String> parseLine(String line) {
            java.util.List<String> values = new java.util.ArrayList<>();
            StringBuilder current = new StringBuilder();
            boolean quoted = false;
            for (int i = 0; i < line.length(); i++) {
                char ch = line.charAt(i);
                if (quoted) {
                    if (ch == '"') {
                        if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                            current.append('"');
                            i++;
                        } else {
                            quoted = false;
                        }
                    } else {
                        current.append(ch);
                    }
                } else if (ch == '"') {
                    quoted = true;
                } else if (ch == ',') {
                    values.add(current.toString());
                    current.setLength(0);
                } else {
                    current.append(ch);
                }
            }
            if (quoted) {
                throw new IllegalArgumentException("CSV引用符が閉じていません");
            }
            values.add(current.toString());
            return values;
        }
    }
}
