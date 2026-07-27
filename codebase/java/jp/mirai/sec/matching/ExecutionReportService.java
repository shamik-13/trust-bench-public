/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2025-01-21  渡辺 隆 (E-260)    初版作成
 */

package jp.mirai.sec.matching;

public class ExecutionReportService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final String SIDE_BUY = "B";
    private static final String SIDE_SELL = "S";

    private static final java.time.format.DateTimeFormatter TS_FORMAT =
            java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

    public static void main(String[] a) throws Exception {
        if (a.length != 3) {
            System.err.println("使用方法: java ExecutionReportService SCEXEC.csv SCORDS.csv SCDROP.csv");
            System.exit(2);
        }

        java.nio.file.Path execPath = java.nio.file.Paths.get(a[0]);
        java.nio.file.Path ordsPath = java.nio.file.Paths.get(a[1]);
        java.nio.file.Path dropPath = java.nio.file.Paths.get(a[2]);

        java.util.Map<String, OrderRecord> orders = readOrders(ordsPath);
        java.util.List<ExecutionRecord> executions = readExecutions(execPath);

        DropSequencer sequencer = new DropSequencer();
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("DROP-ID,EXEC-ID,ORDER-ID,CIF-NO,INSTR-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,SEND-TS");

        int written = 0;
        int skipped = 0;

        for (ExecutionRecord exec : executions) {
            OrderRecord order = orders.get(exec.orderId);
            if (order == null) {
                System.err.println("注文未検出: EXEC-ID=" + exec.execId + " ORDER-ID=" + exec.orderId);
                skipped++;
                continue;
            }

            if (!exec.instrCode.equals(order.instrCode)) {
                System.err.println("銘柄不一致: EXEC-ID=" + exec.execId + " ORDER-ID=" + exec.orderId);
                skipped++;
                continue;
            }

            if (!isValidSide(exec.sideKbn)) {
                System.err.println("売買区分不正: EXEC-ID=" + exec.execId + " SIDE-KBN=" + exec.sideKbn);
                skipped++;
                continue;
            }

            InstrumentMeta meta = RefDataService.lookupInstrument(exec.instrCode);
            if (meta == null) {
                System.err.println("銘柄属性未検出: INSTR-CODE=" + exec.instrCode);
                skipped++;
                continue;
            }

            BoardMeta board = RefDataService.lookupBoard(meta.boardCode);
            if (board == null || !board.active) {
                System.err.println("市場属性不正: INSTR-CODE=" + exec.instrCode + " BOARD-CODE=" + meta.boardCode);
                skipped++;
                continue;
            }

            long price = exec.fillAmt / exec.fillQty;
            if (price <= 0L || price % meta.tickSize != 0L) {
                System.err.println("呼値不正: EXEC-ID=" + exec.execId + " PRICE=" + price);
                skipped++;
                continue;
            }

            long notional = exec.fillAmt;
            if (notional > MIHFT_MAX_NOTIONAL) {
                System.err.println("約定代金上限超過: EXEC-ID=" + exec.execId + " FILL-AMT=" + exec.fillAmt);
                skipped++;
                continue;
            }

            long expectedCum = order.cumQty + exec.fillQty;
            long expectedLeaves = order.leavesQty - exec.fillQty;
            if (expectedLeaves < 0L) {
                System.err.println("残数量超過: EXEC-ID=" + exec.execId + " ORDER-ID=" + exec.orderId);
                skipped++;
                continue;
            }

            long avgAfter = computeAverageAmount(order.cumQty, order.avgFillAmt, exec.fillQty, exec.fillAmt);
            order.cumQty = expectedCum;
            order.leavesQty = expectedLeaves;
            order.avgFillAmt = avgAfter;
            order.lastUpdTs = maxTimestamp(order.lastUpdTs, exec.execTs);

            String dropId = sequencer.nextDropId(exec.execTs);
            String sendTs = java.time.LocalDateTime.now().format(TS_FORMAT);

            DropRecord drop = new DropRecord(
                    dropId,
                    exec.execId,
                    exec.orderId,
                    order.cifNo,
                    exec.instrCode,
                    exec.sideKbn,
                    exec.fillQty,
                    exec.fillAmt,
                    sendTs);

            sequencer.accept(drop);
            out.add(drop.toCsv());
            written++;
        }

        java.nio.file.Files.write(dropPath, out, java.nio.charset.StandardCharsets.UTF_8);
        System.err.println("約定通知作成完了: 出力=" + written + " 除外=" + skipped);
    }

    private static java.util.Map<String, OrderRecord> readOrders(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, OrderRecord> map = new java.util.LinkedHashMap<>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);

        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            if (line.isEmpty()) {
                continue;
            }
            if (i == 0 && line.startsWith("ORDER-ID,")) {
                continue;
            }

            java.util.List<String> c = parseCsv(line);
            if (c.size() != 8) {
                throw new IllegalArgumentException("SCORDS項目数不正: 行=" + (i + 1));
            }

            OrderRecord r = new OrderRecord(
                    c.get(0),
                    c.get(1),
                    c.get(2),
                    c.get(3),
                    parseLong(c.get(4), "LEAVES-QTY", i),
                    parseLong(c.get(5), "CUM-QTY", i),
                    parseLong(c.get(6), "AVG-FILL-AMT", i),
                    c.get(7));

            if (r.orderId.isEmpty() || r.cifNo.isEmpty() || r.instrCode.isEmpty()) {
                throw new IllegalArgumentException("SCORDSキー項目不正: 行=" + (i + 1));
            }
            map.put(r.orderId, r);
        }

        return map;
    }

    private static java.util.List<ExecutionRecord> readExecutions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<ExecutionRecord> list = new java.util.ArrayList<>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);

        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            if (line.isEmpty()) {
                continue;
            }
            if (i == 0 && line.startsWith("EXEC-ID,")) {
                continue;
            }

            java.util.List<String> c = parseCsv(line);
            if (c.size() != 7) {
                throw new IllegalArgumentException("SCEXEC項目数不正: 行=" + (i + 1));
            }

            long fillQty = parseLong(c.get(4), "FILL-QTY", i);
            long fillAmt = parseLong(c.get(5), "FILL-AMT", i);
            if (fillQty <= 0L || fillAmt <= 0L) {
                throw new IllegalArgumentException("約定数量金額不正: 行=" + (i + 1));
            }

            ExecutionRecord r = new ExecutionRecord(
                    c.get(0),
                    c.get(1),
                    c.get(2),
                    c.get(3),
                    fillQty,
                    fillAmt,
                    c.get(6));

            if (r.execId.isEmpty() || r.orderId.isEmpty() || r.instrCode.isEmpty()) {
                throw new IllegalArgumentException("SCEXECキー項目不正: 行=" + (i + 1));
            }
            list.add(r);
        }

        list.sort(java.util.Comparator
                .comparing((ExecutionRecord r) -> r.execTs)
                .thenComparing(r -> r.execId));
        return list;
    }

    private static long computeAverageAmount(long oldQty, long oldAvgAmt, long fillQty, long fillAmt) {
        long totalQty = oldQty + fillQty;
        if (totalQty == 0L) {
            return 0L;
        }
        long oldTotal = oldQty * oldAvgAmt;
        return (oldTotal + fillAmt) / totalQty;
    }

    private static boolean isValidSide(String sideKbn) {
        return SIDE_BUY.equals(sideKbn) || SIDE_SELL.equals(sideKbn);
    }

    private static String maxTimestamp(String left, String right) {
        if (left == null || left.isEmpty()) {
            return right;
        }
        if (right == null || right.isEmpty()) {
            return left;
        }
        return left.compareTo(right) >= 0 ? left : right;
    }

    private static long parseLong(String v, String name, int lineIndex) {
        try {
            return Long.parseLong(v.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(name + "数値不正: 行=" + (lineIndex + 1), e);
        }
    }

    private static java.util.List<String> parseCsv(String line) {
        java.util.List<String> values = new java.util.ArrayList<>();
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
                values.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }

        values.add(b.toString().trim());
        return values;
    }

    private static final class ExecutionRecord {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final String execTs;

        ExecutionRecord(String execId, String orderId, String instrCode, String sideKbn,
                        long fillQty, long fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class OrderRecord {
        final String orderId;
        final String cifNo;
        final String instrCode;
        String stateKbn;
        long leavesQty;
        long cumQty;
        long avgFillAmt;
        String lastUpdTs;

        OrderRecord(String orderId, String cifNo, String instrCode, String stateKbn,
                    long leavesQty, long cumQty, long avgFillAmt, String lastUpdTs) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.stateKbn = stateKbn;
            this.leavesQty = leavesQty;
            this.cumQty = cumQty;
            this.avgFillAmt = avgFillAmt;
            this.lastUpdTs = lastUpdTs;
        }
    }

    private static final class DropRecord {
        final String dropId;
        final String execId;
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final String sendTs;

        DropRecord(String dropId, String execId, String orderId, String cifNo, String instrCode,
                   String sideKbn, long fillQty, long fillAmt, String sendTs) {
            this.dropId = dropId;
            this.execId = execId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.sendTs = sendTs;
        }

        String toCsv() {
            return escape(dropId) + ","
                    + escape(execId) + ","
                    + escape(orderId) + ","
                    + escape(cifNo) + ","
                    + escape(instrCode) + ","
                    + escape(sideKbn) + ","
                    + fillQty + ","
                    + fillAmt + ","
                    + escape(sendTs);
        }

        private static String escape(String v) {
            if (v == null) {
                return "";
            }
            if (v.indexOf(',') < 0 && v.indexOf('"') < 0 && v.indexOf('\n') < 0 && v.indexOf('\r') < 0) {
                return v;
            }
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
    }

    private static final class InstrumentMeta {
        final String instrCode;
        final int tier;
        final int marginRateBp;
        final long tickSize;
        final String boardCode;

        InstrumentMeta(String instrCode, int tier, int marginRateBp, long tickSize, String boardCode) {
            this.instrCode = instrCode;
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tickSize = tickSize;
            this.boardCode = boardCode;
        }
    }

    private static final class BoardMeta {
        final String boardCode;
        final boolean active;

        BoardMeta(String boardCode, boolean active) {
            this.boardCode = boardCode;
            this.active = active;
        }
    }

    private static final class RefDataService {
        private static final java.util.Map<String, BoardMeta> BOARDS = new java.util.HashMap<>();
        private static final java.util.Map<String, InstrumentMeta> INSTRUMENTS = new java.util.HashMap<>();

        static {
            BOARDS.put("T1", new BoardMeta("T1", true));
            BOARDS.put("ST", new BoardMeta("ST", true));
            BOARDS.put("ETF", new BoardMeta("ETF", true));

            add("7203", 1, "T1");
            add("6758", 1, "T1");
            add("8306", 1, "T1");
            add("9984", 1, "T1");
            add("9432", 1, "T1");
            add("4755", 2, "T1");
            add("4385", 2, "ST");
            add("4478", 3, "ST");
            add("1306", 1, "ETF");
            add("1321", 1, "ETF");
        }

        static InstrumentMeta lookupInstrument(String instrCode) {
            InstrumentMeta meta = INSTRUMENTS.get(instrCode);
            if (meta != null) {
                return meta;
            }
            if (instrCode != null && instrCode.length() == 4 && instrCode.startsWith("1")) {
                return new InstrumentMeta(instrCode, 1, 1000, 100L, "ETF");
            }
            if (instrCode != null && instrCode.length() == 4 && instrCode.startsWith("4")) {
                return new InstrumentMeta(instrCode, 3, 4000, 1000L, "ST");
            }
            if (instrCode != null && instrCode.length() == 4) {
                return new InstrumentMeta(instrCode, 2, 2000, 500L, "T1");
            }
            return null;
        }

        static BoardMeta lookupBoard(String boardCode) {
            return BOARDS.get(boardCode);
        }

        private static void add(String instrCode, int tier, String boardCode) {
            if (tier == 1) {
                INSTRUMENTS.put(instrCode, new InstrumentMeta(instrCode, tier, 1000, 100L, boardCode));
            } else if (tier == 2) {
                INSTRUMENTS.put(instrCode, new InstrumentMeta(instrCode, tier, 2000, 500L, boardCode));
            } else if (tier == 3) {
                INSTRUMENTS.put(instrCode, new InstrumentMeta(instrCode, tier, 4000, 1000L, boardCode));
            } else {
                throw new IllegalArgumentException("銘柄階層不正: " + tier);
            }
        }
    }

    private static final class DropSequencer {
        private long sequence = 0L;
        private final java.util.Set<String> sentExecIds = new java.util.HashSet<>();

        String nextDropId(String execTs) {
            sequence++;
            String day = execTs == null || execTs.length() < 8
                    ? java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                    : execTs.substring(0, 8);
            return "D" + day + String.format("%010d", sequence);
        }

        void accept(DropRecord drop) {
            if (!sentExecIds.add(drop.execId)) {
                throw new IllegalStateException("重複送信検出: EXEC-ID=" + drop.execId);
            }
        }
    }
}
