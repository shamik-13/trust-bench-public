/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2019-10-22  藤田 和也 (E-271)  初版作成
 */

package jp.mirai.sec.position;

public class LotLedgerService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;
    private static final String SIDE_BUY = "B";
    private static final String SIDE_SELL = "S";

    public static void main(String[] a) {
        if (a == null || a.length < 3) {
            System.err.println("使用方法: java LotLedgerService SCLOT.csv SCEXEC.csv SCPOSF.csv [出力SCLOT.csv]");
            return;
        }

        try {
            java.nio.file.Path lotPath = java.nio.file.Paths.get(a[0]);
            java.nio.file.Path execPath = java.nio.file.Paths.get(a[1]);
            java.nio.file.Path posPath = java.nio.file.Paths.get(a[2]);
            java.nio.file.Path outPath = a.length >= 4 ? java.nio.file.Paths.get(a[3]) : lotPath;

            java.util.List<Lot> lots = readLots(lotPath);
            java.util.List<Execution> executions = readExecutions(execPath);
            java.util.Map<PositionKey, Position> positions = readPositions(posPath);

            Result result = rebuild(lots, executions, positions);
            writeLots(outPath, result.openLots);

            for (String message : result.messages) {
                System.err.println(message);
            }
            System.err.println("処理完了: ロット件数=" + result.openLots.size()
                    + " 実現金額=" + result.realizedAmount
                    + " 検知件数=" + result.messages.size());
        } catch (Exception e) {
            System.err.println("異常終了: " + e.getMessage());
            System.exit(1);
        }
    }

    private static Result rebuild(
            java.util.List<Lot> sourceLots,
            java.util.List<Execution> executions,
            java.util.Map<PositionKey, Position> positions) {
        java.util.Map<LotKey, java.util.ArrayDeque<Lot>> queues = new java.util.HashMap<>();
        java.util.Set<String> execIds = new java.util.HashSet<>();
        java.util.List<String> messages = new java.util.ArrayList<>();
        java.util.List<Lot> rebuilt = new java.util.ArrayList<>();
        long realized = 0L;

        executions.sort(java.util.Comparator
                .comparing((Execution e) -> e.execTs)
                .thenComparing(e -> e.execId));

        for (Execution e : executions) {
            execIds.add(e.execId);
            if (!SIDE_BUY.equals(e.sideKbn) && !SIDE_SELL.equals(e.sideKbn)) {
                messages.add("不正売買区分: EXEC-ID=" + e.execId + " SIDE-KBN=" + e.sideKbn);
                continue;
            }
            if (e.fillQty <= 0L || e.fillAmt < 0L) {
                messages.add("不正約定数量金額: EXEC-ID=" + e.execId);
                continue;
            }
            if (e.fillAmt > MIHFT_MAX_NOTIONAL) {
                messages.add("想定元本上限超過: EXEC-ID=" + e.execId);
            }

            String cifNo = resolveCifNo(e.orderId, sourceLots, positions);
            LotKey key = new LotKey(cifNo, e.instrCode);

            if (SIDE_BUY.equals(e.sideKbn)) {
                Lot lot = new Lot(
                        createLotId(cifNo, e.instrCode, e.execId),
                        cifNo,
                        e.instrCode,
                        e.fillQty,
                        e.fillAmt,
                        e.execTs,
                        e.execId);
                queues.computeIfAbsent(key, k -> new java.util.ArrayDeque<>()).addLast(lot);
                rebuilt.add(lot);
            } else {
                long remainQty = e.fillQty;
                long remainAmt = e.fillAmt;
                java.util.ArrayDeque<Lot> queue = queues.computeIfAbsent(key, k -> new java.util.ArrayDeque<>());

                while (remainQty > 0L && !queue.isEmpty()) {
                    Lot head = queue.peekFirst();
                    long closeQty = Math.min(head.openQty, remainQty);
                    long closeCost = prorate(head.openAmt, closeQty, head.openQty);
                    long closeProceed = prorate(remainAmt, closeQty, remainQty);

                    head.openQty -= closeQty;
                    head.openAmt -= closeCost;
                    remainQty -= closeQty;
                    remainAmt -= closeProceed;
                    realized += closeProceed - closeCost;

                    if (head.openQty < 0L) {
                        messages.add("OPEN-QTY負数検知: LOT-ID=" + head.lotId);
                    }
                    if (head.openQty == 0L) {
                        queue.removeFirst();
                    }
                }

                if (remainQty > 0L) {
                    messages.add("売却超過検知: EXEC-ID=" + e.execId + " 未消込数量=" + remainQty);
                    Lot shortLot = new Lot(
                            createLotId(cifNo, e.instrCode, e.execId) + "-S",
                            cifNo,
                            e.instrCode,
                            -remainQty,
                            -remainAmt,
                            e.execTs,
                            e.execId);
                    rebuilt.add(shortLot);
                    queue.addLast(shortLot);
                }
            }
        }

        for (Lot lot : sourceLots) {
            if (lot.srcExecId == null || lot.srcExecId.isEmpty() || !execIds.contains(lot.srcExecId)) {
                messages.add("SRC-EXEC-ID欠落検知: LOT-ID=" + lot.lotId + " SRC-EXEC-ID=" + nvl(lot.srcExecId));
            }
            if (lot.openQty < 0L) {
                messages.add("OPEN-QTY負数検知: LOT-ID=" + lot.lotId);
            }
            if (isFractionalLot(lot) && !containsLot(rebuilt, lot.lotId)) {
                rebuilt.add(lot);
            }
        }

        java.util.List<Lot> openLots = new java.util.ArrayList<>();
        for (Lot lot : rebuilt) {
            if (lot.openQty != 0L) {
                openLots.add(lot);
            }
        }
        openLots.sort(java.util.Comparator
                .comparing((Lot l) -> l.cifNo)
                .thenComparing(l -> l.instrCode)
                .thenComparing(l -> l.acqTs)
                .thenComparing(l -> l.lotId));

        checkPositions(openLots, positions, realized, messages);
        return new Result(openLots, messages, realized);
    }

    private static void checkPositions(
            java.util.List<Lot> lots,
            java.util.Map<PositionKey, Position> positions,
            long realized,
            java.util.List<String> messages) {
        java.util.Map<PositionKey, long[]> sums = new java.util.HashMap<>();
        for (Lot lot : lots) {
            long[] v = sums.computeIfAbsent(new PositionKey(lot.cifNo, lot.instrCode), k -> new long[2]);
            v[0] += lot.openQty;
            v[1] += lot.openAmt;
        }

        for (java.util.Map.Entry<PositionKey, Position> entry : positions.entrySet()) {
            long[] v = sums.getOrDefault(entry.getKey(), new long[2]);
            Position p = entry.getValue();
            long avg = v[0] == 0L ? 0L : v[1] / v[0];
            if (v[0] != p.netQty) {
                messages.add("残高数量不一致: CIF-NO=" + p.cifNo + " INSTR-CODE=" + p.instrCode);
            }
            if (p.netQty != 0L && Math.abs(avg - p.avgAmt) > 1L) {
                messages.add("平均単価不一致: CIF-NO=" + p.cifNo + " INSTR-CODE=" + p.instrCode);
            }
            if (p.rlzdAmt != 0L && Math.abs(realized - p.rlzdAmt) > 1L) {
                messages.add("実現金額不一致: CIF-NO=" + p.cifNo + " INSTR-CODE=" + p.instrCode);
            }
        }
    }

    private static java.util.List<Lot> readLots(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<Lot> lots = new java.util.ArrayList<>();
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = parseCsvLine(lines.get(i));
            if (c.size() < 7) {
                continue;
            }
            lots.add(new Lot(c.get(0), c.get(1), c.get(2), parseLong(c.get(3)),
                    parseLong(c.get(4)), c.get(5), c.get(6)));
        }
        return lots;
    }

    private static java.util.List<Execution> readExecutions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<Execution> executions = new java.util.ArrayList<>();
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = parseCsvLine(lines.get(i));
            if (c.size() < 7) {
                continue;
            }
            executions.add(new Execution(c.get(0), c.get(1), c.get(2), c.get(3),
                    parseLong(c.get(4)), parseLong(c.get(5)), c.get(6)));
        }
        return executions;
    }

    private static java.util.Map<PositionKey, Position> readPositions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.Map<PositionKey, Position> positions = new java.util.HashMap<>();
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = parseCsvLine(lines.get(i));
            if (c.size() < 5) {
                continue;
            }
            Position p = new Position(c.get(0), c.get(1), parseLong(c.get(2)), parseLong(c.get(3)), parseLong(c.get(4)));
            positions.put(new PositionKey(p.cifNo, p.instrCode), p);
        }
        return positions;
    }

    private static void writeLots(java.nio.file.Path path, java.util.List<Lot> lots) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add("LOT-ID,CIF-NO,INSTR-CODE,OPEN-QTY,OPEN-AMT,ACQ-TS,SRC-EXEC-ID");
        for (Lot lot : lots) {
            lines.add(csv(lot.lotId) + "," + csv(lot.cifNo) + "," + csv(lot.instrCode) + ","
                    + lot.openQty + "," + lot.openAmt + "," + csv(lot.acqTs) + "," + csv(lot.srcExecId));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> values = new java.util.ArrayList<>();
        StringBuilder value = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    value.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                values.add(value.toString().trim());
                value.setLength(0);
            } else {
                value.append(ch);
            }
        }
        values.add(value.toString().trim());
        return values;
    }

    private static String resolveCifNo(
            String orderId,
            java.util.List<Lot> sourceLots,
            java.util.Map<PositionKey, Position> positions) {
        String prefix = orderId == null ? "" : orderId.trim();
        int split = prefix.indexOf('-');
        if (split > 0) {
            return prefix.substring(0, split);
        }
        if (!sourceLots.isEmpty()) {
            return sourceLots.get(0).cifNo;
        }
        if (!positions.isEmpty()) {
            return positions.values().iterator().next().cifNo;
        }
        return "CIF000000";
    }

    private static long prorate(long amount, long qty, long baseQty) {
        if (baseQty == 0L) {
            return 0L;
        }
        return java.math.BigDecimal.valueOf(amount)
                .multiply(java.math.BigDecimal.valueOf(qty))
                .divide(java.math.BigDecimal.valueOf(baseQty), 0, java.math.RoundingMode.HALF_UP)
                .longValue();
    }

    private static boolean isFractionalLot(Lot lot) {
        return lot.openQty > 0L && lot.openQty < 100L;
    }

    private static boolean containsLot(java.util.List<Lot> lots, String lotId) {
        for (Lot lot : lots) {
            if (lot.lotId.equals(lotId)) {
                return true;
            }
        }
        return false;
    }

    private static String createLotId(String cifNo, String instrCode, String execId) {
        return "LOT-" + sanitize(cifNo) + "-" + sanitize(instrCode) + "-" + sanitize(execId);
    }

    private static String sanitize(String value) {
        if (value == null || value.isEmpty()) {
            return "NA";
        }
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9')) {
                b.append(ch);
            }
        }
        return b.length() == 0 ? "NA" : b.toString();
    }

    private static long parseLong(String value) {
        if (value == null || value.trim().isEmpty()) {
            return 0L;
        }
        return new java.math.BigDecimal(value.trim().replace(",", "")).longValue();
    }

    private static String csv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }

    private static String nvl(String value) {
        return value == null ? "" : value;
    }

    private static final class Lot {
        private final String lotId;
        private final String cifNo;
        private final String instrCode;
        private long openQty;
        private long openAmt;
        private final String acqTs;
        private final String srcExecId;

        private Lot(String lotId, String cifNo, String instrCode, long openQty, long openAmt, String acqTs, String srcExecId) {
            this.lotId = lotId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.openQty = openQty;
            this.openAmt = openAmt;
            this.acqTs = acqTs;
            this.srcExecId = srcExecId;
        }
    }

    private static final class Execution {
        private final String execId;
        private final String orderId;
        private final String instrCode;
        private final String sideKbn;
        private final long fillQty;
        private final long fillAmt;
        private final String execTs;

        private Execution(String execId, String orderId, String instrCode, String sideKbn, long fillQty, long fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class Position {
        private final String cifNo;
        private final String instrCode;
        private final long netQty;
        private final long avgAmt;
        private final long rlzdAmt;

        private Position(String cifNo, String instrCode, long netQty, long avgAmt, long rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    private static final class PositionKey {
        private final String cifNo;
        private final String instrCode;

        private PositionKey(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof PositionKey)) {
                return false;
            }
            PositionKey that = (PositionKey) o;
            return java.util.Objects.equals(cifNo, that.cifNo)
                    && java.util.Objects.equals(instrCode, that.instrCode);
        }

        public int hashCode() {
            return java.util.Objects.hash(cifNo, instrCode);
        }
    }

    private static final class LotKey {
        private final String cifNo;
        private final String instrCode;

        private LotKey(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof LotKey)) {
                return false;
            }
            LotKey that = (LotKey) o;
            return java.util.Objects.equals(cifNo, that.cifNo)
                    && java.util.Objects.equals(instrCode, that.instrCode);
        }

        public int hashCode() {
            return java.util.Objects.hash(cifNo, instrCode);
        }
    }

    private static final class Result {
        private final java.util.List<Lot> openLots;
        private final java.util.List<String> messages;
        private final long realizedAmount;

        private Result(java.util.List<Lot> openLots, java.util.List<String> messages, long realizedAmount) {
            this.openLots = openLots;
            this.messages = messages;
            this.realizedAmount = realizedAmount;
        }
    }
}
