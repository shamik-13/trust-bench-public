/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2020-09-02  西村 亮 (E-204)     初版作成。約定、ポジション、保有残を照合し監査イベントを出力。
 */

package jp.mirai.sec.position;

public class PositionReconcileService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;
    private static final int DECISION_ACCEPT = 0;
    private static final int DECISION_REJECT_MARGIN = 4;
    private static final int DECISION_REJECT_NOTIONAL = 8;
    private static final int DECISION_REJECT_TICK = 12;

    private static final java.math.BigDecimal ZERO = java.math.BigDecimal.ZERO;
    private static final java.math.BigDecimal AVG_TOLERANCE = new java.math.BigDecimal("0.01");

    public static void main(String[] a) throws Exception {
        if (a.length != 4) {
            System.err.println("使用法: java PositionReconcileService SCEXEC.csv SCPOSF.csv SCHLDF.csv SCRISKF2.csv");
            System.exit(2);
        }

        PositionReconcileService service = new PositionReconcileService();
        service.reconcile(
                java.nio.file.Paths.get(a[0]),
                java.nio.file.Paths.get(a[1]),
                java.nio.file.Paths.get(a[2]),
                java.nio.file.Paths.get(a[3]));
    }

    public void reconcile(java.nio.file.Path scexec,
                          java.nio.file.Path scposf,
                          java.nio.file.Path schldf,
                          java.nio.file.Path scriskf2) throws java.io.IOException {
        java.util.Map<Key, ExecTotal> execByKey = readExec(scexec);
        java.util.Map<Key, PositionRecord> posByKey = readPosition(scposf);
        java.util.Map<Key, HoldingRecord> holdingByKey = readHolding(schldf);
        java.util.List<RiskEvent> events = new java.util.ArrayList<>();

        java.util.Set<Key> keys = new java.util.TreeSet<>();
        keys.addAll(execByKey.keySet());
        keys.addAll(posByKey.keySet());
        keys.addAll(holdingByKey.keySet());

        java.time.LocalDateTime eventTs = java.time.LocalDateTime.now().withNano(0);
        int seq = 1;

        for (Key key : keys) {
            ExecTotal exec = execByKey.getOrDefault(key, ExecTotal.empty());
            PositionRecord pos = posByKey.get(key);
            HoldingRecord holding = holdingByKey.get(key);

            long netQty = pos == null ? 0L : pos.netQty;
            java.math.BigDecimal avgAmt = pos == null ? ZERO : pos.avgAmt;
            java.math.BigDecimal realizedAmt = pos == null ? ZERO : pos.realizedAmt;
            long tradeQty = holding == null ? 0L : holding.tradeQty;
            long settledQty = holding == null ? 0L : holding.settledQty;
            long restrictedQty = holding == null ? 0L : holding.restrictedQty;

            long qtyDiff = exec.netQty - netQty;
            if (qtyDiff != 0L) {
                events.add(new RiskEvent(eventId(eventTs, seq++), key.cifNo, key.instrCode, eventTs,
                        abs(exec.grossAmt), java.math.BigDecimal.valueOf(Math.abs(qtyDiff)),
                        DECISION_REJECT_NOTIONAL));
            }

            if (exec.grossQty > 0L && netQty != 0L) {
                java.math.BigDecimal execAvg = exec.grossAmt.divide(
                        java.math.BigDecimal.valueOf(exec.grossQty), 8, java.math.RoundingMode.HALF_UP);
                java.math.BigDecimal avgDiff = execAvg.subtract(avgAmt).abs();
                if (avgDiff.compareTo(AVG_TOLERANCE) > 0) {
                    events.add(new RiskEvent(eventId(eventTs, seq++), key.cifNo, key.instrCode, eventTs,
                            execAvg, avgDiff, DECISION_REJECT_TICK));
                }
            }

            long expectedOpenQty = tradeQty - settledQty - restrictedQty;
            if (expectedOpenQty != netQty) {
                events.add(new RiskEvent(eventId(eventTs, seq++), key.cifNo, key.instrCode, eventTs,
                        java.math.BigDecimal.valueOf(Math.abs(expectedOpenQty)),
                        java.math.BigDecimal.valueOf(Math.abs(expectedOpenQty - netQty)),
                        DECISION_REJECT_MARGIN));
            }

            java.math.BigDecimal exposure = avgAmt.multiply(java.math.BigDecimal.valueOf(Math.abs(netQty)))
                    .add(realizedAmt.abs());
            if (exposure.compareTo(java.math.BigDecimal.valueOf(MIHFT_MAX_NOTIONAL)) > 0) {
                events.add(new RiskEvent(eventId(eventTs, seq++), key.cifNo, key.instrCode, eventTs,
                        java.math.BigDecimal.valueOf(MIHFT_MAX_NOTIONAL), exposure,
                        DECISION_REJECT_NOTIONAL));
            }
        }

        writeRisk(scriskf2, events);
    }

    private static java.util.Map<Key, ExecTotal> readExec(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<Key, ExecTotal> map = new java.util.HashMap<>();
        for (java.util.Map<String, String> row : readCsv(path)) {
            String orderId = required(row, "ORDER-ID");
            String cifNo = cifFromOrder(orderId);
            String instrCode = required(row, "INSTR-CODE");
            String side = required(row, "SIDE-KBN");
            long qty = parseLong(required(row, "FILL-QTY"), "FILL-QTY");
            java.math.BigDecimal amt = parseDecimal(required(row, "FILL-AMT"), "FILL-AMT");

            if (!"B".equals(side) && !"S".equals(side)) {
                throw new IllegalArgumentException("SIDE-KBN不正: " + side);
            }
            if (qty <= 0L) {
                throw new IllegalArgumentException("約定数量不正: " + qty);
            }
            if (amt.signum() < 0) {
                throw new IllegalArgumentException("約定金額不正: " + amt);
            }

            long signedQty = "B".equals(side) ? qty : -qty;
            Key key = new Key(cifNo, instrCode);
            ExecTotal total = map.getOrDefault(key, ExecTotal.empty());
            map.put(key, total.add(signedQty, qty, amt));
        }
        return map;
    }

    private static java.util.Map<Key, PositionRecord> readPosition(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<Key, PositionRecord> map = new java.util.HashMap<>();
        for (java.util.Map<String, String> row : readCsv(path)) {
            Key key = new Key(required(row, "CIF-NO"), required(row, "INSTR-CODE"));
            PositionRecord record = new PositionRecord(
                    parseLong(required(row, "NET-QTY"), "NET-QTY"),
                    parseDecimal(required(row, "AVG-AMT"), "AVG-AMT"),
                    parseDecimal(required(row, "RLZD-AMT"), "RLZD-AMT"));
            map.put(key, record);
        }
        return map;
    }

    private static java.util.Map<Key, HoldingRecord> readHolding(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<Key, HoldingRecord> map = new java.util.HashMap<>();
        for (java.util.Map<String, String> row : readCsv(path)) {
            Key key = new Key(required(row, "CIF-NO"), required(row, "INSTR-CODE"));
            HoldingRecord record = new HoldingRecord(
                    required(row, "ASOF-DT"),
                    parseLong(required(row, "SETTLED-QTY"), "SETTLED-QTY"),
                    parseLong(required(row, "TRADE-QTY"), "TRADE-QTY"),
                    parseLong(required(row, "RESTRICTED-QTY"), "RESTRICTED-QTY"));
            map.put(key, record);
        }
        return map;
    }

    private static void writeRisk(java.nio.file.Path path, java.util.List<RiskEvent> events) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add("RISK-EVENT-ID,CIF-NO,INSTR-CODE,EVENT-TS,LIMIT-AMT,USED-AMT,DECISION-KBN");
        for (RiskEvent event : events) {
            lines.add(csv(event.riskEventId) + "," +
                    csv(event.cifNo) + "," +
                    csv(event.instrCode) + "," +
                    csv(event.eventTs.toString()) + "," +
                    event.limitAmt.toPlainString() + "," +
                    event.usedAmt.toPlainString() + "," +
                    event.decisionKbn);
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static java.util.List<java.util.Map<String, String>> readCsv(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<>();
        if (lines.isEmpty()) {
            return rows;
        }

        java.util.List<String> header = splitCsv(lines.get(0));
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.trim().isEmpty()) {
                continue;
            }
            java.util.List<String> values = splitCsv(line);
            if (values.size() != header.size()) {
                throw new IllegalArgumentException("CSV項目数不一致: " + path + ":" + (i + 1));
            }
            java.util.Map<String, String> row = new java.util.HashMap<>();
            for (int j = 0; j < header.size(); j++) {
                row.put(header.get(j), values.get(j));
            }
            rows.add(row);
        }
        return rows;
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> values = new java.util.ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    current.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (c == ',' && !quoted) {
                values.add(current.toString().trim());
                current.setLength(0);
            } else {
                current.append(c);
            }
        }
        values.add(current.toString().trim());
        return values;
    }

    private static String required(java.util.Map<String, String> row, String name) {
        String value = row.get(name);
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException("必須項目なし: " + name);
        }
        return value.trim();
    }

    private static long parseLong(String value, String name) {
        try {
            return Long.parseLong(value.replace(",", ""));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値項目不正: " + name + "=" + value, e);
        }
    }

    private static java.math.BigDecimal parseDecimal(String value, String name) {
        try {
            return new java.math.BigDecimal(value.replace(",", ""));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("金額項目不正: " + name + "=" + value, e);
        }
    }

    private static String cifFromOrder(String orderId) {
        int p = orderId.indexOf('-');
        if (p > 0) {
            return orderId.substring(0, p);
        }
        if (orderId.length() >= 10) {
            return orderId.substring(0, 10);
        }
        return orderId;
    }

    private static String csv(String value) {
        if (value.indexOf(',') < 0 && value.indexOf('"') < 0 && value.indexOf('\n') < 0) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }

    private static java.math.BigDecimal abs(java.math.BigDecimal value) {
        return value.signum() < 0 ? value.negate() : value;
    }

    private static String eventId(java.time.LocalDateTime ts, int seq) {
        return "RCN" + ts.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss")) +
                String.format("%06d", seq);
    }

    private static final class Key implements Comparable<Key> {
        private final String cifNo;
        private final String instrCode;

        private Key(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        @Override
        public int compareTo(Key other) {
            int c = this.cifNo.compareTo(other.cifNo);
            if (c != 0) {
                return c;
            }
            return this.instrCode.compareTo(other.instrCode);
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof Key)) {
                return false;
            }
            Key other = (Key) obj;
            return cifNo.equals(other.cifNo) && instrCode.equals(other.instrCode);
        }

        @Override
        public int hashCode() {
            return java.util.Objects.hash(cifNo, instrCode);
        }
    }

    private static final class ExecTotal {
        private final long netQty;
        private final long grossQty;
        private final java.math.BigDecimal grossAmt;

        private ExecTotal(long netQty, long grossQty, java.math.BigDecimal grossAmt) {
            this.netQty = netQty;
            this.grossQty = grossQty;
            this.grossAmt = grossAmt;
        }

        private static ExecTotal empty() {
            return new ExecTotal(0L, 0L, ZERO);
        }

        private ExecTotal add(long signedQty, long fillQty, java.math.BigDecimal fillAmt) {
            return new ExecTotal(netQty + signedQty, grossQty + fillQty, grossAmt.add(fillAmt));
        }
    }

    private static final class PositionRecord {
        private final long netQty;
        private final java.math.BigDecimal avgAmt;
        private final java.math.BigDecimal realizedAmt;

        private PositionRecord(long netQty, java.math.BigDecimal avgAmt, java.math.BigDecimal realizedAmt) {
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.realizedAmt = realizedAmt;
        }
    }

    private static final class HoldingRecord {
        private final String asofDate;
        private final long settledQty;
        private final long tradeQty;
        private final long restrictedQty;

        private HoldingRecord(String asofDate, long settledQty, long tradeQty, long restrictedQty) {
            this.asofDate = asofDate;
            this.settledQty = settledQty;
            this.tradeQty = tradeQty;
            this.restrictedQty = restrictedQty;
        }
    }

    private static final class RiskEvent {
        private final String riskEventId;
        private final String cifNo;
        private final String instrCode;
        private final java.time.LocalDateTime eventTs;
        private final java.math.BigDecimal limitAmt;
        private final java.math.BigDecimal usedAmt;
        private final int decisionKbn;

        private RiskEvent(String riskEventId,
                          String cifNo,
                          String instrCode,
                          java.time.LocalDateTime eventTs,
                          java.math.BigDecimal limitAmt,
                          java.math.BigDecimal usedAmt,
                          int decisionKbn) {
            this.riskEventId = riskEventId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.eventTs = eventTs;
            this.limitAmt = limitAmt;
            this.usedAmt = usedAmt;
            this.decisionKbn = decisionKbn;
        }
    }
}
