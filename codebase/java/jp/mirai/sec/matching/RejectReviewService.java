package jp.mirai.sec.matching;

public class RejectReviewService {
    /*
     * 変更履歴
     * 版数  年月日      担当        概要
     * 1.00  2019/04/16  市場基盤部  拒否注文レビューサービス初版
     */

    private static final long MIHFT_MAX_NOTIONAL = 500_000_000L;

    private static final String STATE_REJECT_MARGIN = "4";
    private static final String STATE_REJECT_NOTIONAL = "8";
    private static final String STATE_REJECT_TICK = "12";
    private static final String STATE_REJECT_OTHER = "R";

    public static void main(String[] a) throws Exception {
        java.nio.file.Path screj = java.nio.file.Paths.get(a.length > 0 ? a[0] : "SCREJ");
        java.nio.file.Path scordf = java.nio.file.Paths.get(a.length > 1 ? a[1] : "SCORDF.csv");
        java.nio.file.Path scinstf = java.nio.file.Paths.get(a.length > 2 ? a[2] : "SCINSTF.csv");
        java.nio.file.Path scords = java.nio.file.Paths.get(a.length > 3 ? a[3] : "SCORDS.csv");

        java.util.Map<String, Order> orders = readOrders(scordf);
        java.util.Map<String, Instrument> instruments = readInstruments(scinstf);
        java.util.Map<String, OrderState> states = readStates(scords);
        java.util.List<RejectRecord> rejects = readRejects(screj);

        ReviewSummary summary = review(rejects, orders, instruments, states);
        writeStates(scords, states);
        System.out.println(summary.toOperatorMessage());
    }

    private static ReviewSummary review(
            java.util.List<RejectRecord> rejects,
            java.util.Map<String, Order> orders,
            java.util.Map<String, Instrument> instruments,
            java.util.Map<String, OrderState> states) {
        java.util.Map<String, Integer> byReason = new java.util.TreeMap<String, Integer>();
        java.util.Map<String, Integer> byTier = new java.util.TreeMap<String, Integer>();
        java.util.Map<String, Integer> byBoard = new java.util.TreeMap<String, Integer>();
        int written = 0;
        int missingOrder = 0;
        int missingInstrument = 0;

        for (RejectRecord reject : rejects) {
            Order order = orders.get(reject.orderId);
            if (order == null) {
                missingOrder++;
                add(byReason, "注文未登録");
                continue;
            }

            Instrument instrument = instruments.get(order.instrumentCode);
            if (instrument == null) {
                missingInstrument++;
                add(byReason, "銘柄未登録");
                continue;
            }

            String reason = classifyReject(reject, order, instrument);
            add(byReason, reason);
            add(byTier, "階層" + instrument.tier);
            add(byBoard, instrument.boardCode);

            if (!states.containsKey(order.orderId)) {
                String state = stateFor(reason, reject.rejectCode);
                states.put(order.orderId, new OrderState(
                        order.orderId,
                        order.cifNo,
                        order.instrumentCode,
                        state,
                        order.quantity,
                        0L,
                        java.math.BigDecimal.ZERO,
                        reject.rejectTimestamp));
                written++;
            }
        }

        return new ReviewSummary(rejects.size(), written, missingOrder, missingInstrument, byReason, byTier, byBoard);
    }

    private static String classifyReject(RejectRecord reject, Order order, Instrument instrument) {
        if (reject.rejectCode.equals(STATE_REJECT_TICK) || violatesTick(order, instrument)) {
            return "値刻み拒否";
        }
        if (violatesLot(order, instrument)) {
            return "売買単位拒否";
        }
        if (reject.rejectCode.equals(STATE_REJECT_NOTIONAL) || notional(order).compareTo(java.math.BigDecimal.valueOf(MIHFT_MAX_NOTIONAL)) > 0) {
            return "上限金額拒否";
        }
        if (reject.rejectCode.equals(STATE_REJECT_MARGIN) || estimatedMargin(order, instrument).compareTo(java.math.BigDecimal.valueOf(MIHFT_MAX_NOTIONAL)) > 0) {
            return "証拠金拒否";
        }
        if (isClosingSession(reject.rejectTimestamp) && order.tifCode.equals("DAY")) {
            return "立会状態拒否";
        }
        return "審査元不明";
    }

    private static String stateFor(String reason, String rejectCode) {
        if ("証拠金拒否".equals(reason)) {
            return STATE_REJECT_MARGIN;
        }
        if ("上限金額拒否".equals(reason)) {
            return STATE_REJECT_NOTIONAL;
        }
        if ("値刻み拒否".equals(reason)) {
            return STATE_REJECT_TICK;
        }
        if (STATE_REJECT_MARGIN.equals(rejectCode) || STATE_REJECT_NOTIONAL.equals(rejectCode) || STATE_REJECT_TICK.equals(rejectCode)) {
            return rejectCode;
        }
        return STATE_REJECT_OTHER;
    }

    private static boolean violatesTick(Order order, Instrument instrument) {
        if (order.orderType.equals("M")) {
            return false;
        }
        if (order.price.signum() <= 0) {
            return true;
        }
        java.math.BigDecimal[] div = order.price.divideAndRemainder(instrument.tick);
        return div[1].compareTo(java.math.BigDecimal.ZERO) != 0;
    }

    private static boolean violatesLot(Order order, Instrument instrument) {
        return instrument.lotQuantity > 0 && order.quantity % instrument.lotQuantity != 0;
    }

    private static java.math.BigDecimal estimatedMargin(Order order, Instrument instrument) {
        return notional(order).multiply(java.math.BigDecimal.valueOf(marginRateBp(instrument.tier)))
                .divide(java.math.BigDecimal.valueOf(10_000L), 0, java.math.RoundingMode.CEILING);
    }

    private static java.math.BigDecimal notional(Order order) {
        java.math.BigDecimal price = order.orderType.equals("M") ? referenceMarketPrice(order.instrumentTier) : order.price;
        return price.multiply(java.math.BigDecimal.valueOf(order.quantity));
    }

    private static java.math.BigDecimal referenceMarketPrice(int tier) {
        if (tier == 1) {
            return java.math.BigDecimal.valueOf(25_000L);
        }
        if (tier == 2) {
            return java.math.BigDecimal.valueOf(8_000L);
        }
        return java.math.BigDecimal.valueOf(2_500L);
    }

    private static int marginRateBp(int tier) {
        if (tier == 1) {
            return 1_000;
        }
        if (tier == 2) {
            return 2_000;
        }
        if (tier == 3) {
            return 4_000;
        }
        throw new IllegalArgumentException("銘柄階層不正:" + tier);
    }

    private static boolean isClosingSession(String timestamp) {
        java.time.LocalTime time = parseTime(timestamp);
        return !time.isBefore(java.time.LocalTime.of(11, 25)) && time.isBefore(java.time.LocalTime.of(12, 30))
                || !time.isBefore(java.time.LocalTime.of(15, 20));
    }

    private static java.time.LocalTime parseTime(String timestamp) {
        String text = timestamp.trim();
        if (text.length() >= 19 && text.charAt(10) == 'T') {
            return java.time.LocalDateTime.parse(text).toLocalTime();
        }
        if (text.length() >= 19 && text.charAt(10) == ' ') {
            return java.time.LocalDateTime.parse(text.replace(' ', 'T')).toLocalTime();
        }
        if (text.length() >= 14) {
            return java.time.LocalTime.of(
                    Integer.parseInt(text.substring(8, 10)),
                    Integer.parseInt(text.substring(10, 12)),
                    Integer.parseInt(text.substring(12, 14)));
        }
        return java.time.LocalTime.parse(text);
    }

    private static java.util.List<RejectRecord> readRejects(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<RejectRecord> rows = new java.util.ArrayList<RejectRecord>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (line.trim().isEmpty() || isHeader(line, "REJECT-ID")) {
                continue;
            }
            java.util.List<String> c = splitRecord(line);
            if (c.size() < 6) {
                throw new IllegalArgumentException("SCREJ項目不足:" + line);
            }
            rows.add(new RejectRecord(c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), c.get(5)));
        }
        return rows;
    }

    private static java.util.Map<String, Order> readOrders(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, Order> rows = new java.util.TreeMap<String, Order>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (line.trim().isEmpty() || isHeader(line, "ORDER-ID")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            if (c.size() < 9) {
                throw new IllegalArgumentException("SCORDF項目不足:" + line);
            }
            Order order = new Order(c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), c.get(5),
                    Long.parseLong(c.get(6)), decimal(c.get(7)), Integer.parseInt(c.get(8)));
            validateOrderCode(order);
            rows.put(order.orderId, order);
        }
        return rows;
    }

    private static java.util.Map<String, Instrument> readInstruments(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, Instrument> rows = new java.util.TreeMap<String, Instrument>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (line.trim().isEmpty() || isHeader(line, "INSTR-CODE")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            if (c.size() < 6) {
                throw new IllegalArgumentException("SCINSTF項目不足:" + line);
            }
            Instrument instrument = new Instrument(c.get(0), c.get(1), Integer.parseInt(c.get(2)),
                    decimal(c.get(3)), Long.parseLong(c.get(4)), c.get(5));
            validateInstrumentCode(instrument);
            rows.put(instrument.instrumentCode, instrument);
        }
        return rows;
    }

    private static java.util.Map<String, OrderState> readStates(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, OrderState> rows = new java.util.TreeMap<String, OrderState>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (line.trim().isEmpty() || isHeader(line, "ORDER-ID")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            if (c.size() < 8) {
                throw new IllegalArgumentException("SCORDS項目不足:" + line);
            }
            rows.put(c.get(0), new OrderState(c.get(0), c.get(1), c.get(2), c.get(3),
                    Long.parseLong(c.get(4)), Long.parseLong(c.get(5)), decimal(c.get(6)), c.get(7)));
        }
        return rows;
    }

    private static void writeStates(java.nio.file.Path path, java.util.Map<String, OrderState> states) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<String>();
        lines.add("ORDER-ID,CIF-NO,INSTR-CODE,STATE-KBN,LEAVES-QTY,CUM-QTY,AVG-FILL-AMT,LAST-UPD-TS");
        for (OrderState s : states.values()) {
            lines.add(csv(s.orderId) + "," + csv(s.cifNo) + "," + csv(s.instrumentCode) + "," + csv(s.state) + ","
                    + s.leavesQuantity + "," + s.cumQuantity + "," + s.averageFill.toPlainString() + "," + csv(s.lastUpdated));
        }
        java.nio.file.Path parent = path.toAbsolutePath().getParent();
        if (parent != null) {
            java.nio.file.Files.createDirectories(parent);
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void validateOrderCode(Order order) {
        if (!order.side.equals("B") && !order.side.equals("S")) {
            throw new IllegalArgumentException("売買区分不正:" + order.orderId);
        }
        if (!order.orderType.equals("L") && !order.orderType.equals("M")) {
            throw new IllegalArgumentException("注文種別不正:" + order.orderId);
        }
        if (!order.tifCode.equals("DAY") && !order.tifCode.equals("IOC") && !order.tifCode.equals("FOK")) {
            throw new IllegalArgumentException("有効期限区分不正:" + order.orderId);
        }
        marginRateBp(order.instrumentTier);
    }

    private static void validateInstrumentCode(Instrument instrument) {
        marginRateBp(instrument.tier);
        if (!instrument.boardCode.equals("T1") && !instrument.boardCode.equals("ST") && !instrument.boardCode.equals("ETF")) {
            throw new IllegalArgumentException("市場区分不正:" + instrument.instrumentCode);
        }
    }

    private static java.util.List<String> splitRecord(String line) {
        if (line.indexOf(',') >= 0) {
            return splitCsv(line);
        }
        String[] parts = line.trim().split("\\s+");
        java.util.List<String> values = new java.util.ArrayList<String>();
        for (String part : parts) {
            values.add(part.trim());
        }
        return values;
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> values = new java.util.ArrayList<String>();
        StringBuilder current = new StringBuilder();
        boolean quote = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quote && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    current.append('"');
                    i++;
                } else {
                    quote = !quote;
                }
            } else if (ch == ',' && !quote) {
                values.add(current.toString().trim());
                current.setLength(0);
            } else {
                current.append(ch);
            }
        }
        values.add(current.toString().trim());
        return values;
    }

    private static String csv(String value) {
        if (value.indexOf(',') < 0 && value.indexOf('"') < 0 && value.indexOf('\n') < 0 && value.indexOf('\r') < 0) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }

    private static boolean isHeader(String line, String firstName) {
        return line.trim().startsWith(firstName);
    }

    private static java.math.BigDecimal decimal(String value) {
        if (value == null || value.trim().isEmpty()) {
            return java.math.BigDecimal.ZERO;
        }
        return new java.math.BigDecimal(value.trim());
    }

    private static void add(java.util.Map<String, Integer> map, String key) {
        Integer count = map.get(key);
        map.put(key, count == null ? 1 : count + 1);
    }

    private static final class RejectRecord {
        final String rejectId;
        final String orderId;
        final String cifNo;
        final String instrumentCode;
        final String rejectCode;
        final String rejectTimestamp;

        RejectRecord(String rejectId, String orderId, String cifNo, String instrumentCode, String rejectCode, String rejectTimestamp) {
            this.rejectId = rejectId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrumentCode = instrumentCode;
            this.rejectCode = rejectCode;
            this.rejectTimestamp = rejectTimestamp;
        }
    }

    private static final class Order {
        final String orderId;
        final String cifNo;
        final String instrumentCode;
        final String side;
        final String orderType;
        final String tifCode;
        final long quantity;
        final java.math.BigDecimal price;
        final int instrumentTier;

        Order(String orderId, String cifNo, String instrumentCode, String side, String orderType, String tifCode,
              long quantity, java.math.BigDecimal price, int instrumentTier) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrumentCode = instrumentCode;
            this.side = side;
            this.orderType = orderType;
            this.tifCode = tifCode;
            this.quantity = quantity;
            this.price = price;
            this.instrumentTier = instrumentTier;
        }
    }

    private static final class Instrument {
        final String instrumentCode;
        final String name;
        final int tier;
        final java.math.BigDecimal tick;
        final long lotQuantity;
        final String boardCode;

        Instrument(String instrumentCode, String name, int tier, java.math.BigDecimal tick, long lotQuantity, String boardCode) {
            this.instrumentCode = instrumentCode;
            this.name = name;
            this.tier = tier;
            this.tick = tick;
            this.lotQuantity = lotQuantity;
            this.boardCode = boardCode;
        }
    }

    private static final class OrderState {
        final String orderId;
        final String cifNo;
        final String instrumentCode;
        final String state;
        final long leavesQuantity;
        final long cumQuantity;
        final java.math.BigDecimal averageFill;
        final String lastUpdated;

        OrderState(String orderId, String cifNo, String instrumentCode, String state,
                   long leavesQuantity, long cumQuantity, java.math.BigDecimal averageFill, String lastUpdated) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrumentCode = instrumentCode;
            this.state = state;
            this.leavesQuantity = leavesQuantity;
            this.cumQuantity = cumQuantity;
            this.averageFill = averageFill;
            this.lastUpdated = lastUpdated;
        }
    }

    private static final class ReviewSummary {
        final int readRejects;
        final int writtenStates;
        final int missingOrders;
        final int missingInstruments;
        final java.util.Map<String, Integer> byReason;
        final java.util.Map<String, Integer> byTier;
        final java.util.Map<String, Integer> byBoard;

        ReviewSummary(int readRejects, int writtenStates, int missingOrders, int missingInstruments,
                      java.util.Map<String, Integer> byReason, java.util.Map<String, Integer> byTier,
                      java.util.Map<String, Integer> byBoard) {
            this.readRejects = readRejects;
            this.writtenStates = writtenStates;
            this.missingOrders = missingOrders;
            this.missingInstruments = missingInstruments;
            this.byReason = byReason;
            this.byTier = byTier;
            this.byBoard = byBoard;
        }

        String toOperatorMessage() {
            return "拒否注文レビュー完了"
                    + " 読込=" + readRejects
                    + " 状態作成=" + writtenStates
                    + " 注文未登録=" + missingOrders
                    + " 銘柄未登録=" + missingInstruments
                    + " 理由別=" + byReason
                    + " 階層別=" + byTier
                    + " 市場別=" + byBoard;
        }
    }
}
