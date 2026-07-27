/*
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.0     2019-04-16  三宅 拓也 (E-241)      初版作成
 */

package jp.mirai.sec.grouprisk;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class OmsService {
    private static final long MIHFT_MAX_NOTIONAL = 500_000_000L;

    private enum OrderState {
        NEW,
        ACCEPTED,
        PARTIALLY_FILLED,
        FILLED,
        CANCELLED,
        REJECTED
    }

    private static final class TierRule {
        final int rateBp;
        final long tick;

        TierRule(int rateBp, long tick) {
            this.rateBp = rateBp;
            this.tick = tick;
        }
    }

    private static final class Order {
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final String ordType;
        final String tifCode;
        final long ordQty;
        final long priceAmt;
        final int instrTier;
        final int decisionCode;
        final long notionalAmt;
        final long marginAmt;
        long filledQty;
        long filledAmt;
        OrderState state;

        Order(String orderId, String cifNo, String instrCode, String sideKbn, String ordType,
              String tifCode, long ordQty, long priceAmt, int instrTier, int decisionCode,
              long notionalAmt, long marginAmt, OrderState state) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.ordType = ordType;
            this.tifCode = tifCode;
            this.ordQty = ordQty;
            this.priceAmt = priceAmt;
            this.instrTier = instrTier;
            this.decisionCode = decisionCode;
            this.notionalAmt = notionalAmt;
            this.marginAmt = marginAmt;
            this.state = state;
        }
    }

    private static final class Execution {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final Instant execTs;

        Execution(String execId, String orderId, String instrCode, String sideKbn,
                  long fillQty, long fillAmt, Instant execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    public static void main(String[] a) throws Exception {
        if (a.length < 2 || a.length > 3) {
            throw new IllegalArgumentException("引数は注文CSV、約定CSV、任意の出力CSVを指定してください");
        }

        LinkedHashMap<String, Order> orders = readOrders(Path.of(a[0]));
        List<Execution> executions = readExecutions(Path.of(a[1]));
        applyExecutions(orders, executions);
        closeImmediateValidityOrders(orders);

        if (a.length == 3) {
            try (BufferedWriter writer = Files.newBufferedWriter(Path.of(a[2]), StandardCharsets.UTF_8)) {
                writeLifecycle(orders, writer);
            }
        } else {
            BufferedWriter writer = new BufferedWriter(new java.io.OutputStreamWriter(System.out, StandardCharsets.UTF_8));
            writeLifecycle(orders, writer);
            writer.flush();
        }
    }

    private static LinkedHashMap<String, Order> readOrders(Path path) throws IOException {
        LinkedHashMap<String, Order> orders = new LinkedHashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            Map<String, Integer> header = readHeader(reader, "注文CSV");
            String line;
            int row = 1;
            while ((line = reader.readLine()) != null) {
                row++;
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> values = parseCsv(line);
                String orderId = get(values, header, "ORDER-ID", row);
                String cifNo = get(values, header, "CIF-NO", row);
                String instrCode = get(values, header, "INSTR-CODE", row);
                String sideKbn = get(values, header, "SIDE-KBN", row);
                String ordType = get(values, header, "ORD-TYPE", row);
                String tifCode = get(values, header, "TIF-CODE", row);
                long ordQty = parseLong(get(values, header, "ORD-QTY", row), "ORD-QTY", row);
                long priceAmt = parseLong(get(values, header, "PRICE-AMT", row), "PRICE-AMT", row);
                int instrTier = parseInt(get(values, header, "INSTR-TIER", row), "INSTR-TIER", row);

                if (orders.containsKey(orderId)) {
                    throw new IllegalArgumentException("注文IDが重複しています: " + orderId);
                }

                TierRule rule = tierRule(instrTier);
                long notional = estimateNotional(ordType, ordQty, priceAmt);
                long margin = ceilDiv(notional * rule.rateBp, 10_000L);
                int decision = decide(sideKbn, ordType, tifCode, ordQty, priceAmt, rule, notional, margin);
                OrderState state = decision == 0 ? OrderState.ACCEPTED : OrderState.REJECTED;

                orders.put(orderId, new Order(orderId, cifNo, instrCode, sideKbn, ordType, tifCode,
                        ordQty, priceAmt, instrTier, decision, notional, margin, state));
            }
        }
        return orders;
    }

    private static List<Execution> readExecutions(Path path) throws IOException {
        List<Execution> executions = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            Map<String, Integer> header = readHeader(reader, "約定CSV");
            String line;
            int row = 1;
            while ((line = reader.readLine()) != null) {
                row++;
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> values = parseCsv(line);
                executions.add(new Execution(
                        get(values, header, "EXEC-ID", row),
                        get(values, header, "ORDER-ID", row),
                        get(values, header, "INSTR-CODE", row),
                        get(values, header, "SIDE-KBN", row),
                        parseLong(get(values, header, "FILL-QTY", row), "FILL-QTY", row),
                        parseLong(get(values, header, "FILL-AMT", row), "FILL-AMT", row),
                        Instant.parse(get(values, header, "EXEC-TS", row))
                ));
            }
        }
        executions.sort((x, y) -> {
            int c = x.execTs.compareTo(y.execTs);
            return c != 0 ? c : x.execId.compareTo(y.execId);
        });
        return executions;
    }

    private static void applyExecutions(LinkedHashMap<String, Order> orders, List<Execution> executions) {
        for (Execution execution : executions) {
            Order order = orders.get(execution.orderId);
            if (order == null || order.state == OrderState.REJECTED || order.state == OrderState.CANCELLED || order.state == OrderState.FILLED) {
                continue;
            }
            if (!order.instrCode.equals(execution.instrCode) || !order.sideKbn.equals(execution.sideKbn)) {
                continue;
            }
            if (execution.fillQty <= 0 || execution.fillAmt < 0) {
                continue;
            }

            long remaining = order.ordQty - order.filledQty;
            long appliedQty = Math.min(remaining, execution.fillQty);
            long appliedAmt = execution.fillQty == appliedQty
                    ? execution.fillAmt
                    : ceilDiv(execution.fillAmt * appliedQty, execution.fillQty);

            order.filledQty += appliedQty;
            order.filledAmt += appliedAmt;

            if (order.filledQty >= order.ordQty) {
                order.state = OrderState.FILLED;
            } else {
                order.state = OrderState.PARTIALLY_FILLED;
            }
        }
    }

    private static void closeImmediateValidityOrders(LinkedHashMap<String, Order> orders) {
        for (Order order : orders.values()) {
            if (order.state == OrderState.REJECTED || order.state == OrderState.FILLED) {
                continue;
            }
            if ("IOC".equals(order.tifCode)) {
                order.state = order.filledQty > 0 ? OrderState.PARTIALLY_FILLED : OrderState.CANCELLED;
            } else if ("FOK".equals(order.tifCode) && order.filledQty < order.ordQty) {
                order.filledQty = 0L;
                order.filledAmt = 0L;
                order.state = OrderState.CANCELLED;
            }
        }
    }

    private static void writeLifecycle(LinkedHashMap<String, Order> orders, BufferedWriter writer) throws IOException {
        writer.write("ORDER-ID,CIF-NO,INSTR-CODE,SIDE-KBN,ORD-TYPE,TIF-CODE,ORD-QTY,PRICE-AMT,INSTR-TIER,DECISION-CODE,STATE,FILLED-QTY,OPEN-QTY,FILLED-AMT,NOTIONAL-AMT,MARGIN-AMT");
        writer.newLine();
        for (Order order : orders.values()) {
            writeCsvRow(writer,
                    order.orderId,
                    order.cifNo,
                    order.instrCode,
                    order.sideKbn,
                    order.ordType,
                    order.tifCode,
                    String.valueOf(order.ordQty),
                    String.valueOf(order.priceAmt),
                    String.valueOf(order.instrTier),
                    String.valueOf(order.decisionCode),
                    order.state.name(),
                    String.valueOf(order.filledQty),
                    String.valueOf(Math.max(0L, order.ordQty - order.filledQty)),
                    String.valueOf(order.filledAmt),
                    String.valueOf(order.notionalAmt),
                    String.valueOf(order.marginAmt));
        }
    }

    private static int decide(String sideKbn, String ordType, String tifCode, long ordQty,
                              long priceAmt, TierRule rule, long notional, long margin) {
        if (!"B".equals(sideKbn) && !"S".equals(sideKbn)) {
            return 8;
        }
        if (!"L".equals(ordType) && !"M".equals(ordType)) {
            return 8;
        }
        if (!"DAY".equals(tifCode) && !"IOC".equals(tifCode) && !"FOK".equals(tifCode)) {
            return 8;
        }
        if (ordQty <= 0 || priceAmt < 0) {
            return 8;
        }
        if ("L".equals(ordType) && (priceAmt <= 0 || priceAmt % rule.tick != 0)) {
            return 12;
        }
        if (notional > MIHFT_MAX_NOTIONAL) {
            return 8;
        }
        if (margin > MIHFT_MAX_NOTIONAL) {
            return 4;
        }
        return 0;
    }

    private static long estimateNotional(String ordType, long ordQty, long priceAmt) {
        if (!"L".equals(ordType)) {
            return 0L;
        }
        return ordQty * priceAmt;
    }

    private static TierRule tierRule(int tier) {
        if (tier == 1) {
            return new TierRule(1000, 100L);
        }
        if (tier == 2) {
            return new TierRule(2000, 500L);
        }
        if (tier == 3) {
            return new TierRule(4000, 1000L);
        }
        throw new IllegalArgumentException("INSTR-TIERが不正です: " + tier);
    }

    private static Map<String, Integer> readHeader(BufferedReader reader, String name) throws IOException {
        String line = reader.readLine();
        if (line == null) {
            throw new IllegalArgumentException(name + "が空です");
        }
        List<String> columns = parseCsv(line);
        Map<String, Integer> header = new HashMap<>();
        for (int i = 0; i < columns.size(); i++) {
            header.put(columns.get(i), i);
        }
        return header;
    }

    private static String get(List<String> values, Map<String, Integer> header, String column, int row) {
        Integer index = header.get(column);
        if (index == null) {
            throw new IllegalArgumentException("列がありません: " + column);
        }
        if (index >= values.size()) {
            throw new IllegalArgumentException("値がありません: " + row + "行目 " + column);
        }
        return values.get(index).trim();
    }

    private static int parseInt(String value, String column, int row) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値が不正です: " + row + "行目 " + column, e);
        }
    }

    private static long parseLong(String value, String column, int row) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値が不正です: " + row + "行目 " + column, e);
        }
    }

    private static long ceilDiv(long value, long divisor) {
        if (value == 0L) {
            return 0L;
        }
        return (value + divisor - 1L) / divisor;
    }

    private static List<String> parseCsv(String line) {
        List<String> values = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    current.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                values.add(current.toString());
                current.setLength(0);
            } else {
                current.append(ch);
            }
        }
        values.add(current.toString());
        return values;
    }

    private static void writeCsvRow(BufferedWriter writer, String... values) throws IOException {
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                writer.write(',');
            }
            writer.write(escapeCsv(values[i]));
        }
        writer.newLine();
    }

    private static String escapeCsv(String value) {
        if (value.indexOf(',') < 0 && value.indexOf('"') < 0 && value.indexOf('\n') < 0 && value.indexOf('\r') < 0) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }
}
