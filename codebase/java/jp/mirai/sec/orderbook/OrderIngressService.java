package jp.mirai.sec.orderbook;

public class OrderIngressService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.0   2020-03-10  開発一課  注文受付サービス初版
     */

    private static final java.math.BigDecimal BP_DENOMINATOR = new java.math.BigDecimal("10000");
    private static final java.math.BigDecimal MIHFT_MAX_NOTIONAL = new java.math.BigDecimal("500000000");

    private static final String EVENT_ACCEPT = "0";
    private static final String EVENT_REJECT_MARGIN = "4";
    private static final String EVENT_REJECT_NOTIONAL = "8";
    private static final String EVENT_REJECT_TICK = "12";

    private static final int COL_ORDER_ID = 0;
    private static final int COL_CIF_NO = 1;
    private static final int COL_INSTR_CODE = 2;
    private static final int COL_SIDE_KBN = 3;
    private static final int COL_ORD_TYPE = 4;
    private static final int COL_TIF_CODE = 5;
    private static final int COL_ORD_QTY = 6;
    private static final int COL_PRICE_AMT = 7;
    private static final int COL_INSTR_TIER = 8;

    public static void main(String[] a) throws Exception {
        if (a.length != 4) {
            System.err.println("使用方法: java OrderIngressService SCORDF.csv SCCUST.csv SCRISK2.csv SCAUDF.csv");
            System.exit(2);
        }

        java.util.Map<String, String[]> customerByCif = readCsvByKey(a[1], 0, 4);
        java.util.Map<String, String[]> riskByCifTier = readCsvByCompositeKey(a[2], 0, 1, 5);
        java.util.List<String[]> orders = readCsv(a[0], 9);
        java.util.List<String[]> auditRows = new java.util.ArrayList<>();

        long auditSeq = 1L;
        java.time.format.DateTimeFormatter tsFormat =
                java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS");
        java.time.ZoneId zone = java.time.ZoneId.of("Asia/Tokyo");

        for (String[] order : orders) {
            String orderId = value(order, COL_ORDER_ID);
            String cifNo = value(order, COL_CIF_NO);
            String instrCode = value(order, COL_INSTR_CODE);
            String instrTier = value(order, COL_INSTR_TIER);
            String eventKbn;
            String detailCd;

            java.util.List<String> missing = requiredOrderMissing(order);
            if (!missing.isEmpty()) {
                eventKbn = EVENT_REJECT_NOTIONAL;
                detailCd = "必須項目不足:" + String.join("+", missing);
            } else if (!customerByCif.containsKey(cifNo)) {
                eventKbn = EVENT_REJECT_NOTIONAL;
                detailCd = "顧客未登録";
            } else if (!validCode(order)) {
                eventKbn = EVENT_REJECT_NOTIONAL;
                detailCd = "区分値不正";
            } else {
                String[] customer = customerByCif.get(cifNo);
                String[] risk = riskByCifTier.get(cifNo + "\u0001" + instrTier);
                Decision decision = decide(order, customer, risk);
                eventKbn = decision.eventKbn;
                detailCd = decision.detailCd;
            }

            auditRows.add(new String[] {
                    nextAuditId(auditSeq++),
                    orderId,
                    eventKbn,
                    cifNo,
                    instrCode,
                    java.time.LocalDateTime.now(zone).format(tsFormat),
                    detailCd
            });
        }

        writeCsv(a[3], auditRows);
    }

    private static Decision decide(String[] order, String[] customer, String[] risk) {
        int tier = parseInt(value(order, COL_INSTR_TIER), -1);
        long qty = parseLong(value(order, COL_ORD_QTY), -1L);
        java.math.BigDecimal price = parseDecimal(value(order, COL_PRICE_AMT));
        if (tierRateBp(tier) < 0 || qty <= 0 || price == null || price.signum() < 0) {
            return new Decision(EVENT_REJECT_NOTIONAL, "数値不正");
        }

        if ("M".equals(value(order, COL_ORD_TYPE)) && price.signum() != 0) {
            return new Decision(EVENT_REJECT_TICK, "成行価格指定");
        }

        if ("L".equals(value(order, COL_ORD_TYPE))) {
            int tick = tierTick(tier);
            if (tick < 0 || price.remainder(new java.math.BigDecimal(tick)).signum() != 0) {
                return new Decision(EVENT_REJECT_TICK, "呼値不適合");
            }
        }

        if (risk == null) {
            return new Decision(EVENT_REJECT_NOTIONAL, "限度未登録");
        }
        if ("1".equals(value(risk, 4))) {
            return new Decision(EVENT_REJECT_NOTIONAL, "停止中");
        }

        java.math.BigDecimal notional = price.multiply(new java.math.BigDecimal(qty));
        java.math.BigDecimal maxNotional = parseDecimal(value(risk, 2));
        long maxQty = parseLong(value(risk, 3), -1L);
        if (notional.compareTo(MIHFT_MAX_NOTIONAL) > 0) {
            return new Decision(EVENT_REJECT_NOTIONAL, "受付上限超過");
        }
        if (maxNotional == null || notional.compareTo(maxNotional) > 0 || maxQty < 0 || qty > maxQty) {
            return new Decision(EVENT_REJECT_NOTIONAL, "銘柄階層限度超過");
        }

        java.math.BigDecimal groupLimit = parseDecimal(value(customer, 1));
        java.math.BigDecimal groupUsed = parseDecimal(value(customer, 2));
        java.math.BigDecimal acctUsed = parseDecimal(value(customer, 3));
        if (groupLimit == null || groupUsed == null || acctUsed == null) {
            return new Decision(EVENT_REJECT_MARGIN, "顧客残高不正");
        }

        java.math.BigDecimal margin = notional
                .multiply(new java.math.BigDecimal(tierRateBp(tier)))
                .divide(BP_DENOMINATOR, 0, java.math.RoundingMode.CEILING);
        java.math.BigDecimal usedAfter = groupUsed.add(acctUsed).add(margin);
        if (usedAfter.compareTo(groupLimit) > 0) {
            return new Decision(EVENT_REJECT_MARGIN, "保証金限度超過");
        }

        return new Decision(EVENT_ACCEPT, "受付完了");
    }

    private static java.util.List<String> requiredOrderMissing(String[] order) {
        java.util.List<String> missing = new java.util.ArrayList<>();
        String[] names = {
                "ORDER-ID", "CIF-NO", "INSTR-CODE", "SIDE-KBN", "ORD-TYPE",
                "TIF-CODE", "ORD-QTY", "PRICE-AMT", "INSTR-TIER"
        };
        for (int i = 0; i < names.length; i++) {
            if (value(order, i).isEmpty()) {
                missing.add(names[i]);
            }
        }
        return missing;
    }

    private static boolean validCode(String[] order) {
        String side = value(order, COL_SIDE_KBN);
        String ordType = value(order, COL_ORD_TYPE);
        String tif = value(order, COL_TIF_CODE);
        return ("B".equals(side) || "S".equals(side))
                && ("L".equals(ordType) || "M".equals(ordType))
                && ("DAY".equals(tif) || "IOC".equals(tif) || "FOK".equals(tif));
    }

    private static int tierRateBp(int tier) {
        if (tier == 1) {
            return 1000;
        }
        if (tier == 2) {
            return 2000;
        }
        if (tier == 3) {
            return 4000;
        }
        return -1;
    }

    private static int tierTick(int tier) {
        if (tier == 1) {
            return 100;
        }
        if (tier == 2) {
            return 500;
        }
        if (tier == 3) {
            return 1000;
        }
        return -1;
    }

    private static java.util.Map<String, String[]> readCsvByKey(String path, int keyCol, int width) throws java.io.IOException {
        java.util.Map<String, String[]> map = new java.util.HashMap<>();
        for (String[] row : readCsv(path, width)) {
            map.put(value(row, keyCol), row);
        }
        return map;
    }

    private static java.util.Map<String, String[]> readCsvByCompositeKey(String path, int keyCol1, int keyCol2, int width)
            throws java.io.IOException {
        java.util.Map<String, String[]> map = new java.util.HashMap<>();
        for (String[] row : readCsv(path, width)) {
            map.put(value(row, keyCol1) + "\u0001" + value(row, keyCol2), row);
        }
        return map;
    }

    private static java.util.List<String[]> readCsv(String path, int width) throws java.io.IOException {
        java.util.List<String[]> rows = new java.util.ArrayList<>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = reader.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                java.util.List<String> cols = parseCsvLine(line);
                if (first && !cols.isEmpty() && looksLikeHeader(cols.get(0))) {
                    first = false;
                    continue;
                }
                first = false;
                String[] row = new String[width];
                for (int i = 0; i < width; i++) {
                    row[i] = i < cols.size() ? cols.get(i).trim() : "";
                }
                rows.add(row);
            }
        }
        return rows;
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> cols = new java.util.ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (quoted) {
                if (ch == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        cur.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    cur.append(ch);
                }
            } else if (ch == ',') {
                cols.add(cur.toString());
                cur.setLength(0);
            } else if (ch == '"') {
                quoted = true;
            } else {
                cur.append(ch);
            }
        }
        cols.add(cur.toString());
        return cols;
    }

    private static void writeCsv(String path, java.util.List<String[]> rows) throws java.io.IOException {
        try (java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8)) {
            writer.write("AUDIT-ID,ORDER-ID,EVENT-KBN,CIF-NO,INSTR-CODE,EVENT-TS,DETAIL-CD");
            writer.newLine();
            for (String[] row : rows) {
                for (int i = 0; i < row.length; i++) {
                    if (i > 0) {
                        writer.write(',');
                    }
                    writer.write(escapeCsv(row[i]));
                }
                writer.newLine();
            }
        }
    }

    private static String escapeCsv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }

    private static boolean looksLikeHeader(String firstValue) {
        return "ORDER-ID".equals(firstValue) || "CIF-NO".equals(firstValue);
    }

    private static String value(String[] row, int index) {
        if (row == null || index < 0 || index >= row.length || row[index] == null) {
            return "";
        }
        return row[index].trim();
    }

    private static int parseInt(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (RuntimeException e) {
            return defaultValue;
        }
    }

    private static long parseLong(String value, long defaultValue) {
        try {
            return Long.parseLong(value);
        } catch (RuntimeException e) {
            return defaultValue;
        }
    }

    private static java.math.BigDecimal parseDecimal(String value) {
        try {
            return new java.math.BigDecimal(value);
        } catch (RuntimeException e) {
            return null;
        }
    }

    private static String nextAuditId(long seq) {
        return "AUD" + String.format("%014d", seq);
    }

    private static final class Decision {
        private final String eventKbn;
        private final String detailCd;

        private Decision(String eventKbn, String detailCd) {
            this.eventKbn = eventKbn;
            this.detailCd = detailCd;
        }
    }
}
