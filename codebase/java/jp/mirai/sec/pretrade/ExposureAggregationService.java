package jp.mirai.sec.pretrade;

public class ExposureAggregationService {
    /**
     * 変更履歴
     * 版数    年月日      担当      概要
     * 1.00    2025/01/21  村上 健司 (E-301)  初版作成
     */

    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final int DECISION_ACCEPT = 0;
    private static final int DECISION_REJECT_MARGIN = 4;
    private static final int DECISION_REJECT_NOTIONAL = 8;
    private static final int DECISION_REJECT_TICK = 12;

    private static final String SIDE_BUY = "B";
    private static final String SIDE_SELL = "S";

    private static final String HEADER_SCEXPR =
            "CIF-NO,INSTR-CODE,NET-NOTIONAL-AMT,BUY-OPEN-AMT,SELL-OPEN-AMT,UPDATED-TS";

    public static void main(String[] a) throws Exception {
        if (a == null || a.length != 5) {
            throw new IllegalArgumentException("起動引数は SCPOSF SCEXEC SCMKTD HFDEC SCEXPR の5件を指定してください");
        }
        new ExposureAggregationService().aggregate(a[0], a[1], a[2], a[3], a[4]);
    }

    public void aggregate(String scposfPath,
                          String scexecPath,
                          String scmktdPath,
                          String hfdecPath,
                          String scexprPath) throws java.io.IOException {
        java.util.Map<String, MarketRow> marketByInstr = readMarket(scmktdPath);
        java.util.Map<String, DecisionRow> decisionByOrder = readDecisions(hfdecPath);
        java.util.Map<ExposureKey, ExposureWork> exposureByKey = new java.util.TreeMap<ExposureKey, ExposureWork>();

        loadPositions(scposfPath, marketByInstr, exposureByKey);
        loadExecutions(scexecPath, marketByInstr, decisionByOrder, exposureByKey);
        loadAcceptedOpenOrders(decisionByOrder, exposureByKey);

        writeExposure(scexprPath, exposureByKey);
    }

    private static void loadPositions(String path,
                                      java.util.Map<String, MarketRow> marketByInstr,
                                      java.util.Map<ExposureKey, ExposureWork> exposureByKey) throws java.io.IOException {
        java.util.List<String[]> rows = readCsv(path);
        for (String[] row : rows) {
            requireColumns(row, 5, "SCPOSF");
            String cifNo = requireText(row[0], "CIF-NO");
            String instrCode = requireText(row[1], "INSTR-CODE");
            long netQty = parseLong(row[2], "NET-QTY");
            parseLong(row[3], "AVG-AMT");
            parseLong(row[4], "RLZD-AMT");

            MarketRow market = marketByInstr.get(instrCode);
            if (market == null) {
                throw new IllegalArgumentException("市場データ未登録です: " + instrCode);
            }

            long px = roundedValuationPrice(market);
            long notional = multiplyExact(netQty, px, "保有評価額");
            getWork(exposureByKey, cifNo, instrCode).netNotional += notional;
        }
    }

    private static void loadExecutions(String path,
                                       java.util.Map<String, MarketRow> marketByInstr,
                                       java.util.Map<String, DecisionRow> decisionByOrder,
                                       java.util.Map<ExposureKey, ExposureWork> exposureByKey) throws java.io.IOException {
        java.util.List<String[]> rows = readCsv(path);
        java.util.Set<String> seenExec = new java.util.HashSet<String>();

        for (String[] row : rows) {
            requireColumns(row, 7, "SCEXEC");
            String execId = requireText(row[0], "EXEC-ID");
            String orderId = requireText(row[1], "ORDER-ID");
            String instrCode = requireText(row[2], "INSTR-CODE");
            String sideKbn = requireSide(row[3]);
            long fillQty = parseLong(row[4], "FILL-QTY");
            long fillAmt = parseLong(row[5], "FILL-AMT");
            requireText(row[6], "EXEC-TS");

            if (!seenExec.add(execId)) {
                throw new IllegalArgumentException("約定IDが重複しています: " + execId);
            }
            if (fillQty <= 0L || fillAmt <= 0L) {
                throw new IllegalArgumentException("約定数量または約定金額が不正です: " + execId);
            }

            DecisionRow decision = decisionByOrder.get(orderId);
            if (decision == null) {
                throw new IllegalArgumentException("リスク判定が存在しない約定です: " + orderId);
            }
            if (decision.decisionCd != DECISION_ACCEPT) {
                throw new IllegalArgumentException("否認注文に約定が存在します: " + orderId);
            }
            if (!decision.instrCode.equals(instrCode)) {
                throw new IllegalArgumentException("注文と約定の銘柄が一致しません: " + orderId);
            }

            MarketRow market = marketByInstr.get(instrCode);
            if (market == null) {
                throw new IllegalArgumentException("市場データ未登録です: " + instrCode);
            }

            long executionAmt = roundToTick(fillAmt, market.tickMinor);
            long signedAmt = SIDE_BUY.equals(sideKbn) ? executionAmt : -executionAmt;
            ExposureWork work = getWork(exposureByKey, decision.cifNo, instrCode);
            work.netNotional += signedAmt;
            work.executedNotionalByOrder.put(orderId,
                    work.executedNotionalByOrder.containsKey(orderId)
                            ? work.executedNotionalByOrder.get(orderId) + executionAmt
                            : executionAmt);
            work.sideByOrder.put(orderId, sideKbn);
        }
    }

    private static void loadAcceptedOpenOrders(java.util.Map<String, DecisionRow> decisionByOrder,
                                               java.util.Map<ExposureKey, ExposureWork> exposureByKey) {
        for (DecisionRow decision : decisionByOrder.values()) {
            if (decision.decisionCd == DECISION_REJECT_MARGIN
                    || decision.decisionCd == DECISION_REJECT_NOTIONAL
                    || decision.decisionCd == DECISION_REJECT_TICK) {
                continue;
            }
            if (decision.decisionCd != DECISION_ACCEPT) {
                throw new IllegalArgumentException("リスク判定コードが不正です: " + decision.decisionId);
            }
            if (decision.notionalAmt <= 0L || decision.notionalAmt > MIHFT_MAX_NOTIONAL) {
                throw new IllegalArgumentException("注文元本が上限外です: " + decision.orderId);
            }
            if (decision.limitUsedAmt < 0L || decision.limitUsedAmt > decision.notionalAmt) {
                throw new IllegalArgumentException("使用限度額が不正です: " + decision.orderId);
            }

            ExposureWork work = getWork(exposureByKey, decision.cifNo, decision.instrCode);
            long executed = work.executedNotionalByOrder.containsKey(decision.orderId)
                    ? work.executedNotionalByOrder.get(decision.orderId)
                    : 0L;
            long openAmt = Math.max(0L, decision.notionalAmt - executed);
            if (openAmt == 0L) {
                continue;
            }

            String side = work.sideByOrder.get(decision.orderId);
            if (side == null) {
                side = inferSideFromOrderId(decision.orderId);
            }
            if (SIDE_BUY.equals(side)) {
                work.buyOpen += openAmt;
            } else if (SIDE_SELL.equals(side)) {
                work.sellOpen += openAmt;
            } else {
                throw new IllegalArgumentException("売買区分を判定できません: " + decision.orderId);
            }
        }
    }

    private static java.util.Map<String, MarketRow> readMarket(String path) throws java.io.IOException {
        java.util.Map<String, MarketRow> map = new java.util.HashMap<String, MarketRow>();
        java.util.List<String[]> rows = readCsv(path);
        for (String[] row : rows) {
            requireColumns(row, 6, "SCMKTD");
            String instrCode = requireText(row[0], "INSTR-CODE");
            long bidAmt = parseLong(row[1], "BID-AMT");
            long askAmt = parseLong(row[2], "ASK-AMT");
            long lastAmt = parseLong(row[3], "LAST-AMT");
            long volQty = parseLong(row[4], "VOL-QTY");
            requireText(row[5], "TICK-TS");

            if (bidAmt < 0L || askAmt < 0L || lastAmt <= 0L || volQty < 0L) {
                throw new IllegalArgumentException("市場データの値が不正です: " + instrCode);
            }
            if (bidAmt > 0L && askAmt > 0L && bidAmt > askAmt) {
                throw new IllegalArgumentException("気配値が逆転しています: " + instrCode);
            }

            InstrumentTier tier = tierOf(instrCode);
            map.put(instrCode, new MarketRow(instrCode, bidAmt, askAmt, lastAmt, volQty, tier.tickMinor));
        }
        return map;
    }

    private static java.util.Map<String, DecisionRow> readDecisions(String path) throws java.io.IOException {
        java.util.Map<String, DecisionRow> map = new java.util.LinkedHashMap<String, DecisionRow>();
        java.util.Set<String> seenDecision = new java.util.HashSet<String>();
        java.util.List<String[]> rows = readCsv(path);

        for (String[] row : rows) {
            requireColumns(row, 9, "HFDEC");
            String decisionId = requireText(row[0], "DECISION-ID");
            String orderId = requireText(row[1], "ORDER-ID");
            String cifNo = requireText(row[2], "CIF-NO");
            String instrCode = requireText(row[3], "INSTR-CODE");
            int decisionCd = (int) parseLong(row[4], "DECISION-CD");
            requireText(row[5], "REASON-CD");
            long notionalAmt = parseLong(row[6], "NOTIONAL-AMT");
            long limitUsedAmt = parseLong(row[7], "LIMIT-USED-AMT");
            requireText(row[8], "DECISION-TS");

            if (!seenDecision.add(decisionId)) {
                throw new IllegalArgumentException("判定IDが重複しています: " + decisionId);
            }

            DecisionRow current = new DecisionRow(decisionId, orderId, cifNo, instrCode,
                    decisionCd, notionalAmt, limitUsedAmt);
            DecisionRow prior = map.put(orderId, current);
            if (prior != null && prior.decisionCd == DECISION_ACCEPT && current.decisionCd == DECISION_ACCEPT) {
                throw new IllegalArgumentException("同一注文に承認判定が重複しています: " + orderId);
            }
        }
        return map;
    }

    private static void writeExposure(String path,
                                      java.util.Map<ExposureKey, ExposureWork> exposureByKey) throws java.io.IOException {
        java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        try {
            writer.write(HEADER_SCEXPR);
            writer.newLine();
            String updatedTs = java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
                    .format(java.time.LocalDateTime.now());

            for (java.util.Map.Entry<ExposureKey, ExposureWork> entry : exposureByKey.entrySet()) {
                ExposureKey key = entry.getKey();
                ExposureWork work = entry.getValue();
                writer.write(csv(key.cifNo));
                writer.write(',');
                writer.write(csv(key.instrCode));
                writer.write(',');
                writer.write(Long.toString(work.netNotional));
                writer.write(',');
                writer.write(Long.toString(work.buyOpen));
                writer.write(',');
                writer.write(Long.toString(work.sellOpen));
                writer.write(',');
                writer.write(csv(updatedTs));
                writer.newLine();
            }
        } finally {
            writer.close();
        }
    }

    private static java.util.List<String[]> readCsv(String path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<String[]> rows = new java.util.ArrayList<String[]>();
        boolean headerSkipped = false;
        for (String line : lines) {
            if (line == null || line.trim().isEmpty()) {
                continue;
            }
            String[] cols = parseCsvLine(line);
            if (!headerSkipped) {
                headerSkipped = true;
                if (cols.length > 0 && !isNumeric(cols[Math.min(cols.length - 1, 2)])) {
                    continue;
                }
            }
            rows.add(cols);
        }
        return rows;
    }

    private static String[] parseCsvLine(String line) {
        java.util.List<String> cols = new java.util.ArrayList<String>();
        StringBuilder sb = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    sb.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                cols.add(sb.toString().trim());
                sb.setLength(0);
            } else {
                sb.append(ch);
            }
        }
        if (quoted) {
            throw new IllegalArgumentException("CSV引用符が閉じていません");
        }
        cols.add(sb.toString().trim());
        return cols.toArray(new String[0]);
    }

    private static String csv(String v) {
        if (v.indexOf(',') < 0 && v.indexOf('"') < 0 && v.indexOf('\n') < 0 && v.indexOf('\r') < 0) {
            return v;
        }
        return "\"" + v.replace("\"", "\"\"") + "\"";
    }

    private static long roundedValuationPrice(MarketRow market) {
        long price;
        if (market.lastAmt > 0L) {
            price = market.lastAmt;
        } else if (market.bidAmt > 0L && market.askAmt > 0L) {
            price = (market.bidAmt + market.askAmt) / 2L;
        } else if (market.bidAmt > 0L) {
            price = market.bidAmt;
        } else if (market.askAmt > 0L) {
            price = market.askAmt;
        } else {
            throw new IllegalArgumentException("評価価格がありません: " + market.instrCode);
        }
        return roundToTick(price, market.tickMinor);
    }

    private static long roundToTick(long amount, long tick) {
        if (tick <= 0L) {
            throw new IllegalArgumentException("呼値単位が不正です");
        }
        long remainder = Math.floorMod(amount, tick);
        if (remainder == 0L) {
            return amount;
        }
        long down = amount - remainder;
        long up = down + tick;
        return remainder * 2L < tick ? down : up;
    }

    private static InstrumentTier tierOf(String instrCode) {
        if (instrCode.startsWith("13") || instrCode.startsWith("15")) {
            return new InstrumentTier(1, 1000, 100L);
        }
        if (instrCode.startsWith("2") || instrCode.startsWith("3") || instrCode.startsWith("6")) {
            return new InstrumentTier(2, 2000, 500L);
        }
        return new InstrumentTier(3, 4000, 1000L);
    }

    private static ExposureWork getWork(java.util.Map<ExposureKey, ExposureWork> map,
                                        String cifNo,
                                        String instrCode) {
        ExposureKey key = new ExposureKey(cifNo, instrCode);
        ExposureWork work = map.get(key);
        if (work == null) {
            work = new ExposureWork();
            map.put(key, work);
        }
        return work;
    }

    private static String inferSideFromOrderId(String orderId) {
        String upper = orderId.toUpperCase(java.util.Locale.ROOT);
        if (upper.startsWith("B") || upper.contains("-B-") || upper.endsWith("-B")) {
            return SIDE_BUY;
        }
        if (upper.startsWith("S") || upper.contains("-S-") || upper.endsWith("-S")) {
            return SIDE_SELL;
        }
        return "";
    }

    private static void requireColumns(String[] row, int min, String fileName) {
        if (row.length < min) {
            throw new IllegalArgumentException(fileName + " の項目数が不足しています");
        }
    }

    private static String requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + " が未設定です");
        }
        return value.trim();
    }

    private static String requireSide(String value) {
        String side = requireText(value, "SIDE-KBN");
        if (!SIDE_BUY.equals(side) && !SIDE_SELL.equals(side)) {
            throw new IllegalArgumentException("売買区分が不正です: " + side);
        }
        return side;
    }

    private static long parseLong(String value, String name) {
        try {
            return Long.parseLong(requireText(value, name));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " が数値ではありません: " + value, e);
        }
    }

    private static boolean isNumeric(String value) {
        if (value == null || value.trim().isEmpty()) {
            return false;
        }
        try {
            Long.parseLong(value.trim());
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private static long multiplyExact(long left, long right, String name) {
        try {
            return Math.multiplyExact(left, right);
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException(name + " が上限を超過しました", e);
        }
    }

    private static final class MarketRow {
        final String instrCode;
        final long bidAmt;
        final long askAmt;
        final long lastAmt;
        final long volQty;
        final long tickMinor;

        MarketRow(String instrCode, long bidAmt, long askAmt, long lastAmt, long volQty, long tickMinor) {
            this.instrCode = instrCode;
            this.bidAmt = bidAmt;
            this.askAmt = askAmt;
            this.lastAmt = lastAmt;
            this.volQty = volQty;
            this.tickMinor = tickMinor;
        }
    }

    private static final class DecisionRow {
        final String decisionId;
        final String orderId;
        final String cifNo;
        final String instrCode;
        final int decisionCd;
        final long notionalAmt;
        final long limitUsedAmt;

        DecisionRow(String decisionId,
                    String orderId,
                    String cifNo,
                    String instrCode,
                    int decisionCd,
                    long notionalAmt,
                    long limitUsedAmt) {
            this.decisionId = decisionId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.decisionCd = decisionCd;
            this.notionalAmt = notionalAmt;
            this.limitUsedAmt = limitUsedAmt;
        }
    }

    private static final class InstrumentTier {
        final int tier;
        final int rateBp;
        final long tickMinor;

        InstrumentTier(int tier, int rateBp, long tickMinor) {
            this.tier = tier;
            this.rateBp = rateBp;
            this.tickMinor = tickMinor;
        }
    }

    private static final class ExposureKey implements Comparable<ExposureKey> {
        final String cifNo;
        final String instrCode;

        ExposureKey(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        public int compareTo(ExposureKey other) {
            int c = this.cifNo.compareTo(other.cifNo);
            if (c != 0) {
                return c;
            }
            return this.instrCode.compareTo(other.instrCode);
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof ExposureKey)) {
                return false;
            }
            ExposureKey other = (ExposureKey) obj;
            return cifNo.equals(other.cifNo) && instrCode.equals(other.instrCode);
        }

        public int hashCode() {
            return 31 * cifNo.hashCode() + instrCode.hashCode();
        }
    }

    private static final class ExposureWork {
        long netNotional;
        long buyOpen;
        long sellOpen;
        final java.util.Map<String, Long> executedNotionalByOrder = new java.util.HashMap<String, Long>();
        final java.util.Map<String, String> sideByOrder = new java.util.HashMap<String, String>();
    }
}
