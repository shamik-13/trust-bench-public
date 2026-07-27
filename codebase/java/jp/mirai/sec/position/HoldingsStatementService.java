package jp.mirai.sec.position;

public class HoldingsStatementService {
    /*
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2022/02/22  今井 彩 (E-230)  初版作成
     */

    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final java.time.format.DateTimeFormatter DATE_FMT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    private static final java.util.Map<Integer, Integer> MARGIN_RATE_BP;
    private static final java.util.Map<Integer, Long> TIER_TICK_AMT;
    private static final java.util.Set<String> BOARD_CODES =
            new java.util.HashSet<String>(java.util.Arrays.asList("T1", "ST", "ETF"));

    static {
        java.util.Map<Integer, Integer> rate = new java.util.HashMap<Integer, Integer>();
        rate.put(1, 1000);
        rate.put(2, 2000);
        rate.put(3, 4000);
        MARGIN_RATE_BP = java.util.Collections.unmodifiableMap(rate);

        java.util.Map<Integer, Long> tick = new java.util.HashMap<Integer, Long>();
        tick.put(1, 100L);
        tick.put(2, 500L);
        tick.put(3, 1000L);
        TIER_TICK_AMT = java.util.Collections.unmodifiableMap(tick);
    }

    public static void main(String[] a) {
        java.util.List<String> schldf = java.util.Arrays.asList(
                "CIF10001,7203,20250115,12000,15000,2000",
                "CIF10001,6758,20250115,3500,3500,0",
                "CIF10001,1306,20250115,900,900,100",
                "CIF10002,9984,20250115,1000,1800,400",
                "CIF10002,4385,20250115,600,600,600",
                "CIF10003,8306,20250115,24000,27000,5000"
        );

        java.util.List<String> scposf = java.util.Arrays.asList(
                "CIF10001,7203,13000,2715000,186000",
                "CIF10001,6758,3500,4520000,-52000",
                "CIF10001,1306,900,3120000,8000",
                "CIF10002,9984,1200,8950000,-340000",
                "CIF10002,4385,600,1870000,0",
                "CIF10003,8306,25000,1250000,91000"
        );

        java.util.List<String> scinstf = java.util.Arrays.asList(
                "7203,トヨタ自動車,1,100,100,T1",
                "6758,ソニーグループ,1,100,100,T1",
                "1306,TOPIX連動型上場投信,1,100,10,ETF",
                "9984,ソフトバンクグループ,2,500,100,T1",
                "4385,メルカリ,3,1000,100,ST",
                "8306,三菱UFJフィナンシャル・グループ,1,100,100,T1"
        );

        HoldingsStatementService service = new HoldingsStatementService();
        Result result = service.buildStatement(schldf, scposf, scinstf);

        for (StatementLine line : result.lines) {
            System.out.println(line.toDisplay());
        }
        for (SchldfRecord record : result.updatedHoldings) {
            System.out.println(record.toCsv());
        }
    }

    private Result buildStatement(
            java.util.List<String> schldfLines,
            java.util.List<String> scposfLines,
            java.util.List<String> scinstfLines) {
        java.util.Map<String, InstrumentRecord> instruments = readInstruments(scinstfLines);
        java.util.Map<PositionKey, PositionRecord> positions = readPositions(scposfLines);
        java.util.Map<PositionKey, SchldfRecord> holdings = readHoldings(schldfLines);

        java.util.List<StatementLine> out = new java.util.ArrayList<StatementLine>();
        java.util.List<SchldfRecord> writeBack = new java.util.ArrayList<SchldfRecord>();

        for (java.util.Map.Entry<PositionKey, SchldfRecord> entry : holdings.entrySet()) {
            PositionKey key = entry.getKey();
            SchldfRecord holding = entry.getValue();
            PositionRecord position = positions.get(key);
            InstrumentRecord instrument = instruments.get(key.instrCode);

            if (position == null || instrument == null) {
                continue;
            }

            long displayQty = Math.min(holding.tradeQty, position.netQty);
            long sellableQty = Math.max(0L, holding.tradeQty - holding.restrictedQty);
            long marketAmt = checkedMultiply(displayQty, position.avgAmt / Math.max(1L, displayQty));
            long marginNeed = marketAmt * MARGIN_RATE_BP.get(instrument.instrTier) / 10000L;
            int decisionCode = decide(instrument, marketAmt);

            out.add(new StatementLine(
                    key.cifNo,
                    key.instrCode,
                    instrument.instrName,
                    instrument.instrTier,
                    instrument.boardCode,
                    holding.asofDt,
                    holding.settledQty,
                    displayQty,
                    holding.restrictedQty,
                    sellableQty,
                    position.netQty,
                    position.avgAmt,
                    position.rlzdAmt,
                    marginNeed,
                    decisionCode));

            writeBack.add(new SchldfRecord(
                    key.cifNo,
                    key.instrCode,
                    holding.asofDt,
                    holding.settledQty,
                    holding.tradeQty,
                    holding.restrictedQty));
        }

        out.sort(new java.util.Comparator<StatementLine>() {
            @Override
            public int compare(StatementLine x, StatementLine y) {
                int c = x.cifNo.compareTo(y.cifNo);
                if (c != 0) {
                    return c;
                }
                return x.instrCode.compareTo(y.instrCode);
            }
        });

        writeBack.sort(new java.util.Comparator<SchldfRecord>() {
            @Override
            public int compare(SchldfRecord x, SchldfRecord y) {
                int c = x.cifNo.compareTo(y.cifNo);
                if (c != 0) {
                    return c;
                }
                return x.instrCode.compareTo(y.instrCode);
            }
        });

        return new Result(out, writeBack);
    }

    private java.util.Map<PositionKey, SchldfRecord> readHoldings(java.util.List<String> lines) {
        java.util.Map<PositionKey, SchldfRecord> map = new java.util.LinkedHashMap<PositionKey, SchldfRecord>();
        for (String line : lines) {
            String[] f = split(line, 6);
            String cifNo = code(f[0], "CIF-NO");
            String instrCode = code(f[1], "INSTR-CODE");
            java.time.LocalDate asofDt = date(f[2], "ASOF-DT");
            long settledQty = nonNegativeLong(f[3], "SETTLED-QTY");
            long tradeQty = nonNegativeLong(f[4], "TRADE-QTY");
            long restrictedQty = nonNegativeLong(f[5], "RESTRICTED-QTY");

            if (restrictedQty > tradeQty) {
                throw new IllegalArgumentException("制限数量が取引数量を超過しています: " + cifNo + "/" + instrCode);
            }

            PositionKey key = new PositionKey(cifNo, instrCode);
            SchldfRecord prior = map.get(key);
            if (prior == null) {
                map.put(key, new SchldfRecord(cifNo, instrCode, asofDt, settledQty, tradeQty, restrictedQty));
            } else {
                map.put(key, new SchldfRecord(
                        cifNo,
                        instrCode,
                        maxDate(prior.asofDt, asofDt),
                        prior.settledQty + settledQty,
                        prior.tradeQty + tradeQty,
                        prior.restrictedQty + restrictedQty));
            }
        }
        return map;
    }

    private java.util.Map<PositionKey, PositionRecord> readPositions(java.util.List<String> lines) {
        java.util.Map<PositionKey, PositionRecord> map = new java.util.HashMap<PositionKey, PositionRecord>();
        for (String line : lines) {
            String[] f = split(line, 5);
            String cifNo = code(f[0], "CIF-NO");
            String instrCode = code(f[1], "INSTR-CODE");
            long netQty = nonNegativeLong(f[2], "NET-QTY");
            long avgAmt = signedLong(f[3], "AVG-AMT");
            long rlzdAmt = signedLong(f[4], "RLZD-AMT");

            PositionKey key = new PositionKey(cifNo, instrCode);
            PositionRecord prior = map.get(key);
            if (prior == null) {
                map.put(key, new PositionRecord(cifNo, instrCode, netQty, avgAmt, rlzdAmt));
            } else {
                map.put(key, new PositionRecord(
                        cifNo,
                        instrCode,
                        prior.netQty + netQty,
                        prior.avgAmt + avgAmt,
                        prior.rlzdAmt + rlzdAmt));
            }
        }
        return map;
    }

    private java.util.Map<String, InstrumentRecord> readInstruments(java.util.List<String> lines) {
        java.util.Map<String, InstrumentRecord> map = new java.util.HashMap<String, InstrumentRecord>();
        for (String line : lines) {
            String[] f = split(line, 6);
            String instrCode = code(f[0], "INSTR-CODE");
            String instrName = text(f[1], "INSTR-NAME");
            int instrTier = positiveInt(f[2], "INSTR-TIER");
            long tickAmt = nonNegativeLong(f[3], "TICK-AMT");
            long lotQty = nonNegativeLong(f[4], "LOT-QTY");
            String boardCode = code(f[5], "BOARD-CODE");

            if (!MARGIN_RATE_BP.containsKey(instrTier)) {
                throw new IllegalArgumentException("銘柄階層が不正です: " + instrCode);
            }
            if (tickAmt != TIER_TICK_AMT.get(instrTier)) {
                throw new IllegalArgumentException("呼値単位が階層と不一致です: " + instrCode);
            }
            if (lotQty == 0L) {
                throw new IllegalArgumentException("売買単位がゼロです: " + instrCode);
            }
            if (!BOARD_CODES.contains(boardCode)) {
                throw new IllegalArgumentException("市場コードが不正です: " + instrCode);
            }

            map.put(instrCode, new InstrumentRecord(instrCode, instrName, instrTier, tickAmt, lotQty, boardCode));
        }
        return map;
    }

    private int decide(InstrumentRecord instrument, long notional) {
        if (notional > MIHFT_MAX_NOTIONAL) {
            return 8;
        }
        if (instrument.tickAmt != TIER_TICK_AMT.get(instrument.instrTier)) {
            return 12;
        }
        long margin = notional * MARGIN_RATE_BP.get(instrument.instrTier) / 10000L;
        if (margin > MIHFT_MAX_NOTIONAL / 2L) {
            return 4;
        }
        return 0;
    }

    private static String[] split(String line, int size) {
        String[] f = line.split(",", -1);
        if (f.length != size) {
            throw new IllegalArgumentException("項目数が不正です: " + line);
        }
        for (int i = 0; i < f.length; i++) {
            f[i] = f[i].trim();
        }
        return f;
    }

    private static String code(String s, String name) {
        if (s == null || s.trim().isEmpty()) {
            throw new IllegalArgumentException(name + " が未設定です");
        }
        return s.trim();
    }

    private static String text(String s, String name) {
        if (s == null || s.trim().isEmpty()) {
            throw new IllegalArgumentException(name + " が未設定です");
        }
        return s.trim();
    }

    private static java.time.LocalDate date(String s, String name) {
        try {
            return java.time.LocalDate.parse(s, DATE_FMT);
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException(name + " が不正です: " + s, e);
        }
    }

    private static long nonNegativeLong(String s, String name) {
        long n = signedLong(s, name);
        if (n < 0L) {
            throw new IllegalArgumentException(name + " が負数です: " + s);
        }
        return n;
    }

    private static long signedLong(String s, String name) {
        try {
            return Long.parseLong(s);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " が数値ではありません: " + s, e);
        }
    }

    private static int positiveInt(String s, String name) {
        try {
            int n = Integer.parseInt(s);
            if (n <= 0) {
                throw new IllegalArgumentException(name + " が正数ではありません: " + s);
            }
            return n;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " が数値ではありません: " + s, e);
        }
    }

    private static java.time.LocalDate maxDate(java.time.LocalDate x, java.time.LocalDate y) {
        return x.isAfter(y) ? x : y;
    }

    private static long checkedMultiply(long x, long y) {
        try {
            return Math.multiplyExact(x, y);
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException("金額計算が上限を超過しました", e);
        }
    }

    private static final class PositionKey {
        private final String cifNo;
        private final String instrCode;

        private PositionKey(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof PositionKey)) {
                return false;
            }
            PositionKey other = (PositionKey) o;
            return cifNo.equals(other.cifNo) && instrCode.equals(other.instrCode);
        }

        @Override
        public int hashCode() {
            return 31 * cifNo.hashCode() + instrCode.hashCode();
        }
    }

    private static final class SchldfRecord {
        private final String cifNo;
        private final String instrCode;
        private final java.time.LocalDate asofDt;
        private final long settledQty;
        private final long tradeQty;
        private final long restrictedQty;

        private SchldfRecord(
                String cifNo,
                String instrCode,
                java.time.LocalDate asofDt,
                long settledQty,
                long tradeQty,
                long restrictedQty) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.asofDt = asofDt;
            this.settledQty = settledQty;
            this.tradeQty = tradeQty;
            this.restrictedQty = restrictedQty;
        }

        private String toCsv() {
            return "SCHLDF書込,"
                    + cifNo + ","
                    + instrCode + ","
                    + DATE_FMT.format(asofDt) + ","
                    + settledQty + ","
                    + tradeQty + ","
                    + restrictedQty;
        }
    }

    private static final class PositionRecord {
        private final String cifNo;
        private final String instrCode;
        private final long netQty;
        private final long avgAmt;
        private final long rlzdAmt;

        private PositionRecord(String cifNo, String instrCode, long netQty, long avgAmt, long rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    private static final class InstrumentRecord {
        private final String instrCode;
        private final String instrName;
        private final int instrTier;
        private final long tickAmt;
        private final long lotQty;
        private final String boardCode;

        private InstrumentRecord(
                String instrCode,
                String instrName,
                int instrTier,
                long tickAmt,
                long lotQty,
                String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class StatementLine {
        private final String cifNo;
        private final String instrCode;
        private final String instrName;
        private final int instrTier;
        private final String boardCode;
        private final java.time.LocalDate asofDt;
        private final long settledQty;
        private final long tradeQty;
        private final long restrictedQty;
        private final long sellableQty;
        private final long netQty;
        private final long avgAmt;
        private final long rlzdAmt;
        private final long marginNeed;
        private final int decisionCode;

        private StatementLine(
                String cifNo,
                String instrCode,
                String instrName,
                int instrTier,
                String boardCode,
                java.time.LocalDate asofDt,
                long settledQty,
                long tradeQty,
                long restrictedQty,
                long sellableQty,
                long netQty,
                long avgAmt,
                long rlzdAmt,
                long marginNeed,
                int decisionCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.boardCode = boardCode;
            this.asofDt = asofDt;
            this.settledQty = settledQty;
            this.tradeQty = tradeQty;
            this.restrictedQty = restrictedQty;
            this.sellableQty = sellableQty;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
            this.marginNeed = marginNeed;
            this.decisionCode = decisionCode;
        }

        private String toDisplay() {
            return cifNo
                    + " " + instrCode
                    + " " + instrName
                    + " 階層=" + instrTier
                    + " 市場=" + boardCode
                    + " 基準日=" + DATE_FMT.format(asofDt)
                    + " 受渡済=" + settledQty
                    + " 取引=" + tradeQty
                    + " 制限=" + restrictedQty
                    + " 売却可能=" + sellableQty
                    + " ネット=" + netQty
                    + " 平均金額=" + avgAmt
                    + " 実現損益=" + rlzdAmt
                    + " 必要証拠金=" + marginNeed
                    + " 判定=" + decisionCode;
        }
    }

    private static final class Result {
        private final java.util.List<StatementLine> lines;
        private final java.util.List<SchldfRecord> updatedHoldings;

        private Result(java.util.List<StatementLine> lines, java.util.List<SchldfRecord> updatedHoldings) {
            this.lines = lines;
            this.updatedHoldings = updatedHoldings;
        }
    }
}
