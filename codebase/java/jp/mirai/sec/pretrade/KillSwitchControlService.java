/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2020/03/10  渡辺 隆 (E-260)    初版作成。重大イベントからHFKILLを作成または解除する。
 */

package jp.mirai.sec.pretrade;

public class KillSwitchControlService {
    /* MIHFT_MAX_NOTIONAL=500000000 */
    private static final long MIHFT_MAX_NOTIONAL = 500_000_000L;
    private static final java.time.ZoneId JST = java.time.ZoneId.of("Asia/Tokyo");
    private static final java.time.LocalTime ZENBA_START = java.time.LocalTime.of(9, 0);
    private static final java.time.LocalTime ZENBA_END = java.time.LocalTime.of(11, 30);
    private static final java.time.LocalTime GOBA_START = java.time.LocalTime.of(12, 30);
    private static final java.time.LocalTime GOBA_END = java.time.LocalTime.of(15, 30);
    private static final String UPDATED_BY = "KILSWCTL";

    public static void main(String[] a) {
        java.util.List<Scrisk> riskEvents = loadScrisk(a.length > 0 ? a[0] : null);
        java.util.Map<String, Sccust> customers = loadSccust(a.length > 1 ? a[1] : null);
        java.util.Map<String, Scinstf> instruments = loadScinstf(a.length > 2 ? a[2] : null);

        java.util.Map<String, Hfkill> current = new java.util.LinkedHashMap<String, Hfkill>();
        current.put("CIF:10020001", new Hfkill("CIF:10020001", "1", "MARGIN", "2025-01-15T09:18:02+09:00", "OPRCTL"));
        current.put("INS:7203", new Hfkill("INS:7203", "1", "TICK", "2025-01-15T09:24:31+09:00", "OPRCTL"));

        RefDataService refData = new RefDataService(instruments);
        java.time.ZonedDateTime now = java.time.ZonedDateTime.now(JST);
        java.util.Map<String, Decision> decisions = evaluate(riskEvents, customers, refData, current, now);

        java.util.List<Hfkill> output = new java.util.ArrayList<Hfkill>();
        for (Decision decision : decisions.values()) {
            Hfkill before = current.get(decision.scopeKey);
            if (before == null || !before.killFlg.equals(decision.killFlg) || !before.reasonCd.equals(decision.reasonCd)) {
                output.add(new Hfkill(decision.scopeKey, decision.killFlg, decision.reasonCd, now.toString(), UPDATED_BY));
            }
        }

        for (Hfkill row : output) {
            System.out.println(row.scopeKey + "," + row.killFlg + "," + row.reasonCd + "," + row.updatedTs + "," + row.updatedBy);
        }
    }

    private static java.util.Map<String, Decision> evaluate(
            java.util.List<Scrisk> riskEvents,
            java.util.Map<String, Sccust> customers,
            RefDataService refData,
            java.util.Map<String, Hfkill> current,
            java.time.ZonedDateTime now) {
        java.util.Map<String, Aggregation> byCustomer = new java.util.LinkedHashMap<String, Aggregation>();
        java.util.Map<String, Aggregation> byInstrument = new java.util.LinkedHashMap<String, Aggregation>();
        Aggregation firm = new Aggregation();

        for (Scrisk event : riskEvents) {
            Scinstf instrument = refData.instrument(event.instrCode);
            Sccust customer = customers.get(event.cifNo);
            long exposure = exposure(event, customer, instrument);
            int severity = severityValue(event.severityKbn);

            aggregate(byCustomer, "CIF:" + event.cifNo, exposure, severity, event);
            aggregate(byInstrument, "INS:" + event.instrCode, exposure, severity, event);
            firm.add(exposure, severity, event);
        }

        java.util.Map<String, Decision> result = new java.util.LinkedHashMap<String, Decision>();
        boolean inSession = refData.isTradingTime(now);

        for (java.util.Map.Entry<String, Aggregation> entry : byCustomer.entrySet()) {
            Aggregation agg = entry.getValue();
            Sccust customer = customers.get(agg.last.cifNo);
            boolean overLimit = customer != null && customer.groupLimit > 0 && customer.groupUsedAmt + agg.exposure > customer.groupLimit;
            if (agg.maxSeverity >= 4 || overLimit || agg.rejectedCount >= 2) {
                result.put(entry.getKey(), new Decision(entry.getKey(), "1", reasonFor(agg, overLimit)));
            } else if (!inSession && current.containsKey(entry.getKey())) {
                result.put(entry.getKey(), new Decision(entry.getKey(), "0", "NORMAL"));
            }
        }

        for (java.util.Map.Entry<String, Aggregation> entry : byInstrument.entrySet()) {
            Aggregation agg = entry.getValue();
            Scinstf instrument = refData.instrument(agg.last.instrCode);
            boolean unsupportedBoard = instrument == null || !refData.isTradableBoard(instrument.boardCode);
            boolean excessNotional = agg.exposure >= MIHFT_MAX_NOTIONAL;
            boolean tierStress = instrument != null && instrument.instrTier >= 3 && agg.maxSeverity >= 3;
            if (unsupportedBoard || excessNotional || tierStress) {
                result.put(entry.getKey(), new Decision(entry.getKey(), "1", reasonFor(agg, unsupportedBoard || excessNotional)));
            } else if (!inSession && current.containsKey(entry.getKey())) {
                result.put(entry.getKey(), new Decision(entry.getKey(), "0", "NORMAL"));
            }
        }

        if (firm.maxSeverity >= 5 || firm.exposure >= MIHFT_MAX_NOTIONAL * 2L || firm.rejectedCount >= 5) {
            result.put("FIRM:ALL", new Decision("FIRM:ALL", "1", "SYSTEMIC"));
        } else if (!inSession && current.containsKey("FIRM:ALL")) {
            result.put("FIRM:ALL", new Decision("FIRM:ALL", "0", "NORMAL"));
        }

        if (inSession) {
            for (Hfkill row : current.values()) {
                if ("1".equals(row.killFlg) && !result.containsKey(row.scopeKey)) {
                    result.put(row.scopeKey, new Decision(row.scopeKey, "1", row.reasonCd));
                }
            }
        }

        return result;
    }

    private static void aggregate(java.util.Map<String, Aggregation> map, String key, long exposure, int severity, Scrisk event) {
        Aggregation agg = map.get(key);
        if (agg == null) {
            agg = new Aggregation();
            map.put(key, agg);
        }
        agg.add(exposure, severity, event);
    }

    private static long exposure(Scrisk event, Sccust customer, Scinstf instrument) {
        long observed = Math.max(0L, event.observedAmt);
        long thresholdGap = Math.max(0L, event.observedAmt - event.thresholdAmt);
        long accountUse = customer == null ? 0L : Math.max(customer.acctUsedAmt, customer.groupUsedAmt / 10L);
        int marginBp = instrument == null ? 4000 : marginBp(instrument.instrTier);
        long stressed = observed + thresholdGap + accountUse;
        return Math.max(observed, stressed * marginBp / 10_000L);
    }

    private static int marginBp(int tier) {
        if (tier == 1) {
            return 1000;
        }
        if (tier == 2) {
            return 2000;
        }
        return 4000;
    }

    private static String reasonFor(Aggregation agg, boolean hardLimit) {
        if (hardLimit && agg.exposure >= MIHFT_MAX_NOTIONAL) {
            return "NOTIONAL";
        }
        if ("MARGIN".equals(agg.last.riskCd) || "4".equals(agg.last.riskCd)) {
            return "MARGIN";
        }
        if ("TICK".equals(agg.last.riskCd) || "12".equals(agg.last.riskCd)) {
            return "TICK";
        }
        if (agg.maxSeverity >= 4) {
            return "SEVERE";
        }
        return "LIMIT";
    }

    private static int severityValue(String kbn) {
        if (kbn == null || kbn.trim().isEmpty()) {
            return 0;
        }
        String s = kbn.trim();
        if ("CRIT".equals(s) || "重大".equals(s)) {
            return 5;
        }
        if ("HIGH".equals(s) || "高".equals(s)) {
            return 4;
        }
        if ("MID".equals(s) || "中".equals(s)) {
            return 3;
        }
        if ("LOW".equals(s) || "低".equals(s)) {
            return 1;
        }
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    private static java.util.List<Scrisk> loadScrisk(String path) {
        if (path != null) {
            java.util.List<String[]> rows = readCsv(path);
            java.util.List<Scrisk> list = new java.util.ArrayList<Scrisk>();
            for (String[] r : rows) {
                if (r.length >= 9 && !"EVENT-ID".equals(r[0])) {
                    list.add(new Scrisk(r[0], r[1], r[2], r[3], r[4], r[5], parseLong(r[6]), parseLong(r[7]), r[8]));
                }
            }
            return list;
        }
        java.util.List<Scrisk> list = new java.util.ArrayList<Scrisk>();
        list.add(new Scrisk("E202501150001", "O90001421", "10020001", "7203", "MARGIN", "4", 410000000L, 300000000L, "2025-01-15T09:31:02+09:00"));
        list.add(new Scrisk("E202501150002", "O90001488", "10020001", "9984", "8", "3", 260000000L, 200000000L, "2025-01-15T09:32:19+09:00"));
        list.add(new Scrisk("E202501150003", "O90001511", "10020991", "4565", "TICK", "4", 89000000L, 20000000L, "2025-01-15T09:34:45+09:00"));
        list.add(new Scrisk("E202501150004", "O90001577", "10030008", "1570", "8", "2", 120000000L, 150000000L, "2025-01-15T09:37:09+09:00"));
        return list;
    }

    private static java.util.Map<String, Sccust> loadSccust(String path) {
        java.util.Map<String, Sccust> map = new java.util.LinkedHashMap<String, Sccust>();
        if (path != null) {
            for (String[] r : readCsv(path)) {
                if (r.length >= 4 && !"CIF-NO".equals(r[0])) {
                    map.put(r[0], new Sccust(r[0], parseLong(r[1]), parseLong(r[2]), parseLong(r[3])));
                }
            }
            return map;
        }
        map.put("10020001", new Sccust("10020001", 700000000L, 520000000L, 210000000L));
        map.put("10020991", new Sccust("10020991", 180000000L, 120000000L, 76000000L));
        map.put("10030008", new Sccust("10030008", 900000000L, 260000000L, 88000000L));
        return map;
    }

    private static java.util.Map<String, Scinstf> loadScinstf(String path) {
        java.util.Map<String, Scinstf> map = new java.util.LinkedHashMap<String, Scinstf>();
        if (path != null) {
            for (String[] r : readCsv(path)) {
                if (r.length >= 6 && !"INSTR-CODE".equals(r[0])) {
                    map.put(r[0], new Scinstf(r[0], r[1], (int) parseLong(r[2]), parseLong(r[3]), parseLong(r[4]), r[5]));
                }
            }
            return map;
        }
        map.put("7203", new Scinstf("7203", "トヨタ自動車", 1, 100L, 100L, "T1"));
        map.put("9984", new Scinstf("9984", "ソフトバンクグループ", 1, 100L, 100L, "T1"));
        map.put("4565", new Scinstf("4565", "そーせいグループ", 3, 1000L, 100L, "ST"));
        map.put("1570", new Scinstf("1570", "日経レバETF", 2, 500L, 1L, "ETF"));
        return map;
    }

    private static java.util.List<String[]> readCsv(String path) {
        java.util.List<String[]> rows = new java.util.ArrayList<String[]>();
        try {
            java.util.List<String> lines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
            for (String line : lines) {
                String trimmed = line.trim();
                if (!trimmed.isEmpty()) {
                    rows.add(trimmed.split("\\s*,\\s*", -1));
                }
            }
            return rows;
        } catch (java.io.IOException e) {
            throw new IllegalArgumentException("入力ファイルを読めません: " + path, e);
        }
    }

    private static long parseLong(String s) {
        if (s == null || s.trim().isEmpty()) {
            return 0L;
        }
        return Long.parseLong(s.trim());
    }

    private static final class RefDataService {
        private final java.util.Map<String, Scinstf> instruments;

        private RefDataService(java.util.Map<String, Scinstf> instruments) {
            this.instruments = instruments;
        }

        private Scinstf instrument(String instrCode) {
            return instruments.get(instrCode);
        }

        private boolean isTradableBoard(String boardCode) {
            return "T1".equals(boardCode) || "ST".equals(boardCode) || "ETF".equals(boardCode);
        }

        private boolean isTradingTime(java.time.ZonedDateTime ts) {
            java.time.DayOfWeek day = ts.getDayOfWeek();
            if (day == java.time.DayOfWeek.SATURDAY || day == java.time.DayOfWeek.SUNDAY) {
                return false;
            }
            java.time.LocalTime t = ts.withZoneSameInstant(JST).toLocalTime();
            return (!t.isBefore(ZENBA_START) && t.isBefore(ZENBA_END))
                    || (!t.isBefore(GOBA_START) && t.isBefore(GOBA_END));
        }
    }

    private static final class Scrisk {
        private final String eventId;
        private final String orderId;
        private final String cifNo;
        private final String instrCode;
        private final String riskCd;
        private final String severityKbn;
        private final long observedAmt;
        private final long thresholdAmt;
        private final String eventTs;

        private Scrisk(String eventId, String orderId, String cifNo, String instrCode, String riskCd, String severityKbn,
                       long observedAmt, long thresholdAmt, String eventTs) {
            this.eventId = eventId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.riskCd = riskCd;
            this.severityKbn = severityKbn;
            this.observedAmt = observedAmt;
            this.thresholdAmt = thresholdAmt;
            this.eventTs = eventTs;
        }
    }

    private static final class Sccust {
        private final String cifNo;
        private final long groupLimit;
        private final long groupUsedAmt;
        private final long acctUsedAmt;

        private Sccust(String cifNo, long groupLimit, long groupUsedAmt, long acctUsedAmt) {
            this.cifNo = cifNo;
            this.groupLimit = groupLimit;
            this.groupUsedAmt = groupUsedAmt;
            this.acctUsedAmt = acctUsedAmt;
        }
    }

    private static final class Scinstf {
        private final String instrCode;
        private final String instrName;
        private final int instrTier;
        private final long tickAmt;
        private final long lotQty;
        private final String boardCode;

        private Scinstf(String instrCode, String instrName, int instrTier, long tickAmt, long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class Hfkill {
        private final String scopeKey;
        private final String killFlg;
        private final String reasonCd;
        private final String updatedTs;
        private final String updatedBy;

        private Hfkill(String scopeKey, String killFlg, String reasonCd, String updatedTs, String updatedBy) {
            this.scopeKey = scopeKey;
            this.killFlg = killFlg;
            this.reasonCd = reasonCd;
            this.updatedTs = updatedTs;
            this.updatedBy = updatedBy;
        }
    }

    private static final class Decision {
        private final String scopeKey;
        private final String killFlg;
        private final String reasonCd;

        private Decision(String scopeKey, String killFlg, String reasonCd) {
            this.scopeKey = scopeKey;
            this.killFlg = killFlg;
            this.reasonCd = reasonCd;
        }
    }

    private static final class Aggregation {
        private long exposure;
        private int maxSeverity;
        private int rejectedCount;
        private Scrisk last;

        private void add(long eventExposure, int severity, Scrisk event) {
            exposure += eventExposure;
            maxSeverity = Math.max(maxSeverity, severity);
            last = event;
            if (event.observedAmt > event.thresholdAmt || severity >= 3) {
                rejectedCount++;
            }
        }
    }
}
