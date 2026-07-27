/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2025-01-21  開発一課    初版
 *
 * MIHFT_MAX_NOTIONAL=500000000
 */

package jp.mirai.sec.orderbook;

public class ReferenceWarmupService {
    private static final String SERVICE_ID = "REFWARM";
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    public static void main(String[] a) {
        RefDataService ref = new RefDataService();
        AuditSink audit = new AuditSink();

        WarmupResult result = new WarmupEngine(ref, audit).run();

        System.out.println("サービス=" + SERVICE_ID
                + " 判定=" + (result.stopRequired ? "停止" : "継続")
                + " 銘柄件数=" + result.instrumentCount
                + " ボード件数=" + result.boardCount
                + " ルート件数=" + result.routeCount
                + " 監査件数=" + audit.size());

        for (String row : audit.rows()) {
            System.out.println(row);
        }
    }

    private static final class WarmupEngine {
        private final RefDataService ref;
        private final AuditSink audit;

        private WarmupEngine(RefDataService ref, AuditSink audit) {
            this.ref = ref;
            this.audit = audit;
        }

        private WarmupResult run() {
            java.util.Map<String, Instrument> instruments = new java.util.LinkedHashMap<>();
            java.util.Set<String> boards = new java.util.LinkedHashSet<>();
            boolean stop = false;

            for (Instrument inst : ref.readInstruments()) {
                if (isBlank(inst.code)) {
                    audit.write("INSTR", "不明", "KEY-MISSING", "INSTR-CODE");
                    stop = true;
                    continue;
                }
                if (instruments.containsKey(inst.code)) {
                    audit.write("INSTR", inst.code, "DUPLICATE", "INSTR-CODE");
                    stop = true;
                    continue;
                }
                if (!validTier(inst.tier)) {
                    audit.write("INSTR", inst.code, "VALUE-ERROR", "INSTR-TIER");
                    stop = true;
                }
                int expectedTick = tickForTier(inst.tier);
                if (expectedTick > 0 && inst.tickAmount != expectedTick) {
                    audit.write("INSTR", inst.code, "VALUE-ERROR", "TICK-AMT");
                    stop = true;
                }
                if (inst.lotQuantity <= 0) {
                    audit.write("INSTR", inst.code, "VALUE-ERROR", "LOT-QTY");
                    stop = true;
                }
                if (!validBoard(inst.boardCode)) {
                    audit.write("INSTR", inst.code, "KEY-MISSING", "BOARD-CODE");
                    stop = true;
                }
                if (MIHFT_MAX_NOTIONAL <= 0) {
                    audit.write("SERVICE", SERVICE_ID, "VALUE-ERROR", "LIMIT");
                    stop = true;
                }
                instruments.put(inst.code, inst);
                boards.add(inst.boardCode);
            }

            java.util.Map<String, Fee> fees = new java.util.LinkedHashMap<>();
            for (Fee fee : ref.readFees()) {
                if (!validBoard(fee.boardCode)) {
                    audit.write("BOARD", nullToUnknown(fee.boardCode), "KEY-MISSING", "BOARD-CODE");
                    stop = true;
                    continue;
                }
                if (fees.containsKey(fee.boardCode)) {
                    audit.write("BOARD", fee.boardCode, "DUPLICATE", "BOARD-CODE");
                    stop = true;
                    continue;
                }
                if (fee.rate < 0.0d || fee.minFeeAmount < 0L) {
                    audit.write("BOARD", fee.boardCode, "VALUE-ERROR", "FEE");
                    stop = true;
                }
                fees.put(fee.boardCode, fee);
            }

            for (String board : boards) {
                if (!fees.containsKey(board)) {
                    audit.write("BOARD", board, "KEY-MISSING", "FEE");
                    stop = true;
                }
            }

            java.util.Set<String> calendarKeys = new java.util.LinkedHashSet<>();
            for (SessionCalendar cal : ref.readCalendars()) {
                String key = cal.sessionDate + ":" + cal.sessionKind;
                if (isBlank(cal.sessionDate) || isBlank(cal.sessionKind)) {
                    audit.write("CAL", "不明", "KEY-MISSING", "SESS");
                    stop = true;
                    continue;
                }
                if (!calendarKeys.add(key)) {
                    audit.write("CAL", key, "DUPLICATE", "SESS");
                    stop = true;
                    continue;
                }
                if ("1".equals(cal.sessionKind) && compareTime(cal.openTime, cal.closeTime) >= 0) {
                    audit.write("CAL", key, "VALUE-ERROR", "OPEN-CLOSE");
                    stop = true;
                }
            }

            java.util.Set<String> routeKeys = new java.util.LinkedHashSet<>();
            java.util.Map<String, Integer> enabledRouteCountByInstrument = new java.util.LinkedHashMap<>();
            int routeCount = 0;

            for (Route route : ref.readRoutes()) {
                routeCount++;
                if (isBlank(route.routeKey)) {
                    audit.write("ROUTE", "不明", "KEY-MISSING", "ROUTE-KEY");
                    stop = true;
                    continue;
                }
                if (!routeKeys.add(route.routeKey)) {
                    audit.write("ROUTE", route.routeKey, "DUPLICATE", "ROUTE-KEY");
                    stop = true;
                    continue;
                }
                if (!instruments.containsKey(route.instrumentCode)) {
                    audit.write("INSTR", nullToUnknown(route.instrumentCode), "KEY-MISSING", "ROUTE-INSTR");
                    stop = true;
                }
                if (!fees.containsKey(route.boardCode)) {
                    audit.write("BOARD", nullToUnknown(route.boardCode), "KEY-MISSING", "ROUTE-BOARD");
                    stop = true;
                }
                if (route.priorityNo <= 0 || route.maxSliceQuantity <= 0) {
                    audit.write("ROUTE", route.routeKey, "VALUE-ERROR", "ROUTE-LIMIT");
                    stop = true;
                }
                if (!"Y".equals(route.enabledFlag) && !"N".equals(route.enabledFlag)) {
                    audit.write("ROUTE", route.routeKey, "VALUE-ERROR", "ENABLED-FLG");
                    stop = true;
                }
                if ("Y".equals(route.enabledFlag)) {
                    enabledRouteCountByInstrument.put(
                            route.instrumentCode,
                            enabledRouteCountByInstrument.getOrDefault(route.instrumentCode, 0) + 1);
                }
            }

            for (String code : instruments.keySet()) {
                if (enabledRouteCountByInstrument.getOrDefault(code, 0) == 0) {
                    audit.write("INSTR", code, "KEY-MISSING", "ENABLED-ROUTE");
                    stop = true;
                }
            }

            return new WarmupResult(stop, instruments.size(), fees.size(), routeCount);
        }

        private static boolean validTier(int tier) {
            return tier == 1 || tier == 2 || tier == 3;
        }

        private static int tickForTier(int tier) {
            switch (tier) {
                case 1:
                    return 100;
                case 2:
                    return 500;
                case 3:
                    return 1000;
                default:
                    return -1;
            }
        }

        private static boolean validBoard(String board) {
            return "T1".equals(board) || "ST".equals(board) || "ETF".equals(board);
        }

        private static boolean isBlank(String s) {
            return s == null || s.trim().isEmpty();
        }

        private static String nullToUnknown(String s) {
            return isBlank(s) ? "不明" : s;
        }

        private static int compareTime(String left, String right) {
            if (isBlank(left) || isBlank(right)) {
                return 1;
            }
            return left.compareTo(right);
        }
    }

    private static final class RefDataService {
        private java.util.List<Instrument> readInstruments() {
            return parseCsv(INSTR_CSV, row -> new Instrument(
                    row[0], row[1], parseInt(row[2]), parseInt(row[3]), parseLong(row[4]), row[5]));
        }

        private java.util.List<Fee> readFees() {
            return parseCsv(FEE_CSV, row -> new Fee(row[0], parseDouble(row[1]), parseLong(row[2])));
        }

        private java.util.List<SessionCalendar> readCalendars() {
            return parseCsv(CAL_CSV, row -> new SessionCalendar(row[0], row[1], row[2], row[3]));
        }

        private java.util.List<Route> readRoutes() {
            return parseCsv(ROUTE_KSDS, row -> new Route(
                    row[0], row[1], row[2], row[3], parseInt(row[4]), parseLong(row[5]), row[6]));
        }

        private static <T> java.util.List<T> parseCsv(String csv, RowMapper<T> mapper) {
            java.util.List<T> list = new java.util.ArrayList<>();
            String[] lines = csv.split("\\R");
            for (int i = 1; i < lines.length; i++) {
                String line = lines[i].trim();
                if (line.isEmpty()) {
                    continue;
                }
                list.add(mapper.map(line.split(",", -1)));
            }
            return list;
        }

        private static int parseInt(String s) {
            return Integer.parseInt(s.trim());
        }

        private static long parseLong(String s) {
            return Long.parseLong(s.trim());
        }

        private static double parseDouble(String s) {
            return Double.parseDouble(s.trim());
        }
    }

    private interface RowMapper<T> {
        T map(String[] row);
    }

    private static final class AuditSink {
        private final java.util.List<String> rows = new java.util.ArrayList<>();
        private long sequence = 1L;

        private void write(String eventKind, String objectId, String detailCode, String fieldCode) {
            String auditId = String.format("AUD%08d", sequence++);
            String eventTs = java.time.OffsetDateTime.now(java.time.ZoneOffset.ofHours(9)).toString();
            rows.add(auditId + "," + eventTs + "," + SERVICE_ID + ","
                    + objectId + "," + eventKind + "," + detailCode + ":" + fieldCode);
        }

        private int size() {
            return rows.size();
        }

        private java.util.List<String> rows() {
            return java.util.Collections.unmodifiableList(rows);
        }
    }

    private static final class WarmupResult {
        private final boolean stopRequired;
        private final int instrumentCount;
        private final int boardCount;
        private final int routeCount;

        private WarmupResult(boolean stopRequired, int instrumentCount, int boardCount, int routeCount) {
            this.stopRequired = stopRequired;
            this.instrumentCount = instrumentCount;
            this.boardCount = boardCount;
            this.routeCount = routeCount;
        }
    }

    private static final class Instrument {
        private final String code;
        private final String name;
        private final int tier;
        private final int tickAmount;
        private final long lotQuantity;
        private final String boardCode;

        private Instrument(String code, String name, int tier, int tickAmount, long lotQuantity, String boardCode) {
            this.code = code;
            this.name = name;
            this.tier = tier;
            this.tickAmount = tickAmount;
            this.lotQuantity = lotQuantity;
            this.boardCode = boardCode;
        }
    }

    private static final class Fee {
        private final String boardCode;
        private final double rate;
        private final long minFeeAmount;

        private Fee(String boardCode, double rate, long minFeeAmount) {
            this.boardCode = boardCode;
            this.rate = rate;
            this.minFeeAmount = minFeeAmount;
        }
    }

    private static final class SessionCalendar {
        private final String sessionDate;
        private final String sessionKind;
        private final String openTime;
        private final String closeTime;

        private SessionCalendar(String sessionDate, String sessionKind, String openTime, String closeTime) {
            this.sessionDate = sessionDate;
            this.sessionKind = sessionKind;
            this.openTime = openTime;
            this.closeTime = closeTime;
        }
    }

    private static final class Route {
        private final String routeKey;
        private final String instrumentCode;
        private final String boardCode;
        private final String venueKind;
        private final int priorityNo;
        private final long maxSliceQuantity;
        private final String enabledFlag;

        private Route(String routeKey, String instrumentCode, String boardCode, String venueKind,
                      int priorityNo, long maxSliceQuantity, String enabledFlag) {
            this.routeKey = routeKey;
            this.instrumentCode = instrumentCode;
            this.boardCode = boardCode;
            this.venueKind = venueKind;
            this.priorityNo = priorityNo;
            this.maxSliceQuantity = maxSliceQuantity;
            this.enabledFlag = enabledFlag;
        }
    }

    private static final String INSTR_CSV =
            "INSTR-CODE,INSTR-NAME,INSTR-TIER,TICK-AMT,LOT-QTY,BOARD-CODE\n"
                    + "7203,トヨタ自動車,1,100,100,T1\n"
                    + "6758,ソニーグループ,1,100,100,T1\n"
                    + "9984,ソフトバンクグループ,2,500,100,T1\n"
                    + "4385,メルカリ,2,100,100,ST\n"
                    + "1306,TOPIX連動ETF,1,100,10,ETF\n"
                    + "7777,架空バイオ,3,1000,100,ST\n"
                    + "7203,トヨタ自動車重複,1,100,100,T1\n"
                    + "9999,未定義市場銘柄,3,1000,0,XX\n";

    private static final String FEE_CSV =
            "BOARD-CODE,FEE-RATE,MIN-FEE-AMT\n"
                    + "T1,0.00012,80\n"
                    + "ST,0.00018,100\n"
                    + "T1,0.00011,70\n";

    private static final String CAL_CSV =
            "SESS-DT,SESS-KBN,OPEN-TS,CLOSE-TS\n"
                    + "20250115,1,09:00:00,15:30:00\n"
                    + "20250115,0,,\n"
                    + "20250115,1,09:00:00,15:30:00\n"
                    + "20250115,1,15:30:00,09:00:00\n";

    private static final String ROUTE_KSDS =
            "ROUTE-KEY,INSTR-CODE,BOARD-CODE,VENUE-KBN,PRIORITY-NO,MAX-SLICE-QTY,ENABLED-FLG\n"
                    + "R-T1-7203-A,7203,T1,PRI,1,5000,Y\n"
                    + "R-T1-6758-A,6758,T1,PRI,1,3000,Y\n"
                    + "R-T1-9984-A,9984,T1,PRI,1,1000,Y\n"
                    + "R-ST-4385-A,4385,ST,ALT,1,800,Y\n"
                    + "R-ST-7777-A,7777,ST,ALT,1,500,N\n"
                    + "R-ETF-1306-A,1306,ETF,PRI,1,10000,Y\n"
                    + "R-ETF-1306-A,1306,ETF,PRI,2,9000,Y\n"
                    + "R-MISS-0000,0000,T1,PRI,1,100,Y\n";
}
