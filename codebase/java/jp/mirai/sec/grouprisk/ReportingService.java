/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2025-06-03  市場基盤部  顧客別約定・建玉レポート初版作成
 */

package jp.mirai.sec.grouprisk;

public class ReportingService {
    private static final java.nio.charset.Charset CSV_CHARSET = java.nio.charset.StandardCharsets.UTF_8;
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final int IDX_EXEC_ID = 0;
    private static final int IDX_ORDER_ID = 1;
    private static final int IDX_INSTR_CODE = 2;
    private static final int IDX_SIDE_KBN = 3;
    private static final int IDX_FILL_QTY = 4;
    private static final int IDX_FILL_AMT = 5;
    private static final int IDX_EXEC_TS = 6;

    private static final int IDX_CIF_NO = 0;
    private static final int IDX_POS_INSTR_CODE = 1;
    private static final int IDX_NET_QTY = 2;
    private static final int IDX_AVG_AMT = 3;
    private static final int IDX_RLZD_AMT = 4;

    public static void main(String[] a) throws Exception {
        if (a.length != 2) {
            System.err.println("使用法: java ReportingService SCEXEC.csv SCPOSF.csv");
            System.exit(2);
        }

        java.util.List<Execution> executions = readExecutions(java.nio.file.Paths.get(a[0]));
        java.util.List<Position> positions = readPositions(java.nio.file.Paths.get(a[1]));
        java.util.List<CustomerReport> reports = buildReports(executions, positions);

        writeReports(reports, System.out);
    }

    public static java.util.List<CustomerReport> buildReports(
            java.util.List<Execution> executions,
            java.util.List<Position> positions) {
        java.util.Map<String, InstrumentExecutionSummary> executionByInstrument = new java.util.TreeMap<>();
        for (Execution e : executions) {
            if (!isValidSide(e.sideKbn)) {
                continue;
            }
            InstrumentExecutionSummary s = executionByInstrument.computeIfAbsent(
                    e.instrCode, InstrumentExecutionSummary::new);
            s.add(e);
        }

        java.util.Map<String, CustomerReport> customerMap = new java.util.TreeMap<>();
        for (Position p : positions) {
            CustomerReport report = customerMap.computeIfAbsent(p.cifNo, CustomerReport::new);
            report.addPosition(p);

            InstrumentExecutionSummary s = executionByInstrument.get(p.instrCode);
            if (s != null) {
                report.addExecutionSummary(s);
            }
        }

        return new java.util.ArrayList<>(customerMap.values());
    }

    private static java.util.List<Execution> readExecutions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Execution> rows = new java.util.ArrayList<>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.trim().isEmpty()) {
                continue;
            }
            java.util.List<String> c = parseCsvLine(line);
            if (i == 0 && isExecHeader(c)) {
                continue;
            }
            if (c.size() < 7) {
                throw new IllegalArgumentException("SCEXEC列数不正: 行=" + (i + 1));
            }
            rows.add(new Execution(
                    c.get(IDX_EXEC_ID),
                    c.get(IDX_ORDER_ID),
                    c.get(IDX_INSTR_CODE),
                    c.get(IDX_SIDE_KBN),
                    parseLong(c.get(IDX_FILL_QTY), "FILL-QTY", i + 1),
                    parseLong(c.get(IDX_FILL_AMT), "FILL-AMT", i + 1),
                    parseDateTime(c.get(IDX_EXEC_TS), "EXEC-TS", i + 1)));
        }
        return rows;
    }

    private static java.util.List<Position> readPositions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Position> rows = new java.util.ArrayList<>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.trim().isEmpty()) {
                continue;
            }
            java.util.List<String> c = parseCsvLine(line);
            if (i == 0 && isPositionHeader(c)) {
                continue;
            }
            if (c.size() < 5) {
                throw new IllegalArgumentException("SCPOSF列数不正: 行=" + (i + 1));
            }
            rows.add(new Position(
                    c.get(IDX_CIF_NO),
                    c.get(IDX_POS_INSTR_CODE),
                    parseLong(c.get(IDX_NET_QTY), "NET-QTY", i + 1),
                    parseLong(c.get(IDX_AVG_AMT), "AVG-AMT", i + 1),
                    parseLong(c.get(IDX_RLZD_AMT), "RLZD-AMT", i + 1)));
        }
        return rows;
    }

    private static void writeReports(java.util.List<CustomerReport> reports, java.io.PrintStream out) {
        out.println("CIF-NO,EXEC-COUNT,BUY-QTY,SELL-QTY,BUY-AMT,SELL-AMT,NET-QTY,POSITION-AMT,RLZD-AMT,EST-MARGIN-AMT,ALERT-KBN");
        for (CustomerReport r : reports) {
            out.println(joinCsv(
                    r.cifNo,
                    String.valueOf(r.execCount),
                    String.valueOf(r.buyQty),
                    String.valueOf(r.sellQty),
                    String.valueOf(r.buyAmt),
                    String.valueOf(r.sellAmt),
                    String.valueOf(r.netQty),
                    String.valueOf(r.positionAmt),
                    String.valueOf(r.rlzdAmt),
                    String.valueOf(r.marginAmt),
                    r.alertKbn()));
        }
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> cols = new java.util.ArrayList<>();
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
                cols.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        cols.add(b.toString().trim());
        return cols;
    }

    private static String joinCsv(String... cols) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < cols.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            b.append(escapeCsv(cols[i]));
        }
        return b.toString();
    }

    private static String escapeCsv(String v) {
        if (v == null) {
            return "";
        }
        if (v.indexOf(',') < 0 && v.indexOf('"') < 0 && v.indexOf('\n') < 0 && v.indexOf('\r') < 0) {
            return v;
        }
        return "\"" + v.replace("\"", "\"\"") + "\"";
    }

    private static long parseLong(String value, String name, int row) {
        try {
            return Long.parseLong(value.replace(",", ""));
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(name + "数値不正: 行=" + row + " 値=" + value, e);
        }
    }

    private static java.time.LocalDateTime parseDateTime(String value, String name, int row) {
        String v = value.trim();
        java.time.format.DateTimeFormatter[] formats = new java.time.format.DateTimeFormatter[] {
                java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME,
                java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"),
                java.time.format.DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss")
        };
        for (java.time.format.DateTimeFormatter f : formats) {
            try {
                return java.time.LocalDateTime.parse(v, f);
            } catch (RuntimeException ignored) {
                // 次形式を試行する。
            }
        }
        throw new IllegalArgumentException(name + "日時不正: 行=" + row + " 値=" + value);
    }

    private static boolean isExecHeader(java.util.List<String> c) {
        return !c.isEmpty() && "EXEC-ID".equalsIgnoreCase(c.get(0));
    }

    private static boolean isPositionHeader(java.util.List<String> c) {
        return !c.isEmpty() && "CIF-NO".equalsIgnoreCase(c.get(0));
    }

    private static boolean isValidSide(String sideKbn) {
        return "B".equals(sideKbn) || "S".equals(sideKbn);
    }

    private static int tierOf(String instrCode) {
        int h = Math.abs(instrCode == null ? 0 : instrCode.hashCode());
        int m = h % 10;
        if (m < 5) {
            return 1;
        }
        if (m < 8) {
            return 2;
        }
        return 3;
    }

    private static int marginRateBp(int tier) {
        if (tier == 1) {
            return 1000;
        }
        if (tier == 2) {
            return 2000;
        }
        return 4000;
    }

    public static final class Execution {
        public final String execId;
        public final String orderId;
        public final String instrCode;
        public final String sideKbn;
        public final long fillQty;
        public final long fillAmt;
        public final java.time.LocalDateTime execTs;

        Execution(String execId, String orderId, String instrCode, String sideKbn,
                  long fillQty, long fillAmt, java.time.LocalDateTime execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    public static final class Position {
        public final String cifNo;
        public final String instrCode;
        public final long netQty;
        public final long avgAmt;
        public final long rlzdAmt;

        Position(String cifNo, String instrCode, long netQty, long avgAmt, long rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    public static final class CustomerReport {
        public final String cifNo;
        public long execCount;
        public long buyQty;
        public long sellQty;
        public long buyAmt;
        public long sellAmt;
        public long netQty;
        public long positionAmt;
        public long rlzdAmt;
        public long marginAmt;

        CustomerReport(String cifNo) {
            this.cifNo = cifNo;
        }

        void addPosition(Position p) {
            netQty += p.netQty;
            positionAmt += Math.abs(p.netQty) * p.avgAmt;
            rlzdAmt += p.rlzdAmt;
            marginAmt += Math.abs(p.netQty) * p.avgAmt * marginRateBp(tierOf(p.instrCode)) / 10000L;
        }

        void addExecutionSummary(InstrumentExecutionSummary s) {
            execCount += s.execCount;
            buyQty += s.buyQty;
            sellQty += s.sellQty;
            buyAmt += s.buyAmt;
            sellAmt += s.sellAmt;
        }

        String alertKbn() {
            long gross = buyAmt + sellAmt + positionAmt;
            if (gross > MIHFT_MAX_NOTIONAL) {
                return "8";
            }
            if (marginAmt > Math.max(0L, positionAmt + rlzdAmt)) {
                return "4";
            }
            return "0";
        }
    }

    private static final class InstrumentExecutionSummary {
        final String instrCode;
        long execCount;
        long buyQty;
        long sellQty;
        long buyAmt;
        long sellAmt;

        InstrumentExecutionSummary(String instrCode) {
            this.instrCode = instrCode;
        }

        void add(Execution e) {
            execCount++;
            if ("B".equals(e.sideKbn)) {
                buyQty += e.fillQty;
                buyAmt += e.fillAmt;
            } else if ("S".equals(e.sideKbn)) {
                sellQty += e.fillQty;
                sellAmt += e.fillAmt;
            }
        }
    }
}
