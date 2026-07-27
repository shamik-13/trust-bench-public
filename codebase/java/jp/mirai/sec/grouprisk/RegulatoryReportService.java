/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2020-03-10  開発一課  規制向け約定レポートサービス初版作成
 */

package jp.mirai.sec.grouprisk;

public class RegulatoryReportService {
    private static final String SERVICE_ID = "RGRPT01";
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final java.time.format.DateTimeFormatter TS_FMT =
            java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    public static void main(String[] a) {
        String execPath = a.length > 0 ? a[0] : "SCEXEC.csv";
        String ordPath = a.length > 1 ? a[1] : "SCORDF.csv";
        String posPath = a.length > 2 ? a[2] : "SCPOSF.csv";
        String instPath = a.length > 3 ? a[3] : "SCINSTF.csv";
        String reportPath = a.length > 4 ? a[4] : "REG_REPORT.csv";
        String auditPath = a.length > 5 ? a[5] : "SCAUDTF.csv";

        RegulatoryReportService s = new RegulatoryReportService();
        int rc = s.run(execPath, ordPath, posPath, instPath, reportPath, auditPath);
        if (rc != 0) {
            System.exit(rc);
        }
    }

    private int run(String execPath, String ordPath, String posPath, String instPath,
                    String reportPath, String auditPath) {
        java.util.List<AuditEvent> audits = new java.util.ArrayList<>();
        try {
            java.util.Map<String, OrderRecord> orders = readOrders(ordPath, audits);
            java.util.Map<String, PositionRecord> positions = readPositions(posPath, audits);
            java.util.Map<String, InstrumentRecord> instruments = readInstruments(instPath, audits);
            java.util.List<ExecRecord> executions = readExecutions(execPath, audits);

            java.util.Map<String, ReportBucket> buckets = new java.util.TreeMap<>();
            java.util.Map<String, Long> fillQtyByOrder = new java.util.HashMap<>();

            for (ExecRecord e : executions) {
                OrderRecord o = orders.get(e.orderId);
                if (o == null) {
                    audits.add(audit(e.execId, "欠落", "注文なし"));
                    continue;
                }

                InstrumentRecord i = instruments.get(e.instrCode);
                if (i == null) {
                    audits.add(audit(e.execId, "欠落", "銘柄なし"));
                    continue;
                }

                PositionRecord p = positions.get(o.cifNo + "\u0001" + e.instrCode);
                if (p == null) {
                    audits.add(audit(e.execId, "欠落", "建玉なし"));
                    continue;
                }

                if (!same(e.instrCode, o.instrCode) || !same(e.sideKbn, o.sideKbn)) {
                    audits.add(audit(e.execId, "不整合", "注文約定不一致"));
                    continue;
                }
                if (!same(o.instrCode, i.instrCode) || o.instrTier != i.instrTier) {
                    audits.add(audit(e.execId, "不整合", "銘柄階層不一致"));
                    continue;
                }
                if (!validSide(e.sideKbn) || !validOrdType(o.ordType) || !validTif(o.tifCode)
                        || !validBoard(i.boardCode) || !validTier(i.instrTier)) {
                    audits.add(audit(e.execId, "不正", "コード不正"));
                    continue;
                }
                if (e.fillQty <= 0 || e.fillAmt <= 0 || o.ordQty <= 0 || i.lotQty <= 0) {
                    audits.add(audit(e.execId, "不正", "数量金額不正"));
                    continue;
                }
                if (e.fillQty % i.lotQty != 0) {
                    audits.add(audit(e.execId, "不正", "売買単位不一致"));
                    continue;
                }
                if (o.ordType.equals("L") && o.priceAmt > 0 && o.priceAmt % i.tickAmt != 0) {
                    audits.add(audit(e.execId, "不正", "呼値不一致"));
                    continue;
                }
                if (e.fillAmt > MIHFT_MAX_NOTIONAL) {
                    audits.add(audit(e.execId, "除外", "想定元本超過"));
                    continue;
                }

                long newQty = fillQtyByOrder.getOrDefault(e.orderId, 0L) + e.fillQty;
                if (newQty > o.ordQty) {
                    audits.add(audit(e.execId, "不整合", "注文数量超過"));
                    continue;
                }
                fillQtyByOrder.put(e.orderId, newQty);

                java.time.LocalDate bizDate = e.execTs.toLocalDate();
                String key = bizDate + "|" + i.instrTier + "|" + i.boardCode + "|" + e.sideKbn
                        + "|" + e.instrCode;
                ReportBucket b = buckets.computeIfAbsent(key, k -> new ReportBucket(
                        bizDate, e.instrCode, i.instrName, i.instrTier, i.boardCode, e.sideKbn));
                b.execCount++;
                b.fillQty += e.fillQty;
                b.fillAmt += e.fillAmt;
                b.marginAmt += calcMargin(e.fillAmt, i.instrTier);
                b.netQty += p.netQty;
                b.realizedAmt += p.rlzdAmt;
                if (b.firstExecTs == null || e.execTs.isBefore(b.firstExecTs)) {
                    b.firstExecTs = e.execTs;
                }
                if (b.lastExecTs == null || e.execTs.isAfter(b.lastExecTs)) {
                    b.lastExecTs = e.execTs;
                }
            }

            writeReport(reportPath, buckets.values());
            writeAudits(auditPath, audits);
            return 0;
        } catch (java.io.IOException | RuntimeException ex) {
            audits.add(audit("SYSTEM", "異常", "入出力異常"));
            try {
                writeAudits(auditPath, audits);
            } catch (java.io.IOException ignored) {
                // 監査出力不能時は終了コードのみ返す。
            }
            return 16;
        }
    }

    private static java.util.List<ExecRecord> readExecutions(String path, java.util.List<AuditEvent> audits)
            throws java.io.IOException {
        java.util.List<ExecRecord> list = new java.util.ArrayList<>();
        for (String[] r : readCsv(path)) {
            if (r.length < 7 || isHeader(r[0], "EXEC-ID")) {
                continue;
            }
            try {
                list.add(new ExecRecord(
                        r[0].trim(), r[1].trim(), r[2].trim(), r[3].trim(),
                        Long.parseLong(r[4].trim()), Long.parseLong(r[5].trim()),
                        parseTs(r[6].trim())));
            } catch (RuntimeException ex) {
                audits.add(audit(r.length == 0 ? "SCEXEC" : r[0].trim(), "不正", "約定形式不正"));
            }
        }
        return list;
    }

    private static java.util.Map<String, OrderRecord> readOrders(String path, java.util.List<AuditEvent> audits)
            throws java.io.IOException {
        java.util.Map<String, OrderRecord> map = new java.util.HashMap<>();
        for (String[] r : readCsv(path)) {
            if (r.length < 9 || isHeader(r[0], "ORDER-ID")) {
                continue;
            }
            try {
                OrderRecord o = new OrderRecord(
                        r[0].trim(), r[1].trim(), r[2].trim(), r[3].trim(), r[4].trim(),
                        r[5].trim(), Long.parseLong(r[6].trim()), Long.parseLong(r[7].trim()),
                        Integer.parseInt(r[8].trim()));
                map.put(o.orderId, o);
            } catch (RuntimeException ex) {
                audits.add(audit(r.length == 0 ? "SCORDF" : r[0].trim(), "不正", "注文形式不正"));
            }
        }
        return map;
    }

    private static java.util.Map<String, PositionRecord> readPositions(String path, java.util.List<AuditEvent> audits)
            throws java.io.IOException {
        java.util.Map<String, PositionRecord> map = new java.util.HashMap<>();
        for (String[] r : readCsv(path)) {
            if (r.length < 5 || isHeader(r[0], "CIF-NO")) {
                continue;
            }
            try {
                PositionRecord p = new PositionRecord(
                        r[0].trim(), r[1].trim(), Long.parseLong(r[2].trim()),
                        Long.parseLong(r[3].trim()), Long.parseLong(r[4].trim()));
                map.put(p.cifNo + "\u0001" + p.instrCode, p);
            } catch (RuntimeException ex) {
                audits.add(audit(r.length == 0 ? "SCPOSF" : r[0].trim(), "不正", "建玉形式不正"));
            }
        }
        return map;
    }

    private static java.util.Map<String, InstrumentRecord> readInstruments(String path, java.util.List<AuditEvent> audits)
            throws java.io.IOException {
        java.util.Map<String, InstrumentRecord> map = new java.util.HashMap<>();
        for (String[] r : readCsv(path)) {
            if (r.length < 6 || isHeader(r[0], "INSTR-CODE")) {
                continue;
            }
            try {
                InstrumentRecord i = new InstrumentRecord(
                        r[0].trim(), r[1].trim(), Integer.parseInt(r[2].trim()),
                        Long.parseLong(r[3].trim()), Long.parseLong(r[4].trim()), r[5].trim());
                map.put(i.instrCode, i);
            } catch (RuntimeException ex) {
                audits.add(audit(r.length == 0 ? "SCINSTF" : r[0].trim(), "不正", "銘柄形式不正"));
            }
        }
        return map;
    }

    private static void writeReport(String path, java.util.Collection<ReportBucket> buckets)
            throws java.io.IOException {
        try (java.io.BufferedWriter w = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8)) {
            w.write("BUS-DATE,INSTR-CODE,INSTR-NAME,INSTR-TIER,BOARD-CODE,SIDE-KBN,EXEC-COUNT,FILL-QTY,FILL-AMT,MARGIN-AMT,NET-QTY,RLZD-AMT,FIRST-EXEC-TS,LAST-EXEC-TS");
            w.newLine();
            for (ReportBucket b : buckets) {
                w.write(b.bizDate + "," + csv(b.instrCode) + "," + csv(b.instrName) + ","
                        + b.instrTier + "," + b.boardCode + "," + b.sideKbn + ","
                        + b.execCount + "," + b.fillQty + "," + b.fillAmt + ","
                        + b.marginAmt + "," + b.netQty + "," + b.realizedAmt + ","
                        + TS_FMT.format(b.firstExecTs) + "," + TS_FMT.format(b.lastExecTs));
                w.newLine();
            }
        }
    }

    private static void writeAudits(String path, java.util.List<AuditEvent> audits)
            throws java.io.IOException {
        try (java.io.BufferedWriter w = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8)) {
            w.write("AUDIT-ID,EVENT-TS,SERVICE-ID,OBJECT-ID,EVENT-KBN,DETAIL-CODE");
            w.newLine();
            for (int n = 0; n < audits.size(); n++) {
                AuditEvent a = audits.get(n);
                String id = String.format("AU%012d", n + 1);
                w.write(id + "," + TS_FMT.format(a.eventTs) + "," + SERVICE_ID + ","
                        + csv(a.objectId) + "," + csv(a.eventKbn) + "," + csv(a.detailCode));
                w.newLine();
            }
        }
    }

    private static java.util.List<String[]> readCsv(String path) throws java.io.IOException {
        java.util.List<String[]> rows = new java.util.ArrayList<>();
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    rows.add(parseCsvLine(line));
                }
            }
        }
        return rows;
    }

    private static String[] parseCsvLine(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean q = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                if (q && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cur.append('"');
                    i++;
                } else {
                    q = !q;
                }
            } else if (c == ',' && !q) {
                out.add(cur.toString());
                cur.setLength(0);
            } else {
                cur.append(c);
            }
        }
        out.add(cur.toString());
        return out.toArray(new String[0]);
    }

    private static String csv(String s) {
        if (s == null) {
            return "";
        }
        if (s.indexOf(',') < 0 && s.indexOf('"') < 0 && s.indexOf('\n') < 0 && s.indexOf('\r') < 0) {
            return s;
        }
        return "\"" + s.replace("\"", "\"\"") + "\"";
    }

    private static AuditEvent audit(String objectId, String eventKbn, String detailCode) {
        return new AuditEvent(java.time.LocalDateTime.now(), objectId, eventKbn, detailCode);
    }

    private static java.time.LocalDateTime parseTs(String s) {
        String x = s.replace('T', ' ');
        if (x.length() == 10) {
            x = x + " 00:00:00";
        }
        return java.time.LocalDateTime.parse(x, TS_FMT);
    }

    private static long calcMargin(long amt, int tier) {
        long bp = tier == 1 ? 1000L : tier == 2 ? 2000L : 4000L;
        return Math.multiplyExact(amt, bp) / 10000L;
    }

    private static boolean same(String a, String b) {
        return java.util.Objects.equals(a, b);
    }

    private static boolean isHeader(String actual, String expected) {
        return expected.equalsIgnoreCase(actual == null ? "" : actual.trim());
    }

    private static boolean validSide(String v) {
        return "B".equals(v) || "S".equals(v);
    }

    private static boolean validOrdType(String v) {
        return "L".equals(v) || "M".equals(v);
    }

    private static boolean validTif(String v) {
        return "DAY".equals(v) || "IOC".equals(v) || "FOK".equals(v);
    }

    private static boolean validBoard(String v) {
        return "T1".equals(v) || "ST".equals(v) || "ETF".equals(v);
    }

    private static boolean validTier(int v) {
        return v == 1 || v == 2 || v == 3;
    }

    private static final class ExecRecord {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final java.time.LocalDateTime execTs;

        ExecRecord(String execId, String orderId, String instrCode, String sideKbn,
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

    private static final class OrderRecord {
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final String ordType;
        final String tifCode;
        final long ordQty;
        final long priceAmt;
        final int instrTier;

        OrderRecord(String orderId, String cifNo, String instrCode, String sideKbn,
                    String ordType, String tifCode, long ordQty, long priceAmt, int instrTier) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.ordType = ordType;
            this.tifCode = tifCode;
            this.ordQty = ordQty;
            this.priceAmt = priceAmt;
            this.instrTier = instrTier;
        }
    }

    private static final class PositionRecord {
        final String cifNo;
        final String instrCode;
        final long netQty;
        final long avgAmt;
        final long rlzdAmt;

        PositionRecord(String cifNo, String instrCode, long netQty, long avgAmt, long rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    private static final class InstrumentRecord {
        final String instrCode;
        final String instrName;
        final int instrTier;
        final long tickAmt;
        final long lotQty;
        final String boardCode;

        InstrumentRecord(String instrCode, String instrName, int instrTier,
                         long tickAmt, long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class AuditEvent {
        final java.time.LocalDateTime eventTs;
        final String objectId;
        final String eventKbn;
        final String detailCode;

        AuditEvent(java.time.LocalDateTime eventTs, String objectId, String eventKbn, String detailCode) {
            this.eventTs = eventTs;
            this.objectId = objectId;
            this.eventKbn = eventKbn;
            this.detailCode = detailCode;
        }
    }

    private static final class ReportBucket {
        final java.time.LocalDate bizDate;
        final String instrCode;
        final String instrName;
        final int instrTier;
        final String boardCode;
        final String sideKbn;
        long execCount;
        long fillQty;
        long fillAmt;
        long marginAmt;
        long netQty;
        long realizedAmt;
        java.time.LocalDateTime firstExecTs;
        java.time.LocalDateTime lastExecTs;

        ReportBucket(java.time.LocalDate bizDate, String instrCode, String instrName,
                     int instrTier, String boardCode, String sideKbn) {
            this.bizDate = bizDate;
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.boardCode = boardCode;
            this.sideKbn = sideKbn;
        }
    }
}
