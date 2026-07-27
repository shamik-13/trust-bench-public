/**
 * 変更履歴
 * 版数  年月日      担当    概要
 * 1.0   2020/03/10  三宅 拓也 (E-241)    与信限度同期サービス初版
 */

package jp.mirai.sec.grouprisk;

public class RiskLimitSyncService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;
    private static final long DEFAULT_TOLERANCE_AMT = 1000000L;
    private static final String ACTOR_ID = "RISK-SYNC";
    private static final String ACTION_KBN = "LIMIT-SYNC";

    public static void main(String[] a) throws Exception {
        if (a.length < 5) {
            System.err.println("使用方法: java RiskLimitSyncService SCCUST.csv SCPOSF.csv SCINSTF.csv HFRISKC.csv SCAUDF2.csv [許容額]");
            System.exit(2);
        }

        long toleranceAmt = a.length >= 6 ? parseLong(a[5], "許容額") : DEFAULT_TOLERANCE_AMT;
        RiskLimitSyncService service = new RiskLimitSyncService();
        SyncResult result = service.sync(a[0], a[1], a[2], a[3], a[4], toleranceAmt);

        System.out.println("同期完了 件数=" + result.customerCount
                + " 監査=" + result.auditCount
                + " 再配信=" + result.redistributionCount);
    }

    private SyncResult sync(String sccustPath,
                            String scposfPath,
                            String scinstfPath,
                            String hfriskcPath,
                            String scaudf2Path,
                            long toleranceAmt) throws Exception {
        java.util.Map<String, CustomerLimit> customers = readCustomers(sccustPath);
        java.util.Map<String, InstrumentRef> instruments = readInstruments(scinstfPath);
        java.util.Map<String, java.util.List<PositionRow>> positionsByCif = readPositions(scposfPath, instruments);
        java.util.Map<String, java.util.List<HotRiskRow>> hotRiskByCif = readHotRisk(hfriskcPath);

        java.util.List<AuditRow> audits = new java.util.ArrayList<AuditRow>();
        long now = System.currentTimeMillis();

        for (CustomerLimit customer : customers.values()) {
            java.util.List<PositionRow> positions = positionsByCif.get(customer.cifNo);
            if (positions == null) {
                positions = java.util.Collections.emptyList();
            }

            java.util.List<HotRiskRow> hotRows = hotRiskByCif.get(customer.cifNo);
            if (hotRows == null) {
                hotRows = java.util.Collections.emptyList();
            }

            long groupExposure = aggregateControlExposure(customer.cifNo, positions, instruments);
            long hotExposure = aggregateHotExposure(hotRows);
            long officialUsed = safeAdd(customer.groupUsedAmt, customer.acctUsedAmt);
            long diff = Math.abs(safeAdd(groupExposure, -hotExposure));

            int resultCode = 0;
            if (groupExposure > MIHFT_MAX_NOTIONAL) {
                resultCode = 8;
            } else if (safeAdd(officialUsed, groupExposure) > customer.groupLimit) {
                resultCode = 4;
            } else if (hasTickMismatch(positions, instruments)) {
                resultCode = 12;
            }

            if (diff > toleranceAmt || resultCode != 0) {
                customer.redistribute = true;
                audits.add(new AuditRow(
                        auditId(customer.cifNo, now, audits.size() + 1),
                        ACTOR_ID,
                        ACTION_KBN,
                        customer.cifNo,
                        String.valueOf(resultCode),
                        formatTimestamp(now)));
            }
        }

        writeAudits(scaudf2Path, audits);
        return new SyncResult(customers.size(), audits.size(), countRedistribution(customers));
    }

    private java.util.Map<String, CustomerLimit> readCustomers(String path) throws Exception {
        java.util.Map<String, CustomerLimit> rows = new java.util.LinkedHashMap<String, CustomerLimit>();
        java.util.List<String[]> csv = readCsv(path);
        for (int i = 1; i < csv.size(); i++) {
            String[] r = csv.get(i);
            requireColumns(path, i + 1, r, 4);
            CustomerLimit c = new CustomerLimit(
                    value(r, 0),
                    parseLong(value(r, 1), "GROUP-LIMIT"),
                    parseLong(value(r, 2), "GROUP-USED-AMT"),
                    parseLong(value(r, 3), "ACCT-USED-AMT"));
            if (c.cifNo.length() == 0) {
                throw new IllegalArgumentException("CIF-NO未設定 行=" + (i + 1));
            }
            rows.put(c.cifNo, c);
        }
        return rows;
    }

    private java.util.Map<String, InstrumentRef> readInstruments(String path) throws Exception {
        java.util.Map<String, InstrumentRef> rows = new java.util.LinkedHashMap<String, InstrumentRef>();
        java.util.List<String[]> csv = readCsv(path);
        for (int i = 1; i < csv.size(); i++) {
            String[] r = csv.get(i);
            requireColumns(path, i + 1, r, 6);
            int tier = (int) parseLong(value(r, 2), "INSTR-TIER");
            long tickAmt = parseLong(value(r, 3), "TICK-AMT");
            long lotQty = parseLong(value(r, 4), "LOT-QTY");
            String boardCode = value(r, 5);

            if (!("T1".equals(boardCode) || "ST".equals(boardCode) || "ETF".equals(boardCode))) {
                throw new IllegalArgumentException("BOARD-CODE不正 行=" + (i + 1));
            }
            if (marginRateBp(tier) == 0) {
                throw new IllegalArgumentException("INSTR-TIER不正 行=" + (i + 1));
            }
            if (tickAmt <= 0 || lotQty <= 0) {
                throw new IllegalArgumentException("取引単位不正 行=" + (i + 1));
            }

            rows.put(value(r, 0), new InstrumentRef(value(r, 0), value(r, 1), tier, tickAmt, lotQty, boardCode));
        }
        return rows;
    }

    private java.util.Map<String, java.util.List<PositionRow>> readPositions(
            String path,
            java.util.Map<String, InstrumentRef> instruments) throws Exception {
        java.util.Map<String, java.util.List<PositionRow>> rows = new java.util.LinkedHashMap<String, java.util.List<PositionRow>>();
        java.util.List<String[]> csv = readCsv(path);
        for (int i = 1; i < csv.size(); i++) {
            String[] r = csv.get(i);
            requireColumns(path, i + 1, r, 5);
            String cifNo = value(r, 0);
            String instrCode = value(r, 1);
            if (!instruments.containsKey(instrCode)) {
                throw new IllegalArgumentException("銘柄未登録 行=" + (i + 1) + " 銘柄=" + instrCode);
            }
            PositionRow p = new PositionRow(
                    cifNo,
                    instrCode,
                    parseLong(value(r, 2), "NET-QTY"),
                    parseLong(value(r, 3), "AVG-AMT"),
                    parseLong(value(r, 4), "RLZD-AMT"));
            listFor(rows, cifNo).add(p);
        }
        return rows;
    }

    private java.util.Map<String, java.util.List<HotRiskRow>> readHotRisk(String path) throws Exception {
        java.util.Map<String, java.util.List<HotRiskRow>> rows = new java.util.LinkedHashMap<String, java.util.List<HotRiskRow>>();
        java.util.List<String[]> csv = readCsv(path);
        for (int i = 1; i < csv.size(); i++) {
            String[] r = csv.get(i);
            requireColumns(path, i + 1, r, 5);
            HotRiskRow h = new HotRiskRow(
                    value(r, 0),
                    value(r, 1),
                    parseLong(value(r, 2), "OPEN-NOTIONAL-AMT"),
                    parseLong(value(r, 3), "REJECT-CNT"),
                    value(r, 4));
            listFor(rows, h.cifNo).add(h);
        }
        return rows;
    }

    private long aggregateControlExposure(String cifNo,
                                          java.util.List<PositionRow> positions,
                                          java.util.Map<String, InstrumentRef> instruments) {
        long total = 0L;
        for (PositionRow p : positions) {
            InstrumentRef ref = instruments.get(p.instrCode);
            long notional = safeMultiply(Math.abs(p.netQty), p.avgAmt);
            long margin = safeDivideCeil(safeMultiply(notional, marginRateBp(ref.tier)), 10000L);
            total = safeAdd(total, margin);
        }
        return total;
    }

    private long aggregateHotExposure(java.util.List<HotRiskRow> rows) {
        long total = 0L;
        for (HotRiskRow r : rows) {
            total = safeAdd(total, Math.abs(r.openNotionalAmt));
        }
        return total;
    }

    private boolean hasTickMismatch(java.util.List<PositionRow> positions,
                                    java.util.Map<String, InstrumentRef> instruments) {
        for (PositionRow p : positions) {
            InstrumentRef ref = instruments.get(p.instrCode);
            if (ref == null) {
                return true;
            }
            if (p.avgAmt % ref.tickAmt != 0L) {
                return true;
            }
            if (Math.abs(p.netQty) % ref.lotQty != 0L) {
                return true;
            }
        }
        return false;
    }

    private void writeAudits(String path, java.util.List<AuditRow> rows) throws Exception {
        java.io.BufferedWriter w = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path),
                java.nio.charset.StandardCharsets.UTF_8);
        try {
            w.write("AUDIT-ID,ACTOR-ID,ACTION-KBN,OBJECT-ID,RESULT-CODE,AUDIT-TS");
            w.newLine();
            for (AuditRow r : rows) {
                w.write(csv(r.auditId));
                w.write(',');
                w.write(csv(r.actorId));
                w.write(',');
                w.write(csv(r.actionKbn));
                w.write(',');
                w.write(csv(r.objectId));
                w.write(',');
                w.write(csv(r.resultCode));
                w.write(',');
                w.write(csv(r.auditTs));
                w.newLine();
            }
        } finally {
            w.close();
        }
    }

    private java.util.List<String[]> readCsv(String path) throws Exception {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(
                java.nio.file.Paths.get(path),
                java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<String[]> rows = new java.util.ArrayList<String[]>();
        for (String line : lines) {
            if (line.trim().length() == 0) {
                continue;
            }
            rows.add(parseCsvLine(line));
        }
        return rows;
    }

    private String[] parseCsvLine(String line) {
        java.util.List<String> values = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (quoted) {
                if (ch == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        b.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    b.append(ch);
                }
            } else if (ch == ',') {
                values.add(b.toString().trim());
                b.setLength(0);
            } else if (ch == '"') {
                quoted = true;
            } else {
                b.append(ch);
            }
        }
        values.add(b.toString().trim());
        return values.toArray(new String[values.size()]);
    }

    private static <T> java.util.List<T> listFor(
            java.util.Map<String, java.util.List<T>> map,
            String key) {
        java.util.List<T> list = map.get(key);
        if (list == null) {
            list = new java.util.ArrayList<T>();
            map.put(key, list);
        }
        return list;
    }

    private static int marginRateBp(int tier) {
        if (tier == 1) {
            return 1000;
        }
        if (tier == 2) {
            return 2000;
        }
        if (tier == 3) {
            return 4000;
        }
        return 0;
    }

    private static void requireColumns(String path, int lineNo, String[] row, int count) {
        if (row.length < count) {
            throw new IllegalArgumentException("項目不足 ファイル=" + path + " 行=" + lineNo);
        }
    }

    private static String value(String[] row, int index) {
        return row[index] == null ? "" : row[index].trim();
    }

    private static long parseLong(String s, String name) {
        try {
            return Long.parseLong(s.replace(",", "").trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(name + "数値不正 値=" + s, e);
        }
    }

    private static long safeAdd(long a, long b) {
        return Math.addExact(a, b);
    }

    private static long safeMultiply(long a, long b) {
        return Math.multiplyExact(a, b);
    }

    private static long safeDivideCeil(long value, long divisor) {
        if (value < 0L || divisor <= 0L) {
            throw new IllegalArgumentException("除算値不正");
        }
        return (value + divisor - 1L) / divisor;
    }

    private static String auditId(String cifNo, long now, int seq) {
        return "AUD" + now + "-" + cifNo + "-" + String.format("%05d", Integer.valueOf(seq));
    }

    private static String formatTimestamp(long millis) {
        java.time.Instant instant = java.time.Instant.ofEpochMilli(millis);
        java.time.ZonedDateTime zdt = java.time.ZonedDateTime.ofInstant(instant, java.time.ZoneId.of("Asia/Tokyo"));
        return java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss").format(zdt);
    }

    private static String csv(String v) {
        if (v == null) {
            return "";
        }
        if (v.indexOf(',') < 0 && v.indexOf('"') < 0 && v.indexOf('\n') < 0 && v.indexOf('\r') < 0) {
            return v;
        }
        return "\"" + v.replace("\"", "\"\"") + "\"";
    }

    private static int countRedistribution(java.util.Map<String, CustomerLimit> customers) {
        int count = 0;
        for (CustomerLimit c : customers.values()) {
            if (c.redistribute) {
                count++;
            }
        }
        return count;
    }

    private static final class CustomerLimit {
        final String cifNo;
        final long groupLimit;
        final long groupUsedAmt;
        final long acctUsedAmt;
        boolean redistribute;

        CustomerLimit(String cifNo, long groupLimit, long groupUsedAmt, long acctUsedAmt) {
            this.cifNo = cifNo;
            this.groupLimit = groupLimit;
            this.groupUsedAmt = groupUsedAmt;
            this.acctUsedAmt = acctUsedAmt;
        }
    }

    private static final class PositionRow {
        final String cifNo;
        final String instrCode;
        final long netQty;
        final long avgAmt;
        final long rlzdAmt;

        PositionRow(String cifNo, String instrCode, long netQty, long avgAmt, long rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    private static final class InstrumentRef {
        final String instrCode;
        final String instrName;
        final int tier;
        final long tickAmt;
        final long lotQty;
        final String boardCode;

        InstrumentRef(String instrCode, String instrName, int tier, long tickAmt, long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.tier = tier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class HotRiskRow {
        final String cifNo;
        final String instrCode;
        final long openNotionalAmt;
        final long rejectCnt;
        final String lastUpdTs;

        HotRiskRow(String cifNo, String instrCode, long openNotionalAmt, long rejectCnt, String lastUpdTs) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.openNotionalAmt = openNotionalAmt;
            this.rejectCnt = rejectCnt;
            this.lastUpdTs = lastUpdTs;
        }
    }

    private static final class AuditRow {
        final String auditId;
        final String actorId;
        final String actionKbn;
        final String objectId;
        final String resultCode;
        final String auditTs;

        AuditRow(String auditId, String actorId, String actionKbn, String objectId, String resultCode, String auditTs) {
            this.auditId = auditId;
            this.actorId = actorId;
            this.actionKbn = actionKbn;
            this.objectId = objectId;
            this.resultCode = resultCode;
            this.auditTs = auditTs;
        }
    }

    private static final class SyncResult {
        final int customerCount;
        final int auditCount;
        final int redistributionCount;

        SyncResult(int customerCount, int auditCount, int redistributionCount) {
            this.customerCount = customerCount;
            this.auditCount = auditCount;
            this.redistributionCount = redistributionCount;
        }
    }
}
