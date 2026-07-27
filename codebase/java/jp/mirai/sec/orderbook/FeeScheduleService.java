/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2019-04-16  渡辺 隆 (E-260)  手数料スケジュール検証の初版
 */

package jp.mirai.sec.orderbook;

public class FeeScheduleService {
    private FeeScheduleService() {
    }

    public static void main(String[] a) throws Exception {
        if (a.length < 4) {
            System.err.println("使用方法: java FeeScheduleService SCFEEF入力 SCVENF入力 SCFEEF出力 SCAUDF出力");
            return;
        }

        java.util.List<FeeRow> feeRows = readFeeRows(java.nio.file.Paths.get(a[0]));
        java.util.List<VenueRow> venueRows = readVenueRows(java.nio.file.Paths.get(a[1]));

        java.util.Map<String, BoardAggregate> aggregates = aggregateVenues(venueRows);
        java.util.List<FeeRow> accepted = new java.util.ArrayList<>();
        java.util.List<AuditRow> audits = new java.util.ArrayList<>();

        java.time.format.DateTimeFormatter tsFmt =
                java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS").withZone(java.time.ZoneId.of("Asia/Tokyo"));
        String now = tsFmt.format(java.time.Instant.now());

        java.util.Set<String> seenBoards = new java.util.HashSet<>();
        long seq = 1L;

        for (FeeRow row : feeRows) {
            java.util.List<String> errors = validateFeeRow(row, aggregates, seenBoards);
            if (errors.isEmpty()) {
                accepted.add(row);
                audits.add(new AuditRow(auditId(now, seq++), "-", "FEE_OK", "-", "-", now, "手数料更新受付:" + row.boardCode));
            } else {
                audits.add(new AuditRow(auditId(now, seq++), "-", "FEE_NG", "-", "-", now,
                        "手数料更新棄却:" + row.boardCode + ":" + String.join("|", errors)));
            }
        }

        writeFeeRows(java.nio.file.Paths.get(a[2]), accepted);
        writeAuditRows(java.nio.file.Paths.get(a[3]), audits);

        System.out.println("処理完了:受付=" + accepted.size() + ",監査=" + audits.size());
    }

    private static java.util.List<FeeRow> readFeeRows(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<FeeRow> rows = new java.util.ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            if (line.isEmpty()) {
                continue;
            }
            if (i == 0 && line.toUpperCase(java.util.Locale.ROOT).contains("BOARD-CODE")) {
                continue;
            }
            String[] c = splitCsv(line, 3);
            rows.add(new FeeRow(c[0].trim(), parseDecimal(c[1].trim(), "FEE-RATE", i + 1),
                    parseLong(c[2].trim(), "MIN-FEE-AMT", i + 1), i + 1));
        }
        return rows;
    }

    private static java.util.List<VenueRow> readVenueRows(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<VenueRow> rows = new java.util.ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            if (line.isEmpty()) {
                continue;
            }
            if (i == 0 && line.toUpperCase(java.util.Locale.ROOT).contains("VENUE-CODE")) {
                continue;
            }
            String[] c = splitCsv(line, 6);
            rows.add(new VenueRow(c[0].trim(), c[1].trim(), parseLong(c[2].trim(), "LATENCY-US", i + 1),
                    parseDecimal(c[3].trim(), "FEE-BPS", i + 1), c[4].trim(),
                    parseLong(c[5].trim(), "CAPACITY-QTY", i + 1), i + 1));
        }
        return rows;
    }

    private static java.util.Map<String, BoardAggregate> aggregateVenues(java.util.List<VenueRow> rows) {
        java.util.Map<String, BoardAggregate> map = new java.util.HashMap<>();
        for (VenueRow v : rows) {
            if (!"1".equals(v.enabledKbn)) {
                continue;
            }
            BoardAggregate a = map.get(v.boardCode);
            if (a == null) {
                a = new BoardAggregate();
                map.put(v.boardCode, a);
            }
            a.enabledVenueCount++;
            a.totalCapacity = checkedAdd(a.totalCapacity, v.capacityQty, "会場容量合計");
            a.totalLatencyUs = checkedAdd(a.totalLatencyUs, v.latencyUs, "遅延合計");
            a.totalFeeBps = a.totalFeeBps.add(v.feeBps);
        }
        return map;
    }

    private static java.util.List<String> validateFeeRow(
            FeeRow row, java.util.Map<String, BoardAggregate> aggregates, java.util.Set<String> seenBoards) {
        java.util.List<String> errors = new java.util.ArrayList<>();

        if (!row.boardCode.matches("[0-9A-Z]{4,8}")) {
            errors.add("銘柄板コード形式不正");
        }
        if (!seenBoards.add(row.boardCode)) {
            errors.add("銘柄板コード重複");
        }
        if (row.feeRate.signum() < 0 || row.feeRate.compareTo(new java.math.BigDecimal("0.050000")) > 0) {
            errors.add("料率範囲外");
        }
        if (row.feeRate.scale() > 6) {
            errors.add("料率小数桁過大");
        }
        if (row.minFeeAmt < 0L) {
            errors.add("最低手数料負値");
        }

        BoardAggregate aggregate = aggregates.get(row.boardCode);
        if (aggregate == null || aggregate.enabledVenueCount == 0) {
            errors.add("有効会場なし");
            return errors;
        }

        long calculatedMinimum = checkedMultiply(row.minFeeAmt, aggregate.enabledVenueCount, "会場別最低手数料");
        checkedMultiply(calculatedMinimum, aggregate.totalCapacity == 0L ? 1L : aggregate.totalCapacity, "容量加味最低手数料");

        java.math.BigDecimal routeRate = row.feeRate
                .add(aggregate.totalFeeBps.divide(new java.math.BigDecimal("10000"), 10, java.math.RoundingMode.HALF_UP));
        if (routeRate.compareTo(new java.math.BigDecimal("0.075000")) > 0) {
            errors.add("会場費用込み料率過大");
        }
        if (aggregate.totalLatencyUs / aggregate.enabledVenueCount > 50000L) {
            errors.add("平均遅延過大");
        }

        return errors;
    }

    private static void writeFeeRows(java.nio.file.Path path, java.util.List<FeeRow> rows) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("BOARD-CODE,FEE-RATE,MIN-FEE-AMT");
        for (FeeRow r : rows) {
            out.add(csv(r.boardCode) + "," + r.feeRate.toPlainString() + "," + r.minFeeAmt);
        }
        java.nio.file.Files.write(path, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeAuditRows(java.nio.file.Path path, java.util.List<AuditRow> rows) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("AUDIT-ID,ORDER-ID,EVENT-KBN,CIF-NO,INSTR-CODE,EVENT-TS,DETAIL-CD");
        for (AuditRow r : rows) {
            out.add(csv(r.auditId) + "," + csv(r.orderId) + "," + csv(r.eventKbn) + "," + csv(r.cifNo) + ","
                    + csv(r.instrCode) + "," + csv(r.eventTs) + "," + csv(r.detailCd));
        }
        java.nio.file.Files.write(path, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static String[] splitCsv(String line, int expected) {
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
                cols.add(b.toString());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        cols.add(b.toString());
        if (cols.size() != expected) {
            throw new IllegalArgumentException("CSV項目数不正:" + line);
        }
        return cols.toArray(new String[0]);
    }

    private static java.math.BigDecimal parseDecimal(String value, String name, int lineNo) {
        try {
            return new java.math.BigDecimal(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + "数値不正:行=" + lineNo, e);
        }
    }

    private static long parseLong(String value, String name, int lineNo) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + "整数不正:行=" + lineNo, e);
        }
    }

    private static long checkedAdd(long left, long right, String label) {
        try {
            return Math.addExact(left, right);
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException(label + "が長整数範囲外", e);
        }
    }

    private static long checkedMultiply(long left, long right, String label) {
        try {
            return Math.multiplyExact(left, right);
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException(label + "が長整数範囲外", e);
        }
    }

    private static String csv(String value) {
        if (value.indexOf(',') < 0 && value.indexOf('"') < 0 && value.indexOf('\n') < 0 && value.indexOf('\r') < 0) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }

    private static String auditId(String now, long seq) {
        return "AF" + now + String.format(java.util.Locale.ROOT, "%06d", seq);
    }

    private static final class FeeRow {
        final String boardCode;
        final java.math.BigDecimal feeRate;
        final long minFeeAmt;
        final int lineNo;

        FeeRow(String boardCode, java.math.BigDecimal feeRate, long minFeeAmt, int lineNo) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmt = minFeeAmt;
            this.lineNo = lineNo;
        }
    }

    private static final class VenueRow {
        final String venueCode;
        final String boardCode;
        final long latencyUs;
        final java.math.BigDecimal feeBps;
        final String enabledKbn;
        final long capacityQty;
        final int lineNo;

        VenueRow(String venueCode, String boardCode, long latencyUs, java.math.BigDecimal feeBps,
                 String enabledKbn, long capacityQty, int lineNo) {
            this.venueCode = venueCode;
            this.boardCode = boardCode;
            this.latencyUs = latencyUs;
            this.feeBps = feeBps;
            this.enabledKbn = enabledKbn;
            this.capacityQty = capacityQty;
            this.lineNo = lineNo;
        }
    }

    private static final class BoardAggregate {
        long enabledVenueCount;
        long totalCapacity;
        long totalLatencyUs;
        java.math.BigDecimal totalFeeBps = java.math.BigDecimal.ZERO;
    }

    private static final class AuditRow {
        final String auditId;
        final String orderId;
        final String eventKbn;
        final String cifNo;
        final String instrCode;
        final String eventTs;
        final String detailCd;

        AuditRow(String auditId, String orderId, String eventKbn, String cifNo,
                 String instrCode, String eventTs, String detailCd) {
            this.auditId = auditId;
            this.orderId = orderId;
            this.eventKbn = eventKbn;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.eventTs = eventTs;
            this.detailCd = detailCd;
        }
    }
}
