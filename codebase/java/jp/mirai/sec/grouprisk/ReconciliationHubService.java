/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2022/02/22  大野 修 (E-225)      初版作成
 */

package jp.mirai.sec.grouprisk;

public class ReconciliationHubService {
    private static final String SERVICE_ID = "RECHUB";
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    public static void main(String[] a) throws Exception {
        java.nio.file.Path execPath = java.nio.file.Paths.get(a.length > 0 ? a[0] : "SCEXEC.csv");
        java.nio.file.Path posPath = java.nio.file.Paths.get(a.length > 1 ? a[1] : "SCPOSF.csv");
        java.nio.file.Path ordPath = java.nio.file.Paths.get(a.length > 2 ? a[2] : "SCORDF.csv");
        java.nio.file.Path feePath = java.nio.file.Paths.get(a.length > 3 ? a[3] : "SCFEEF.csv");
        java.nio.file.Path auditPath = java.nio.file.Paths.get(a.length > 4 ? a[4] : "SCAUDTF.csv");

        java.util.Map<String, FeeRule> fees = readFees(feePath);
        java.util.Map<String, OrderRow> orders = readOrders(ordPath);
        java.util.List<ExecRow> execs = readExecs(execPath);
        java.util.Map<ReconKey, PositionRow> positions = readPositions(posPath);

        java.util.Map<ReconKey, ReconBucket> buckets = new java.util.LinkedHashMap<>();
        java.util.List<AuditRow> audits = new java.util.ArrayList<>();
        String runTs = nowText();

        for (OrderRow order : orders.values()) {
            if (!isSide(order.side)) {
                audits.add(audit(runTs, order.orderId, "入力検証", "高:再実行不可:売買区分不正"));
            }
            if (!isOrderType(order.orderType)) {
                audits.add(audit(runTs, order.orderId, "入力検証", "中:再実行不可:注文種別不正"));
            }
            if (!isTif(order.tif)) {
                audits.add(audit(runTs, order.orderId, "入力検証", "中:再実行不可:有効条件不正"));
            }
            if (!tierRateBp(order.tier).isPresent()) {
                audits.add(audit(runTs, order.orderId, "入力検証", "高:再実行不可:銘柄階層不正"));
            }
            long notional = multiply(order.quantity, order.price);
            if (notional > MIHFT_MAX_NOTIONAL) {
                audits.add(audit(runTs, order.orderId, "事前判定", "高:再実行不可:8"));
            }
            java.util.OptionalInt tick = tierTick(order.tier);
            if (tick.isPresent() && order.price > 0L && order.price % tick.getAsInt() != 0L) {
                audits.add(audit(runTs, order.orderId, "事前判定", "高:再実行不可:12"));
            }
        }

        for (ExecRow exec : execs) {
            OrderRow order = orders.get(exec.orderId);
            if (order == null) {
                audits.add(audit(runTs, exec.execId, "約定照合", "高:再実行可:注文未検出"));
                continue;
            }
            if (!order.instrument.equals(exec.instrument)) {
                audits.add(audit(runTs, exec.execId, "約定照合", "高:再実行不可:銘柄不一致"));
                continue;
            }
            if (!order.side.equals(exec.side)) {
                audits.add(audit(runTs, exec.execId, "約定照合", "高:再実行不可:売買方向不一致"));
                continue;
            }
            ReconKey key = new ReconKey(order.cif, exec.instrument, exec.side);
            ReconBucket bucket = buckets.get(key);
            if (bucket == null) {
                bucket = new ReconBucket(key);
                buckets.put(key, bucket);
            }
            bucket.fillQty += exec.quantity;
            bucket.fillAmt += exec.amount;
            bucket.orderQty += order.quantity;
            bucket.maxExecTs = maxText(bucket.maxExecTs, exec.execTs);
            String board = boardByTier(order.tier);
            FeeRule fee = fees.get(board);
            if (fee == null) {
                audits.add(audit(runTs, order.orderId, "手数料照合", "中:再実行可:手数料表未検出"));
            } else {
                bucket.recalcFee += fee.apply(exec.amount);
            }
        }

        java.util.Set<ReconKey> allKeys = new java.util.LinkedHashSet<>();
        allKeys.addAll(buckets.keySet());
        allKeys.addAll(positions.keySet());

        for (ReconKey key : allKeys) {
            ReconBucket bucket = buckets.get(key);
            PositionRow pos = positions.get(key);
            long fillQty = bucket == null ? 0L : bucket.fillQty;
            long signedFillQty = "B".equals(key.side) ? fillQty : -fillQty;
            long posQty = pos == null ? 0L : pos.netQty;
            String objectId = key.cif + "|" + key.instrument + "|" + key.side;

            if (pos == null && fillQty != 0L) {
                audits.add(audit(runTs, objectId, "残高照合", "高:再実行可:ポジション未検出"));
            } else if (signedFillQty != posQty) {
                long diff = signedFillQty - posQty;
                audits.add(audit(runTs, objectId, "残高照合", severity(diff) + ":再実行可:数量差分=" + diff));
            }

            if (bucket != null && pos != null) {
                long expectedRlzd = expectedRealized(key.side, bucket.fillAmt, bucket.fillQty, pos.avgAmt, pos.netQty);
                long pnlDiff = expectedRlzd - pos.realizedAmt;
                if (java.lang.Math.abs(pnlDiff) >= 1000L) {
                    audits.add(audit(runTs, objectId, "損益照合", severity(pnlDiff) + ":再実行可:実現損益差分=" + pnlDiff));
                }
                if (bucket.recalcFee <= 0L && bucket.fillAmt > 0L) {
                    audits.add(audit(runTs, objectId, "手数料照合", "中:再実行可:再計算額ゼロ"));
                } else if (bucket.recalcFee > 0L) {
                    long feeBasis = java.lang.Math.max(1L, bucket.fillAmt / 10000L);
                    long feeDiff = bucket.recalcFee - feeBasis;
                    if (java.lang.Math.abs(feeDiff) >= 500L) {
                        audits.add(audit(runTs, objectId, "手数料照合", severity(feeDiff) + ":再実行可:手数料差分=" + feeDiff));
                    }
                }
            }
        }

        writeAudits(auditPath, audits);
    }

    private static java.util.Map<String, FeeRule> readFees(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, FeeRule> map = new java.util.LinkedHashMap<>();
        for (java.util.Map<String, String> row : readCsv(path)) {
            String board = value(row, "BOARD-CODE");
            if (board.length() == 0) {
                continue;
            }
            map.put(board, new FeeRule(board, parseDecimalBp(value(row, "FEE-RATE")), parseLong(value(row, "MIN-FEE-AMT"))));
        }
        return map;
    }

    private static java.util.Map<String, OrderRow> readOrders(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, OrderRow> map = new java.util.LinkedHashMap<>();
        for (java.util.Map<String, String> row : readCsv(path)) {
            OrderRow order = new OrderRow(
                    value(row, "ORDER-ID"),
                    value(row, "CIF-NO"),
                    value(row, "INSTR-CODE"),
                    value(row, "SIDE-KBN"),
                    value(row, "ORD-TYPE"),
                    value(row, "TIF-CODE"),
                    parseLong(value(row, "ORD-QTY")),
                    parseLong(value(row, "PRICE-AMT")),
                    parseInt(value(row, "INSTR-TIER")));
            if (order.orderId.length() > 0) {
                map.put(order.orderId, order);
            }
        }
        return map;
    }

    private static java.util.List<ExecRow> readExecs(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<ExecRow> list = new java.util.ArrayList<>();
        for (java.util.Map<String, String> row : readCsv(path)) {
            list.add(new ExecRow(
                    value(row, "EXEC-ID"),
                    value(row, "ORDER-ID"),
                    value(row, "INSTR-CODE"),
                    value(row, "SIDE-KBN"),
                    parseLong(value(row, "FILL-QTY")),
                    parseLong(value(row, "FILL-AMT")),
                    value(row, "EXEC-TS")));
        }
        return list;
    }

    private static java.util.Map<ReconKey, PositionRow> readPositions(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<ReconKey, PositionRow> map = new java.util.LinkedHashMap<>();
        for (java.util.Map<String, String> row : readCsv(path)) {
            PositionRow pos = new PositionRow(
                    value(row, "CIF-NO"),
                    value(row, "INSTR-CODE"),
                    parseLong(value(row, "NET-QTY")),
                    parseLong(value(row, "AVG-AMT")),
                    parseLong(value(row, "RLZD-AMT")));
            String side = pos.netQty < 0L ? "S" : "B";
            map.put(new ReconKey(pos.cif, pos.instrument, side), pos);
        }
        return map;
    }

    private static java.util.List<java.util.Map<String, String>> readCsv(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        if (lines.isEmpty()) {
            return rows;
        }
        java.util.List<String> header = splitCsv(lines.get(0));
        for (int i = 1; i < lines.size(); i++) {
            if (lines.get(i).trim().length() == 0) {
                continue;
            }
            java.util.List<String> cols = splitCsv(lines.get(i));
            java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
            for (int j = 0; j < header.size(); j++) {
                row.put(header.get(j), j < cols.size() ? cols.get(j) : "");
            }
            rows.add(row);
        }
        return rows;
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
        StringBuilder cell = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cell.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (c == ',' && !quoted) {
                out.add(cell.toString().trim());
                cell.setLength(0);
            } else {
                cell.append(c);
            }
        }
        out.add(cell.toString().trim());
        return out;
    }

    private static void writeAudits(java.nio.file.Path path, java.util.List<AuditRow> audits) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add("AUDIT-ID,EVENT-TS,SERVICE-ID,OBJECT-ID,EVENT-KBN,DETAIL-CODE");
        for (int i = 0; i < audits.size(); i++) {
            AuditRow row = audits.get(i);
            lines.add(csv(row.auditId) + "," + csv(row.eventTs) + "," + csv(row.serviceId) + "," + csv(row.objectId)
                    + "," + csv(row.eventKind) + "," + csv(row.detailCode));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static AuditRow audit(String eventTs, String objectId, String eventKind, String detailCode) {
        String id = "AUD" + java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
                + String.format("%06d", nextAuditSeq());
        return new AuditRow(id, eventTs, SERVICE_ID, objectId, eventKind, detailCode);
    }

    private static synchronized int nextAuditSeq() {
        auditSeq++;
        return auditSeq;
    }

    private static String value(java.util.Map<String, String> row, String key) {
        String v = row.get(key);
        return v == null ? "" : v.trim();
    }

    private static long parseLong(String s) {
        if (s == null || s.trim().length() == 0) {
            return 0L;
        }
        return new java.math.BigDecimal(s.trim()).setScale(0, java.math.RoundingMode.HALF_UP).longValue();
    }

    private static int parseInt(String s) {
        if (s == null || s.trim().length() == 0) {
            return 0;
        }
        return java.lang.Integer.parseInt(s.trim());
    }

    private static int parseDecimalBp(String s) {
        if (s == null || s.trim().length() == 0) {
            return 0;
        }
        java.math.BigDecimal rate = new java.math.BigDecimal(s.trim());
        if (rate.compareTo(java.math.BigDecimal.ONE) <= 0) {
            return rate.multiply(new java.math.BigDecimal("10000")).setScale(0, java.math.RoundingMode.HALF_UP).intValue();
        }
        return rate.setScale(0, java.math.RoundingMode.HALF_UP).intValue();
    }

    private static String csv(String s) {
        String v = s == null ? "" : s;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }

    private static boolean isSide(String side) {
        return "B".equals(side) || "S".equals(side);
    }

    private static boolean isOrderType(String orderType) {
        return "L".equals(orderType) || "M".equals(orderType);
    }

    private static boolean isTif(String tif) {
        return "DAY".equals(tif) || "IOC".equals(tif) || "FOK".equals(tif);
    }

    private static java.util.OptionalInt tierRateBp(int tier) {
        if (tier == 1) {
            return java.util.OptionalInt.of(1000);
        }
        if (tier == 2) {
            return java.util.OptionalInt.of(2000);
        }
        if (tier == 3) {
            return java.util.OptionalInt.of(4000);
        }
        return java.util.OptionalInt.empty();
    }

    private static java.util.OptionalInt tierTick(int tier) {
        if (tier == 1) {
            return java.util.OptionalInt.of(100);
        }
        if (tier == 2) {
            return java.util.OptionalInt.of(500);
        }
        if (tier == 3) {
            return java.util.OptionalInt.of(1000);
        }
        return java.util.OptionalInt.empty();
    }

    private static String boardByTier(int tier) {
        if (tier == 1) {
            return "T1";
        }
        if (tier == 2) {
            return "ST";
        }
        if (tier == 3) {
            return "ETF";
        }
        return "";
    }

    private static long expectedRealized(String side, long fillAmt, long fillQty, long avgAmt, long netQty) {
        if (fillQty == 0L) {
            return 0L;
        }
        long unitFill = fillAmt / fillQty;
        long closedQty = java.lang.Math.min(java.lang.Math.abs(netQty), fillQty);
        if ("S".equals(side)) {
            return (unitFill - avgAmt) * closedQty;
        }
        return (avgAmt - unitFill) * closedQty;
    }

    private static String severity(long diff) {
        long abs = java.lang.Math.abs(diff);
        if (abs >= 1000000L) {
            return "高";
        }
        if (abs >= 10000L) {
            return "中";
        }
        return "低";
    }

    private static long multiply(long a, long b) {
        try {
            return java.lang.Math.multiplyExact(a, b);
        } catch (ArithmeticException ex) {
            return java.lang.Long.MAX_VALUE;
        }
    }

    private static String nowText() {
        return java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"));
    }

    private static String maxText(String a, String b) {
        if (a == null || a.length() == 0) {
            return b;
        }
        if (b == null || b.length() == 0) {
            return a;
        }
        return a.compareTo(b) >= 0 ? a : b;
    }

    private static int auditSeq = 0;

    private static final class ExecRow {
        final String execId;
        final String orderId;
        final String instrument;
        final String side;
        final long quantity;
        final long amount;
        final String execTs;

        ExecRow(String execId, String orderId, String instrument, String side, long quantity, long amount, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrument = instrument;
            this.side = side;
            this.quantity = quantity;
            this.amount = amount;
            this.execTs = execTs;
        }
    }

    private static final class PositionRow {
        final String cif;
        final String instrument;
        final long netQty;
        final long avgAmt;
        final long realizedAmt;

        PositionRow(String cif, String instrument, long netQty, long avgAmt, long realizedAmt) {
            this.cif = cif;
            this.instrument = instrument;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.realizedAmt = realizedAmt;
        }
    }

    private static final class OrderRow {
        final String orderId;
        final String cif;
        final String instrument;
        final String side;
        final String orderType;
        final String tif;
        final long quantity;
        final long price;
        final int tier;

        OrderRow(String orderId, String cif, String instrument, String side, String orderType, String tif,
                long quantity, long price, int tier) {
            this.orderId = orderId;
            this.cif = cif;
            this.instrument = instrument;
            this.side = side;
            this.orderType = orderType;
            this.tif = tif;
            this.quantity = quantity;
            this.price = price;
            this.tier = tier;
        }
    }

    private static final class FeeRule {
        final String board;
        final int rateBp;
        final long minFee;

        FeeRule(String board, int rateBp, long minFee) {
            this.board = board;
            this.rateBp = rateBp;
            this.minFee = minFee;
        }

        /*
         * 照合用の概算手数料。レートを乗じるだけの粗い見積りで、最低手数料の補正は行わない。
         * 確定額の算定は mihft_fee 本体に従い、本サービスはその出力との差分検知にのみ用いる。
         */
        long apply(long amount) {
            return new java.math.BigDecimal(amount).multiply(new java.math.BigDecimal(rateBp))
                    .divide(new java.math.BigDecimal("10000"), 0, java.math.RoundingMode.DOWN).longValue();
        }
    }

    private static final class ReconKey {
        final String cif;
        final String instrument;
        final String side;

        ReconKey(String cif, String instrument, String side) {
            this.cif = cif;
            this.instrument = instrument;
            this.side = side;
        }

        public boolean equals(Object other) {
            if (!(other instanceof ReconKey)) {
                return false;
            }
            ReconKey that = (ReconKey) other;
            return this.cif.equals(that.cif) && this.instrument.equals(that.instrument) && this.side.equals(that.side);
        }

        public int hashCode() {
            return java.util.Objects.hash(cif, instrument, side);
        }
    }

    private static final class ReconBucket {
        final ReconKey key;
        long fillQty;
        long fillAmt;
        long orderQty;
        long recalcFee;
        String maxExecTs;

        ReconBucket(ReconKey key) {
            this.key = key;
        }
    }

    private static final class AuditRow {
        final String auditId;
        final String eventTs;
        final String serviceId;
        final String objectId;
        final String eventKind;
        final String detailCode;

        AuditRow(String auditId, String eventTs, String serviceId, String objectId, String eventKind, String detailCode) {
            this.auditId = auditId;
            this.eventTs = eventTs;
            this.serviceId = serviceId;
            this.objectId = objectId;
            this.eventKind = eventKind;
            this.detailCode = detailCode;
        }
    }
}
