/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2022-02-22  今井 彩 (E-230)  注文復旧サービス初版
 *
 * MIHFT_MAX_NOTIONAL=500000000
 */

package jp.mirai.sec.grouprisk;

public class OrderRecoveryService {
    private static final long MIHFT_MAX_NOTIONAL = 500_000_000L;
    private static final java.time.ZoneId SESSION_ZONE = java.time.ZoneId.of("Asia/Tokyo");
    private static final String ACTOR_ID = "ORDER-RECOVERY-SVC";

    public static void main(String[] a) {
        if (a.length != 4) {
            System.err.println("引数不正: SCORDF SCEXEC HFDECLOG SCAUDF2 を指定してください");
            System.exit(2);
        }

        try {
            OrderRecoveryService service = new OrderRecoveryService();
            RecoverySummary summary = service.recover(
                    java.nio.file.Paths.get(a[0]),
                    java.nio.file.Paths.get(a[1]),
                    java.nio.file.Paths.get(a[2]),
                    java.nio.file.Paths.get(a[3]));
            System.out.println("注文復旧監査出力完了: 判定=" + summary.decisions
                    + " 候補=" + summary.candidates
                    + " 抑止=" + summary.suppressed);
        } catch (Exception e) {
            System.err.println("注文復旧処理失敗: " + e.getMessage());
            System.exit(1);
        }
    }

    public RecoverySummary recover(
            java.nio.file.Path scorfd,
            java.nio.file.Path scexec,
            java.nio.file.Path hfdeclog,
            java.nio.file.Path scaudf2) throws java.io.IOException {

        java.util.Map<String, OrderRecord> orders = readOrders(scorfd);
        java.util.Map<String, ExecAggregate> execs = readExecutions(scexec);
        java.util.Map<String, DecisionRecord> finals = readFinalDecisions(hfdeclog);

        java.time.LocalDate sessionDate = java.time.LocalDate.now(SESSION_ZONE);
        java.time.Instant now = java.time.Instant.now();
        RefData refData = new RefData(sessionDate);

        java.util.List<AuditRecord> audits = new java.util.ArrayList<>();
        int candidates = 0;
        int suppressed = 0;

        for (DecisionRecord decision : finals.values()) {
            if (decision.actionCode != 0) {
                continue;
            }

            OrderRecord order = orders.get(decision.orderId);
            ExecAggregate exec = execs.get(decision.orderId);
            RecoveryVerdict verdict = judgeRecovery(decision, order, exec, refData);

            if (verdict.recoverable) {
                candidates++;
            } else {
                suppressed++;
            }

            audits.add(new AuditRecord(
                    auditId(decision.orderId, decision.decisionId),
                    ACTOR_ID,
                    verdict.recoverable ? "RCV-CAND" : "RCV-SUPP",
                    decision.orderId,
                    verdict.resultCode,
                    now));
        }

        writeAudits(scaudf2, audits);
        return new RecoverySummary(finals.size(), candidates, suppressed);
    }

    private RecoveryVerdict judgeRecovery(
            DecisionRecord decision,
            OrderRecord order,
            ExecAggregate exec,
            RefData refData) {

        if (!refData.isSessionOpen(decision.decisionTs)) {
            return new RecoveryVerdict(false, "NG-SESSION");
        }
        if (!refData.isTradable(decision.instrCode)) {
            return new RecoveryVerdict(false, "NG-INSTR");
        }
        if (order == null) {
            return new RecoveryVerdict(true, "OK-MISSING-OMS");
        }

        String orderError = validateOrder(order);
        if (orderError != null) {
            return new RecoveryVerdict(false, orderError);
        }
        if (exec != null && exec.fillQty > order.orderQty) {
            return new RecoveryVerdict(false, "NG-EXEC-QTY");
        }
        if (exec != null && exec.fillQty > 0) {
            return new RecoveryVerdict(false, "NG-EXEC-EXISTS");
        }
        return new RecoveryVerdict(false, "NG-OMS-REFLECTED");
    }

    private String validateOrder(OrderRecord order) {
        TierSpec tier = TierSpec.of(order.instrTier);
        if (tier == null) {
            return "NG-TIER";
        }
        if (!"B".equals(order.sideKbn) && !"S".equals(order.sideKbn)) {
            return "NG-SIDE";
        }
        if (!"L".equals(order.ordType) && !"M".equals(order.ordType)) {
            return "NG-ORDTYPE";
        }
        if (!"DAY".equals(order.tifCode) && !"IOC".equals(order.tifCode) && !"FOK".equals(order.tifCode)) {
            return "NG-TIF";
        }
        if ("L".equals(order.ordType) && order.priceAmt <= 0L) {
            return "NG-PRICE";
        }
        if (order.orderQty <= 0L) {
            return "NG-QTY";
        }

        long notional = safeMultiply(order.orderQty, order.priceAmt);
        if (notional > MIHFT_MAX_NOTIONAL) {
            return "NG-NOTIONAL";
        }
        if ("L".equals(order.ordType) && order.priceAmt % tier.tick != 0L) {
            return "NG-TICK";
        }

        long margin = notional * tier.marginRateBp / 10_000L;
        if (margin <= 0L && notional > 0L) {
            return "NG-MARGIN";
        }
        return null;
    }

    private java.util.Map<String, OrderRecord> readOrders(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, OrderRecord> map = new java.util.LinkedHashMap<>();
        for (String line : readDataLines(path)) {
            java.util.List<String> c = splitCsv(line);
            requireColumns(path, c, 9);
            OrderRecord r = new OrderRecord(
                    c.get(0),
                    c.get(1),
                    c.get(2),
                    c.get(3),
                    c.get(4),
                    c.get(5),
                    parseLong(c.get(6), "ORD-QTY"),
                    parseMoney(c.get(7), "PRICE-AMT"),
                    parseInt(c.get(8), "INSTR-TIER"));
            map.put(r.orderId, r);
        }
        return map;
    }

    private java.util.Map<String, ExecAggregate> readExecutions(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, ExecAggregate> map = new java.util.LinkedHashMap<>();
        for (String line : readDataLines(path)) {
            java.util.List<String> c = splitCsv(line);
            requireColumns(path, c, 7);
            String orderId = c.get(1);
            long qty = parseLong(c.get(4), "FILL-QTY");
            long amt = parseMoney(c.get(5), "FILL-AMT");
            java.time.Instant ts = parseTimestamp(c.get(6), "EXEC-TS");

            ExecAggregate current = map.get(orderId);
            if (current == null) {
                map.put(orderId, new ExecAggregate(orderId, qty, amt, ts));
            } else {
                current.fillQty += qty;
                current.fillAmt += amt;
                if (ts.isAfter(current.lastExecTs)) {
                    current.lastExecTs = ts;
                }
            }
        }
        return map;
    }

    private java.util.Map<String, DecisionRecord> readFinalDecisions(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, DecisionRecord> map = new java.util.LinkedHashMap<>();
        for (String line : readDataLines(path)) {
            java.util.List<String> c = splitCsv(line);
            requireColumns(path, c, 6);
            DecisionRecord r = new DecisionRecord(
                    c.get(0),
                    c.get(1),
                    c.get(2),
                    parseInt(c.get(3), "ACTION-CODE"),
                    c.get(4),
                    parseTimestamp(c.get(5), "DECISION-TS"));

            DecisionRecord current = map.get(r.orderId);
            if (current == null || r.decisionTs.isAfter(current.decisionTs)
                    || (r.decisionTs.equals(current.decisionTs) && r.decisionId.compareTo(current.decisionId) > 0)) {
                map.put(r.orderId, r);
            }
        }
        return map;
    }

    private void writeAudits(java.nio.file.Path path, java.util.List<AuditRecord> audits) throws java.io.IOException {
        java.nio.file.Path parent = path.toAbsolutePath().getParent();
        if (parent != null) {
            java.nio.file.Files.createDirectories(parent);
        }

        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add("AUDIT-ID,ACTOR-ID,ACTION-KBN,OBJECT-ID,RESULT-CODE,AUDIT-TS");
        for (AuditRecord a : audits) {
            lines.add(csv(a.auditId) + ","
                    + csv(a.actorId) + ","
                    + csv(a.actionKbn) + ","
                    + csv(a.objectId) + ","
                    + csv(a.resultCode) + ","
                    + csv(java.time.format.DateTimeFormatter.ISO_INSTANT.format(a.auditTs)));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private java.util.List<String> readDataLines(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        int row = 0;
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            row++;
            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            if (row == 1 && looksLikeHeader(trimmed)) {
                continue;
            }
            out.add(line);
        }
        return out;
    }

    private boolean looksLikeHeader(String line) {
        String upper = line.toUpperCase(java.util.Locale.ROOT);
        return upper.contains("ORDER-ID")
                || upper.contains("EXEC-ID")
                || upper.contains("DECISION-ID")
                || upper.contains("AUDIT-ID");
    }

    private java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
        StringBuilder cell = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cell.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                out.add(cell.toString().trim());
                cell.setLength(0);
            } else {
                cell.append(ch);
            }
        }
        out.add(cell.toString().trim());
        return out;
    }

    private String csv(String value) {
        boolean quote = value.indexOf(',') >= 0 || value.indexOf('"') >= 0 || value.indexOf('\n') >= 0;
        if (!quote) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }

    private void requireColumns(java.nio.file.Path path, java.util.List<String> c, int min) {
        if (c.size() < min) {
            throw new IllegalArgumentException("項目数不正: " + path + " 必要=" + min + " 実際=" + c.size());
        }
    }

    private int parseInt(String v, String name) {
        try {
            return Integer.parseInt(v.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("数値不正: " + name + "=" + v);
        }
    }

    private long parseLong(String v, String name) {
        try {
            return Long.parseLong(v.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("数値不正: " + name + "=" + v);
        }
    }

    private long parseMoney(String v, String name) {
        try {
            java.math.BigDecimal bd = new java.math.BigDecimal(v.trim());
            return bd.setScale(0, java.math.RoundingMode.UNNECESSARY).longValueExact();
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("金額不正: " + name + "=" + v);
        }
    }

    private java.time.Instant parseTimestamp(String v, String name) {
        String s = v.trim();
        try {
            return java.time.Instant.parse(s);
        } catch (RuntimeException ignored) {
            try {
                return java.time.LocalDateTime.parse(s)
                        .atZone(SESSION_ZONE)
                        .toInstant();
            } catch (RuntimeException e) {
                throw new IllegalArgumentException("時刻不正: " + name + "=" + v);
            }
        }
    }

    private long safeMultiply(long a, long b) {
        try {
            return Math.multiplyExact(a, b);
        } catch (ArithmeticException e) {
            return Long.MAX_VALUE;
        }
    }

    private String auditId(String orderId, String decisionId) {
        String raw = orderId + "-" + decisionId;
        java.util.zip.CRC32 crc = new java.util.zip.CRC32();
        crc.update(raw.getBytes(java.nio.charset.StandardCharsets.UTF_8));
        return "AR" + Long.toUnsignedString(crc.getValue(), 36).toUpperCase(java.util.Locale.ROOT);
    }

    private static final class RefData {
        private final java.time.LocalDate sessionDate;

        RefData(java.time.LocalDate sessionDate) {
            this.sessionDate = sessionDate;
        }

        boolean isSessionOpen(java.time.Instant decisionTs) {
            java.time.LocalDate d = decisionTs.atZone(SESSION_ZONE).toLocalDate();
            java.time.DayOfWeek w = d.getDayOfWeek();
            return d.equals(sessionDate)
                    && w != java.time.DayOfWeek.SATURDAY
                    && w != java.time.DayOfWeek.SUNDAY;
        }

        boolean isTradable(String instrCode) {
            if (instrCode == null || instrCode.isEmpty()) {
                return false;
            }
            String s = instrCode.toUpperCase(java.util.Locale.ROOT);
            return !(s.startsWith("HALT") || s.startsWith("STOP") || s.endsWith("-S"));
        }
    }

    private static final class TierSpec {
        final int tier;
        final long marginRateBp;
        final long tick;

        private TierSpec(int tier, long marginRateBp, long tick) {
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tick = tick;
        }

        static TierSpec of(int tier) {
            switch (tier) {
                case 1:
                    return new TierSpec(1, 1_000L, 100L);
                case 2:
                    return new TierSpec(2, 2_000L, 500L);
                case 3:
                    return new TierSpec(3, 4_000L, 1_000L);
                default:
                    return null;
            }
        }
    }

    public static final class RecoverySummary {
        public final int decisions;
        public final int candidates;
        public final int suppressed;

        RecoverySummary(int decisions, int candidates, int suppressed) {
            this.decisions = decisions;
            this.candidates = candidates;
            this.suppressed = suppressed;
        }
    }

    private static final class RecoveryVerdict {
        final boolean recoverable;
        final String resultCode;

        RecoveryVerdict(boolean recoverable, String resultCode) {
            this.recoverable = recoverable;
            this.resultCode = resultCode;
        }
    }

    private static final class OrderRecord {
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final String ordType;
        final String tifCode;
        final long orderQty;
        final long priceAmt;
        final int instrTier;

        OrderRecord(String orderId, String cifNo, String instrCode, String sideKbn,
                    String ordType, String tifCode, long orderQty, long priceAmt, int instrTier) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.ordType = ordType;
            this.tifCode = tifCode;
            this.orderQty = orderQty;
            this.priceAmt = priceAmt;
            this.instrTier = instrTier;
        }
    }

    private static final class ExecAggregate {
        final String orderId;
        long fillQty;
        long fillAmt;
        java.time.Instant lastExecTs;

        ExecAggregate(String orderId, long fillQty, long fillAmt, java.time.Instant lastExecTs) {
            this.orderId = orderId;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.lastExecTs = lastExecTs;
        }
    }

    private static final class DecisionRecord {
        final String decisionId;
        final String orderId;
        final String instrCode;
        final int actionCode;
        final String reasonCode;
        final java.time.Instant decisionTs;

        DecisionRecord(String decisionId, String orderId, String instrCode,
                       int actionCode, String reasonCode, java.time.Instant decisionTs) {
            this.decisionId = decisionId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.actionCode = actionCode;
            this.reasonCode = reasonCode;
            this.decisionTs = decisionTs;
        }
    }

    private static final class AuditRecord {
        final String auditId;
        final String actorId;
        final String actionKbn;
        final String objectId;
        final String resultCode;
        final java.time.Instant auditTs;

        AuditRecord(String auditId, String actorId, String actionKbn,
                    String objectId, String resultCode, java.time.Instant auditTs) {
            this.auditId = auditId;
            this.actorId = actorId;
            this.actionKbn = actionKbn;
            this.objectId = objectId;
            this.resultCode = resultCode;
            this.auditTs = auditTs;
        }
    }
}
