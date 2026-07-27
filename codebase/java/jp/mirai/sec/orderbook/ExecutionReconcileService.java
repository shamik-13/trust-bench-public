/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2019-10-22  開発部    初版作成
 */

package jp.mirai.sec.orderbook;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ExecutionReconcileService {
    private static final BigDecimal MIHFT_MAX_NOTIONAL = new BigDecimal("500000000");
    private static final BigDecimal AVG_TOLERANCE = new BigDecimal("1.00");

    private ExecutionReconcileService() {
    }

    public static void main(String[] a) throws Exception {
        if (a.length != 4) {
            throw new IllegalArgumentException("引数はSCEXEC、SCORDF、SCPOSF、SCAUDFの順に指定してください");
        }

        Map<String, OrderRow> orders = readOrders(Path.of(a[1]));
        List<ExecRow> executions = readExecutions(Path.of(a[0]));
        Map<PositionKey, PositionRow> positions = readPositions(Path.of(a[2]));

        List<AuditRow> audits = reconcile(executions, orders, positions);
        writeAudits(Path.of(a[3]), audits);
    }

    private static List<AuditRow> reconcile(List<ExecRow> executions,
                                            Map<String, OrderRow> orders,
                                            Map<PositionKey, PositionRow> positions) {
        Map<String, OrderAgg> byOrder = new LinkedHashMap<String, OrderAgg>();
        Map<PositionKey, PositionAgg> byPosition = new LinkedHashMap<PositionKey, PositionAgg>();
        List<AuditRow> audits = new ArrayList<AuditRow>();

        for (ExecRow exec : executions) {
            OrderRow order = orders.get(exec.orderId);
            if (order == null) {
                audits.add(AuditRow.of(exec.orderId, "1", "", exec.instrCode, exec.execTs, "ORD-NOTFOUND"));
                continue;
            }

            if (!order.instrCode.equals(exec.instrCode)) {
                audits.add(AuditRow.of(exec.orderId, "1", order.cifNo, exec.instrCode, exec.execTs, "INSTR-UNMATCH"));
                continue;
            }

            if (!order.sideKbn.equals(exec.sideKbn)) {
                audits.add(AuditRow.of(exec.orderId, "1", order.cifNo, exec.instrCode, exec.execTs, "SIDE-UNMATCH"));
            }

            if (exec.fillQty.signum() <= 0 || exec.fillAmt.signum() < 0) {
                audits.add(AuditRow.of(exec.orderId, "1", order.cifNo, exec.instrCode, exec.execTs, "EXEC-VALUE"));
                continue;
            }

            if (!isTickAligned(exec.fillAmt, order.instrTier, exec.fillQty)) {
                audits.add(AuditRow.of(exec.orderId, "1", order.cifNo, exec.instrCode, exec.execTs, "TICK-UNMATCH"));
            }

            if (exec.fillAmt.compareTo(MIHFT_MAX_NOTIONAL) > 0) {
                audits.add(AuditRow.of(exec.orderId, "1", order.cifNo, exec.instrCode, exec.execTs, "NOTIONAL-OVER"));
            }

            OrderAgg orderAgg = byOrder.get(exec.orderId);
            if (orderAgg == null) {
                orderAgg = new OrderAgg(order);
                byOrder.put(exec.orderId, orderAgg);
            }
            orderAgg.add(exec);

            PositionKey key = new PositionKey(order.cifNo, order.instrCode);
            PositionAgg positionAgg = byPosition.get(key);
            if (positionAgg == null) {
                positionAgg = new PositionAgg();
                byPosition.put(key, positionAgg);
            }
            positionAgg.add(exec);
        }

        for (Map.Entry<String, OrderAgg> e : byOrder.entrySet()) {
            OrderAgg agg = e.getValue();
            OrderRow order = agg.order;
            if (agg.fillQty.compareTo(order.ordQty) > 0) {
                audits.add(AuditRow.of(order.orderId, "2", order.cifNo, order.instrCode, agg.lastTs, "ORDQTY-OVER"));
            }

            BigDecimal avg = agg.averageAmount();
            if (order.priceAmt.signum() > 0 && "L".equals(order.ordType)) {
                if ("B".equals(order.sideKbn) && avg.compareTo(order.priceAmt) > 0) {
                    audits.add(AuditRow.of(order.orderId, "2", order.cifNo, order.instrCode, agg.lastTs, "PRICE-OVER"));
                }
                if ("S".equals(order.sideKbn) && avg.compareTo(order.priceAmt) < 0) {
                    audits.add(AuditRow.of(order.orderId, "2", order.cifNo, order.instrCode, agg.lastTs, "PRICE-UNDER"));
                }
            }

            if ("FOK".equals(order.tifCode) && agg.fillQty.compareTo(order.ordQty) != 0) {
                audits.add(AuditRow.of(order.orderId, "2", order.cifNo, order.instrCode, agg.lastTs, "FOK-PARTIAL"));
            }
        }

        for (Map.Entry<PositionKey, PositionAgg> e : byPosition.entrySet()) {
            PositionKey key = e.getKey();
            PositionAgg agg = e.getValue();
            PositionRow position = positions.get(key);
            if (position == null) {
                audits.add(AuditRow.of("", "3", key.cifNo, key.instrCode, agg.lastTs, "POS-NOTFOUND"));
                continue;
            }

            if (position.netQty.compareTo(agg.netQty) != 0) {
                audits.add(AuditRow.of("", "3", key.cifNo, key.instrCode, agg.lastTs, "POSQTY-UNMATCH"));
            }

            if (agg.netQty.signum() != 0) {
                BigDecimal execAvg = agg.averageAmount();
                if (position.avgAmt.subtract(execAvg).abs().compareTo(AVG_TOLERANCE) > 0) {
                    audits.add(AuditRow.of("", "3", key.cifNo, key.instrCode, agg.lastTs, "POSAVG-UNMATCH"));
                }
            }
        }

        return audits;
    }

    private static boolean isTickAligned(BigDecimal fillAmt, int tier, BigDecimal fillQty) {
        BigDecimal tick = tickAmount(tier);
        if (tick.signum() == 0 || fillQty.signum() == 0) {
            return false;
        }
        BigDecimal unit = fillAmt.divide(fillQty, 8, RoundingMode.HALF_UP);
        return unit.remainder(tick).compareTo(BigDecimal.ZERO) == 0;
    }

    private static BigDecimal tickAmount(int tier) {
        if (tier == 1) {
            return new BigDecimal("100");
        }
        if (tier == 2) {
            return new BigDecimal("500");
        }
        if (tier == 3) {
            return new BigDecimal("1000");
        }
        return BigDecimal.ZERO;
    }

    private static Map<String, OrderRow> readOrders(Path path) throws IOException {
        Map<String, OrderRow> rows = new HashMap<String, OrderRow>();
        try (BufferedReader br = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (first) {
                    first = false;
                    if (line.startsWith("ORDER-ID,")) {
                        continue;
                    }
                }
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> c = splitCsv(line);
                OrderRow row = new OrderRow(
                        c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), c.get(5),
                        new BigDecimal(c.get(6)), new BigDecimal(c.get(7)), Integer.parseInt(c.get(8)));
                rows.put(row.orderId, row);
            }
        }
        return rows;
    }

    private static List<ExecRow> readExecutions(Path path) throws IOException {
        List<ExecRow> rows = new ArrayList<ExecRow>();
        try (BufferedReader br = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (first) {
                    first = false;
                    if (line.startsWith("EXEC-ID,")) {
                        continue;
                    }
                }
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> c = splitCsv(line);
                rows.add(new ExecRow(c.get(0), c.get(1), c.get(2), c.get(3),
                        new BigDecimal(c.get(4)), new BigDecimal(c.get(5)), c.get(6)));
            }
        }
        return rows;
    }

    private static Map<PositionKey, PositionRow> readPositions(Path path) throws IOException {
        Map<PositionKey, PositionRow> rows = new HashMap<PositionKey, PositionRow>();
        try (BufferedReader br = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (first) {
                    first = false;
                    if (line.startsWith("CIF-NO,")) {
                        continue;
                    }
                }
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> c = splitCsv(line);
                PositionRow row = new PositionRow(c.get(0), c.get(1),
                        new BigDecimal(c.get(2)), new BigDecimal(c.get(3)), new BigDecimal(c.get(4)));
                rows.put(new PositionKey(row.cifNo, row.instrCode), row);
            }
        }
        return rows;
    }

    private static void writeAudits(Path path, List<AuditRow> audits) throws IOException {
        try (BufferedWriter bw = Files.newBufferedWriter(path, StandardCharsets.UTF_8)) {
            bw.write("AUDIT-ID,ORDER-ID,EVENT-KBN,CIF-NO,INSTR-CODE,EVENT-TS,DETAIL-CD");
            bw.newLine();
            for (AuditRow audit : audits) {
                bw.write(csv(audit.auditId));
                bw.write(',');
                bw.write(csv(audit.orderId));
                bw.write(',');
                bw.write(csv(audit.eventKbn));
                bw.write(',');
                bw.write(csv(audit.cifNo));
                bw.write(',');
                bw.write(csv(audit.instrCode));
                bw.write(',');
                bw.write(csv(audit.eventTs));
                bw.write(',');
                bw.write(csv(audit.detailCd));
                bw.newLine();
            }
        }
    }

    private static List<String> splitCsv(String line) {
        List<String> out = new ArrayList<String>();
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
                out.add(sb.toString().trim());
                sb.setLength(0);
            } else {
                sb.append(ch);
            }
        }
        out.add(sb.toString().trim());
        return out;
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

    private static final class ExecRow {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final BigDecimal fillQty;
        final BigDecimal fillAmt;
        final String execTs;

        ExecRow(String execId, String orderId, String instrCode, String sideKbn,
                BigDecimal fillQty, BigDecimal fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class OrderRow {
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final String ordType;
        final String tifCode;
        final BigDecimal ordQty;
        final BigDecimal priceAmt;
        final int instrTier;

        OrderRow(String orderId, String cifNo, String instrCode, String sideKbn, String ordType,
                 String tifCode, BigDecimal ordQty, BigDecimal priceAmt, int instrTier) {
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

    private static final class PositionRow {
        final String cifNo;
        final String instrCode;
        final BigDecimal netQty;
        final BigDecimal avgAmt;
        final BigDecimal rlzdAmt;

        PositionRow(String cifNo, String instrCode, BigDecimal netQty, BigDecimal avgAmt, BigDecimal rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
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

        static AuditRow of(String orderId, String eventKbn, String cifNo,
                           String instrCode, String eventTs, String detailCd) {
            String baseTs = eventTs == null || eventTs.isEmpty() ? LocalDateTime.now().toString() : eventTs;
            String auditId = "AU" + baseTs.replace("-", "").replace(":", "").replace("T", "").replace(".", "") + detailCd;
            return new AuditRow(auditId, orderId, eventKbn, cifNo, instrCode, baseTs, detailCd);
        }
    }

    private static final class PositionKey {
        final String cifNo;
        final String instrCode;

        PositionKey(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        public boolean equals(Object o) {
            if (!(o instanceof PositionKey)) {
                return false;
            }
            PositionKey other = (PositionKey) o;
            return cifNo.equals(other.cifNo) && instrCode.equals(other.instrCode);
        }

        public int hashCode() {
            return 31 * cifNo.hashCode() + instrCode.hashCode();
        }
    }

    private static final class OrderAgg {
        final OrderRow order;
        BigDecimal fillQty = BigDecimal.ZERO;
        BigDecimal fillAmt = BigDecimal.ZERO;
        String lastTs = "";

        OrderAgg(OrderRow order) {
            this.order = order;
        }

        void add(ExecRow exec) {
            fillQty = fillQty.add(exec.fillQty);
            fillAmt = fillAmt.add(exec.fillAmt);
            lastTs = exec.execTs;
        }

        BigDecimal averageAmount() {
            if (fillQty.signum() == 0) {
                return BigDecimal.ZERO;
            }
            return fillAmt.divide(fillQty, 8, RoundingMode.HALF_UP);
        }
    }

    private static final class PositionAgg {
        BigDecimal netQty = BigDecimal.ZERO;
        BigDecimal grossQty = BigDecimal.ZERO;
        BigDecimal fillAmt = BigDecimal.ZERO;
        String lastTs = "";

        void add(ExecRow exec) {
            BigDecimal signedQty = "B".equals(exec.sideKbn) ? exec.fillQty : exec.fillQty.negate();
            netQty = netQty.add(signedQty);
            grossQty = grossQty.add(exec.fillQty);
            fillAmt = fillAmt.add(exec.fillAmt);
            lastTs = exec.execTs;
        }

        BigDecimal averageAmount() {
            if (grossQty.signum() == 0) {
                return BigDecimal.ZERO;
            }
            return fillAmt.divide(grossQty, 8, RoundingMode.HALF_UP);
        }
    }
}
