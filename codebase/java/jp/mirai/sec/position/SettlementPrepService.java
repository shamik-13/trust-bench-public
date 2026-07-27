/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2019/10/22  藤田 和也 (E-271)     初版作成
 *
 * 決済準備サービス。
 * 閾値: MIHFT_MAX_NOTIONAL=500000000
 */

package jp.mirai.sec.position;

public class SettlementPrepService {
    private static final java.nio.charset.Charset CS = java.nio.charset.StandardCharsets.UTF_8;
    private static final java.math.BigDecimal MIHFT_MAX_NOTIONAL = new java.math.BigDecimal("500000000");
    private static final java.math.BigDecimal BP_DENOM = new java.math.BigDecimal("10000");
    private static final String STATUS_UNFIXED = "0";
    private static final String STATUS_FIXED = "1";
    private static final String STATUS_HOLD = "4";
    private static final String STATUS_CANCEL = "9";

    public static void main(String[] a) throws Exception {
        java.nio.file.Path scsetf = path(a, 0, "SCSETF", "SCSETF.csv");
        java.nio.file.Path scexec = path(a, 1, "SCEXEC", "SCEXEC.csv");
        java.nio.file.Path sccalf = path(a, 2, "SCCALF", "SCCALF.csv");
        java.nio.file.Path scfeef = path(a, 3, "SCFEEF", "SCFEEF.csv");
        java.nio.file.Path out = path(a, 4, "SCSETF_OUT", scsetf.toString());

        java.util.Map<java.time.LocalDate, CalRow> cal = readCalendar(sccalf);
        java.util.Map<String, FeeRow> fee = readFees(scfeef);
        java.util.List<ExecRow> execs = readExecs(scexec);
        java.util.List<SettleRow> settles = readSettles(scsetf);

        java.util.List<SettleRow> updated = new java.util.ArrayList<>();
        for (SettleRow s : settles) {
            if (!STATUS_UNFIXED.equals(s.statusKbn)) {
                updated.add(s);
                continue;
            }

            CalRow sess = cal.get(s.settleDt);
            if (sess == null || !"1".equals(sess.sessKbn)) {
                updated.add(s.withStatus(STATUS_HOLD));
                continue;
            }

            java.util.List<ExecRow> matched = new java.util.ArrayList<>();
            boolean mixedCancel = false;
            boolean outOfSession = false;
            for (ExecRow e : execs) {
                if (!s.instrCode.equals(e.instrCode)) {
                    continue;
                }
                if (!s.settleDt.equals(e.execTs.toLocalDate())) {
                    continue;
                }
                if (e.cancelled) {
                    mixedCancel = true;
                    continue;
                }
                java.time.LocalTime t = e.execTs.toLocalTime();
                if (t.isBefore(sess.openTs) || t.isAfter(sess.closeTs)) {
                    outOfSession = true;
                }
                matched.add(e);
            }

            if (mixedCancel || outOfSession || "2".equals(sess.sessKbn)) {
                updated.add(s.withStatus(STATUS_HOLD));
                continue;
            }
            if (matched.isEmpty()) {
                updated.add(s.withStatus(STATUS_HOLD));
                continue;
            }

            long qty = 0L;
            java.math.BigDecimal cash = java.math.BigDecimal.ZERO;
            java.math.BigDecimal notional = java.math.BigDecimal.ZERO;
            for (ExecRow e : matched) {
                int sign = "B".equals(e.sideKbn) ? -1 : 1;
                qty += sign * e.fillQty;
                notional = notional.add(e.fillAmt.abs());
                cash = cash.add(e.fillAmt.multiply(java.math.BigDecimal.valueOf(sign)));
            }

            if (notional.compareTo(MIHFT_MAX_NOTIONAL) > 0) {
                updated.add(s.withStatus(STATUS_HOLD));
                continue;
            }

            FeeRow fr = fee.get(boardCode(s.instrCode));
            if (fr == null) {
                updated.add(s.withStatus(STATUS_HOLD));
                continue;
            }

            java.math.BigDecimal commission = notional
                    .multiply(fr.feeRate)
                    .divide(BP_DENOM, 0, java.math.RoundingMode.UP);
            if (commission.compareTo(fr.minFeeAmt) < 0) {
                commission = fr.minFeeAmt;
            }

            java.math.BigDecimal netCash = cash.subtract(commission).setScale(0, java.math.RoundingMode.HALF_UP);
            updated.add(new SettleRow(s.settleId, s.cifNo, s.instrCode, s.settleDt, qty, netCash, STATUS_FIXED));
        }

        writeSettles(out, updated);
        System.out.println("決済準備件数=" + updated.size());
    }

    private static java.nio.file.Path path(String[] a, int ix, String env, String def) {
        if (a.length > ix && a[ix] != null && !a[ix].trim().isEmpty()) {
            return java.nio.file.Paths.get(a[ix]);
        }
        String v = System.getenv(env);
        if (v != null && !v.trim().isEmpty()) {
            return java.nio.file.Paths.get(v);
        }
        return java.nio.file.Paths.get(def);
    }

    private static java.util.List<SettleRow> readSettles(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<SettleRow> rows = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(p, CS)) {
            if (skip(line)) {
                continue;
            }
            java.util.List<String> c = csv(line);
            if ("SETTLE-ID".equals(c.get(0)) || "SETTLE_ID".equals(c.get(0))) {
                continue;
            }
            rows.add(new SettleRow(c.get(0), c.get(1), c.get(2), date(c.get(3)),
                    Long.parseLong(c.get(4)), yen(c.get(5)), c.get(6)));
        }
        return rows;
    }

    private static java.util.List<ExecRow> readExecs(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<ExecRow> rows = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(p, CS)) {
            if (skip(line)) {
                continue;
            }
            java.util.List<String> c = csv(line);
            if ("EXEC-ID".equals(c.get(0)) || "EXEC_ID".equals(c.get(0))) {
                continue;
            }
            String execId = c.get(0);
            boolean cancelled = execId.startsWith("CXL") || execId.endsWith("-C") || STATUS_CANCEL.equals(last(c));
            rows.add(new ExecRow(execId, c.get(1), c.get(2), c.get(3), Long.parseLong(c.get(4)),
                    yen(c.get(5)), ts(c.get(6)), cancelled));
        }
        return rows;
    }

    private static java.util.Map<java.time.LocalDate, CalRow> readCalendar(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<java.time.LocalDate, CalRow> rows = new java.util.HashMap<>();
        for (String line : java.nio.file.Files.readAllLines(p, CS)) {
            if (skip(line)) {
                continue;
            }
            java.util.List<String> c = csv(line);
            if ("SESS-DT".equals(c.get(0)) || "SESS_DT".equals(c.get(0))) {
                continue;
            }
            rows.put(date(c.get(0)), new CalRow(date(c.get(0)), c.get(1), time(c.get(2)), time(c.get(3))));
        }
        return rows;
    }

    private static java.util.Map<String, FeeRow> readFees(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, FeeRow> rows = new java.util.HashMap<>();
        for (String line : java.nio.file.Files.readAllLines(p, CS)) {
            if (skip(line)) {
                continue;
            }
            java.util.List<String> c = csv(line);
            if ("BOARD-CODE".equals(c.get(0)) || "BOARD_CODE".equals(c.get(0))) {
                continue;
            }
            rows.put(c.get(0), new FeeRow(c.get(0), new java.math.BigDecimal(c.get(1)), yen(c.get(2))));
        }
        return rows;
    }

    private static void writeSettles(java.nio.file.Path p, java.util.List<SettleRow> rows) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("SETTLE-ID,CIF-NO,INSTR-CODE,SETTLE-DT,NET-QTY,NET-CASH-AMT,STATUS-KBN");
        for (SettleRow r : rows) {
            out.add(join(r.settleId, r.cifNo, r.instrCode, r.settleDt.toString(),
                    Long.toString(r.netQty), r.netCashAmt.toPlainString(), r.statusKbn));
        }
        java.nio.file.Files.write(p, out, CS);
    }

    private static String boardCode(String instrCode) {
        if (instrCode.startsWith("13") || instrCode.startsWith("15")) {
            return "ETF";
        }
        if (instrCode.startsWith("4") || instrCode.startsWith("7")) {
            return "ST";
        }
        return "T1";
    }

    private static java.time.LocalDate date(String s) {
        String v = s.trim();
        if (v.indexOf('/') >= 0) {
            return java.time.LocalDate.parse(v, java.time.format.DateTimeFormatter.ofPattern("yyyy/M/d"));
        }
        if (v.length() == 8 && v.charAt(4) != '-') {
            return java.time.LocalDate.parse(v, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        }
        return java.time.LocalDate.parse(v);
    }

    private static java.time.LocalDateTime ts(String s) {
        String v = s.trim().replace(' ', 'T');
        if (v.length() == 14 && v.charAt(8) != 'T') {
            return java.time.LocalDateTime.parse(v, java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
        }
        return java.time.LocalDateTime.parse(v);
    }

    private static java.time.LocalTime time(String s) {
        String v = s.trim();
        if (v.length() == 4 && v.indexOf(':') < 0) {
            return java.time.LocalTime.parse(v, java.time.format.DateTimeFormatter.ofPattern("HHmm"));
        }
        return java.time.LocalTime.parse(v);
    }

    private static java.math.BigDecimal yen(String s) {
        String v = s.trim().replace(",", "");
        if (v.isEmpty()) {
            return java.math.BigDecimal.ZERO;
        }
        return new java.math.BigDecimal(v);
    }

    private static boolean skip(String line) {
        return line == null || line.trim().isEmpty() || line.trim().startsWith("#");
    }

    private static String last(java.util.List<String> c) {
        return c.isEmpty() ? "" : c.get(c.size() - 1);
    }

    private static java.util.List<String> csv(String line) {
        java.util.List<String> r = new java.util.ArrayList<>();
        StringBuilder b = new StringBuilder();
        boolean q = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (q && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    q = !q;
                }
            } else if (ch == ',' && !q) {
                r.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        r.add(b.toString().trim());
        return r;
    }

    private static String join(String... c) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < c.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            String v = c[i] == null ? "" : c[i];
            if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0) {
                b.append('"').append(v.replace("\"", "\"\"")).append('"');
            } else {
                b.append(v);
            }
        }
        return b.toString();
    }

    private static final class SettleRow {
        final String settleId;
        final String cifNo;
        final String instrCode;
        final java.time.LocalDate settleDt;
        final long netQty;
        final java.math.BigDecimal netCashAmt;
        final String statusKbn;

        SettleRow(String settleId, String cifNo, String instrCode, java.time.LocalDate settleDt,
                  long netQty, java.math.BigDecimal netCashAmt, String statusKbn) {
            this.settleId = settleId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.settleDt = settleDt;
            this.netQty = netQty;
            this.netCashAmt = netCashAmt;
            this.statusKbn = statusKbn;
        }

        SettleRow withStatus(String v) {
            return new SettleRow(settleId, cifNo, instrCode, settleDt, netQty, netCashAmt, v);
        }
    }

    private static final class ExecRow {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final java.math.BigDecimal fillAmt;
        final java.time.LocalDateTime execTs;
        final boolean cancelled;

        ExecRow(String execId, String orderId, String instrCode, String sideKbn, long fillQty,
                java.math.BigDecimal fillAmt, java.time.LocalDateTime execTs, boolean cancelled) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
            this.cancelled = cancelled;
        }
    }

    private static final class CalRow {
        final java.time.LocalDate sessDt;
        final String sessKbn;
        final java.time.LocalTime openTs;
        final java.time.LocalTime closeTs;

        CalRow(java.time.LocalDate sessDt, String sessKbn, java.time.LocalTime openTs, java.time.LocalTime closeTs) {
            this.sessDt = sessDt;
            this.sessKbn = sessKbn;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }

    private static final class FeeRow {
        final String boardCode;
        final java.math.BigDecimal feeRate;
        final java.math.BigDecimal minFeeAmt;

        FeeRow(String boardCode, java.math.BigDecimal feeRate, java.math.BigDecimal minFeeAmt) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmt = minFeeAmt;
        }
    }
}
