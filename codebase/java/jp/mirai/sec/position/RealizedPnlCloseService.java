/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2024/07/09  西村 亮 (E-204)    実現損益締めサービス初版
 *
 * MIHFT_MAX_NOTIONAL=500000000
 */

package jp.mirai.sec.position;

public class RealizedPnlCloseService {
    private static final java.math.BigDecimal ZERO = java.math.BigDecimal.ZERO;
    private static final java.time.format.DateTimeFormatter TS_FMT =
            java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd['T'][ ]HH:mm:ss");
    private static final java.time.format.DateTimeFormatter DT_FMT =
            java.time.format.DateTimeFormatter.ISO_LOCAL_DATE;

    public static void main(String[] a) {
        if (a.length != 4) {
            System.err.println("使用法: java RealizedPnlCloseService SCPNLF SCEXEC SCFEEF 出力SCPNLF");
            System.exit(2);
        }

        try {
            java.util.List<PnlRow> pnlRows = readPnl(a[0]);
            java.util.List<ExecRow> execRows = readExec(a[1]);
            FeeTable feeTable = readFee(a[2]);
            java.util.List<PnlRow> closed = close(pnlRows, execRows, feeTable);
            writePnl(a[3], closed);
            System.out.println("実現損益締め完了 件数=" + closed.size());
        } catch (Exception e) {
            System.err.println("実現損益締め異常 " + e.getMessage());
            System.exit(1);
        }
    }

    private static java.util.List<PnlRow> close(java.util.List<PnlRow> pnlRows,
                                                java.util.List<ExecRow> execRows,
                                                FeeTable feeTable) {
        java.util.Map<Key, Calc> calcByKey = new java.util.LinkedHashMap<>();

        for (ExecRow e : execRows) {
            validateExec(e);
            FeeRate fee = feeTable.rateAt(boardCode(e.instrCode), e.execTs);
            java.math.BigDecimal feeAmt = e.fillAmt
                    .multiply(fee.rate)
                    .setScale(0, java.math.RoundingMode.HALF_UP);
            if (feeAmt.compareTo(fee.minFeeAmt) < 0) {
                feeAmt = fee.minFeeAmt;
            }

            String cifNo = cifFromOrder(e.orderId);
            java.time.LocalDate sessDt = e.execTs.toLocalDate();
            Key key = new Key(cifNo, e.instrCode, sessDt);
            Calc calc = calcByKey.computeIfAbsent(key, k -> new Calc());

            if ("B".equals(e.sideKbn)) {
                calc.buyAmt = calc.buyAmt.add(e.fillAmt);
            } else {
                calc.sellAmt = calc.sellAmt.add(e.fillAmt);
            }
            calc.feeAmt = calc.feeAmt.add(feeAmt);
        }

        java.time.LocalDateTime calcTs = java.time.LocalDateTime.now().withNano(0);
        java.util.List<PnlRow> out = new java.util.ArrayList<>();
        java.util.Set<Key> used = new java.util.HashSet<>();

        for (PnlRow r : pnlRows) {
            Key key = new Key(r.cifNo, r.instrCode, r.sessDt);
            Calc calc = calcByKey.get(key);
            if (calc == null) {
                out.add(r.withCalcTs(calcTs));
                continue;
            }
            used.add(key);
            java.math.BigDecimal rlzd = calc.sellAmt.subtract(calc.buyAmt).subtract(calc.feeAmt);
            out.add(new PnlRow(r.cifNo, r.instrCode, r.sessDt, rlzd, r.unrlzdAmt, calc.feeAmt, calcTs));
        }

        for (java.util.Map.Entry<Key, Calc> e : calcByKey.entrySet()) {
            if (used.contains(e.getKey())) {
                continue;
            }
            Key key = e.getKey();
            Calc calc = e.getValue();
            java.math.BigDecimal rlzd = calc.sellAmt.subtract(calc.buyAmt).subtract(calc.feeAmt);
            out.add(new PnlRow(key.cifNo, key.instrCode, key.sessDt, rlzd, ZERO, calc.feeAmt, calcTs));
        }

        out.sort(java.util.Comparator
                .comparing((PnlRow r) -> r.cifNo)
                .thenComparing(r -> r.instrCode)
                .thenComparing(r -> r.sessDt));
        return out;
    }

    private static void validateExec(ExecRow e) {
        if (!"B".equals(e.sideKbn) && !"S".equals(e.sideKbn)) {
            throw new IllegalArgumentException("売買区分不正 EXEC-ID=" + e.execId);
        }
        if (e.fillQty.signum() <= 0 || e.fillAmt.signum() <= 0) {
            throw new IllegalArgumentException("約定数量金額不正 EXEC-ID=" + e.execId);
        }
        if (e.execTs == null) {
            throw new IllegalArgumentException("約定時刻不正 EXEC-ID=" + e.execId);
        }
    }

    private static java.util.List<PnlRow> readPnl(String path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(path));
        java.util.List<PnlRow> rows = new java.util.ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            if (skip(lines.get(i), i)) {
                continue;
            }
            String[] c = split(lines.get(i), 7, "SCPNLF", i);
            rows.add(new PnlRow(c[0], c[1], date(c[2]), dec(c[3]), dec(c[4]), dec(c[5]), ts(c[6])));
        }
        return rows;
    }

    private static java.util.List<ExecRow> readExec(String path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(path));
        java.util.List<ExecRow> rows = new java.util.ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            if (skip(lines.get(i), i)) {
                continue;
            }
            String[] c = split(lines.get(i), 7, "SCEXEC", i);
            rows.add(new ExecRow(c[0], c[1], c[2], c[3], dec(c[4]), dec(c[5]), ts(c[6])));
        }
        return rows;
    }

    private static FeeTable readFee(String path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(path));
        FeeTable table = new FeeTable();
        for (int i = 0; i < lines.size(); i++) {
            if (skip(lines.get(i), i)) {
                continue;
            }
            String[] c = split(lines.get(i), -1, "SCFEEF", i);
            if (c.length == 3) {
                table.add(c[0], java.time.LocalDateTime.MIN, dec(c[1]), dec(c[2]));
            } else if (c.length >= 4) {
                table.add(c[0], ts(c[3]), dec(c[1]), dec(c[2]));
            } else {
                throw new IllegalArgumentException("手数料列数不正 行=" + (i + 1));
            }
        }
        return table;
    }

    private static void writePnl(String path, java.util.List<PnlRow> rows) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("CIF-NO,INSTR-CODE,SESS-DT,RLZD-AMT,UNRLZD-AMT,FEE-AMT,CALC-TS");
        for (PnlRow r : rows) {
            out.add(String.join(",",
                    r.cifNo,
                    r.instrCode,
                    r.sessDt.format(DT_FMT),
                    money(r.rlzdAmt),
                    money(r.unrlzdAmt),
                    money(r.feeAmt),
                    r.calcTs.format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))));
        }
        java.nio.file.Files.write(java.nio.file.Paths.get(path), out);
    }

    private static boolean skip(String line, int index) {
        String s = line == null ? "" : line.trim();
        return s.isEmpty() || index == 0 && (s.startsWith("CIF-NO") || s.startsWith("EXEC-ID") || s.startsWith("BOARD-CODE"));
    }

    private static String[] split(String line, int expected, String file, int index) {
        java.util.List<String> values = new java.util.ArrayList<>();
        StringBuilder b = new StringBuilder();
        boolean q = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                q = !q;
            } else if (ch == ',' && !q) {
                values.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        values.add(b.toString().trim());
        String[] c = values.toArray(new String[0]);
        if (expected > 0 && c.length != expected) {
            throw new IllegalArgumentException(file + "列数不正 行=" + (index + 1));
        }
        return c;
    }

    private static java.math.BigDecimal dec(String s) {
        return new java.math.BigDecimal(s.replace(",", "").trim());
    }

    private static java.time.LocalDate date(String s) {
        return java.time.LocalDate.parse(s.trim(), DT_FMT);
    }

    private static java.time.LocalDateTime ts(String s) {
        String v = s.trim();
        if (v.indexOf('T') >= 0) {
            return java.time.LocalDateTime.parse(v);
        }
        return java.time.LocalDateTime.parse(v, TS_FMT);
    }

    private static String money(java.math.BigDecimal v) {
        return v.setScale(0, java.math.RoundingMode.HALF_UP).toPlainString();
    }

    private static String cifFromOrder(String orderId) {
        int p = orderId.indexOf('-');
        if (p > 0) {
            return orderId.substring(0, p);
        }
        if (orderId.length() >= 10) {
            return orderId.substring(0, 10);
        }
        return orderId;
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

    private static final class PnlRow {
        final String cifNo;
        final String instrCode;
        final java.time.LocalDate sessDt;
        final java.math.BigDecimal rlzdAmt;
        final java.math.BigDecimal unrlzdAmt;
        final java.math.BigDecimal feeAmt;
        final java.time.LocalDateTime calcTs;

        PnlRow(String cifNo, String instrCode, java.time.LocalDate sessDt,
               java.math.BigDecimal rlzdAmt, java.math.BigDecimal unrlzdAmt,
               java.math.BigDecimal feeAmt, java.time.LocalDateTime calcTs) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sessDt = sessDt;
            this.rlzdAmt = rlzdAmt;
            this.unrlzdAmt = unrlzdAmt;
            this.feeAmt = feeAmt;
            this.calcTs = calcTs;
        }

        PnlRow withCalcTs(java.time.LocalDateTime newCalcTs) {
            return new PnlRow(cifNo, instrCode, sessDt, rlzdAmt, unrlzdAmt, feeAmt, newCalcTs);
        }
    }

    private static final class ExecRow {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final java.math.BigDecimal fillQty;
        final java.math.BigDecimal fillAmt;
        final java.time.LocalDateTime execTs;

        ExecRow(String execId, String orderId, String instrCode, String sideKbn,
                java.math.BigDecimal fillQty, java.math.BigDecimal fillAmt, java.time.LocalDateTime execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class FeeRate {
        final java.time.LocalDateTime fromTs;
        final java.math.BigDecimal rate;
        final java.math.BigDecimal minFeeAmt;

        FeeRate(java.time.LocalDateTime fromTs, java.math.BigDecimal rate, java.math.BigDecimal minFeeAmt) {
            this.fromTs = fromTs;
            this.rate = rate;
            this.minFeeAmt = minFeeAmt;
        }
    }

    private static final class FeeTable {
        final java.util.Map<String, java.util.List<FeeRate>> rates = new java.util.HashMap<>();

        void add(String boardCode, java.time.LocalDateTime fromTs,
                 java.math.BigDecimal rate, java.math.BigDecimal minFeeAmt) {
            if (!"T1".equals(boardCode) && !"ST".equals(boardCode) && !"ETF".equals(boardCode)) {
                throw new IllegalArgumentException("市場コード不正 BOARD-CODE=" + boardCode);
            }
            rates.computeIfAbsent(boardCode, k -> new java.util.ArrayList<>())
                    .add(new FeeRate(fromTs, rate, minFeeAmt));
            rates.get(boardCode).sort(java.util.Comparator.comparing(r -> r.fromTs));
        }

        FeeRate rateAt(String boardCode, java.time.LocalDateTime execTs) {
            java.util.List<FeeRate> list = rates.get(boardCode);
            if (list == null || list.isEmpty()) {
                throw new IllegalArgumentException("手数料率未登録 BOARD-CODE=" + boardCode);
            }
            FeeRate selected = null;
            for (FeeRate r : list) {
                if (!r.fromTs.isAfter(execTs)) {
                    selected = r;
                }
            }
            if (selected == null) {
                throw new IllegalArgumentException("手数料率適用不能 BOARD-CODE=" + boardCode);
            }
            return selected;
        }
    }

    private static final class Key {
        final String cifNo;
        final String instrCode;
        final java.time.LocalDate sessDt;

        Key(String cifNo, String instrCode, java.time.LocalDate sessDt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sessDt = sessDt;
        }

        public boolean equals(Object o) {
            if (!(o instanceof Key)) {
                return false;
            }
            Key k = (Key) o;
            return cifNo.equals(k.cifNo) && instrCode.equals(k.instrCode) && sessDt.equals(k.sessDt);
        }

        public int hashCode() {
            return java.util.Objects.hash(cifNo, instrCode, sessDt);
        }
    }

    private static final class Calc {
        java.math.BigDecimal buyAmt = ZERO;
        java.math.BigDecimal sellAmt = ZERO;
        java.math.BigDecimal feeAmt = ZERO;
    }
}
