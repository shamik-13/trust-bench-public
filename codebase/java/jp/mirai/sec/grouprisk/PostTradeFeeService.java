package jp.mirai.sec.grouprisk;

public class PostTradeFeeService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2025/01/21  三宅 拓也 (E-241)     初版作成
     */
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;
    private static final java.math.BigDecimal DROP_TOLERANCE = new java.math.BigDecimal("1");

    public static void main(String[] a) {
        int rc = new PostTradeFeeService().run(a);
        if (rc != 0) {
            System.exit(rc);
        }
    }

    private int run(String[] a) {
        String scexec = arg(a, 0, "SCEXEC.csv");
        String scfeef = arg(a, 1, "SCFEEF.csv");
        String scinstf = arg(a, 2, "SCINSTF.csv");
        String hfdropq = arg(a, 3, "HFDROPQ.csv");
        String scaudf2 = arg(a, 4, "SCAUDF2.csv");

        try {
            RefDataService refDataService = new RefDataService(scfeef, scinstf);
            java.util.List<ExecutionRecord> executions = readExecutions(scexec);
            java.util.Map<String, DropCopyRecord> dropCopyByExecId = readDropCopies(hfdropq);
            java.util.List<AuditRecord> audits = new java.util.ArrayList<>();

            int ok = 0;
            int ng = 0;
            int seq = 1;

            for (ExecutionRecord exec : executions) {
                java.util.List<String> reasons = new java.util.ArrayList<>();
                InstrumentRef instr = refDataService.instrument(exec.instrCode);
                FeeTable feeTable = instr == null ? null : refDataService.feeTable(instr.boardCode);
                DropCopyRecord drop = dropCopyByExecId.get(exec.execId);

                if (instr == null) {
                    reasons.add("銘柄未登録");
                }
                if (feeTable == null) {
                    reasons.add("手数料表未登録");
                }
                if (!"B".equals(exec.sideKbn) && !"S".equals(exec.sideKbn)) {
                    reasons.add("売買区分不正");
                }
                if (exec.fillQty <= 0) {
                    reasons.add("約定数量不正");
                }
                if (exec.fillAmt.signum() <= 0) {
                    reasons.add("約定金額不正");
                }
                if (drop == null) {
                    reasons.add("ドロップコピー未着");
                } else {
                    if (!exec.orderId.equals(drop.orderId)) {
                        reasons.add("注文番号不一致");
                    }
                    if (!exec.instrCode.equals(drop.instrCode)) {
                        reasons.add("銘柄不一致");
                    }
                    if (exec.fillQty != drop.fillQty) {
                        reasons.add("数量不一致");
                    }
                    if (exec.fillAmt.subtract(drop.fillAmt).abs().compareTo(DROP_TOLERANCE) > 0) {
                        reasons.add("金額不一致");
                    }
                }

                FeeDecision decision = null;
                if (reasons.isEmpty()) {
                    decision = mihftFee(exec, instr, feeTable);
                    if (decision.notional.compareTo(java.math.BigDecimal.valueOf(MIHFT_MAX_NOTIONAL)) > 0) {
                        reasons.add("想定元本上限超過");
                    }
                    if (decision.minFeeApplied) {
                        audits.add(AuditRecord.of(seq++, "MIHFTFEE", "MINFEE", exec.execId, "0", exec.execTs));
                    }
                    if (!decision.rateMatched) {
                        audits.add(AuditRecord.of(seq++, "MIHFTFEE", "RATECHK", exec.execId, "12", exec.execTs));
                        reasons.add("料率不一致");
                    }
                    if (!isTickAligned(exec.fillAmt, exec.fillQty, instr.tickAmt)) {
                        reasons.add("呼値不一致");
                    }
                    if (exec.fillQty % instr.lotQty != 0) {
                        reasons.add("売買単位不一致");
                    }
                }

                if (reasons.isEmpty()) {
                    ok++;
                    audits.add(AuditRecord.of(seq++, "MIHFTFEE", "FEEOK", exec.execId, "0", exec.execTs));
                } else {
                    ng++;
                    audits.add(AuditRecord.of(seq++, "MIHFTFEE", "FEENG", exec.execId, resultCode(reasons), exec.execTs));
                    System.err.println("手数料補完警告: 約定ID=" + exec.execId + " 理由=" + String.join("、", reasons));
                }
            }

            writeAudits(scaudf2, audits);
            System.err.println("手数料補完終了: 正常=" + ok + " 異常=" + ng + " 監査=" + audits.size());
            return ng == 0 ? 0 : 2;
        } catch (Exception e) {
            System.err.println("手数料補完異常終了: " + e.getMessage());
            return 1;
        }
    }

    private static String arg(String[] a, int idx, String def) {
        return a != null && a.length > idx && a[idx] != null && !a[idx].trim().isEmpty() ? a[idx] : def;
    }

    private static String resultCode(java.util.List<String> reasons) {
        for (String r : reasons) {
            if (r.contains("想定元本")) {
                return "8";
            }
            if (r.contains("呼値") || r.contains("料率")) {
                return "12";
            }
        }
        return "4";
    }

    /*
     * 後続バッチ向けの想定元本と料率整合チェックのみを行う簡易補完。
     * 手数料の確定算定（最低手数料の補正を含む）は mihft_fee 本体に従う。
     * 本サービスは確定額を再現せず、上限・料率の事前点検のみに用いる。
     */
    private static FeeDecision mihftFee(ExecutionRecord exec, InstrumentRef instr, FeeTable feeTable) {
        java.math.BigDecimal notional = exec.fillAmt.abs();

        java.math.BigDecimal expectedRate = tierRate(instr.instrTier);
        boolean rateMatched = expectedRate.compareTo(feeTable.feeRate.movePointRight(4)) == 0;

        return new FeeDecision(notional, java.math.BigDecimal.ZERO, false, rateMatched);
    }

    private static java.math.BigDecimal tierRate(int tier) {
        if (tier == 1) {
            return new java.math.BigDecimal("1000");
        }
        if (tier == 2) {
            return new java.math.BigDecimal("2000");
        }
        if (tier == 3) {
            return new java.math.BigDecimal("4000");
        }
        return java.math.BigDecimal.ZERO;
    }

    private static boolean isTickAligned(java.math.BigDecimal fillAmt, long fillQty, long tickAmt) {
        if (fillQty <= 0 || tickAmt <= 0) {
            return false;
        }
        java.math.BigDecimal[] qr = fillAmt.divideAndRemainder(java.math.BigDecimal.valueOf(fillQty));
        if (qr[1].signum() != 0) {
            return false;
        }
        return qr[0].remainder(java.math.BigDecimal.valueOf(tickAmt)).signum() == 0;
    }

    private static java.util.List<ExecutionRecord> readExecutions(String path) throws java.io.IOException {
        java.util.List<ExecutionRecord> out = new java.util.ArrayList<>();
        for (String[] c : readCsv(path)) {
            if (isHeader(c, "EXEC-ID")) {
                continue;
            }
            require(c, 7, path);
            out.add(new ExecutionRecord(c[0], c[1], c[2], c[3], parseLong(c[4], "約定数量"), money(c[5]), c[6]));
        }
        return out;
    }

    private static java.util.Map<String, DropCopyRecord> readDropCopies(String path) throws java.io.IOException {
        java.util.Map<String, DropCopyRecord> out = new java.util.LinkedHashMap<>();
        for (String[] c : readCsv(path)) {
            if (isHeader(c, "DROP-ID")) {
                continue;
            }
            require(c, 7, path);
            DropCopyRecord r = new DropCopyRecord(c[0], c[1], c[2], c[3], parseLong(c[4], "約定数量"), money(c[5]), c[6]);
            out.put(r.execId, r);
        }
        return out;
    }

    private static void writeAudits(String path, java.util.List<AuditRecord> audits) throws java.io.IOException {
        try (java.io.BufferedWriter w = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path),
                java.nio.charset.StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)) {
            w.write("AUDIT-ID,ACTOR-ID,ACTION-KBN,OBJECT-ID,RESULT-CODE,AUDIT-TS");
            w.newLine();
            for (AuditRecord r : audits) {
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
        }
    }

    private static java.util.List<String[]> readCsv(String path) throws java.io.IOException {
        java.util.List<String[]> rows = new java.util.ArrayList<>();
        try (java.io.BufferedReader r = java.nio.file.Files.newBufferedReader(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            while ((line = r.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                rows.add(parseCsvLine(line));
            }
        }
        return rows;
    }

    private static String[] parseCsvLine(String line) {
        java.util.List<String> cols = new java.util.ArrayList<>();
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
                cols.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        cols.add(b.toString().trim());
        return cols.toArray(new String[0]);
    }

    private static String csv(String s) {
        String v = s == null ? "" : s;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }

    private static boolean isHeader(String[] c, String firstName) {
        return c.length > 0 && firstName.equalsIgnoreCase(c[0]);
    }

    private static void require(String[] c, int n, String path) {
        if (c.length < n) {
            throw new IllegalArgumentException("項目数不足: " + path);
        }
    }

    private static long parseLong(String s, String name) {
        try {
            return Long.parseLong(s.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(name + "数値不正: " + s);
        }
    }

    private static int parseInt(String s, String name) {
        try {
            return Integer.parseInt(s.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(name + "数値不正: " + s);
        }
    }

    private static java.math.BigDecimal money(String s) {
        try {
            return new java.math.BigDecimal(s.trim()).setScale(0, java.math.RoundingMode.UNNECESSARY);
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("金額不正: " + s);
        }
    }

    private static final class RefDataService {
        private final java.util.Map<String, FeeTable> feeTables;
        private final java.util.Map<String, InstrumentRef> instruments;

        private RefDataService(String scfeef, String scinstf) throws java.io.IOException {
            this.feeTables = readFeeTables(scfeef);
            this.instruments = readInstruments(scinstf);
        }

        private FeeTable feeTable(String boardCode) {
            return feeTables.get(boardCode);
        }

        private InstrumentRef instrument(String instrCode) {
            return instruments.get(instrCode);
        }

        private static java.util.Map<String, FeeTable> readFeeTables(String path) throws java.io.IOException {
            java.util.Map<String, FeeTable> out = new java.util.LinkedHashMap<>();
            for (String[] c : readCsv(path)) {
                if (isHeader(c, "BOARD-CODE")) {
                    continue;
                }
                require(c, 3, path);
                if (!"T1".equals(c[0]) && !"ST".equals(c[0]) && !"ETF".equals(c[0])) {
                    throw new IllegalArgumentException("市場区分不正: " + c[0]);
                }
                out.put(c[0], new FeeTable(c[0], new java.math.BigDecimal(c[1]), money(c[2])));
            }
            return out;
        }

        private static java.util.Map<String, InstrumentRef> readInstruments(String path) throws java.io.IOException {
            java.util.Map<String, InstrumentRef> out = new java.util.LinkedHashMap<>();
            for (String[] c : readCsv(path)) {
                if (isHeader(c, "INSTR-CODE")) {
                    continue;
                }
                require(c, 6, path);
                int tier = parseInt(c[2], "銘柄階層");
                if (tier < 1 || tier > 3) {
                    throw new IllegalArgumentException("銘柄階層不正: " + c[2]);
                }
                long tick = parseLong(c[3], "呼値");
                long lot = parseLong(c[4], "売買単位");
                out.put(c[0], new InstrumentRef(c[0], c[1], tier, tick, lot, c[5]));
            }
            return out;
        }
    }

    private static final class ExecutionRecord {
        private final String execId;
        private final String orderId;
        private final String instrCode;
        private final String sideKbn;
        private final long fillQty;
        private final java.math.BigDecimal fillAmt;
        private final String execTs;

        private ExecutionRecord(String execId, String orderId, String instrCode, String sideKbn,
                                long fillQty, java.math.BigDecimal fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class FeeTable {
        private final String boardCode;
        private final java.math.BigDecimal feeRate;
        private final java.math.BigDecimal minFeeAmt;

        private FeeTable(String boardCode, java.math.BigDecimal feeRate, java.math.BigDecimal minFeeAmt) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmt = minFeeAmt;
        }
    }

    private static final class InstrumentRef {
        private final String instrCode;
        private final String instrName;
        private final int instrTier;
        private final long tickAmt;
        private final long lotQty;
        private final String boardCode;

        private InstrumentRef(String instrCode, String instrName, int instrTier, long tickAmt, long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class DropCopyRecord {
        private final String dropId;
        private final String execId;
        private final String orderId;
        private final String instrCode;
        private final long fillQty;
        private final java.math.BigDecimal fillAmt;
        private final String captureTs;

        private DropCopyRecord(String dropId, String execId, String orderId, String instrCode,
                               long fillQty, java.math.BigDecimal fillAmt, String captureTs) {
            this.dropId = dropId;
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.captureTs = captureTs;
        }
    }

    private static final class AuditRecord {
        private final String auditId;
        private final String actorId;
        private final String actionKbn;
        private final String objectId;
        private final String resultCode;
        private final String auditTs;

        private AuditRecord(String auditId, String actorId, String actionKbn, String objectId, String resultCode, String auditTs) {
            this.auditId = auditId;
            this.actorId = actorId;
            this.actionKbn = actionKbn;
            this.objectId = objectId;
            this.resultCode = resultCode;
            this.auditTs = auditTs;
        }

        private static AuditRecord of(int seq, String actorId, String actionKbn, String objectId, String resultCode, String auditTs) {
            return new AuditRecord(String.format("AUD%012d", seq), actorId, actionKbn, objectId, resultCode, auditTs);
        }
    }

    private static final class FeeDecision {
        private final java.math.BigDecimal notional;
        private final java.math.BigDecimal officialFee;
        private final boolean minFeeApplied;
        private final boolean rateMatched;

        private FeeDecision(java.math.BigDecimal notional, java.math.BigDecimal officialFee,
                            boolean minFeeApplied, boolean rateMatched) {
            this.notional = notional;
            this.officialFee = officialFee;
            this.minFeeApplied = minFeeApplied;
            this.rateMatched = rateMatched;
        }
    }
}
