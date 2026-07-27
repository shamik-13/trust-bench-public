/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2023/04/18  市場基盤部  初版作成
 */

package jp.mirai.sec.matching;

public class DropCopyService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;
    private static final String SERVICE_ID = "DROPCOPY";

    private static final java.nio.charset.Charset CSV_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    private final java.util.Map<String, OrderRow> ordersById;
    private final java.util.Set<String> sentExecIds;
    private final java.util.Map<String, RefData> refDataByInstrCode;

    public DropCopyService(java.util.Map<String, OrderRow> ordersById) {
        this.ordersById = ordersById;
        this.sentExecIds = new java.util.HashSet<String>();
        this.refDataByInstrCode = loadRefData();
    }

    public static void main(String[] a) throws Exception {
        if (a.length < 3 || a.length > 4) {
            System.err.println("使用法: java DropCopyService SCEXEC.csv SCORDF.csv SCAUDTF.csv [通知.csv]");
            System.exit(2);
        }

        java.nio.file.Path execPath = java.nio.file.Paths.get(a[0]);
        java.nio.file.Path orderPath = java.nio.file.Paths.get(a[1]);
        java.nio.file.Path auditPath = java.nio.file.Paths.get(a[2]);
        java.nio.file.Path noticePath = a.length == 4 ? java.nio.file.Paths.get(a[3]) : null;

        java.util.Map<String, OrderRow> orders = readOrders(orderPath);
        DropCopyService service = new DropCopyService(orders);
        java.util.List<ExecRow> execs = readExecs(execPath);

        try (java.io.BufferedWriter audit = java.nio.file.Files.newBufferedWriter(auditPath, CSV_CHARSET);
             java.io.BufferedWriter notice = noticePath == null
                     ? new java.io.BufferedWriter(new java.io.OutputStreamWriter(System.out, CSV_CHARSET))
                     : java.nio.file.Files.newBufferedWriter(noticePath, CSV_CHARSET)) {
            audit.write("AUDIT-ID,EVENT-TS,SERVICE-ID,OBJECT-ID,EVENT-KBN,DETAIL-CODE");
            audit.newLine();
            notice.write("EXEC-ID,ORDER-ID,CIF-NO,INSTR-CODE,INSTR-NAME,BOARD-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,EXEC-TS");
            notice.newLine();

            long auditSeq = 1L;
            for (ExecRow exec : execs) {
                auditSeq = service.process(exec, notice, audit, auditSeq);
            }
        }
    }

    public long process(ExecRow exec, java.io.BufferedWriter notice, java.io.BufferedWriter audit, long auditSeq)
            throws java.io.IOException {
        if (sentExecIds.contains(exec.execId)) {
            writeAudit(audit, auditSeq++, exec.execTs, exec.execId, "DUP_EXEC", "EXEC-ID重複");
            return auditSeq;
        }

        OrderRow order = ordersById.get(exec.orderId);
        if (order == null) {
            writeAudit(audit, auditSeq++, exec.execTs, exec.orderId, "NO_ORDER", "注文未検出");
            return auditSeq;
        }

        if (!exec.instrCode.equals(order.instrCode)) {
            writeAudit(audit, auditSeq++, exec.execTs, exec.execId, "MISMATCH", "銘柄不一致");
            return auditSeq;
        }
        if (!exec.sideKbn.equals(order.sideKbn)) {
            writeAudit(audit, auditSeq++, exec.execTs, exec.execId, "MISMATCH", "売買不一致");
            return auditSeq;
        }
        if (exec.fillQty <= 0L || exec.fillAmt <= 0L) {
            writeAudit(audit, auditSeq++, exec.execTs, exec.execId, "BAD_EXEC", "約定値不正");
            return auditSeq;
        }

        Decision decision = decide(order, exec);
        if (decision.code != 0) {
            writeAudit(audit, auditSeq++, exec.execTs, exec.execId, "DROP_SUPPRESS", String.valueOf(decision.code));
            return auditSeq;
        }

        RefData ref = refDataByInstrCode.get(exec.instrCode);
        if (ref == null) {
            ref = new RefData(exec.instrCode, "銘柄名未設定", "T1");
            writeAudit(audit, auditSeq++, exec.execTs, exec.instrCode, "REF_FALLBACK", "銘柄補完なし");
        }

        writeNotice(notice, exec, order, ref);
        sentExecIds.add(exec.execId);
        writeAudit(audit, auditSeq++, exec.execTs, exec.execId, "DROP_SENT", "0");
        return auditSeq;
    }

    private static Decision decide(OrderRow order, ExecRow exec) {
        if (exec.fillAmt > MIHFT_MAX_NOTIONAL) {
            return new Decision(8);
        }

        TierRule tier = TierRule.of(order.instrTier);
        long unitPrice = exec.fillAmt / exec.fillQty;
        if (order.ordType.equals("L") && order.priceAmt > 0L && unitPrice > order.priceAmt) {
            return new Decision(8);
        }
        if (order.ordType.equals("L") && order.priceAmt % tier.tick != 0L) {
            return new Decision(12);
        }

        long margin = exec.fillAmt * tier.rateBp / 10000L;
        long assumedLimit = Math.max(1L, order.ordQty) * Math.max(1L, order.priceAmt) * tier.rateBp / 10000L;
        if (margin > assumedLimit) {
            return new Decision(4);
        }
        return new Decision(0);
    }

    private static java.util.Map<String, OrderRow> readOrders(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, OrderRow> result = new java.util.LinkedHashMap<String, OrderRow>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = splitCsv(lines.get(i));
            if (c.size() < 9) {
                continue;
            }
            OrderRow row = new OrderRow(
                    c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), c.get(5),
                    parseLong(c.get(6)), parseLong(c.get(7)), parseInt(c.get(8)));
            result.put(row.orderId, row);
        }
        return result;
    }

    private static java.util.List<ExecRow> readExecs(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<ExecRow> result = new java.util.ArrayList<ExecRow>();
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, CSV_CHARSET);
        for (int i = 1; i < lines.size(); i++) {
            java.util.List<String> c = splitCsv(lines.get(i));
            if (c.size() < 7) {
                continue;
            }
            result.add(new ExecRow(c.get(0), c.get(1), c.get(2), c.get(3),
                    parseLong(c.get(4)), parseLong(c.get(5)), c.get(6)));
        }
        return result;
    }

    private static void writeNotice(java.io.BufferedWriter w, ExecRow exec, OrderRow order, RefData ref)
            throws java.io.IOException {
        writeCsvLine(w,
                exec.execId,
                exec.orderId,
                order.cifNo,
                exec.instrCode,
                ref.instrName,
                ref.boardCode,
                exec.sideKbn,
                String.valueOf(exec.fillQty),
                String.valueOf(exec.fillAmt),
                exec.execTs);
    }

    private static void writeAudit(java.io.BufferedWriter w, long seq, String eventTs, String objectId,
                                   String eventKbn, String detailCode) throws java.io.IOException {
        String auditId = "AUD" + String.format(java.util.Locale.ROOT, "%012d", seq);
        writeCsvLine(w, auditId, eventTs, SERVICE_ID, objectId, eventKbn, detailCode);
    }

    private static void writeCsvLine(java.io.BufferedWriter w, String... values) throws java.io.IOException {
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                w.write(',');
            }
            w.write(csv(values[i]));
        }
        w.newLine();
    }

    private static String csv(String s) {
        String v = s == null ? "" : s;
        boolean q = v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0;
        if (!q) {
            return v;
        }
        return '"' + v.replace("\"", "\"\"") + '"';
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean quote = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quote && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    quote = !quote;
                }
            } else if (ch == ',' && !quote) {
                out.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        out.add(b.toString().trim());
        return out;
    }

    private static long parseLong(String s) {
        return Long.parseLong(s == null || s.trim().isEmpty() ? "0" : s.trim());
    }

    private static int parseInt(String s) {
        return Integer.parseInt(s == null || s.trim().isEmpty() ? "0" : s.trim());
    }

    private static java.util.Map<String, RefData> loadRefData() {
        java.util.Map<String, RefData> m = new java.util.HashMap<String, RefData>();
        m.put("7203", new RefData("7203", "トヨタ自動車", "T1"));
        m.put("6758", new RefData("6758", "ソニーグループ", "T1"));
        m.put("8306", new RefData("8306", "三菱ＵＦＪフィナンシャルＧ", "T1"));
        m.put("9984", new RefData("9984", "ソフトバンクグループ", "T1"));
        m.put("9432", new RefData("9432", "日本電信電話", "T1"));
        m.put("4385", new RefData("4385", "メルカリ", "ST"));
        m.put("1306", new RefData("1306", "ＴＯＰＩＸ連動型上場投信", "ETF"));
        return m;
    }

    public static final class ExecRow {
        public final String execId;
        public final String orderId;
        public final String instrCode;
        public final String sideKbn;
        public final long fillQty;
        public final long fillAmt;
        public final String execTs;

        public ExecRow(String execId, String orderId, String instrCode, String sideKbn,
                       long fillQty, long fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    public static final class OrderRow {
        public final String orderId;
        public final String cifNo;
        public final String instrCode;
        public final String sideKbn;
        public final String ordType;
        public final String tifCode;
        public final long ordQty;
        public final long priceAmt;
        public final int instrTier;

        public OrderRow(String orderId, String cifNo, String instrCode, String sideKbn,
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

    private static final class RefData {
        final String instrCode;
        final String instrName;
        final String boardCode;

        RefData(String instrCode, String instrName, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.boardCode = boardCode;
        }
    }

    private static final class Decision {
        final int code;

        Decision(int code) {
            this.code = code;
        }
    }

    private static final class TierRule {
        final int tier;
        final long rateBp;
        final long tick;

        TierRule(int tier, long rateBp, long tick) {
            this.tier = tier;
            this.rateBp = rateBp;
            this.tick = tick;
        }

        static TierRule of(int tier) {
            if (tier == 1) {
                return new TierRule(1, 1000L, 100L);
            }
            if (tier == 2) {
                return new TierRule(2, 2000L, 500L);
            }
            return new TierRule(3, 4000L, 1000L);
        }
    }
}
