package jp.mirai.sec.grouprisk;

public class ReconciliationBreakService {
    /**
     * 変更履歴
     * 版数  年月日      担当        概要
     * 1.00  2022-02-22  村上 健司 (E-301)    初版作成
     */

    private static final long MIHFT_MAX_NOTIONAL = 500000000L;
    private static final long QTY_TOLERANCE = 0L;
    private static final long AMT_TOLERANCE = 1L;
    private static final String ACTOR_ID = "RECONCTL";

    public static void main(String[] a) throws Exception {
        String scexec = a.length > 0 ? a[0] : "SCEXEC.csv";
        String scposf = a.length > 1 ? a[1] : "SCPOSF.csv";
        String hfdropq = a.length > 2 ? a[2] : "HFDROPQ.csv";
        String hfdeclog = a.length > 3 ? a[3] : "HFDECLOG.csv";
        String scalrtf = a.length > 4 ? a[4] : "SCALRTF.csv";
        String scaudf2 = a.length > 5 ? a[5] : "SCAUDF2.csv";

        ReconciliationBreakService svc = new ReconciliationBreakService();
        svc.reconcile(scexec, scposf, hfdropq, hfdeclog, scalrtf, scaudf2);
    }

    public void reconcile(String scexecPath,
                          String scposfPath,
                          String hfdropqPath,
                          String hfdeclogPath,
                          String scalrtfPath,
                          String scaudf2Path) throws java.io.IOException {
        java.util.Map<String, ExecRecord> execById = readExec(scexecPath);
        java.util.Map<String, PositionRecord> posByInstr = readPosition(scposfPath);
        java.util.List<DropRecord> drops = readDrop(hfdropqPath);
        java.util.Map<String, DecisionRecord> decisionByOrder = readDecision(hfdeclogPath);

        java.util.Map<String, ExecAgg> execAggByInstr = new java.util.TreeMap<String, ExecAgg>();
        for (ExecRecord e : execById.values()) {
            InstrAttr attr = refData(e.instrCode);
            validateExec(e, attr);
            ExecAgg agg = execAggByInstr.get(e.instrCode);
            if (agg == null) {
                agg = new ExecAgg();
                execAggByInstr.put(e.instrCode, agg);
            }
            long sign = "S".equals(e.sideKbn) ? -1L : 1L;
            agg.netQty += sign * e.fillQty;
            agg.grossAmt += e.fillAmt;
            agg.fillQty += e.fillQty;
        }

        java.util.List<AlertRecord> alerts = new java.util.ArrayList<AlertRecord>();
        java.util.List<AuditRecord> audits = new java.util.ArrayList<AuditRecord>();
        long now = System.currentTimeMillis();

        java.util.Set<String> instrCodes = new java.util.TreeSet<String>();
        instrCodes.addAll(execAggByInstr.keySet());
        instrCodes.addAll(posByInstr.keySet());

        for (String instr : instrCodes) {
            ExecAgg agg = execAggByInstr.get(instr);
            PositionRecord pos = posByInstr.get(instr);
            long execQty = agg == null ? 0L : agg.netQty;
            long posQty = pos == null ? 0L : pos.netQty;
            long qtyDiff = execQty - posQty;

            if (java.lang.Math.abs(qtyDiff) > QTY_TOLERANCE) {
                String sev = severityByQty(instr, java.lang.Math.abs(qtyDiff));
                alerts.add(new AlertRecord(nextId("AL", alerts.size()), "QTY", sev, instr, "QTYDIFF", now));
                audits.add(new AuditRecord(nextId("AU", audits.size()), ACTOR_ID, "CLASSIFY", instr, "QTYDIFF", now));
            }

            if (agg != null && pos != null && agg.fillQty != 0L && pos.netQty != 0L) {
                long execAvg = agg.grossAmt / agg.fillQty;
                long posAvg = pos.avgAmt;
                long diff = java.lang.Math.abs(execAvg - posAvg);
                InstrAttr attr = refData(instr);
                if (diff > java.lang.Math.max(AMT_TOLERANCE, attr.tick)) {
                    alerts.add(new AlertRecord(nextId("AL", alerts.size()), "PRICE", "2", instr, "AVGDIFF", now));
                    audits.add(new AuditRecord(nextId("AU", audits.size()), ACTOR_ID, "CLASSIFY", instr, "AVGDIFF", now));
                }
            }
        }

        for (DropRecord d : drops) {
            if (!execById.containsKey(d.execId)) {
                DecisionRecord dec = decisionByOrder.get(d.orderId);
                String detail = dec == null ? "DROPNODEC" : "DROPDEC" + dec.actionCode;
                String sev = dec != null && dec.actionCode != 0 ? "1" : "3";
                alerts.add(new AlertRecord(nextId("AL", alerts.size()), "MISSING", sev, d.execId, detail, now));
                audits.add(new AuditRecord(nextId("AU", audits.size()), ACTOR_ID, "TRACE", d.execId, detail, now));
            }
        }

        writeAlerts(scalrtfPath, alerts);
        writeAudits(scaudf2Path, audits);
    }

    private static void validateExec(ExecRecord e, InstrAttr attr) {
        if (!"B".equals(e.sideKbn) && !"S".equals(e.sideKbn)) {
            throw new IllegalArgumentException("売買区分不正:" + e.execId);
        }
        if (e.fillQty <= 0L || e.fillAmt <= 0L) {
            throw new IllegalArgumentException("約定数量金額不正:" + e.execId);
        }
        if (e.fillAmt > MIHFT_MAX_NOTIONAL) {
            throw new IllegalArgumentException("想定元本上限超過:" + e.execId);
        }
        long unitPrice = e.fillAmt / e.fillQty;
        if (unitPrice % attr.tick != 0L) {
            throw new IllegalArgumentException("呼値不正:" + e.execId);
        }
    }

    private static String severityByQty(String instr, long qtyDiff) {
        InstrAttr attr = refData(instr);
        if (attr.tier == 3 || qtyDiff >= 10000L) {
            return "1";
        }
        if (attr.tier == 2 || qtyDiff >= 1000L) {
            return "2";
        }
        return "3";
    }

    private static InstrAttr refData(String instrCode) {
        int h = java.lang.Math.abs(instrCode == null ? 0 : instrCode.hashCode());
        int tier = h % 3 + 1;
        if (tier == 1) {
            return new InstrAttr(1, 1000, 100);
        }
        if (tier == 2) {
            return new InstrAttr(2, 2000, 500);
        }
        return new InstrAttr(3, 4000, 1000);
    }

    private static java.util.Map<String, ExecRecord> readExec(String path) throws java.io.IOException {
        java.util.Map<String, ExecRecord> out = new java.util.LinkedHashMap<String, ExecRecord>();
        for (String[] r : readCsv(path)) {
            if (isHeader(r, "EXEC-ID")) {
                continue;
            }
            out.put(r[0], new ExecRecord(r[0], r[1], r[2], r[3], parseLong(r[4]), parseLong(r[5]), r[6]));
        }
        return out;
    }

    private static java.util.Map<String, PositionRecord> readPosition(String path) throws java.io.IOException {
        java.util.Map<String, PositionRecord> out = new java.util.TreeMap<String, PositionRecord>();
        for (String[] r : readCsv(path)) {
            if (isHeader(r, "CIF-NO")) {
                continue;
            }
            PositionRecord p = new PositionRecord(r[0], r[1], parseLong(r[2]), parseLong(r[3]), parseLong(r[4]));
            PositionRecord cur = out.get(p.instrCode);
            if (cur == null) {
                out.put(p.instrCode, p);
            } else {
                long qty = cur.netQty + p.netQty;
                long avg = qty == 0L ? 0L : ((cur.avgAmt * cur.netQty) + (p.avgAmt * p.netQty)) / qty;
                out.put(p.instrCode, new PositionRecord(cur.cifNo, p.instrCode, qty, avg, cur.rlzdAmt + p.rlzdAmt));
            }
        }
        return out;
    }

    private static java.util.List<DropRecord> readDrop(String path) throws java.io.IOException {
        java.util.List<DropRecord> out = new java.util.ArrayList<DropRecord>();
        for (String[] r : readCsv(path)) {
            if (isHeader(r, "DROP-ID")) {
                continue;
            }
            out.add(new DropRecord(r[0], r[1], r[2], r[3], parseLong(r[4]), parseLong(r[5]), r[6]));
        }
        return out;
    }

    private static java.util.Map<String, DecisionRecord> readDecision(String path) throws java.io.IOException {
        java.util.Map<String, DecisionRecord> out = new java.util.HashMap<String, DecisionRecord>();
        for (String[] r : readCsv(path)) {
            if (isHeader(r, "DECISION-ID")) {
                continue;
            }
            int actionCode = parseInt(r[3]);
            if (actionCode != 0 && actionCode != 4 && actionCode != 8 && actionCode != 12) {
                throw new IllegalArgumentException("判定コード不正:" + r[0]);
            }
            out.put(r[1], new DecisionRecord(r[0], r[1], r[2], actionCode, r[4], r[5]));
        }
        return out;
    }

    private static java.util.List<String[]> readCsv(String path) throws java.io.IOException {
        java.util.List<String[]> rows = new java.util.ArrayList<String[]>();
        java.io.File f = new java.io.File(path);
        if (!f.exists()) {
            return rows;
        }
        java.io.BufferedReader br = new java.io.BufferedReader(new java.io.InputStreamReader(new java.io.FileInputStream(f), java.nio.charset.StandardCharsets.UTF_8));
        try {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.trim().length() == 0) {
                    continue;
                }
                rows.add(splitCsv(line));
            }
        } finally {
            br.close();
        }
        return rows;
    }

    private static String[] splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (c == ',' && !quoted) {
                out.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(c);
            }
        }
        out.add(b.toString().trim());
        return out.toArray(new String[out.size()]);
    }

    private static void writeAlerts(String path, java.util.List<AlertRecord> alerts) throws java.io.IOException {
        java.io.BufferedWriter bw = new java.io.BufferedWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(path), java.nio.charset.StandardCharsets.UTF_8));
        try {
            bw.write("ALERT-ID,ALERT-KBN,SEVERITY-CODE,SUBJECT-ID,DETAIL-CODE,RAISED-TS");
            bw.newLine();
            for (AlertRecord a : alerts) {
                bw.write(csv(a.alertId) + "," + csv(a.alertKbn) + "," + csv(a.severityCode) + "," + csv(a.subjectId) + "," + csv(a.detailCode) + "," + a.raisedTs);
                bw.newLine();
            }
        } finally {
            bw.close();
        }
    }

    private static void writeAudits(String path, java.util.List<AuditRecord> audits) throws java.io.IOException {
        java.io.BufferedWriter bw = new java.io.BufferedWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(path), java.nio.charset.StandardCharsets.UTF_8));
        try {
            bw.write("AUDIT-ID,ACTOR-ID,ACTION-KBN,OBJECT-ID,RESULT-CODE,AUDIT-TS");
            bw.newLine();
            for (AuditRecord a : audits) {
                bw.write(csv(a.auditId) + "," + csv(a.actorId) + "," + csv(a.actionKbn) + "," + csv(a.objectId) + "," + csv(a.resultCode) + "," + a.auditTs);
                bw.newLine();
            }
        } finally {
            bw.close();
        }
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

    private static boolean isHeader(String[] r, String first) {
        return r.length > 0 && first.equalsIgnoreCase(r[0]);
    }

    private static long parseLong(String v) {
        return Long.parseLong(v.trim());
    }

    private static int parseInt(String v) {
        return Integer.parseInt(v.trim());
    }

    private static String nextId(String p, int n) {
        String s = String.valueOf(n + 1);
        while (s.length() < 10) {
            s = "0" + s;
        }
        return p + s;
    }

    private static final class InstrAttr {
        final int tier;
        final int rateBp;
        final int tick;

        InstrAttr(int tier, int rateBp, int tick) {
            this.tier = tier;
            this.rateBp = rateBp;
            this.tick = tick;
        }
    }

    private static final class ExecAgg {
        long netQty;
        long fillQty;
        long grossAmt;
    }

    private static final class ExecRecord {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final String execTs;

        ExecRecord(String execId, String orderId, String instrCode, String sideKbn, long fillQty, long fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
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

    private static final class DropRecord {
        final String dropId;
        final String execId;
        final String orderId;
        final String instrCode;
        final long fillQty;
        final long fillAmt;
        final String captureTs;

        DropRecord(String dropId, String execId, String orderId, String instrCode, long fillQty, long fillAmt, String captureTs) {
            this.dropId = dropId;
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.captureTs = captureTs;
        }
    }

    private static final class DecisionRecord {
        final String decisionId;
        final String orderId;
        final String instrCode;
        final int actionCode;
        final String reasonCode;
        final String decisionTs;

        DecisionRecord(String decisionId, String orderId, String instrCode, int actionCode, String reasonCode, String decisionTs) {
            this.decisionId = decisionId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.actionCode = actionCode;
            this.reasonCode = reasonCode;
            this.decisionTs = decisionTs;
        }
    }

    private static final class AlertRecord {
        final String alertId;
        final String alertKbn;
        final String severityCode;
        final String subjectId;
        final String detailCode;
        final long raisedTs;

        AlertRecord(String alertId, String alertKbn, String severityCode, String subjectId, String detailCode, long raisedTs) {
            this.alertId = alertId;
            this.alertKbn = alertKbn;
            this.severityCode = severityCode;
            this.subjectId = subjectId;
            this.detailCode = detailCode;
            this.raisedTs = raisedTs;
        }
    }

    private static final class AuditRecord {
        final String auditId;
        final String actorId;
        final String actionKbn;
        final String objectId;
        final String resultCode;
        final long auditTs;

        AuditRecord(String auditId, String actorId, String actionKbn, String objectId, String resultCode, long auditTs) {
            this.auditId = auditId;
            this.actorId = actorId;
            this.actionKbn = actionKbn;
            this.objectId = objectId;
            this.resultCode = resultCode;
            this.auditTs = auditTs;
        }
    }
}
