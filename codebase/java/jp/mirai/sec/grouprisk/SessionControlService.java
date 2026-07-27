/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2021-07-15  中川 美和 (E-283)        初版作成。FIXセッション単位の制御判定、重要アラート、監査出力を実装。
 */

package jp.mirai.sec.grouprisk;

public class SessionControlService {
    private static final java.nio.charset.Charset CS = java.nio.charset.StandardCharsets.UTF_8;
    private static final java.time.format.DateTimeFormatter TS_FMT =
            java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
    private static final String ACTOR_ID = "SCSVC";
    private static final int SEQ_WARN_GAP = 10;
    private static final int THROTTLE_STOP_COUNT = 5;
    private static final int REJECT_STOP_COUNT = 3;

    public static void main(String[] a) throws Exception {
        java.nio.file.Path base = java.nio.file.Paths.get(a.length == 0 ? "." : a[0]);
        java.nio.file.Path calFile = base.resolve("SCCALF.csv");
        java.nio.file.Path decFile = firstExisting(base, "HFDECLOG", "HFDECLOG.csv", "HFDECLOG.dat");
        java.nio.file.Path alertFile = firstExisting(base, "SCALRTF", "SCALRTF.csv", "SCALRTF.dat");
        java.nio.file.Path auditFile = base.resolve("SCAUDF2");

        java.util.List<CalendarRow> calendars = readCalendar(calFile);
        java.util.Set<String> existingAlerts = readAlertKeys(alertFile);
        java.util.List<DecisionRow> decisions = readDecisions(decFile);

        java.util.Map<String, SessionBucket> buckets = new java.util.LinkedHashMap<>();
        for (DecisionRow d : decisions) {
            CalendarRow cal = findCalendar(calendars, d.decisionTs);
            String sessionId = cal == null ? "未定義-" + d.decisionTs.toLocalDate() : cal.sessionKbn;
            SessionBucket b = buckets.get(sessionId);
            if (b == null) {
                b = new SessionBucket(sessionId, cal);
                buckets.put(sessionId, b);
            }
            b.add(d);
        }

        java.util.List<String> alertLines = new java.util.ArrayList<>();
        java.util.List<String> auditLines = new java.util.ArrayList<>();
        java.time.LocalDateTime now = java.time.LocalDateTime.now();

        for (SessionBucket b : buckets.values()) {
            ControlResult r = decide(b);
            if ("継続".equals(r.actionKbn)) {
                continue;
            }

            String subject = b.sessionId;
            String detail = r.detailCode;
            String alertKey = subject + "|" + detail;
            String result = "送信待";

            if (!existingAlerts.contains(alertKey)) {
                String alertId = "SCA" + TS_FMT.format(now) + pad(alertLines.size() + 1, 4);
                alertLines.add(csv(alertId, "FIX制御", r.severityCode, subject, detail, TS_FMT.format(now)));
                existingAlerts.add(alertKey);
            } else {
                result = "重複";
            }

            String auditId = "AUD" + TS_FMT.format(now) + pad(auditLines.size() + 1, 4);
            auditLines.add(csv(auditId, ACTOR_ID, r.actionKbn, subject, result, TS_FMT.format(now)));
        }

        appendLines(alertFile, alertLines);
        appendLines(auditFile, auditLines);
    }

    private static ControlResult decide(SessionBucket b) {
        if (b.sequenceGapCount >= SEQ_WARN_GAP && b.rejectCount >= REJECT_STOP_COUNT) {
            return new ControlResult("停止", "重大", "SEQ異常_REJECT過多");
        }
        if (b.throttleCount >= THROTTLE_STOP_COUNT) {
            return new ControlResult("停止", "重大", "THROTTLE過多");
        }
        if (b.sequenceGapCount > 0) {
            return new ControlResult("リセット", "警告", "SEQ異常");
        }
        if (b.resumeCandidate && b.totalCount > 0) {
            return new ControlResult("再開", "情報", "取引再開候補");
        }
        return new ControlResult("継続", "情報", "異常なし");
    }

    private static java.nio.file.Path firstExisting(java.nio.file.Path base, String... names) {
        for (String n : names) {
            java.nio.file.Path p = base.resolve(n);
            if (java.nio.file.Files.exists(p)) {
                return p;
            }
        }
        return base.resolve(names[0]);
    }

    private static java.util.List<CalendarRow> readCalendar(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<CalendarRow> rows = new java.util.ArrayList<>();
        if (!java.nio.file.Files.exists(p)) {
            return rows;
        }
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(p, CS)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                java.util.List<String> c = splitCsv(line);
                if (first && c.get(0).contains("SESS-DT")) {
                    first = false;
                    continue;
                }
                first = false;
                if (c.size() < 4) {
                    continue;
                }
                java.time.LocalDate dt = parseDate(c.get(0));
                rows.add(new CalendarRow(dt, c.get(1), parseTs(dt, c.get(2)), parseTs(dt, c.get(3))));
            }
        }
        return rows;
    }

    private static java.util.Set<String> readAlertKeys(java.nio.file.Path p) throws java.io.IOException {
        java.util.Set<String> keys = new java.util.HashSet<>();
        if (!java.nio.file.Files.exists(p)) {
            return keys;
        }
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(p, CS)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                java.util.List<String> c = splitCsv(line);
                if (first && c.get(0).contains("ALERT-ID")) {
                    first = false;
                    continue;
                }
                first = false;
                if (c.size() >= 5) {
                    keys.add(c.get(3) + "|" + c.get(4));
                }
            }
        }
        return keys;
    }

    private static java.util.List<DecisionRow> readDecisions(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<DecisionRow> rows = new java.util.ArrayList<>();
        if (!java.nio.file.Files.exists(p)) {
            return rows;
        }
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(p, CS)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                java.util.List<String> c = splitCsv(line);
                if (first && c.get(0).contains("DECISION-ID")) {
                    first = false;
                    continue;
                }
                first = false;
                if (c.size() < 6) {
                    continue;
                }
                rows.add(new DecisionRow(c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), parseAnyTs(c.get(5))));
            }
        }
        java.util.Collections.sort(rows, new java.util.Comparator<DecisionRow>() {
            public int compare(DecisionRow x, DecisionRow y) {
                int t = x.decisionTs.compareTo(y.decisionTs);
                return t != 0 ? t : x.decisionId.compareTo(y.decisionId);
            }
        });
        return rows;
    }

    private static CalendarRow findCalendar(java.util.List<CalendarRow> rows, java.time.LocalDateTime ts) {
        for (CalendarRow r : rows) {
            if (!ts.isBefore(r.openTs) && ts.isBefore(r.closeTs)) {
                return r;
            }
        }
        return null;
    }

    private static void appendLines(java.nio.file.Path p, java.util.List<String> lines) throws java.io.IOException {
        if (lines.isEmpty()) {
            return;
        }
        java.nio.file.Files.write(p, lines, CS,
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.APPEND);
    }

    private static java.time.LocalDate parseDate(String s) {
        String v = digits(s);
        if (v.length() == 8) {
            return java.time.LocalDate.parse(v, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        }
        return java.time.LocalDate.parse(s.trim());
    }

    private static java.time.LocalDateTime parseTs(java.time.LocalDate d, String s) {
        String v = s.trim();
        if (digits(v).length() == 14) {
            return parseAnyTs(v);
        }
        return java.time.LocalDateTime.of(d, java.time.LocalTime.parse(v));
    }

    private static java.time.LocalDateTime parseAnyTs(String s) {
        String v = s.trim();
        String d = digits(v);
        if (d.length() == 14) {
            return java.time.LocalDateTime.parse(d, TS_FMT);
        }
        return java.time.LocalDateTime.parse(v);
    }

    private static String digits(String s) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (ch >= '0' && ch <= '9') {
                b.append(ch);
            }
        }
        return b.toString();
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
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
                out.add(b.toString().trim());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        out.add(b.toString().trim());
        return out;
    }

    private static String csv(String... v) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < v.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            String s = v[i] == null ? "" : v[i];
            if (s.indexOf(',') >= 0 || s.indexOf('"') >= 0 || s.indexOf('\n') >= 0) {
                b.append('"').append(s.replace("\"", "\"\"")).append('"');
            } else {
                b.append(s);
            }
        }
        return b.toString();
    }

    private static String pad(int n, int width) {
        String s = String.valueOf(n);
        StringBuilder b = new StringBuilder();
        for (int i = s.length(); i < width; i++) {
            b.append('0');
        }
        return b.append(s).toString();
    }

    private static final class CalendarRow {
        final java.time.LocalDate sessionDate;
        final String sessionKbn;
        final java.time.LocalDateTime openTs;
        final java.time.LocalDateTime closeTs;

        CalendarRow(java.time.LocalDate sessionDate, String sessionKbn,
                    java.time.LocalDateTime openTs, java.time.LocalDateTime closeTs) {
            this.sessionDate = sessionDate;
            this.sessionKbn = sessionKbn;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }

    private static final class DecisionRow {
        final String decisionId;
        final String orderId;
        final String instrCode;
        final String actionCode;
        final String reasonCode;
        final java.time.LocalDateTime decisionTs;

        DecisionRow(String decisionId, String orderId, String instrCode,
                    String actionCode, String reasonCode, java.time.LocalDateTime decisionTs) {
            this.decisionId = decisionId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.actionCode = actionCode;
            this.reasonCode = reasonCode;
            this.decisionTs = decisionTs;
        }
    }

    private static final class SessionBucket {
        final String sessionId;
        final CalendarRow calendar;
        final java.util.Map<String, Integer> lastSeqByInstr = new java.util.HashMap<>();
        int totalCount;
        int sequenceGapCount;
        int throttleCount;
        int rejectCount;
        boolean resumeCandidate;

        SessionBucket(String sessionId, CalendarRow calendar) {
            this.sessionId = sessionId;
            this.calendar = calendar;
        }

        void add(DecisionRow d) {
            totalCount++;
            if ("THROTTLE".equals(d.actionCode) || "抑止".equals(d.actionCode) || d.reasonCode.contains("THR")) {
                throttleCount++;
            }
            if ("REJECT".equals(d.actionCode) || "拒否".equals(d.actionCode)) {
                rejectCount++;
            }
            if ("ACCEPT".equals(d.actionCode) || "通過".equals(d.actionCode)) {
                resumeCandidate = true;
            }

            int seq = trailingNumber(d.decisionId);
            if (seq >= 0) {
                Integer last = lastSeqByInstr.get(d.instrCode);
                if (last != null && seq > last.intValue() + 1) {
                    sequenceGapCount += seq - last.intValue() - 1;
                }
                if (last == null || seq > last.intValue()) {
                    lastSeqByInstr.put(d.instrCode, Integer.valueOf(seq));
                }
            }
        }

        private int trailingNumber(String s) {
            int i = s.length() - 1;
            while (i >= 0 && Character.isDigit(s.charAt(i))) {
                i--;
            }
            if (i == s.length() - 1) {
                return -1;
            }
            try {
                return Integer.parseInt(s.substring(i + 1));
            } catch (NumberFormatException e) {
                return -1;
            }
        }
    }

    private static final class ControlResult {
        final String actionKbn;
        final String severityCode;
        final String detailCode;

        ControlResult(String actionKbn, String severityCode, String detailCode) {
            this.actionKbn = actionKbn;
            this.severityCode = severityCode;
            this.detailCode = detailCode;
        }
    }
}
