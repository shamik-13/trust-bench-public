/*
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2022-02-22  村上 健司 (E-301)  初版作成
 */

package jp.mirai.sec.grouprisk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

public class FixSessionService {
    private static final String SERVICE_ID = "FIXSESS";
    private static final DateTimeFormatter DT = DateTimeFormatter.ISO_LOCAL_DATE;
    private static final DateTimeFormatter TS = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    public static void main(String[] a) {
        if (a.length != 5) {
            System.err.println("引数不正: 操作 SCSESSF SCCALF SCAUDTF 依頼CSV を指定してください");
            System.exit(2);
        }

        try {
            new Engine(Path.of(a[1]), Path.of(a[2]), Path.of(a[3])).process(Path.of(a[4]), a[0]);
        } catch (RuntimeException | IOException e) {
            System.err.println("処理異常: " + e.getMessage());
            System.exit(1);
        }
    }

    private static final class Engine {
        private final Path sessionFile;
        private final Path calendarFile;
        private final Path auditFile;
        private final Map<String, SessionRow> sessions;
        private final List<CalendarRow> calendars;

        private Engine(Path sessionFile, Path calendarFile, Path auditFile) throws IOException {
            this.sessionFile = sessionFile;
            this.calendarFile = calendarFile;
            this.auditFile = auditFile;
            this.sessions = readSessions(sessionFile);
            this.calendars = readCalendars(calendarFile);
        }

        private void process(Path requestFile, String defaultOp) throws IOException {
            List<AuditRow> audits = new ArrayList<>();
            for (RequestRow req : readRequests(requestFile, defaultOp)) {
                SessionRow before = sessions.get(req.sessKey);
                if (before == null) {
                    reject(req, audits, "E001");
                    continue;
                }

                Optional<CalendarRow> cal = calendarFor(before.sessDt, before.boardCode);
                if (cal.isEmpty()) {
                    reject(req, audits, "E002");
                    continue;
                }

                LocalDateTime now = req.eventTs;
                if (("START".equals(req.op) || "LOGON".equals(req.op)) && !within(cal.get(), now)) {
                    reject(req, audits, "E003");
                    continue;
                }

                SessionRow after = transition(before, req, cal.get(), audits);
                sessions.put(after.sessKey, after);
            }
            writeSessions();
            appendAudits(audits);
        }

        private SessionRow transition(SessionRow s, RequestRow r, CalendarRow c, List<AuditRow> audits) {
            String state = s.stateKbn;
            int lastSeq = s.lastSeqNo;

            switch (r.op) {
                case "START":
                    if (!"CLOSED".equals(state) && !"LOGOUT".equals(state)) {
                        reject(r, audits, "E101");
                        return s;
                    }
                    return changed(s, "WAIT_LOGON", lastSeq, r.eventTs, r, audits, "START", "I001");

                case "LOGON":
                    if (!"WAIT_LOGON".equals(state) && !"LOGOUT".equals(state)) {
                        reject(r, audits, "E102");
                        return s;
                    }
                    if (r.msgSeqNo <= lastSeq) {
                        reject(r, audits, "E103");
                        return s;
                    }
                    return changed(s, "ACTIVE", r.msgSeqNo, r.eventTs, r, audits, "LOGON", "I002");

                case "HEARTBEAT":
                    if (!"ACTIVE".equals(state)) {
                        reject(r, audits, "E104");
                        return s;
                    }
                    if (r.msgSeqNo != lastSeq + 1) {
                        reject(r, audits, "E105");
                        return s;
                    }
                    return changed(s, "ACTIVE", r.msgSeqNo, r.eventTs, r, audits, "HEARTBEAT", "I003");

                case "RESEND":
                    if (!"ACTIVE".equals(state) && !"WAIT_LOGON".equals(state)) {
                        reject(r, audits, "E106");
                        return s;
                    }
                    if (r.beginSeqNo < 1 || r.endSeqNo < r.beginSeqNo || r.endSeqNo > lastSeq) {
                        reject(r, audits, "E107");
                        return s;
                    }
                    if (r.eventTs.toLocalDate().isBefore(c.sessDt)) {
                        reject(r, audits, "E108");
                        return s;
                    }
                    audit(audits, r.eventTs, r.sessKey, "RESEND", "I004");
                    return s.withUpdated(r.eventTs);

                case "LOGOUT":
                    if ("CLOSED".equals(state)) {
                        reject(r, audits, "E109");
                        return s;
                    }
                    int nextSeq = r.msgSeqNo > 0 ? Math.max(lastSeq, r.msgSeqNo) : lastSeq;
                    return changed(s, "LOGOUT", nextSeq, r.eventTs, r, audits, "LOGOUT", "I005");

                case "CLOSE":
                    if (r.eventTs.isBefore(c.closeTs)) {
                        reject(r, audits, "E110");
                        return s;
                    }
                    return changed(s, "CLOSED", lastSeq, r.eventTs, r, audits, "CLOSE", "I006");

                default:
                    reject(r, audits, "E199");
                    return s;
            }
        }

        private SessionRow changed(SessionRow s, String state, int seq, LocalDateTime ts,
                                   RequestRow r, List<AuditRow> audits, String event, String code) {
            audit(audits, ts, r.sessKey, event, code);
            return new SessionRow(s.sessKey, s.sessDt, s.boardCode, state, seq, ts);
        }

        private void reject(RequestRow r, List<AuditRow> audits, String code) {
            audit(audits, r.eventTs, r.sessKey, "REJECT", code);
        }

        private void audit(List<AuditRow> audits, LocalDateTime ts, String objectId, String event, String code) {
            audits.add(new AuditRow(UUID.randomUUID().toString(), ts, SERVICE_ID, objectId, event, code));
        }

        private Optional<CalendarRow> calendarFor(LocalDate sessDt, String boardCode) {
            return calendars.stream()
                    .filter(c -> c.sessDt.equals(sessDt))
                    .filter(c -> c.sessKbn.equals(boardCode) || "ALL".equals(c.sessKbn))
                    .max(Comparator.comparing(c -> c.sessKbn.equals(boardCode)));
        }

        private boolean within(CalendarRow c, LocalDateTime ts) {
            return !ts.isBefore(c.openTs) && ts.isBefore(c.closeTs);
        }

        private void writeSessions() throws IOException {
            try (BufferedWriter w = Files.newBufferedWriter(sessionFile, StandardCharsets.UTF_8)) {
                w.write("SESS-KEY,SESS-DT,BOARD-CODE,STATE-KBN,LAST-SEQ-NO,UPDATED-TS");
                w.newLine();
                for (SessionRow r : sessions.values()) {
                    w.write(csv(r.sessKey, DT.format(r.sessDt), r.boardCode, r.stateKbn,
                            Integer.toString(r.lastSeqNo), TS.format(r.updatedTs)));
                    w.newLine();
                }
            }
        }

        private void appendAudits(List<AuditRow> audits) throws IOException {
            boolean exists = Files.exists(auditFile) && Files.size(auditFile) > 0;
            try (BufferedWriter w = Files.newBufferedWriter(auditFile, StandardCharsets.UTF_8,
                    exists ? java.nio.file.StandardOpenOption.APPEND : java.nio.file.StandardOpenOption.CREATE)) {
                if (!exists) {
                    w.write("AUDIT-ID,EVENT-TS,SERVICE-ID,OBJECT-ID,EVENT-KBN,DETAIL-CODE");
                    w.newLine();
                }
                for (AuditRow r : audits) {
                    w.write(csv(r.auditId, TS.format(r.eventTs), r.serviceId, r.objectId, r.eventKbn, r.detailCode));
                    w.newLine();
                }
            }
        }
    }

    private static Map<String, SessionRow> readSessions(Path p) throws IOException {
        Map<String, SessionRow> m = new LinkedHashMap<>();
        for (List<String> f : readCsv(p)) {
            SessionRow r = new SessionRow(f.get(0), LocalDate.parse(f.get(1), DT), f.get(2), f.get(3),
                    Integer.parseInt(f.get(4)), LocalDateTime.parse(f.get(5), TS));
            m.put(r.sessKey, r);
        }
        return m;
    }

    private static List<CalendarRow> readCalendars(Path p) throws IOException {
        List<CalendarRow> rows = new ArrayList<>();
        for (List<String> f : readCsv(p)) {
            rows.add(new CalendarRow(LocalDate.parse(f.get(0), DT), f.get(1),
                    LocalDateTime.parse(f.get(2), TS), LocalDateTime.parse(f.get(3), TS)));
        }
        return rows;
    }

    private static List<RequestRow> readRequests(Path p, String defaultOp) throws IOException {
        List<RequestRow> rows = new ArrayList<>();
        for (List<String> f : readCsv(p)) {
            String op = f.get(0).isEmpty() ? defaultOp : f.get(0);
            rows.add(new RequestRow(op, f.get(1), parseInt(f.get(2)), parseInt(f.get(3)),
                    parseInt(f.get(4)), LocalDateTime.parse(f.get(5), TS)));
        }
        return rows;
    }

    private static List<List<String>> readCsv(Path p) throws IOException {
        List<List<String>> rows = new ArrayList<>();
        try (BufferedReader r = Files.newBufferedReader(p, StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = r.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> f = splitCsv(line);
                if (first && f.stream().anyMatch(v -> v.indexOf('-') >= 0 || "OP".equals(v))) {
                    first = false;
                    continue;
                }
                first = false;
                rows.add(f);
            }
        }
        return rows;
    }

    private static List<String> splitCsv(String line) {
        List<String> out = new ArrayList<>();
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

    private static String csv(String... v) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < v.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            String s = Objects.toString(v[i], "");
            if (s.indexOf(',') >= 0 || s.indexOf('"') >= 0 || s.indexOf('\n') >= 0) {
                b.append('"').append(s.replace("\"", "\"\"")).append('"');
            } else {
                b.append(s);
            }
        }
        return b.toString();
    }

    private static int parseInt(String v) {
        if (v == null || v.isEmpty()) {
            return 0;
        }
        return Integer.parseInt(v);
    }

    private static final class SessionRow {
        final String sessKey;
        final LocalDate sessDt;
        final String boardCode;
        final String stateKbn;
        final int lastSeqNo;
        final LocalDateTime updatedTs;

        SessionRow(String sessKey, LocalDate sessDt, String boardCode, String stateKbn,
                   int lastSeqNo, LocalDateTime updatedTs) {
            this.sessKey = sessKey;
            this.sessDt = sessDt;
            this.boardCode = boardCode;
            this.stateKbn = stateKbn;
            this.lastSeqNo = lastSeqNo;
            this.updatedTs = updatedTs;
        }

        SessionRow withUpdated(LocalDateTime ts) {
            return new SessionRow(sessKey, sessDt, boardCode, stateKbn, lastSeqNo, ts);
        }
    }

    private static final class CalendarRow {
        final LocalDate sessDt;
        final String sessKbn;
        final LocalDateTime openTs;
        final LocalDateTime closeTs;

        CalendarRow(LocalDate sessDt, String sessKbn, LocalDateTime openTs, LocalDateTime closeTs) {
            if (!openTs.isBefore(closeTs)) {
                throw new DateTimeParseException("営業時刻不正", openTs + "/" + closeTs, 0);
            }
            this.sessDt = sessDt;
            this.sessKbn = sessKbn;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }

    private static final class RequestRow {
        final String op;
        final String sessKey;
        final int msgSeqNo;
        final int beginSeqNo;
        final int endSeqNo;
        final LocalDateTime eventTs;

        RequestRow(String op, String sessKey, int msgSeqNo, int beginSeqNo, int endSeqNo, LocalDateTime eventTs) {
            this.op = op;
            this.sessKey = sessKey;
            this.msgSeqNo = msgSeqNo;
            this.beginSeqNo = beginSeqNo;
            this.endSeqNo = endSeqNo;
            this.eventTs = eventTs;
        }
    }

    private static final class AuditRow {
        final String auditId;
        final LocalDateTime eventTs;
        final String serviceId;
        final String objectId;
        final String eventKbn;
        final String detailCode;

        AuditRow(String auditId, LocalDateTime eventTs, String serviceId, String objectId,
                 String eventKbn, String detailCode) {
            this.auditId = auditId;
            this.eventTs = eventTs;
            this.serviceId = serviceId;
            this.objectId = objectId;
            this.eventKbn = eventKbn;
            this.detailCode = detailCode;
        }
    }
}
