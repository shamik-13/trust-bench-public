package jp.mirai.sec.orderbook;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/04/01  岡本 涼 (E-294)    初版作成
 * 1.01  2024/09/17  中川 美和 (E-283)    監査出力を追加
 * 1.02  2025/02/03  大野 修 (E-225)    重複セッション検査を追加
 */
public class SessionCalendarService {
    private static final Charset CSV_CHARSET = StandardCharsets.UTF_8;
    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.BASIC_ISO_DATE;
    private static final DateTimeFormatter TS_FMT = DateTimeFormatter.ofPattern("yyyyMMddHHmmss", Locale.ROOT);

    private static final String AUDIT_EVENT_KBN = "CAL";
    private static final String AUDIT_CIF_NO = "0000000000";
    private static final String AUDIT_INSTR_CODE = "SESSION";
    private static final String DETAIL_INSERT = "ADD";
    private static final String DETAIL_REPLACE = "REP";

    public static void main(String[] a) throws Exception {
        if (a.length != 3) {
            System.err.println("引数不正: SCCALF入力 SCCALF出力 SCAUDF出力 を指定してください");
            System.exit(2);
        }

        SessionCalendarService service = new SessionCalendarService();
        int count = service.apply(Path.of(a[0]), Path.of(a[1]), Path.of(a[2]));
        System.out.println("処理完了: セッション件数=" + count);
    }

    private int apply(Path inputSccalf, Path outputSccalf, Path outputScaudf) throws IOException {
        List<SessionWindow> windows = readSccalf(inputSccalf);
        validateNoOverlap(windows);
        windows.sort(Comparator
                .comparing((SessionWindow s) -> s.sessionDate)
                .thenComparing(s -> s.openTs)
                .thenComparing(s -> s.sessionKbn));

        writeSccalf(outputSccalf, windows);
        writeScaudf(outputScaudf, windows);
        return windows.size();
    }

    private List<SessionWindow> readSccalf(Path file) throws IOException {
        List<SessionWindow> rows = new ArrayList<>();
        try (BufferedReader br = Files.newBufferedReader(file, CSV_CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = br.readLine()) != null) {
                lineNo++;
                if (lineNo == 1 && line.startsWith("SESS-DT,")) {
                    continue;
                }
                if (line.trim().isEmpty()) {
                    continue;
                }
                rows.add(parseSccalf(line, lineNo));
            }
        }
        return rows;
    }

    private SessionWindow parseSccalf(String line, int lineNo) {
        List<String> cols = splitCsv(line);
        if (cols.size() != 4) {
            throw new IllegalArgumentException("項目数不正: 行=" + lineNo);
        }

        LocalDate sessDt = parseDate(cols.get(0), lineNo, "SESS-DT");
        String sessKbn = requireCode(cols.get(1), lineNo, "SESS-KBN");
        LocalDateTime openTs = parseTimestamp(cols.get(2), lineNo, "OPEN-TS");
        LocalDateTime closeTs = parseTimestamp(cols.get(3), lineNo, "CLOSE-TS");

        if (!openTs.toLocalDate().equals(sessDt) || !closeTs.toLocalDate().equals(sessDt)) {
            throw new IllegalArgumentException("営業日不一致: 行=" + lineNo);
        }
        if (!openTs.isBefore(closeTs)) {
            throw new IllegalArgumentException("時刻範囲不正: 行=" + lineNo);
        }

        return new SessionWindow(sessDt, sessKbn, openTs, closeTs);
    }

    private void validateNoOverlap(List<SessionWindow> windows) {
        Map<LocalDate, List<SessionWindow>> byDate = new HashMap<>();
        for (SessionWindow window : windows) {
            byDate.computeIfAbsent(window.sessionDate, k -> new ArrayList<>()).add(window);
        }

        for (Map.Entry<LocalDate, List<SessionWindow>> entry : byDate.entrySet()) {
            List<SessionWindow> day = entry.getValue();
            day.sort(Comparator.comparing(s -> s.openTs));
            SessionWindow prev = null;
            for (SessionWindow cur : day) {
                if (prev != null && cur.openTs.isBefore(prev.closeTs)) {
                    throw new IllegalArgumentException(
                            "セッション重複: 日付=" + entry.getKey().format(DATE_FMT)
                                    + ", 前区分=" + prev.sessionKbn
                                    + ", 当区分=" + cur.sessionKbn);
                }
                prev = cur;
            }
        }
    }

    private void writeSccalf(Path file, List<SessionWindow> windows) throws IOException {
        try (BufferedWriter bw = Files.newBufferedWriter(file, CSV_CHARSET)) {
            bw.write("SESS-DT,SESS-KBN,OPEN-TS,CLOSE-TS");
            bw.newLine();
            for (SessionWindow w : windows) {
                bw.write(csv(w.sessionDate.format(DATE_FMT)));
                bw.write(',');
                bw.write(csv(w.sessionKbn));
                bw.write(',');
                bw.write(csv(w.openTs.format(TS_FMT)));
                bw.write(',');
                bw.write(csv(w.closeTs.format(TS_FMT)));
                bw.newLine();
            }
        }
    }

    private void writeScaudf(Path file, List<SessionWindow> windows) throws IOException {
        try (BufferedWriter bw = Files.newBufferedWriter(file, CSV_CHARSET)) {
            bw.write("AUDIT-ID,ORDER-ID,EVENT-KBN,CIF-NO,INSTR-CODE,EVENT-TS,DETAIL-CD");
            bw.newLine();
            int seq = 1;
            LocalDateTime auditTs = LocalDateTime.now();
            for (SessionWindow w : windows) {
                String auditId = w.sessionDate.format(DATE_FMT) + "-" + String.format(Locale.ROOT, "%06d", seq);
                String orderId = "CAL" + w.sessionDate.format(DATE_FMT) + w.sessionKbn;
                String detailCd = seq == 1 ? DETAIL_INSERT : DETAIL_REPLACE;

                bw.write(csv(auditId));
                bw.write(',');
                bw.write(csv(orderId));
                bw.write(',');
                bw.write(csv(AUDIT_EVENT_KBN));
                bw.write(',');
                bw.write(csv(AUDIT_CIF_NO));
                bw.write(',');
                bw.write(csv(AUDIT_INSTR_CODE));
                bw.write(',');
                bw.write(csv(auditTs.format(TS_FMT)));
                bw.write(',');
                bw.write(csv(detailCd));
                bw.newLine();
                seq++;
            }
        }
    }

    private LocalDate parseDate(String v, int lineNo, String name) {
        try {
            return LocalDate.parse(v.trim(), DATE_FMT);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(name + "不正: 行=" + lineNo, e);
        }
    }

    private LocalDateTime parseTimestamp(String v, int lineNo, String name) {
        try {
            return LocalDateTime.parse(v.trim(), TS_FMT);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(name + "不正: 行=" + lineNo, e);
        }
    }

    private String requireCode(String v, int lineNo, String name) {
        String s = Objects.requireNonNull(v, name).trim();
        if (s.isEmpty() || s.length() > 8) {
            throw new IllegalArgumentException(name + "不正: 行=" + lineNo);
        }
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (!(c >= 'A' && c <= 'Z') && !(c >= '0' && c <= '9')) {
                throw new IllegalArgumentException(name + "文字不正: 行=" + lineNo);
            }
        }
        return s;
    }

    private List<String> splitCsv(String line) {
        List<String> out = new ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean quote = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quote) {
                if (c == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        cur.append('"');
                        i++;
                    } else {
                        quote = false;
                    }
                } else {
                    cur.append(c);
                }
            } else {
                if (c == ',') {
                    out.add(cur.toString());
                    cur.setLength(0);
                } else if (c == '"') {
                    quote = true;
                } else {
                    cur.append(c);
                }
            }
        }
        out.add(cur.toString());

        if (quote) {
            throw new IllegalArgumentException("引用符不正");
        }
        return out;
    }

    private String csv(String s) {
        boolean needQuote = s.indexOf(',') >= 0 || s.indexOf('"') >= 0 || s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0;
        if (!needQuote) {
            return s;
        }
        return '"' + s.replace("\"", "\"\"") + '"';
    }

    private static final class SessionWindow {
        private final LocalDate sessionDate;
        private final String sessionKbn;
        private final LocalDateTime openTs;
        private final LocalDateTime closeTs;

        private SessionWindow(LocalDate sessionDate, String sessionKbn, LocalDateTime openTs, LocalDateTime closeTs) {
            this.sessionDate = sessionDate;
            this.sessionKbn = sessionKbn;
            this.openTs = openTs;
            this.closeTs = closeTs;
        }
    }
}
