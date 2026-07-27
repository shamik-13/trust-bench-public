package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数    年月日      担当            概要
 * 1.00    2024/03/15  保険金システムG  初版作成
 */
public class MedicalEvidenceReviewer {
    private static final String KBN_MISHINSA = "00";
    private static final String KBN_SHIBO = "01";
    private static final String KBN_KODO_SHOGAI = "02";
    private static final String KBN_NYUUIN = "03";
    private static final String KBN_SHUJUTSU = "04";
    private static final String KBN_SONOTA = "05";
    private static final String RESULT_TAISHOGAI = "90";
    private static final String STATUS_SHIHARAI_TAISHO = "01";
    private static final int ONE_YEAR_OR_MORE_PAYMENT_PERCENT = 100;

    private static final java.time.format.DateTimeFormatter DATE_FMT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    private static final java.util.Set<String> DEATH_PRIMARY = setOf(
            "C349", "I219", "I259", "I619", "I639", "J189", "K729", "N189", "R99");
    private static final java.util.Set<String> DISABILITY_PRIMARY = setOf(
            "G819", "G822", "H540", "I693", "S141", "S241", "S341");
    private static final java.util.Set<String> HOSPITAL_PRIMARY = setOf(
            "A419", "C509", "E119", "I209", "J180", "K358", "M545", "N390");
    private static final java.util.Set<String> SURGERY_PRIMARY = setOf(
            "C189", "C509", "D259", "I251", "K359", "K409", "M169", "N200");

    private static final java.util.Set<String> DEATH_SECONDARY = setOf(
            "R570", "R578", "R579", "T794");
    private static final java.util.Set<String> DISABILITY_SECONDARY = setOf(
            "F03", "G20", "G35", "H542");
    private static final java.util.Set<String> HOSPITAL_SECONDARY = setOf(
            "R509", "R101", "R104", "Z511");
    private static final java.util.Set<String> SURGERY_SECONDARY = setOf(
            "Z421", "Z480", "Z488", "Z489");

    private static final java.util.Set<String> EXCLUSION = setOf(
            "Z000", "Z021", "Z027", "Z763", "U071", "U072");

    private static String classify(String primary, String secondary, String exclusion) {
        java.util.Set<String> exclusions = splitCodes(exclusion);
        for (String code : exclusions) {
            if (EXCLUSION.contains(code)) {
                return KBN_SONOTA;
            }
        }

        java.util.Set<String> primaryCodes = splitCodes(primary);
        java.util.Set<String> secondaryCodes = splitCodes(secondary);

        if (matches(primaryCodes, DEATH_PRIMARY) || matches(secondaryCodes, DEATH_SECONDARY)) {
            return KBN_SHIBO;
        }
        if (matches(primaryCodes, DISABILITY_PRIMARY) || matches(secondaryCodes, DISABILITY_SECONDARY)) {
            return KBN_KODO_SHOGAI;
        }
        if (matches(primaryCodes, HOSPITAL_PRIMARY) || matches(secondaryCodes, HOSPITAL_SECONDARY)) {
            return KBN_NYUUIN;
        }
        if (matches(primaryCodes, SURGERY_PRIMARY) || matches(secondaryCodes, SURGERY_SECONDARY)) {
            return KBN_SHUJUTSU;
        }
        return KBN_SONOTA;
    }

    private static boolean isOutsideCoverage(String startText, String eventText) {
        java.time.LocalDate start = parseDate(startText);
        java.time.LocalDate event = parseDate(eventText);
        if (start == null || event == null) {
            return true;
        }
        return event.isBefore(start);
    }

    private static java.time.LocalDate parseDate(String text) {
        if (text == null || text.trim().isEmpty()) {
            return null;
        }
        String v = text.trim().replace("-", "").replace("/", "");
        try {
            return java.time.LocalDate.parse(v, DATE_FMT);
        } catch (java.time.format.DateTimeParseException e) {
            return null;
        }
    }

    private static boolean matches(java.util.Set<String> actual, java.util.Set<String> table) {
        for (String code : actual) {
            if (table.contains(code)) {
                return true;
            }
        }
        return false;
    }

    private static java.util.Set<String> splitCodes(String text) {
        java.util.Set<String> codes = new java.util.HashSet<>();
        if (text == null) {
            return codes;
        }
        String[] parts = text.split("[,;: 　]+");
        for (String part : parts) {
            String code = normalizeCode(part);
            if (!code.isEmpty()) {
                codes.add(code);
            }
        }
        return codes;
    }

    private static String normalizeCode(String code) {
        if (code == null) {
            return "";
        }
        return code.trim().toUpperCase(java.util.Locale.ROOT).replace(".", "").replace("-", "");
    }

    private static java.util.Set<String> setOf(String... values) {
        java.util.Set<String> set = new java.util.HashSet<>();
        java.util.Collections.addAll(set, values);
        return java.util.Collections.unmodifiableSet(set);
    }

    private static java.util.List<Row> readCsv(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<Row> rows = new java.util.ArrayList<>();
        if (lines.isEmpty()) {
            return rows;
        }

        java.util.List<String> header = parseCsvLine(lines.get(0));
        for (int i = 1; i < lines.size(); i++) {
            if (lines.get(i).trim().isEmpty()) {
                continue;
            }
            java.util.List<String> values = parseCsvLine(lines.get(i));
            Row row = new Row(header);
            for (int c = 0; c < header.size(); c++) {
                row.put(header.get(c), c < values.size() ? values.get(c) : "");
            }
            rows.add(row);
        }
        return rows;
    }

    private static void writeCsv(java.nio.file.Path path, java.util.List<Row> rows) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        if (rows.isEmpty()) {
            java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
            return;
        }

        java.util.List<String> header = rows.get(0).header();
        lines.add(toCsvLine(header));
        for (Row row : rows) {
            java.util.List<String> values = new java.util.ArrayList<>();
            for (String name : header) {
                values.add(row.get(name));
            }
            lines.add(toCsvLine(values));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
        StringBuilder buf = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    buf.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                out.add(buf.toString());
                buf.setLength(0);
            } else {
                buf.append(ch);
            }
        }
        out.add(buf.toString());
        return out;
    }

    private static String toCsvLine(java.util.List<String> values) {
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < values.size(); i++) {
            if (i > 0) {
                out.append(',');
            }
            String value = values.get(i) == null ? "" : values.get(i);
            boolean quote = value.indexOf(',') >= 0 || value.indexOf('"') >= 0
                    || value.indexOf('\n') >= 0 || value.indexOf('\r') >= 0;
            if (quote) {
                out.append('"');
                for (int p = 0; p < value.length(); p++) {
                    char ch = value.charAt(p);
                    if (ch == '"') {
                        out.append("\"\"");
                    } else {
                        out.append(ch);
                    }
                }
                out.append('"');
            } else {
                out.append(value);
            }
        }
        return out.toString();
    }

    private static final class Row {
        private final java.util.List<String> header;
        private final java.util.Map<String, String> values;

        Row(java.util.List<String> header) {
            this.header = new java.util.ArrayList<>(header);
            this.values = new java.util.LinkedHashMap<>();
            for (String name : header) {
                values.put(name, "");
            }
        }

        java.util.List<String> header() {
            return java.util.Collections.unmodifiableList(header);
        }

        String get(String name) {
            String value = values.get(name);
            return value == null ? "" : value;
        }

        void put(String name, String value) {
            if (!values.containsKey(name)) {
                header.add(name);
            }
            values.put(name, value == null ? "" : value);
        }

        Row copy() {
            Row row = new Row(header);
            row.values.putAll(values);
            return row;
        }
    }
}
