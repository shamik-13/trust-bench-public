package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.0   2024-03-15  保険金システムG  初版作成
 */
public class PayoutCalculationAuditor {
    private static final java.math.BigDecimal ONE = java.math.BigDecimal.ONE;
    private static final java.math.BigDecimal YEN_TOLERANCE = java.math.BigDecimal.ONE;
    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final String CATEGORY_DISCREPANCY = "09";
    private static final String RESULT_PASS = "00";
    private static final String RESULT_FAIL = "09";
    private static final String AUTH_LEVEL_MANUAL = "2";
    private static final String ASSESSOR_ID = "BATCHAUD";
    private static final java.nio.charset.Charset FILE_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    private static java.util.List<String> readHeader(java.nio.file.Path path) throws java.io.IOException {
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, FILE_CHARSET)) {
            String line = reader.readLine();
            if (line == null) {
                return defaultAssessmentHeader();
            }
            java.util.List<String> header = parseCsvLine(line);
            if (header.isEmpty()) {
                return defaultAssessmentHeader();
            }
            return header;
        }
    }

    private static java.util.Map<String, java.util.Map<String, String>> readKeyed(java.nio.file.Path path, String keyName) throws java.io.IOException {
        java.util.Map<String, java.util.Map<String, String>> keyed = new java.util.LinkedHashMap<String, java.util.Map<String, String>>();
        for (java.util.Map<String, String> row : readRows(path)) {
            String key = required(row, keyName);
            keyed.put(key, row);
        }
        return keyed;
    }

    private static java.util.List<java.util.Map<String, String>> readRows(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<java.util.Map<String, String>>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, FILE_CHARSET)) {
            String headerLine = reader.readLine();
            if (headerLine == null) {
                return rows;
            }

            java.util.List<String> header = parseCsvLine(headerLine);
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }

                java.util.List<String> values = parseCsvLine(line);
                java.util.Map<String, String> row = new java.util.LinkedHashMap<String, String>();
                for (int i = 0; i < header.size(); i++) {
                    row.put(header.get(i), i < values.size() ? values.get(i) : "");
                }
                rows.add(row);
            }
        }
        return rows;
    }

    private static void writeRows(
            java.nio.file.Path path,
            java.util.List<String> header,
            java.util.Collection<java.util.Map<String, String>> rows) throws java.io.IOException {
        java.nio.file.Path parent = path.getParent();
        if (parent != null) {
            java.nio.file.Files.createDirectories(parent);
        }

        try (java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(path, FILE_CHARSET)) {
            writer.write(toCsvLine(header));
            writer.newLine();

            for (java.util.Map<String, String> row : rows) {
                java.util.List<String> values = new java.util.ArrayList<String>();
                for (String column : header) {
                    values.add(row.containsKey(column) ? row.get(column) : "");
                }
                writer.write(toCsvLine(values));
                writer.newLine();
            }
        }
    }

    private static java.math.BigDecimal money(String value) {
        return new java.math.BigDecimal(value.trim()).setScale(0, java.math.RoundingMode.HALF_UP);
    }

    private static java.math.BigDecimal rate(String value) {
        java.math.BigDecimal parsed = new java.math.BigDecimal(value.trim());
        if (parsed.compareTo(ONE) > 0) {
            return parsed.divide(new java.math.BigDecimal("100"), 10, java.math.RoundingMode.HALF_UP);
        }
        return parsed;
    }

    private static String required(java.util.Map<String, String> row, String name) {
        String value = row.get(name);
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + " が未設定です。");
        }
        return value.trim();
    }

    private static String valueOrDefault(String value, String defaultValue) {
        if (value == null || value.trim().isEmpty()) {
            return defaultValue;
        }
        return value;
    }

    private static String createAssessId(String payId, String claimId) {
        String source = "RA" + claimId + payId;
        if (source.length() <= 20) {
            return source;
        }
        return source.substring(0, 20);
    }

    private static java.util.List<String> defaultAssessmentHeader() {
        java.util.List<String> header = new java.util.ArrayList<String>();
        header.add("ASSESS-ID");
        header.add("CLAIM-ID");
        header.add("ASSESS-DT");
        header.add("CATEGORY-KBN");
        header.add("AUTH-LEVEL-KBN");
        header.add("RESULT-KBN");
        header.add("ASSESSOR-ID");
        return header;
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> values = new java.util.ArrayList<String>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    current.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (c == ',' && !quoted) {
                values.add(current.toString());
                current.setLength(0);
            } else {
                current.append(c);
            }
        }

        values.add(current.toString());
        return values;
    }

    private static String toCsvLine(java.util.List<String> values) {
        StringBuilder line = new StringBuilder();
        for (int i = 0; i < values.size(); i++) {
            if (i > 0) {
                line.append(',');
            }
            line.append(escapeCsv(values.get(i)));
        }
        return line.toString();
    }

    private static String escapeCsv(String value) {
        String v = value == null ? "" : value;
        boolean needsQuote = v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0;
        if (!needsQuote) {
            return v;
        }
        return '"' + v.replace("\"", "\"\"") + '"';
    }
}
