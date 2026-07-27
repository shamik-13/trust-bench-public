/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2023-09-11  開発      会員向け海外キャッシング照会サービス初版
 * 1.01  2024-07-22  保守      キャッシング合計の表示対応
 * 1.02  2025-02-17  保守      区分整合チェック追加
 */
public class OverseasCashingService {
    private static final int COL_TXN_ID = 0;
    private static final int COL_CARD_NO = 1;
    private static final int COL_TXN_KBN = 2;
    private static final int COL_FEE_KBN = 3;
    private static final int COL_FEE_AMT = 4;
    private static final int COL_INT_START_DT = 5;
    private static final int COL_SETL_AMT = 6;
    private static final int COL_SETL_KBN = 7;
    private static final int COL_PROGRAM_ID = 8;
    private static final int COL_COUNT = 9;

    public static void main(String[] args) throws Exception {
        if (args.length < 2 || args.length > 3) {
            System.err.println("使用法: java OverseasCashingService CDOVSF CDMVWF [CARD-NO]");
            System.exit(2);
        }

        java.nio.file.Path input = java.nio.file.Paths.get(args[0]);
        java.nio.file.Path output = java.nio.file.Paths.get(args[1]);
        String targetCardNo = args.length == 3 ? args[2].trim() : "";

        long readCount = 0L;
        long writeCount = 0L;
        java.math.BigDecimal shoppingTotal = java.math.BigDecimal.ZERO;
        java.math.BigDecimal cashingTotal = java.math.BigDecimal.ZERO;
        java.util.List<String> displayRows = new java.util.ArrayList<>();

        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(input, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                readCount++;
                if (readCount == 1 && line.startsWith("TXN-ID,")) {
                    continue;
                }
                if (line.trim().isEmpty()) {
                    continue;
                }

                String[] cols = parseCsv(line);
                require(cols.length == COL_COUNT, readCount, "項目数不正");
                for (int i = 0; i < cols.length; i++) {
                    cols[i] = cols[i].trim();
                }

                String txnId = requireValue(cols[COL_TXN_ID], readCount, "TXN-ID");
                String cardNo = requireValue(cols[COL_CARD_NO], readCount, "CARD-NO");
                String txnKbn = requireValue(cols[COL_TXN_KBN], readCount, "TXN-KBN");
                String feeKbn = requireValue(cols[COL_FEE_KBN], readCount, "FEE-KBN");
                String intStartDt = requireValue(cols[COL_INT_START_DT], readCount, "INT-START-DT");
                String setlKbn = requireValue(cols[COL_SETL_KBN], readCount, "SETL-KBN");
                String programId = requireValue(cols[COL_PROGRAM_ID], readCount, "PROGRAM-ID");
                java.math.BigDecimal feeAmt = parseAmount(cols[COL_FEE_AMT], readCount, "FEE-AMT");
                java.math.BigDecimal setlAmt = parseAmount(cols[COL_SETL_AMT], readCount, "SETL-AMT");

                validateCode(txnKbn, new String[]{"P1", "P2", "C1", "C2", "A1"}, readCount, "TXN-KBN");
                validateCode(feeKbn, new String[]{"00", "FA", "FB"}, readCount, "FEE-KBN");
                validateCode(setlKbn, new String[]{"D", "H"}, readCount, "SETL-KBN");
                validateDate(intStartDt, readCount, "INT-START-DT");
                require(setlAmt.signum() >= 0, readCount, "SETL-AMT負数不正");
                require(feeAmt.signum() >= 0, readCount, "FEE-AMT負数不正");

                if ("C2".equals(txnKbn)) {
                    require("FA".equals(feeKbn), readCount, "海外キャッシング手数料区分不整合");
                    require(feeAmt.signum() > 0, readCount, "海外ATM事務手数料金額不整合");
                    if (!"CB810B".equals(programId)) {
                        System.err.println("警告: " + readCount + "行目 PROGRAM-ID確認対象 TXN-ID=" + txnId);
                    }
                }

                if (!"D".equals(setlKbn)) {
                    continue;
                }
                if (!targetCardNo.isEmpty() && !targetCardNo.equals(cardNo)) {
                    continue;
                }

                String dispKbn = toDispKbn(txnKbn);
                if (dispKbn == null) {
                    continue;
                }

                String dispLabel = "K".equals(dispKbn) ? "キャッシング" : "ショッピング";
                displayRows.add(csv(cardNo, txnId, dispKbn, setlAmt.toPlainString(), dispLabel));
                writeCount++;

                if ("K".equals(dispKbn)) {
                    cashingTotal = cashingTotal.add(setlAmt);
                } else {
                    shoppingTotal = shoppingTotal.add(setlAmt);
                }
            }
        }

        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("CARD-NO,TXN-ID,DISP-KBN,DISP-AMT,DISP-LABEL");
        out.addAll(displayRows);
        java.nio.file.Files.write(output, out, java.nio.charset.StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.TRUNCATE_EXISTING,
                java.nio.file.StandardOpenOption.WRITE);

        System.err.println("読込件数=" + readCount + " 表示件数=" + writeCount
                + " ショッピング合計=" + shoppingTotal.toPlainString()
                + " キャッシング合計=" + cashingTotal.toPlainString());
    }

    private static String toDispKbn(String txnKbn) {
        switch (txnKbn) {
            case "C1":
            case "C2":
                return "K";
            case "P1":
            case "P2":
                return "S";
            default:
                return null;
        }
    }

    private static String requireValue(String value, long lineNo, String name) {
        require(value != null && !value.isEmpty(), lineNo, name + "未設定");
        return value;
    }

    private static java.math.BigDecimal parseAmount(String value, long lineNo, String name) {
        try {
            return new java.math.BigDecimal(requireValue(value, lineNo, name));
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(lineNo + "行目 " + name + "数値不正");
        }
    }

    private static void validateDate(String value, long lineNo, String name) {
        require(value.matches("\\d{8}"), lineNo, name + "日付形式不正");
        try {
            java.time.LocalDate.parse(value, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        } catch (java.time.format.DateTimeParseException ex) {
            throw new IllegalArgumentException(lineNo + "行目 " + name + "日付不正");
        }
    }

    private static void validateCode(String value, String[] allowed, long lineNo, String name) {
        for (String code : allowed) {
            if (code.equals(value)) {
                return;
            }
        }
        throw new IllegalArgumentException(lineNo + "行目 " + name + "コード不正");
    }

    private static void require(boolean ok, long lineNo, String message) {
        if (!ok) {
            throw new IllegalArgumentException(lineNo + "行目 " + message);
        }
    }

    private static String[] parseCsv(String line) {
        java.util.List<String> cols = new java.util.ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (quoted) {
                if (ch == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        current.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    current.append(ch);
                }
            } else {
                if (ch == ',') {
                    cols.add(current.toString());
                    current.setLength(0);
                } else if (ch == '"') {
                    quoted = true;
                } else {
                    current.append(ch);
                }
            }
        }
        cols.add(current.toString());

        if (quoted) {
            throw new IllegalArgumentException("CSV引用符不正");
        }
        return cols.toArray(new String[0]);
    }

    private static String csv(String... values) {
        StringBuilder row = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                row.append(',');
            }
            String value = values[i];
            boolean quote = value.indexOf(',') >= 0 || value.indexOf('"') >= 0 || value.indexOf('\n') >= 0 || value.indexOf('\r') >= 0;
            if (quote) {
                row.append('"');
                for (int j = 0; j < value.length(); j++) {
                    char ch = value.charAt(j);
                    if (ch == '"') {
                        row.append("\"\"");
                    } else {
                        row.append(ch);
                    }
                }
                row.append('"');
            } else {
                row.append(value);
            }
        }
        return row.toString();
    }
}
