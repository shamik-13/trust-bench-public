package jp.mirai.pay.settlement;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2024/03/18  決済基盤T   初版作成
 * 1.01  2024/09/05  決済基盤T   銀行別フォーマット分岐を追加
 */
public class BankTransferFileService {

    private static final java.nio.charset.Charset FILE_CHARSET = java.nio.charset.StandardCharsets.UTF_8;
    private static final java.time.format.DateTimeFormatter DATE_FMT = java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
    private static final java.time.format.DateTimeFormatter TS_FMT =
            java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
    private static final java.math.BigDecimal MAX_PAYOUT_AMT = new java.math.BigDecimal("9999999999");

    public static void main(String[] a) {
        int rc = new BankTransferFileService().execute(a);
        if (rc != 0) {
            System.exit(rc);
        }
    }

    private int execute(String[] args) {
        if (args == null || args.length < 3 || args.length > 4) {
            System.err.println("使用方法: java jp.mirai.pay.settlement.BankTransferFileService PSPAYF入力 PSCONF入力 出力ディレクトリ [処理日yyyyMMdd]");
            return 8;
        }

        java.nio.file.Path psPayf = java.nio.file.Paths.get(args[0]);
        java.nio.file.Path psConf = java.nio.file.Paths.get(args[1]);
        java.nio.file.Path outDir = java.nio.file.Paths.get(args[2]);
        java.time.LocalDate businessDate;
        try {
            businessDate = args.length == 4 ? java.time.LocalDate.parse(args[3], DATE_FMT) : java.time.LocalDate.now();
        } catch (java.time.format.DateTimeParseException e) {
            System.err.println("処理日の形式が不正です: " + args[3]);
            return 8;
        }

        try {
            java.util.Map<String, ConfRecord> conf = readConf(psConf, businessDate);
            java.util.List<PayoutRecord> allRows = readPayf(psPayf);
            java.util.List<PayoutRecord> unsentRows = selectUnsentRows(allRows, businessDate);

            if (unsentRows.isEmpty()) {
                System.out.println("未送信行はありません。処理日=" + businessDate.format(DATE_FMT));
                return 0;
            }

            java.nio.file.Files.createDirectories(outDir);
            java.util.Map<String, BankBatch> batches = buildBatches(unsentRows, conf, businessDate);
            java.util.Map<String, String> resultByPayoutId = new java.util.LinkedHashMap<>();

            for (BankBatch batch : batches.values()) {
                java.nio.file.Path bankFile = outDir.resolve(batch.fileName);
                java.nio.file.Files.write(bankFile, batch.lines, FILE_CHARSET);
                String digest = sha256Hex(bankFile);
                String receiptNo = makeReceiptNo(batch.bankCode, businessDate, digest);
                batch.receiptNo = receiptNo;
                batch.digest = digest;

                for (PayoutRecord row : batch.rows) {
                    resultByPayoutId.put(row.payoutId, "SENT:" + receiptNo);
                }

                System.out.println("銀行送信ファイル作成 銀行=" + batch.bankCode
                        + " 件数=" + batch.rows.size()
                        + " 合計=" + batch.total.toPlainString()
                        + " 受付番号=" + receiptNo);
            }

            java.util.List<PayoutRecord> updated = new java.util.ArrayList<>();
            for (PayoutRecord row : allRows) {
                String result = resultByPayoutId.get(row.payoutId);
                updated.add(result == null ? row : row.withBankResultCd(result));
            }

            java.nio.file.Path updatedPayf = outDir.resolve("PSPAYF.UPDATED." + businessDate.format(DATE_FMT) + ".csv");
            writePayf(updatedPayf, updated);

            java.nio.file.Path auditFile = outDir.resolve("PSCONF.AUDIT." + businessDate.format(DATE_FMT) + ".csv");
            writeAudit(auditFile, batches, businessDate);

            return 0;
        } catch (java.io.IOException e) {
            System.err.println("ファイル入出力で異常が発生しました: " + e.getMessage());
            return 12;
        } catch (IllegalArgumentException e) {
            System.err.println("入力検証で異常が発生しました: " + e.getMessage());
            return 8;
        }
    }

    private java.util.Map<String, ConfRecord> readConf(java.nio.file.Path path, java.time.LocalDate businessDate)
            throws java.io.IOException {
        java.util.Map<String, ConfRecord> map = new java.util.LinkedHashMap<>();
        int lineNo = 0;
        for (String line : java.nio.file.Files.readAllLines(path, FILE_CHARSET)) {
            lineNo++;
            if (line.trim().isEmpty() || line.startsWith("#")) {
                continue;
            }
            String[] f = splitCsv(line, 5, "PSCONF", lineNo);
            if (lineNo == 1 && "CONF-KEY".equals(f[0])) {
                continue;
            }
            ConfRecord r = new ConfRecord(
                    require(f[0], "CONF-KEY", lineNo),
                    require(f[1], "CONF-VALUE", lineNo),
                    parseDate(require(f[2], "APPLY-DT", lineNo), "APPLY-DT", lineNo),
                    parseDate(require(f[3], "EXPIRE-DT", lineNo), "EXPIRE-DT", lineNo),
                    require(f[4], "UPDATED-AT", lineNo));
            if (!businessDate.isBefore(r.applyDt) && businessDate.isBefore(r.expireDt)) {
                map.put(r.confKey, r);
            }
        }
        return map;
    }

    private java.util.List<PayoutRecord> readPayf(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<PayoutRecord> rows = new java.util.ArrayList<>();
        java.util.Set<String> payoutIds = new java.util.HashSet<>();
        int lineNo = 0;
        for (String line : java.nio.file.Files.readAllLines(path, FILE_CHARSET)) {
            lineNo++;
            if (line.trim().isEmpty() || line.startsWith("#")) {
                continue;
            }
            String[] f = splitCsv(line, 6, "PSPAYF", lineNo);
            if (lineNo == 1 && "PAYOUT-ID".equals(f[0])) {
                continue;
            }
            PayoutRecord r = new PayoutRecord(
                    require(f[0], "PAYOUT-ID", lineNo),
                    require(f[1], "MERCHANT-CODE", lineNo),
                    require(f[2], "BANK-ACCT-NO", lineNo),
                    parseAmount(require(f[3], "PAYOUT-AMT", lineNo), lineNo),
                    parseDate(require(f[4], "PAYOUT-DT", lineNo), "PAYOUT-DT", lineNo),
                    f[5].trim());
            validatePayout(r, lineNo);
            if (!payoutIds.add(r.payoutId)) {
                throw new IllegalArgumentException("PAYOUT-IDが重複しています。行=" + lineNo + " 値=" + r.payoutId);
            }
            rows.add(r);
        }
        return rows;
    }

    private java.util.List<PayoutRecord> selectUnsentRows(java.util.List<PayoutRecord> rows, java.time.LocalDate businessDate) {
        java.util.List<PayoutRecord> selected = new java.util.ArrayList<>();
        for (PayoutRecord row : rows) {
            if ((row.bankResultCd.isEmpty() || "UNSENT".equals(row.bankResultCd)) && !row.payoutDt.isAfter(businessDate)) {
                selected.add(row);
            }
        }
        return selected;
    }

    private java.util.Map<String, BankBatch> buildBatches(
            java.util.List<PayoutRecord> rows,
            java.util.Map<String, ConfRecord> conf,
            java.time.LocalDate businessDate) {
        java.util.Map<String, BankBatch> batches = new java.util.TreeMap<>();
        for (PayoutRecord row : rows) {
            String bankCode = bankCodeOf(row.bankAcctNo);
            String formatKey = "BANK_FORMAT." + bankCode;
            String format = conf.containsKey(formatKey) ? conf.get(formatKey).confValue : "ZENGIN";
            BankBatch batch = batches.computeIfAbsent(bankCode, k -> new BankBatch(k, format, businessDate));
            batch.add(row, toBankLine(row, bankCode, format));
        }
        return batches;
    }

    private String toBankLine(PayoutRecord row, String bankCode, String format) {
        String acct = digits(row.bankAcctNo);
        String amt = row.payoutAmt.setScale(0, java.math.RoundingMode.UNNECESSARY).toPlainString();
        if ("CSV".equals(format)) {
            return csv(bankCode, row.payoutId, row.merchantCode, acct, amt, row.payoutDt.format(DATE_FMT));
        }
        StringBuilder b = new StringBuilder();
        b.append(padRight(bankCode, 4));
        b.append(padRight(row.payoutId, 20));
        b.append(padRight(row.merchantCode, 12));
        b.append(padLeft(acct, 14, '0'));
        b.append(padLeft(amt, 12, '0'));
        b.append(row.payoutDt.format(DATE_FMT));
        return b.toString();
    }

    private void writePayf(java.nio.file.Path path, java.util.List<PayoutRecord> rows) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("PAYOUT-ID,MERCHANT-CODE,BANK-ACCT-NO,PAYOUT-AMT,PAYOUT-DT,BANK-RESULT-CD");
        for (PayoutRecord r : rows) {
            out.add(csv(r.payoutId, r.merchantCode, r.bankAcctNo, r.payoutAmt.toPlainString(),
                    r.payoutDt.format(DATE_FMT), r.bankResultCd));
        }
        java.nio.file.Files.write(path, out, FILE_CHARSET);
    }

    private void writeAudit(java.nio.file.Path path, java.util.Map<String, BankBatch> batches, java.time.LocalDate businessDate)
            throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("CONF-KEY,CONF-VALUE,APPLY-DT,EXPIRE-DT,UPDATED-AT");
        String updatedAt = java.time.LocalDateTime.now().format(TS_FMT);
        for (BankBatch b : batches.values()) {
            String keyBase = "AUDIT.BANK_TRANSFER." + businessDate.format(DATE_FMT) + "." + b.bankCode;
            out.add(csv(keyBase + ".COUNT", String.valueOf(b.rows.size()), businessDate.format(DATE_FMT), "99991231", updatedAt));
            out.add(csv(keyBase + ".AMOUNT", b.total.toPlainString(), businessDate.format(DATE_FMT), "99991231", updatedAt));
            out.add(csv(keyBase + ".HASH", b.digest, businessDate.format(DATE_FMT), "99991231", updatedAt));
            out.add(csv(keyBase + ".RECEIPT", b.receiptNo, businessDate.format(DATE_FMT), "99991231", updatedAt));
        }
        java.nio.file.Files.write(path, out, FILE_CHARSET);
    }

    private static void validatePayout(PayoutRecord r, int lineNo) {
        if (!r.payoutId.matches("[A-Za-z0-9_-]{1,20}")) {
            throw new IllegalArgumentException("PAYOUT-IDが不正です。行=" + lineNo);
        }
        if (!r.merchantCode.matches("[A-Za-z0-9]{3,12}")) {
            throw new IllegalArgumentException("MERCHANT-CODEが不正です。行=" + lineNo);
        }
        if (!digits(r.bankAcctNo).matches("[0-9]{10,14}")) {
            throw new IllegalArgumentException("BANK-ACCT-NOが不正です。行=" + lineNo);
        }
        if (r.payoutAmt.signum() <= 0 || r.payoutAmt.compareTo(MAX_PAYOUT_AMT) > 0) {
            throw new IllegalArgumentException("PAYOUT-AMTが範囲外です。行=" + lineNo);
        }
        if (r.payoutAmt.scale() > 0) {
            throw new IllegalArgumentException("PAYOUT-AMTに小数があります。行=" + lineNo);
        }
    }

    private static String bankCodeOf(String bankAcctNo) {
        String d = digits(bankAcctNo);
        return d.substring(0, 4);
    }

    private static String digits(String s) {
        return s == null ? "" : s.replaceAll("[^0-9]", "");
    }

    private static java.math.BigDecimal parseAmount(String s, int lineNo) {
        try {
            return new java.math.BigDecimal(s);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("PAYOUT-AMTが数値ではありません。行=" + lineNo);
        }
    }

    private static java.time.LocalDate parseDate(String s, String name, int lineNo) {
        try {
            return java.time.LocalDate.parse(s, DATE_FMT);
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException(name + "の日付形式が不正です。行=" + lineNo);
        }
    }

    private static String require(String s, String name, int lineNo) {
        if (s == null || s.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "が空です。行=" + lineNo);
        }
        return s.trim();
    }

    private static String[] splitCsv(String line, int size, String fileName, int lineNo) {
        java.util.List<String> values = new java.util.ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean quote = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                if (quote && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cur.append('"');
                    i++;
                } else {
                    quote = !quote;
                }
            } else if (c == ',' && !quote) {
                values.add(cur.toString());
                cur.setLength(0);
            } else {
                cur.append(c);
            }
        }
        values.add(cur.toString());
        if (quote || values.size() != size) {
            throw new IllegalArgumentException(fileName + "の項目数が不正です。行=" + lineNo);
        }
        return values.toArray(new String[0]);
    }

    private static String csv(String... values) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            String v = values[i] == null ? "" : values[i];
            if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
                b.append('"').append(v.replace("\"", "\"\"")).append('"');
            } else {
                b.append(v);
            }
        }
        return b.toString();
    }

    private static String padRight(String s, int len) {
        String v = s == null ? "" : s;
        return v.length() >= len ? v.substring(0, len) : v + repeat(' ', len - v.length());
    }

    private static String padLeft(String s, int len, char ch) {
        String v = s == null ? "" : s;
        return v.length() >= len ? v.substring(v.length() - len) : repeat(ch, len - v.length()) + v;
    }

    private static String repeat(char ch, int n) {
        StringBuilder b = new StringBuilder(n);
        for (int i = 0; i < n; i++) {
            b.append(ch);
        }
        return b.toString();
    }

    private static String sha256Hex(java.nio.file.Path path) throws java.io.IOException {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-256");
            byte[] buf = new byte[8192];
            try (java.io.InputStream in = java.nio.file.Files.newInputStream(path)) {
                int n;
                while ((n = in.read(buf)) >= 0) {
                    md.update(buf, 0, n);
                }
            }
            StringBuilder hex = new StringBuilder();
            for (byte b : md.digest()) {
                hex.append(String.format("%02x", b & 0xff));
            }
            return hex.toString();
        } catch (java.security.NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256を利用できません", e);
        }
    }

    private static String makeReceiptNo(String bankCode, java.time.LocalDate businessDate, String digest) {
        return bankCode + "-" + businessDate.format(DATE_FMT) + "-" + digest.substring(0, 10).toUpperCase(java.util.Locale.ROOT);
    }

    private static final class PayoutRecord {
        final String payoutId;
        final String merchantCode;
        final String bankAcctNo;
        final java.math.BigDecimal payoutAmt;
        final java.time.LocalDate payoutDt;
        final String bankResultCd;

        PayoutRecord(String payoutId, String merchantCode, String bankAcctNo,
                     java.math.BigDecimal payoutAmt, java.time.LocalDate payoutDt, String bankResultCd) {
            this.payoutId = payoutId;
            this.merchantCode = merchantCode;
            this.bankAcctNo = bankAcctNo;
            this.payoutAmt = payoutAmt;
            this.payoutDt = payoutDt;
            this.bankResultCd = bankResultCd == null ? "" : bankResultCd;
        }

        PayoutRecord withBankResultCd(String value) {
            return new PayoutRecord(payoutId, merchantCode, bankAcctNo, payoutAmt, payoutDt, value);
        }
    }

    private static final class ConfRecord {
        final String confKey;
        final String confValue;
        final java.time.LocalDate applyDt;
        final java.time.LocalDate expireDt;
        final String updatedAt;

        ConfRecord(String confKey, String confValue, java.time.LocalDate applyDt,
                   java.time.LocalDate expireDt, String updatedAt) {
            this.confKey = confKey;
            this.confValue = confValue;
            this.applyDt = applyDt;
            this.expireDt = expireDt;
            this.updatedAt = updatedAt;
        }
    }

    private static final class BankBatch {
        final String bankCode;
        final String format;
        final String fileName;
        final java.util.List<PayoutRecord> rows = new java.util.ArrayList<>();
        final java.util.List<String> lines = new java.util.ArrayList<>();
        java.math.BigDecimal total = java.math.BigDecimal.ZERO;
        String receiptNo = "";
        String digest = "";

        BankBatch(String bankCode, String format, java.time.LocalDate businessDate) {
            this.bankCode = bankCode;
            this.format = format;
            this.fileName = "BANK_" + bankCode + "_" + businessDate.format(DATE_FMT) + ".dat";
        }

        void add(PayoutRecord row, String line) {
            rows.add(row);
            lines.add(line);
            total = total.add(row.payoutAmt);
        }
    }
}
