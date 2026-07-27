package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.00  2024/03/15  保険金システムG  初版作成
 */
public class PaymentNotificationPrinter {
    private static final int PAGE_LINES = 66;
    private static final String STATUS_UNSENT = "10";
    private static final String STATUS_SENT = "20";
    private static final String REPORT_TYPE_PAYMENT_NOTICE = "PN";
    private static final String NOTICE_TYPE_PAYMENT = "01";
    @SuppressWarnings("unused")
    private static final Class<?> PINNED_SHARED_MODEL = ClaimModel.class;

    private static java.util.List<NoticeRow> readNotices(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = readDataLines(path);
        java.util.List<NoticeRow> rows = new java.util.ArrayList<>();
        for (String line : lines) {
            java.util.List<String> c = parseCsv(line);
            requireColumns(c, 6, "LFNTCF");
            rows.add(new NoticeRow(c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), c.get(5)));
        }
        return rows;
    }

    private static java.util.Map<String, BeneficiaryRow> readBeneficiaries(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, BeneficiaryRow> rows = new java.util.HashMap<>();
        for (String line : readDataLines(path)) {
            java.util.List<String> c = parseCsv(line);
            requireColumns(c, 8, "LFBENF");
            BeneficiaryRow row = new BeneficiaryRow(c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), c.get(5), c.get(6), parseInt(c.get(7), "PAYMENT-PRIORITY"));
            BeneficiaryRow old = rows.put(row.beneficiaryId, row);
            if (old != null && row.paymentPriority >= old.paymentPriority) {
                rows.put(row.beneficiaryId, old);
            }
        }
        return rows;
    }

    private static java.util.List<TransferRow> readTransfers(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<TransferRow> rows = new java.util.ArrayList<>();
        for (String line : readDataLines(path)) {
            java.util.List<String> c = parseCsv(line);
            requireColumns(c, 8, "LFXFRF");
            rows.add(new TransferRow(c.get(0), c.get(1), c.get(2), c.get(3), c.get(4), c.get(5), parseAmount(c.get(6)), c.get(7)));
        }
        return rows;
    }

    private static java.util.List<String> readDataLines(java.nio.file.Path path) throws java.io.IOException {
        if (!java.nio.file.Files.exists(path)) {
            return java.util.Collections.emptyList();
        }
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<String> data = new java.util.ArrayList<>();
        for (String line : lines) {
            if (line.trim().isEmpty()) {
                continue;
            }
            if (line.startsWith("NOTICE-ID,") || line.startsWith("POL-NO,") || line.startsWith("TRANSFER-ID,")) {
                continue;
            }
            data.add(line);
        }
        return data;
    }

    private static void writeNotices(java.nio.file.Path path, java.util.List<NoticeRow> notices) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add("NOTICE-ID,CLAIM-ID,BENEFICIARY-ID,NOTICE-DT,NOTICE-TYPE-KBN,STATUS-KBN");
        for (NoticeRow notice : notices) {
            lines.add(toCsv(notice.noticeId, notice.claimId, notice.beneficiaryId, notice.noticeDt, notice.noticeTypeKbn, notice.statusKbn));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING);
    }

    private static void validateNotice(NoticeRow notice) {
        requireValue(notice.noticeId, "NOTICE-ID");
        requireValue(notice.claimId, "CLAIM-ID");
        requireValue(notice.beneficiaryId, "BENEFICIARY-ID");
        requireDate(notice.noticeDt, "NOTICE-DT");
        if (!NOTICE_TYPE_PAYMENT.equals(notice.noticeTypeKbn)) {
            throw new IllegalStateException("通知種別対象外");
        }
    }

    private static TransferRow findTransfer(NoticeRow notice, BeneficiaryRow beneficiary, java.util.List<TransferRow> transfers) {
        TransferRow selected = null;
        for (TransferRow transfer : transfers) {
            boolean sameAccount = beneficiary.bankCd.equals(transfer.bankCd)
                    && beneficiary.branchCd.equals(transfer.branchCd)
                    && beneficiary.acctNo.equals(transfer.acctNo);
            boolean sameClaim = notice.claimId.equals(transfer.payId);
            if (sameAccount && sameClaim) {
                if (selected == null || transfer.transferDt.compareTo(selected.transferDt) > 0) {
                    selected = transfer;
                }
            }
        }
        if (selected == null) {
            throw new IllegalStateException("振込照合不能");
        }
        if (selected.amount.signum() <= 0) {
            throw new IllegalStateException("振込金額不正");
        }
        requireDate(selected.transferDt, "TRANSFER-DT");
        return selected;
    }

    private static java.util.List<String> buildPaymentNoticePage(NoticeRow notice, BeneficiaryRow beneficiary, TransferRow transfer, String outputDate) {
        java.util.List<String> lines = new java.util.ArrayList<>(PAGE_LINES);
        java.text.DecimalFormat yen = new java.text.DecimalFormat("#,##0");
        String maskedAccount = maskAccount(beneficiary.acctNo);

        lines.add(center("保険金支払通知書", 80));
        lines.add("");
        lines.add("発行日 " + formatDate(outputDate) + "                                 通知番号 " + notice.noticeId);
        lines.add("");
        lines.add(beneficiary.nameKana + " 様");
        lines.add("");
        lines.add("下記のとおり保険金をお支払いしましたので通知いたします。");
        lines.add("");
        lines.add("請求番号        " + notice.claimId);
        lines.add("受取人番号      " + notice.beneficiaryId);
        lines.add("続柄区分        " + beneficiary.relationshipKbn);
        lines.add("支払日          " + formatDate(transfer.transferDt));
        lines.add("支払金額        " + yen.format(transfer.amount) + " 円");
        lines.add("");
        lines.add("振込先金融機関  " + beneficiary.bankCd + "-" + beneficiary.branchCd);
        lines.add("口座番号        " + maskedAccount);
        lines.add("口座名義        " + transfer.acctHolderKna);
        lines.add("");
        lines.add("この通知は保険金支払処理に基づき作成されています。");
        lines.add("内容に相違がある場合は取扱窓口へお申し出ください。");

        java.math.BigDecimal taxBase = transfer.amount;
        java.math.BigDecimal withholding = taxBase.multiply(new java.math.BigDecimal("0.000")).setScale(0, java.math.RoundingMode.DOWN);
        java.math.BigDecimal netAmount = taxBase.subtract(withholding);
        lines.add("");
        lines.add("支払内訳");
        lines.add("  保険金額      " + yen.format(taxBase) + " 円");
        lines.add("  控除額        " + yen.format(withholding) + " 円");
        lines.add("  差引支払額    " + yen.format(netAmount) + " 円");
        lines.add("");
        lines.add("会社使用欄");
        lines.add("  証券番号      " + beneficiary.polNo);
        lines.add("  振込管理番号  " + transfer.transferId);
        lines.add("  作成日        " + formatDate(outputDate));

        while (lines.size() < PAGE_LINES - 3) {
            lines.add("");
        }
        lines.add("未来生命保険株式会社");
        lines.add("保険金支払部");
        lines.add("頁 1");
        return lines;
    }

    private static void requireColumns(java.util.List<String> columns, int size, String fileName) {
        if (columns.size() < size) {
            throw new IllegalStateException(fileName + " 項目数不足");
        }
    }

    private static void requireValue(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalStateException(name + " 未設定");
        }
    }

    private static void requireDate(String value, String name) {
        requireValue(value, name);
        try {
            java.time.LocalDate.parse(value, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        } catch (java.time.format.DateTimeParseException ex) {
            throw new IllegalStateException(name + " 日付不正");
        }
    }

    private static int parseInt(String value, String name) {
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException ex) {
            throw new IllegalStateException(name + " 数値不正");
        }
    }

    private static java.math.BigDecimal parseAmount(String value) {
        try {
            return new java.math.BigDecimal(value.trim());
        } catch (NumberFormatException ex) {
            throw new IllegalStateException("AMOUNT 数値不正");
        }
    }

    private static String formatDate(String yyyymmdd) {
        if (yyyymmdd == null || yyyymmdd.length() != 8) {
            return yyyymmdd;
        }
        return yyyymmdd.substring(0, 4) + "年" + yyyymmdd.substring(4, 6) + "月" + yyyymmdd.substring(6, 8) + "日";
    }

    private static String maskAccount(String acctNo) {
        if (acctNo == null || acctNo.length() <= 3) {
            return "***";
        }
        return repeat("*", acctNo.length() - 3) + acctNo.substring(acctNo.length() - 3);
    }

    private static String center(String value, int width) {
        int pad = Math.max(0, (width - value.length()) / 2);
        return repeat(" ", pad) + value;
    }

    private static String repeat(String value, int count) {
        StringBuilder b = new StringBuilder(value.length() * Math.max(0, count));
        for (int i = 0; i < count; i++) {
            b.append(value);
        }
        return b.toString();
    }

    private static java.util.List<String> parseCsv(String line) {
        java.util.List<String> cols = new java.util.ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cur.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                cols.add(cur.toString().trim());
                cur.setLength(0);
            } else {
                cur.append(ch);
            }
        }
        cols.add(cur.toString().trim());
        return cols;
    }

    private static String toCsv(String... values) {
        java.util.List<String> escaped = new java.util.ArrayList<>(values.length);
        for (String value : values) {
            String v = value == null ? "" : value;
            if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0) {
                escaped.add("\"" + v.replace("\"", "\"\"") + "\"");
            } else {
                escaped.add(v);
            }
        }
        return String.join(",", escaped);
    }

    private static final class NoticeRow {
        final String noticeId;
        final String claimId;
        final String beneficiaryId;
        final String noticeDt;
        final String noticeTypeKbn;
        final String statusKbn;

        NoticeRow(String noticeId, String claimId, String beneficiaryId, String noticeDt, String noticeTypeKbn, String statusKbn) {
            this.noticeId = noticeId;
            this.claimId = claimId;
            this.beneficiaryId = beneficiaryId;
            this.noticeDt = noticeDt;
            this.noticeTypeKbn = noticeTypeKbn;
            this.statusKbn = statusKbn;
        }

        NoticeRow withStatus(String newStatusKbn) {
            return new NoticeRow(noticeId, claimId, beneficiaryId, noticeDt, noticeTypeKbn, newStatusKbn);
        }
    }

    private static final class BeneficiaryRow {
        final String polNo;
        final String beneficiaryId;
        final String nameKana;
        final String relationshipKbn;
        final String bankCd;
        final String branchCd;
        final String acctNo;
        final int paymentPriority;

        BeneficiaryRow(String polNo, String beneficiaryId, String nameKana, String relationshipKbn, String bankCd, String branchCd, String acctNo, int paymentPriority) {
            this.polNo = polNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.paymentPriority = paymentPriority;
        }
    }

    private static final class TransferRow {
        final String transferId;
        final String payId;
        final String bankCd;
        final String branchCd;
        final String acctNo;
        final String acctHolderKna;
        final java.math.BigDecimal amount;
        final String transferDt;

        TransferRow(String transferId, String payId, String bankCd, String branchCd, String acctNo, String acctHolderKna, java.math.BigDecimal amount, String transferDt) {
            this.transferId = transferId;
            this.payId = payId;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.acctHolderKna = acctHolderKna;
            this.amount = amount;
            this.transferDt = transferDt;
        }
    }
}
