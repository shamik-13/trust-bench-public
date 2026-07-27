package jp.mirai.pay.settlement;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2024/08/06  加盟店精算チーム    初版作成
 * 1.1   2025/01/29  加盟店精算チーム    手数料は格納値を集計する方式に変更
 */
public class SettlementDetailReportService {

    private static final java.nio.charset.Charset CHARSET = java.nio.charset.StandardCharsets.UTF_8;
    private static final String STATUS_SETTLEABLE = "01";

    public static void main(String[] args) throws Exception {
        if (args.length != 11) {
            System.err.println("引数不正: PSSETF PSDTLF PSMERF PSRPTF 加盟店コード 期間FROM 期間TO 精算ID ページ番号 ページサイズ 出力区分");
            System.exit(2);
        }

        java.nio.file.Path pssetf = java.nio.file.Paths.get(args[0]);
        java.nio.file.Path psdtlf = java.nio.file.Paths.get(args[1]);
        java.nio.file.Path psmerf = java.nio.file.Paths.get(args[2]);
        java.nio.file.Path psrptf = java.nio.file.Paths.get(args[3]);
        String merchantCode = args[4].trim();
        java.time.LocalDate periodFrom = parseDate(args[5], "期間FROM");
        java.time.LocalDate periodTo = parseDate(args[6], "期間TO");
        String settleId = args[7].trim();
        int pageNo = parsePositiveInt(args[8], "ページ番号");
        int pageSize = parsePositiveInt(args[9], "ページサイズ");
        String reportKbn = args[10].trim();

        if (!"CSV".equals(reportKbn) && !"PDF".equals(reportKbn)) {
            throw new IllegalArgumentException("出力区分不正: CSV または PDF を指定してください");
        }
        if (periodFrom.isAfter(periodTo)) {
            throw new IllegalArgumentException("期間指定不正: FROM が TO を超過しています");
        }
        if (merchantCode.isEmpty() || settleId.isEmpty()) {
            throw new IllegalArgumentException("必須項目不正: 加盟店コードまたは精算IDが未指定です");
        }

        java.util.Map<String, Merchant> merchants = loadMerchants(psmerf);
        Merchant merchant = merchants.get(merchantCode);
        if (merchant == null) {
            throw new SecurityException("加盟店権限不正: 加盟店が存在しません");
        }
        if (!STATUS_SETTLEABLE.equals(merchant.status)) {
            throw new SecurityException("加盟店権限不正: 精算対象外の加盟店です");
        }

        java.util.List<Settlement> settlements = loadSettlements(pssetf);
        Settlement target = null;
        for (Settlement settlement : settlements) {
            if (settlement.settleId.equals(settleId)) {
                if (!settlement.merchantCode.equals(merchantCode)) {
                    throw new SecurityException("加盟店権限不正: 他加盟店の精算明細参照は拒否されました");
                }
                if (!settlement.settleDate.isBefore(periodFrom) && !settlement.settleDate.isAfter(periodTo)) {
                    target = settlement;
                }
            }
        }
        if (target == null) {
            throw new IllegalArgumentException("照会対象なし: 指定条件に一致する精算がありません");
        }

        java.util.List<Detail> details = loadDetails(psdtlf, settleId, merchantCode);
        details.sort(java.util.Comparator.comparing(d -> d.detailId));

        java.math.BigDecimal totalTxn = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalCharge = java.math.BigDecimal.ZERO;
        for (Detail detail : details) {
            totalTxn = totalTxn.add(detail.txnAmount);
            totalCharge = totalCharge.add(detail.chargeAmount);
        }

        // 手数料は精算ヘッダ（PSSETF）と明細（PSDTLF）に格納済みの値を用いる。
        // ここで再計算は行わず、明細合計と精算ヘッダ値の整合のみ確認する。
        if (target.chargeAmount.compareTo(totalCharge) != 0) {
            System.err.println("警告: 明細手数料合計が精算ヘッダの手数料と一致しません");
        }

        int fromIndex = Math.min((pageNo - 1) * pageSize, details.size());
        int toIndex = Math.min(fromIndex + pageSize, details.size());
        java.util.List<Detail> page = details.subList(fromIndex, toIndex);

        System.out.println("精算ID,加盟店コード,加盟店名,精算日,明細件数,取引金額合計,手数料合計,支払金額");
        System.out.println(csv(target.settleId) + "," + csv(target.merchantCode) + "," + csv(merchant.name) + ","
                + target.settleDate + "," + details.size() + "," + totalTxn.toPlainString() + ","
                + totalCharge.toPlainString() + "," + target.payoutAmount.toPlainString());
        System.out.println("明細ID,取引ID,取引金額,手数料,取引区分");
        for (Detail detail : page) {
            System.out.println(csv(detail.detailId) + "," + csv(detail.txnId) + ","
                    + detail.txnAmount.toPlainString() + "," + detail.chargeAmount.toPlainString()
                    + "," + csv(detail.lineKbn));
        }

        String reportId = "RPT" + java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
                .format(java.time.LocalDateTime.now()) + String.format("%04d", Math.abs(settleId.hashCode()) % 10000);
        String outputPath = "report/" + merchantCode + "/" + reportId + "." + reportKbn.toLowerCase(java.util.Locale.ROOT);
        String line = String.join(",",
                csv(reportId),
                csv(merchantCode),
                csv(reportKbn),
                periodFrom.toString(),
                periodTo.toString(),
                csv(outputPath),
                "受付済") + System.lineSeparator();

        java.nio.file.Files.write(psrptf, line.getBytes(CHARSET),
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.APPEND);
    }

    private static java.util.Map<String, Merchant> loadMerchants(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, Merchant> result = new java.util.HashMap<>();
        for (String line : java.nio.file.Files.readAllLines(path, CHARSET)) {
            if (isSkippable(line, "MERCHANT-CODE")) {
                continue;
            }
            java.util.List<String> row = parseCsv(line);
            requireColumn(row, 4, "PSMERF");
            Merchant merchant = new Merchant(row.get(0), row.get(1), row.get(2), row.get(3));
            result.put(merchant.code, merchant);
        }
        return result;
    }

    private static java.util.List<Settlement> loadSettlements(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Settlement> result = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(path, CHARSET)) {
            if (isSkippable(line, "SETTLE-ID")) {
                continue;
            }
            java.util.List<String> row = parseCsv(line);
            requireColumn(row, 6, "PSSETF");
            result.add(new Settlement(
                    row.get(0),
                    row.get(1),
                    parseAmount(row.get(2), "NET-AMT"),
                    parseAmount(row.get(3), "CHARGE-AMT"),
                    parseAmount(row.get(4), "PAYOUT-AMT"),
                    parseDate(row.get(5), "SETTLE-DT")));
        }
        return result;
    }

    private static java.util.List<Detail> loadDetails(java.nio.file.Path path, String settleId, String merchantCode)
            throws java.io.IOException {
        java.util.List<Detail> result = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(path, CHARSET)) {
            if (isSkippable(line, "DETAIL-ID")) {
                continue;
            }
            java.util.List<String> row = parseCsv(line);
            requireColumn(row, 7, "PSDTLF");
            if (!row.get(1).equals(settleId)) {
                continue;
            }
            if (!row.get(2).equals(merchantCode)) {
                throw new SecurityException("加盟店権限不正: 明細ファイルに他加盟店の精算IDが含まれています");
            }
            String lineKbn = row.get(6);
            if (!"C".equals(lineKbn) && !"R".equals(lineKbn)) {
                throw new IllegalArgumentException("取引区分不正: " + lineKbn);
            }
            result.add(new Detail(
                    row.get(0),
                    row.get(1),
                    row.get(2),
                    row.get(3),
                    parseAmount(row.get(4), "TXN-AMT"),
                    parseAmount(row.get(5), "CHARGE-AMT"),
                    lineKbn));
        }
        return result;
    }

    private static boolean isSkippable(String line, String headerToken) {
        String trimmed = line == null ? "" : line.trim();
        return trimmed.isEmpty() || trimmed.startsWith("#") || trimmed.startsWith(headerToken);
    }

    private static void requireColumn(java.util.List<String> row, int count, String fileName) {
        if (row.size() != count) {
            throw new IllegalArgumentException(fileName + " 項目数不正: " + row.size());
        }
    }

    private static java.time.LocalDate parseDate(String value, String name) {
        try {
            return java.time.LocalDate.parse(value.trim());
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException(name + " 日付不正: " + value, e);
        }
    }

    private static int parsePositiveInt(String value, String name) {
        try {
            int parsed = Integer.parseInt(value.trim());
            if (parsed <= 0) {
                throw new IllegalArgumentException(name + " は1以上で指定してください");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " 数値不正: " + value, e);
        }
    }

    private static java.math.BigDecimal parseAmount(String value, String name) {
        try {
            return new java.math.BigDecimal(value.trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " 金額不正: " + value, e);
        }
    }

    private static java.util.List<String> parseCsv(String line) {
        java.util.List<String> values = new java.util.ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    current.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                values.add(current.toString().trim());
                current.setLength(0);
            } else {
                current.append(ch);
            }
        }
        if (quoted) {
            throw new IllegalArgumentException("CSV引用符不正: " + line);
        }
        values.add(current.toString().trim());
        return values;
    }

    private static String csv(String value) {
        String safe = value == null ? "" : value;
        if (safe.indexOf(',') >= 0 || safe.indexOf('"') >= 0 || safe.indexOf('\n') >= 0 || safe.indexOf('\r') >= 0) {
            return "\"" + safe.replace("\"", "\"\"") + "\"";
        }
        return safe;
    }

    private static final class Merchant {
        final String code;
        final String name;
        final String status;
        final String bankAccountNo;

        Merchant(String code, String name, String status, String bankAccountNo) {
            this.code = code;
            this.name = name;
            this.status = status;
            this.bankAccountNo = bankAccountNo;
        }
    }

    private static final class Settlement {
        final String settleId;
        final String merchantCode;
        final java.math.BigDecimal netAmount;
        final java.math.BigDecimal chargeAmount;
        final java.math.BigDecimal payoutAmount;
        final java.time.LocalDate settleDate;

        Settlement(String settleId, String merchantCode, java.math.BigDecimal netAmount,
                   java.math.BigDecimal chargeAmount, java.math.BigDecimal payoutAmount,
                   java.time.LocalDate settleDate) {
            this.settleId = settleId;
            this.merchantCode = merchantCode;
            this.netAmount = netAmount;
            this.chargeAmount = chargeAmount;
            this.payoutAmount = payoutAmount;
            this.settleDate = settleDate;
        }
    }

    private static final class Detail {
        final String detailId;
        final String settleId;
        final String merchantCode;
        final String txnId;
        final java.math.BigDecimal txnAmount;
        final java.math.BigDecimal chargeAmount;
        final String lineKbn;

        Detail(String detailId, String settleId, String merchantCode, String txnId,
               java.math.BigDecimal txnAmount, java.math.BigDecimal chargeAmount, String lineKbn) {
            this.detailId = detailId;
            this.settleId = settleId;
            this.merchantCode = merchantCode;
            this.txnId = txnId;
            this.txnAmount = txnAmount;
            this.chargeAmount = chargeAmount;
            this.lineKbn = lineKbn;
        }
    }
}
