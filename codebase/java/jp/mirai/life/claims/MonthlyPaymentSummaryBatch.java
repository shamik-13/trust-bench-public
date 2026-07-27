package jp.mirai.life.claims;

/*
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.0   2024-03-15  保険金システムG  初版作成
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

public class MonthlyPaymentSummaryBatch {
    private static final Charset FILE_CHARSET = StandardCharsets.UTF_8;
    private static final String REPORT_TYPE_KBN_CONTROL_TOTAL = "03";
    private static final String CLAIM_STATUS_SETTLED = "01";
    private static final String CLAIM_STATUS_PENDING = "05";
    private static final BigDecimal ONE_YEAR_OR_MORE_PAYOUT_RATE = new BigDecimal("1.0000");
    private static final DateTimeFormatter OUTPUT_DATE_FORMAT = DateTimeFormatter.BASIC_ISO_DATE;

    private MonthlyPaymentSummaryBatch() {
    }

    private int execute(String[] args) {
        try {
            BatchParameter parameter = BatchParameter.parse(args);
            List<PaymentRecord> payments = readPayments(parameter.lfpayf);
            Map<String, ClaimRecord> claims = readClaims(parameter.lfclmf);
            List<MonthlySummaryRecord> summaries = readMonthlySummaries(parameter.lfmstf);

            MonthlyWork work = aggregate(parameter.yearMonth, payments, claims);
            MonthlySummaryRecord masterTotal = findMasterTotal(parameter.yearMonth, summaries);
            List<String> varianceLines = compareWithMaster(parameter.yearMonth, work, masterTotal);

            writeVariance(parameter.varianceFile, varianceLines);
            writeReport(parameter.lfrepmf, parameter.yearMonth, work);
            System.err.println("月次支払集計バッチ 正常終了 対象年月=" + parameter.yearMonth);
            return 0;
        } catch (BatchException e) {
            System.err.println("月次支払集計バッチ 異常終了 " + e.getMessage());
            return 8;
        } catch (IOException | RuntimeException e) {
            System.err.println("月次支払集計バッチ 異常終了 " + e.getClass().getSimpleName() + ":" + e.getMessage());
            return 12;
        }
    }

    private MonthlyWork aggregate(YearMonth targetMonth, List<PaymentRecord> payments, Map<String, ClaimRecord> claims) {
        MonthlyWork work = new MonthlyWork();
        for (PaymentRecord payment : payments) {
            ClaimRecord claim = claims.get(payment.claimId);
            if (claim == null) {
                throw new BatchException("請求マスタ未登録 CLAIM-ID=" + payment.claimId);
            }
            if (!YearMonth.from(payment.outputDate).equals(targetMonth)) {
                continue;
            }

            work.totalClaims++;
            work.totalGross = work.totalGross.add(payment.grossAmount);
            work.totalNet = work.totalNet.add(payment.payoutAmount);
            work.minPayout = work.minPayout == null ? payment.payoutAmount : work.minPayout.min(payment.payoutAmount);
            work.maxPayout = work.maxPayout == null ? payment.payoutAmount : work.maxPayout.max(payment.payoutAmount);

            if (CLAIM_STATUS_SETTLED.equals(claim.claimStatusKbn)) {
                work.settledCount++;
            } else if (CLAIM_STATUS_PENDING.equals(claim.claimStatusKbn)) {
                work.pendingCount++;
            }

            validatePaymentRule(payment, claim);
        }
        return work;
    }

    private void validatePaymentRule(PaymentRecord payment, ClaimRecord claim) {
        if (payment.grossAmount.signum() < 0 || payment.payoutAmount.signum() < 0) {
            throw new BatchException("金額符号不正 PAY-ID=" + payment.payId);
        }
        if (payment.payoutAmount.compareTo(payment.grossAmount) > 0) {
            throw new BatchException("支払額超過 PAY-ID=" + payment.payId);
        }
        long elapsedDays = java.time.temporal.ChronoUnit.DAYS.between(claim.respStartDate, claim.eventDate);
        if (elapsedDays >= 365) {
            BigDecimal expected = payment.grossAmount.multiply(ONE_YEAR_OR_MORE_PAYOUT_RATE).setScale(0, RoundingMode.DOWN);
            if (payment.payoutAmount.compareTo(expected) > 0) {
                throw new BatchException("一年以上支払割合超過 PAY-ID=" + payment.payId);
            }
        }
    }

    private MonthlySummaryRecord findMasterTotal(YearMonth yearMonth, List<MonthlySummaryRecord> summaries) {
        return summaries.stream()
                .filter(s -> yearMonth.equals(s.yearMonth))
                .max(Comparator.comparing(s -> "TOTAL".equals(s.categoryKbn) ? 1 : 0))
                .orElseThrow(() -> new BatchException("月次集計ファイル未登録 YEAR-MONTH=" + yearMonth));
    }

    private List<String> compareWithMaster(YearMonth yearMonth, MonthlyWork work, MonthlySummaryRecord master) {
        List<String> lines = new ArrayList<>();
        long countDiff = work.totalClaims - master.count;
        BigDecimal payoutDiff = work.totalNet.subtract(master.totalPayoutAmount);
        if (countDiff != 0 || payoutDiff.compareTo(BigDecimal.ZERO) != 0) {
            lines.add(csv("VAR-" + yearMonth, yearMonth.toString(), "COUNT", String.valueOf(master.count), String.valueOf(work.totalClaims), String.valueOf(countDiff)));
            lines.add(csv("VAR-" + yearMonth, yearMonth.toString(), "PAYOUT-AMT", yen(master.totalPayoutAmount), yen(work.totalNet), yen(payoutDiff)));
        }
        return lines;
    }

    private void writeReport(Path output, YearMonth yearMonth, MonthlyWork work) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(output, FILE_CHARSET)) {
            LocalDate outputDate = LocalDate.now();
            int pageNo = 1;
            String reportId = "MP-" + yearMonth;
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "対象年月=" + yearMonth));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "総請求件数=" + work.totalClaims));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "支払対象件数=" + work.settledCount));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "査定中件数=" + work.pendingCount));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "総グロス=" + yen(work.totalGross)));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "総ネット=" + yen(work.totalNet)));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "平均ネット=" + yen(work.averageNet())));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "最小支払額=" + yen(work.minPayout())));
            writer.newLine();
            writer.write(csv(reportId, REPORT_TYPE_KBN_CONTROL_TOTAL, outputDate.format(OUTPUT_DATE_FORMAT), String.valueOf(pageNo), "最大支払額=" + yen(work.maxPayout())));
            writer.newLine();
        }
    }

    private void writeVariance(Path output, List<String> lines) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(output, FILE_CHARSET)) {
            for (String line : lines) {
                writer.write(line);
                writer.newLine();
            }
        }
    }

    private List<PaymentRecord> readPayments(Path path) throws IOException {
        List<PaymentRecord> records = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(path, FILE_CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (skip(line, lineNo)) {
                    continue;
                }
                List<String> cols = splitCsv(line);
                requireSize(path, lineNo, cols, 6);
                records.add(new PaymentRecord(cols.get(0), cols.get(1), money(cols.get(2)), decimal(cols.get(3)), money(cols.get(4)), date(cols.get(5))));
            }
        }
        return records;
    }

    private Map<String, ClaimRecord> readClaims(Path path) throws IOException {
        Map<String, ClaimRecord> records = new HashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(path, FILE_CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (skip(line, lineNo)) {
                    continue;
                }
                List<String> cols = splitCsv(line);
                requireSize(path, lineNo, cols, 7);
                ClaimRecord record = new ClaimRecord(cols.get(0), cols.get(1), money(cols.get(2)), money(cols.get(3)), date(cols.get(4)), date(cols.get(5)), cols.get(6));
                records.put(record.claimId, record);
            }
        }
        return records;
    }

    private List<MonthlySummaryRecord> readMonthlySummaries(Path path) throws IOException {
        List<MonthlySummaryRecord> records = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(path, FILE_CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (skip(line, lineNo)) {
                    continue;
                }
                List<String> cols = splitCsv(line);
                requireSize(path, lineNo, cols, 6);
                records.add(new MonthlySummaryRecord(YearMonth.parse(cols.get(0)), cols.get(1), Long.parseLong(cols.get(2)), money(cols.get(3)), money(cols.get(4)), decimal(cols.get(5))));
            }
        }
        return records;
    }

    private static boolean skip(String line, int lineNo) {
        return line.trim().isEmpty() || lineNo == 1 && line.toUpperCase(Locale.ROOT).contains("CLAIM-ID");
    }

    private static void requireSize(Path path, int lineNo, List<String> cols, int size) {
        if (cols.size() < size) {
            throw new BatchException("項目数不足 FILE=" + path + " LINE=" + lineNo);
        }
    }

    private static LocalDate date(String value) {
        return LocalDate.parse(value.trim());
    }

    private static BigDecimal money(String value) {
        return new BigDecimal(value.trim()).setScale(0, RoundingMode.UNNECESSARY);
    }

    private static BigDecimal decimal(String value) {
        return new BigDecimal(value.trim());
    }

    private static String yen(BigDecimal amount) {
        return amount.setScale(0, RoundingMode.HALF_UP).toPlainString() + "円";
    }

    private static String csv(String... values) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                sb.append(',');
            }
            String value = Objects.toString(values[i], "");
            if (value.indexOf(',') >= 0 || value.indexOf('"') >= 0 || value.indexOf('\n') >= 0) {
                sb.append('"').append(value.replace("\"", "\"\"")).append('"');
            } else {
                sb.append(value);
            }
        }
        return sb.toString();
    }

    private static List<String> splitCsv(String line) {
        List<String> cols = new ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (quoted) {
                if (ch == '"' && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cur.append('"');
                    i++;
                } else if (ch == '"') {
                    quoted = false;
                } else {
                    cur.append(ch);
                }
            } else if (ch == ',') {
                cols.add(cur.toString().trim());
                cur.setLength(0);
            } else if (ch == '"') {
                quoted = true;
            } else {
                cur.append(ch);
            }
        }
        cols.add(cur.toString().trim());
        return cols;
    }

    private static final class BatchParameter {
        private final YearMonth yearMonth;
        private final Path lfpayf;
        private final Path lfclmf;
        private final Path lfmstf;
        private final Path lfrepmf;
        private final Path varianceFile;

        private BatchParameter(YearMonth yearMonth, Path lfpayf, Path lfclmf, Path lfmstf, Path lfrepmf, Path varianceFile) {
            this.yearMonth = yearMonth;
            this.lfpayf = lfpayf;
            this.lfclmf = lfclmf;
            this.lfmstf = lfmstf;
            this.lfrepmf = lfrepmf;
            this.varianceFile = varianceFile;
        }

        private static BatchParameter parse(String[] args) {
            if (args.length != 6) {
                throw new BatchException("起動引数不正 YEAR-MONTH LFPAYF LFCLMF LFMSTF LFREPMF VARIANCE");
            }
            return new BatchParameter(YearMonth.parse(args[0]), Path.of(args[1]), Path.of(args[2]), Path.of(args[3]), Path.of(args[4]), Path.of(args[5]));
        }
    }

    private static final class MonthlyWork {
        private long totalClaims;
        private long settledCount;
        private long pendingCount;
        private BigDecimal totalGross = BigDecimal.ZERO;
        private BigDecimal totalNet = BigDecimal.ZERO;
        private BigDecimal minPayout;
        private BigDecimal maxPayout;

        private BigDecimal averageNet() {
            if (totalClaims == 0) {
                return BigDecimal.ZERO;
            }
            return totalNet.divide(BigDecimal.valueOf(totalClaims), 0, RoundingMode.HALF_UP);
        }

        private BigDecimal minPayout() {
            return minPayout == null ? BigDecimal.ZERO : minPayout;
        }

        private BigDecimal maxPayout() {
            return maxPayout == null ? BigDecimal.ZERO : maxPayout;
        }
    }

    private static final class PaymentRecord {
        private final String payId;
        private final String claimId;
        private final BigDecimal grossAmount;
        private final BigDecimal reductionRate;
        private final BigDecimal payoutAmount;
        private final LocalDate outputDate;

        private PaymentRecord(String payId, String claimId, BigDecimal grossAmount, BigDecimal reductionRate, BigDecimal payoutAmount, LocalDate outputDate) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmount = grossAmount;
            this.reductionRate = reductionRate;
            this.payoutAmount = payoutAmount;
            this.outputDate = outputDate;
        }
    }

    private static final class ClaimRecord {
        private final String claimId;
        private final String polNo;
        private final BigDecimal sumAssuredAmount;
        private final BigDecimal loanBalanceAmount;
        private final LocalDate respStartDate;
        private final LocalDate eventDate;
        private final String claimStatusKbn;

        private ClaimRecord(String claimId, String polNo, BigDecimal sumAssuredAmount, BigDecimal loanBalanceAmount, LocalDate respStartDate, LocalDate eventDate, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmount = sumAssuredAmount;
            this.loanBalanceAmount = loanBalanceAmount;
            this.respStartDate = respStartDate;
            this.eventDate = eventDate;
            this.claimStatusKbn = claimStatusKbn;
        }
    }

    private static final class MonthlySummaryRecord {
        private final YearMonth yearMonth;
        private final String categoryKbn;
        private final long count;
        private final BigDecimal totalGrossAmount;
        private final BigDecimal totalPayoutAmount;
        private final BigDecimal avgReductionRate;

        private MonthlySummaryRecord(YearMonth yearMonth, String categoryKbn, long count, BigDecimal totalGrossAmount, BigDecimal totalPayoutAmount, BigDecimal avgReductionRate) {
            this.yearMonth = yearMonth;
            this.categoryKbn = categoryKbn;
            this.count = count;
            this.totalGrossAmount = totalGrossAmount;
            this.totalPayoutAmount = totalPayoutAmount;
            this.avgReductionRate = avgReductionRate;
        }
    }

    private static final class BatchException extends RuntimeException {
        private BatchException(String message) {
            super(message);
        }
    }
}
