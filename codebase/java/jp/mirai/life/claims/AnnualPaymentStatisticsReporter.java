package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-03-15  保険金システムG  年次支払統計レポーター初版
 */
public class AnnualPaymentStatisticsReporter {
    private static final String REPORT_TYPE_KBN = "21";
    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final java.math.BigDecimal ONE_YEAR_OR_MORE_PAYMENT_RATE = new java.math.BigDecimal("1.00");
    private static final java.math.BigDecimal ONE_HUNDRED = new java.math.BigDecimal("100");
    private static final java.math.MathContext MC = new java.math.MathContext(18, java.math.RoundingMode.HALF_UP);

    public java.util.List<ReportLine> report(int year, java.time.LocalDate outputDate,
            java.util.List<Payment> payments, java.util.List<Assessment> assessments,
            java.util.List<Claim> claims, java.util.List<MonthlySnapshot> snapshots) {
        return buildReport(year, outputDate, payments, assessments, claims, snapshots);
    }

    private static java.util.List<ReportLine> buildReport(int year, java.time.LocalDate outputDate,
            java.util.List<Payment> payments, java.util.List<Assessment> assessments,
            java.util.List<Claim> claims, java.util.List<MonthlySnapshot> priorSnapshots) {
        java.util.Map<String, Assessment> assessmentByClaim = latestAssessmentByClaim(assessments);
        java.util.Map<String, Claim> claimById = new java.util.HashMap<String, Claim>();
        for (Claim claim : claims) {
            claimById.put(claim.claimId, claim);
        }

        MonthStat[] monthStats = new MonthStat[12];
        for (int i = 0; i < monthStats.length; i++) {
            monthStats[i] = new MonthStat();
        }

        java.util.Map<String, CategoryStat> categoryStats = new java.util.TreeMap<String, CategoryStat>();
        int[] reductionHistogram = new int[5];
        java.util.List<java.math.BigDecimal> amounts = new java.util.ArrayList<java.math.BigDecimal>();

        for (Payment payment : payments) {
            Claim claim = claimById.get(payment.claimId);
            Assessment assessment = assessmentByClaim.get(payment.claimId);
            if (claim == null || assessment == null) {
                continue;
            }
            if (!CLAIM_STATUS_PAYABLE.equals(claim.claimStatusKbn)) {
                continue;
            }
            if (assessment.assessDt.getYear() != year) {
                continue;
            }

            int monthIndex = assessment.assessDt.getMonthValue() - 1;
            monthStats[monthIndex].add(payment.payoutAmt);
            String category = assessment.categoryKbn;
            CategoryStat categoryStat = categoryStats.get(category);
            if (categoryStat == null) {
                categoryStat = new CategoryStat();
                categoryStats.put(category, categoryStat);
            }
            categoryStat.add(payment.grossAmt, payment.payoutAmt, payment.reductionRate);
            reductionHistogram[bucket(payment.reductionRate)]++;
            amounts.add(payment.payoutAmt);
        }

        java.math.BigDecimal mean = mean(amounts);
        java.math.BigDecimal sigma = standardDeviation(amounts, mean);
        java.math.BigDecimal outlierLimit = mean.add(sigma.multiply(new java.math.BigDecimal("3"), MC), MC);
        java.util.List<Payment> outliers = new java.util.ArrayList<Payment>();
        for (Payment payment : payments) {
            if (payment.payoutAmt.compareTo(outlierLimit) > 0) {
                outliers.add(payment);
            }
        }
        outliers.sort(new java.util.Comparator<Payment>() {
            public int compare(Payment x, Payment y) {
                return y.payoutAmt.compareTo(x.payoutAmt);
            }
        });

        java.util.Map<String, MonthlySnapshot> priorByMonthCategory = new java.util.HashMap<String, MonthlySnapshot>();
        for (MonthlySnapshot s : priorSnapshots) {
            priorByMonthCategory.put(s.yearMonth + "|" + s.categoryKbn, s);
        }

        java.util.List<ReportLine> lines = new java.util.ArrayList<ReportLine>();
        String reportId = "YPS" + year;
        PageWriter writer = new PageWriter(reportId, REPORT_TYPE_KBN, outputDate, lines);

        writer.add("年次支払統計表  対象年=" + year + "  出力日=" + outputDate);
        writer.add("月別推移");
        writer.add("年月,件数,支払合計,平均支払額");
        for (int i = 0; i < 12; i++) {
            MonthStat s = monthStats[i];
            writer.add(String.format(java.util.Locale.ROOT, "%04d-%02d,%d,%s,%s",
                    year, i + 1, s.count, money(s.total), money(s.average())));
        }

        writer.newPage();
        writer.add("商品区分別内訳");
        writer.add("区分,件数,総請求額,支払合計,平均削減率");
        for (java.util.Map.Entry<String, CategoryStat> e : categoryStats.entrySet()) {
            CategoryStat s = e.getValue();
            writer.add(e.getKey() + "," + s.count + "," + money(s.totalGross) + ","
                    + money(s.totalPayout) + "," + percent(s.averageReductionRate()));
        }

        writer.add("");
        writer.add("前年比較");
        writer.add("区分,前年件数,当年件数,件数差,前年支払,当年支払,支払差");
        for (java.util.Map.Entry<String, CategoryStat> e : categoryStats.entrySet()) {
            int priorCount = 0;
            java.math.BigDecimal priorPayout = java.math.BigDecimal.ZERO;
            for (int m = 1; m <= 12; m++) {
                MonthlySnapshot s = priorByMonthCategory.get(String.format(java.util.Locale.ROOT, "%04d-%02d|%s",
                        year - 1, m, e.getKey()));
                if (s != null) {
                    priorCount += s.count;
                    priorPayout = priorPayout.add(s.totalPayoutAmt, MC);
                }
            }
            CategoryStat current = e.getValue();
            writer.add(e.getKey() + "," + priorCount + "," + current.count + "," + (current.count - priorCount)
                    + "," + money(priorPayout) + "," + money(current.totalPayout) + ","
                    + money(current.totalPayout.subtract(priorPayout, MC)));
        }

        writer.newPage();
        writer.add("削減率分布");
        writer.add("範囲,件数");
        writer.add("0%," + reductionHistogram[0]);
        writer.add("1-20%," + reductionHistogram[1]);
        writer.add("21-40%," + reductionHistogram[2]);
        writer.add("41-60%," + reductionHistogram[3]);
        writer.add("61-100%," + reductionHistogram[4]);
        writer.add("");
        writer.add("外れ値判定  平均=" + money(mean) + "  標準偏差=" + money(sigma) + "  閾値=" + money(outlierLimit));
        writer.add("支払ID,請求ID,支払額");
        for (int i = 0; i < outliers.size() && i < 20; i++) {
            Payment p = outliers.get(i);
            writer.add(p.payId + "," + p.claimId + "," + money(p.payoutAmt));
        }

        writer.add("");
        writer.add("責任開始日から1年以上経過の支払割合=" + percent(ONE_YEAR_OR_MORE_PAYMENT_RATE));
        return lines;
    }

    private static java.util.Map<String, Assessment> latestAssessmentByClaim(java.util.List<Assessment> assessments) {
        java.util.Map<String, Assessment> map = new java.util.HashMap<String, Assessment>();
        for (Assessment assessment : assessments) {
            Assessment old = map.get(assessment.claimId);
            if (old == null || assessment.assessDt.compareTo(old.assessDt) > 0) {
                map.put(assessment.claimId, assessment);
            }
        }
        return map;
    }

    private static int bucket(java.math.BigDecimal rate) {
        if (rate.compareTo(java.math.BigDecimal.ZERO) <= 0) return 0;
        if (rate.compareTo(new java.math.BigDecimal("0.20")) <= 0) return 1;
        if (rate.compareTo(new java.math.BigDecimal("0.40")) <= 0) return 2;
        if (rate.compareTo(new java.math.BigDecimal("0.60")) <= 0) return 3;
        return 4;
    }

    private static java.math.BigDecimal mean(java.util.List<java.math.BigDecimal> values) {
        if (values.isEmpty()) return java.math.BigDecimal.ZERO;
        java.math.BigDecimal total = java.math.BigDecimal.ZERO;
        for (java.math.BigDecimal v : values) total = total.add(v, MC);
        return total.divide(new java.math.BigDecimal(values.size()), 2, java.math.RoundingMode.HALF_UP);
    }

    private static java.math.BigDecimal standardDeviation(java.util.List<java.math.BigDecimal> values, java.math.BigDecimal mean) {
        if (values.size() <= 1) return java.math.BigDecimal.ZERO;
        java.math.BigDecimal sum = java.math.BigDecimal.ZERO;
        for (java.math.BigDecimal v : values) {
            java.math.BigDecimal d = v.subtract(mean, MC);
            sum = sum.add(d.multiply(d, MC), MC);
        }
        java.math.BigDecimal variance = sum.divide(new java.math.BigDecimal(values.size()), 8, java.math.RoundingMode.HALF_UP);
        return new java.math.BigDecimal(Math.sqrt(variance.doubleValue())).setScale(2, java.math.RoundingMode.HALF_UP);
    }

    private static String money(java.math.BigDecimal value) {
        return value.setScale(0, java.math.RoundingMode.HALF_UP).toPlainString();
    }

    private static String percent(java.math.BigDecimal rate) {
        return rate.multiply(ONE_HUNDRED, MC).setScale(1, java.math.RoundingMode.HALF_UP).toPlainString() + "%";
    }

    private static java.util.List<Payment> readPayments(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Payment> list = new java.util.ArrayList<Payment>();
        for (String[] c : readCsv(path)) {
            list.add(new Payment(c[0], c[1], bd(c[2]), bd(c[3]), bd(c[4])));
        }
        return list;
    }

    private static java.util.List<Assessment> readAssessments(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Assessment> list = new java.util.ArrayList<Assessment>();
        for (String[] c : readCsv(path)) {
            list.add(new Assessment(c[0], c[1], java.time.LocalDate.parse(c[2]), c[3], c[4], c[5], c[6]));
        }
        return list;
    }

    private static java.util.List<Claim> readClaims(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Claim> list = new java.util.ArrayList<Claim>();
        for (String[] c : readCsv(path)) {
            list.add(new Claim(c[0], c[1], bd(c[2]), bd(c[3]), java.time.LocalDate.parse(c[4]),
                    java.time.LocalDate.parse(c[5]), c[6]));
        }
        return list;
    }

    private static java.util.List<MonthlySnapshot> readSnapshots(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<MonthlySnapshot> list = new java.util.ArrayList<MonthlySnapshot>();
        for (String[] c : readCsv(path)) {
            list.add(new MonthlySnapshot(c[0], c[1], Integer.parseInt(c[2]), bd(c[3]), bd(c[4]), bd(c[5])));
        }
        return list;
    }

    private static java.util.List<String[]> readCsv(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String[]> rows = new java.util.ArrayList<String[]>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String t = line.trim();
            if (t.isEmpty() || t.startsWith("#")) continue;
            rows.add(t.split(",", -1));
        }
        return rows;
    }

    private static void writeReport(java.nio.file.Path path, java.util.List<ReportLine> report) throws java.io.IOException {
        java.util.List<String> rows = new java.util.ArrayList<String>();
        for (ReportLine line : report) rows.add(line.toCsv());
        java.nio.file.Files.write(path, rows, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static java.math.BigDecimal bd(String s) {
        return new java.math.BigDecimal(s.trim());
    }

    private static java.util.List<Payment> syntheticPayments(int year) {
        java.util.List<Payment> list = new java.util.ArrayList<Payment>();
        for (int m = 1; m <= 12; m++) {
            for (int i = 1; i <= 36; i++) {
                String claimId = String.format(java.util.Locale.ROOT, "CL%04d%02d%03d", year, m, i);
                java.math.BigDecimal gross = new java.math.BigDecimal(900000 + m * 42000 + i * 17000);
                java.math.BigDecimal rate = new java.math.BigDecimal(((m + i) % 6) * 10).divide(ONE_HUNDRED, 2, java.math.RoundingMode.HALF_UP);
                if (m == 11 && i == 35) gross = new java.math.BigDecimal("8500000");
                java.math.BigDecimal payout = gross.multiply(java.math.BigDecimal.ONE.subtract(rate, MC), MC);
                list.add(new Payment("PY" + claimId.substring(2), claimId, gross, rate, payout));
            }
        }
        return list;
    }

    private static java.util.List<Assessment> syntheticAssessments(int year, java.util.List<Payment> payments) {
        java.util.List<Assessment> list = new java.util.ArrayList<Assessment>();
        String[] categories = {"01", "02", "03", "04"};
        int n = 0;
        for (Payment p : payments) {
            int month = Integer.parseInt(p.claimId.substring(6, 8));
            list.add(new Assessment("AS" + p.claimId.substring(2), p.claimId,
                    java.time.LocalDate.of(year, month, Math.min(27, 1 + (n % 27))),
                    categories[n % categories.length], n % 3 == 0 ? "2" : "1", "01", "T" + String.format(java.util.Locale.ROOT, "%04d", n % 25)));
            n++;
        }
        return list;
    }

    private static java.util.List<Claim> syntheticClaims(int year, java.util.List<Payment> payments) {
        java.util.List<Claim> list = new java.util.ArrayList<Claim>();
        int n = 0;
        for (Payment p : payments) {
            int month = Integer.parseInt(p.claimId.substring(6, 8));
            String status = n % 29 == 0 ? "09" : (n % 17 == 0 ? "05" : CLAIM_STATUS_PAYABLE);
            list.add(new Claim(p.claimId, "PN" + p.claimId.substring(2), p.grossAmt.add(new java.math.BigDecimal("500000"), MC),
                    new java.math.BigDecimal((n % 8) * 50000), java.time.LocalDate.of(year - 3, month, 1),
                    java.time.LocalDate.of(year, month, Math.min(25, 1 + (n % 25))), status));
            n++;
        }
        return list;
    }

    private static java.util.List<MonthlySnapshot> syntheticPriorSnapshots(int year, java.util.List<Assessment> assessments, java.util.List<Payment> payments) {
        java.util.Map<String, Payment> paymentByClaim = new java.util.HashMap<String, Payment>();
        for (Payment p : payments) paymentByClaim.put(p.claimId, p);
        java.util.Map<String, CategoryStat> map = new java.util.TreeMap<String, CategoryStat>();
        for (Assessment a : assessments) {
            Payment p = paymentByClaim.get(a.claimId);
            if (p == null) continue;
            String ym = String.format(java.util.Locale.ROOT, "%04d-%02d", year - 1, a.assessDt.getMonthValue());
            String key = ym + "|" + a.categoryKbn;
            CategoryStat s = map.get(key);
            if (s == null) {
                s = new CategoryStat();
                map.put(key, s);
            }
            s.add(p.grossAmt.multiply(new java.math.BigDecimal("0.94"), MC),
                    p.payoutAmt.multiply(new java.math.BigDecimal("0.92"), MC), p.reductionRate);
        }

        java.util.List<MonthlySnapshot> list = new java.util.ArrayList<MonthlySnapshot>();
        for (java.util.Map.Entry<String, CategoryStat> e : map.entrySet()) {
            String[] key = e.getKey().split("\\|", -1);
            CategoryStat s = e.getValue();
            list.add(new MonthlySnapshot(key[0], key[1], s.count, s.totalGross, s.totalPayout, s.averageReductionRate()));
        }
        return list;
    }

    private static final class PageWriter {
        private final String reportId;
        private final String reportTypeKbn;
        private final java.time.LocalDate outputDate;
        private final java.util.List<ReportLine> lines;
        private int pageNo = 1;
        private int lineNo = 0;

        PageWriter(String reportId, String reportTypeKbn, java.time.LocalDate outputDate, java.util.List<ReportLine> lines) {
            this.reportId = reportId;
            this.reportTypeKbn = reportTypeKbn;
            this.outputDate = outputDate;
            this.lines = lines;
        }

        void add(String data) {
            if (lineNo >= 45) newPage();
            lines.add(new ReportLine(reportId, reportTypeKbn, outputDate, pageNo, data));
            lineNo++;
        }

        void newPage() {
            pageNo++;
            lineNo = 0;
        }
    }

    private static final class MonthStat {
        int count;
        java.math.BigDecimal total = java.math.BigDecimal.ZERO;

        void add(java.math.BigDecimal amount) {
            count++;
            total = total.add(amount, MC);
        }

        java.math.BigDecimal average() {
            if (count == 0) return java.math.BigDecimal.ZERO;
            return total.divide(new java.math.BigDecimal(count), 2, java.math.RoundingMode.HALF_UP);
        }
    }

    private static final class CategoryStat {
        int count;
        java.math.BigDecimal totalGross = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalPayout = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalReductionRate = java.math.BigDecimal.ZERO;

        void add(java.math.BigDecimal gross, java.math.BigDecimal payout, java.math.BigDecimal reductionRate) {
            count++;
            totalGross = totalGross.add(gross, MC);
            totalPayout = totalPayout.add(payout, MC);
            totalReductionRate = totalReductionRate.add(reductionRate, MC);
        }

        java.math.BigDecimal averageReductionRate() {
            if (count == 0) return java.math.BigDecimal.ZERO;
            return totalReductionRate.divide(new java.math.BigDecimal(count), 4, java.math.RoundingMode.HALF_UP);
        }
    }

    private static final class Payment {
        final String payId;
        final String claimId;
        final java.math.BigDecimal grossAmt;
        final java.math.BigDecimal reductionRate;
        final java.math.BigDecimal payoutAmt;

        Payment(String payId, String claimId, java.math.BigDecimal grossAmt, java.math.BigDecimal reductionRate, java.math.BigDecimal payoutAmt) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
        }
    }

    private static final class Assessment {
        final String assessId;
        final String claimId;
        final java.time.LocalDate assessDt;
        final String categoryKbn;
        final String authLevelKbn;
        final String resultKbn;
        final String assessorId;

        Assessment(String assessId, String claimId, java.time.LocalDate assessDt, String categoryKbn,
                String authLevelKbn, String resultKbn, String assessorId) {
            this.assessId = assessId;
            this.claimId = claimId;
            this.assessDt = assessDt;
            this.categoryKbn = categoryKbn;
            this.authLevelKbn = authLevelKbn;
            this.resultKbn = resultKbn;
            this.assessorId = assessorId;
        }
    }

    private static final class Claim {
        final String claimId;
        final String polNo;
        final java.math.BigDecimal sumAssuredAmt;
        final java.math.BigDecimal loanBalanceAmt;
        final java.time.LocalDate respStartDt;
        final java.time.LocalDate eventDt;
        final String claimStatusKbn;

        Claim(String claimId, String polNo, java.math.BigDecimal sumAssuredAmt, java.math.BigDecimal loanBalanceAmt,
                java.time.LocalDate respStartDt, java.time.LocalDate eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }
    }

    private static final class MonthlySnapshot {
        final String yearMonth;
        final String categoryKbn;
        final int count;
        final java.math.BigDecimal totalGrossAmt;
        final java.math.BigDecimal totalPayoutAmt;
        final java.math.BigDecimal avgReductionRate;

        MonthlySnapshot(String yearMonth, String categoryKbn, int count, java.math.BigDecimal totalGrossAmt,
                java.math.BigDecimal totalPayoutAmt, java.math.BigDecimal avgReductionRate) {
            this.yearMonth = yearMonth;
            this.categoryKbn = categoryKbn;
            this.count = count;
            this.totalGrossAmt = totalGrossAmt;
            this.totalPayoutAmt = totalPayoutAmt;
            this.avgReductionRate = avgReductionRate;
        }
    }

    private static final class ReportLine {
        final String reportId;
        final String reportTypeKbn;
        final java.time.LocalDate outputDt;
        final int pageNo;
        final String lineData;

        ReportLine(String reportId, String reportTypeKbn, java.time.LocalDate outputDt, int pageNo, String lineData) {
            this.reportId = reportId;
            this.reportTypeKbn = reportTypeKbn;
            this.outputDt = outputDt;
            this.pageNo = pageNo;
            this.lineData = lineData;
        }

        String toCsv() {
            return reportId + "," + reportTypeKbn + "," + outputDt + "," + pageNo + ",\"" + lineData.replace("\"", "\"\"") + "\"";
        }
    }
}
