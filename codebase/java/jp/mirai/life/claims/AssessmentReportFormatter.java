package jp.mirai.life.claims;
/**
 * 変更履歴
 * 版数    年月日       担当      概要
 * 1.00    2024-03-15   保険金システムG   初版作成
 */
public class AssessmentReportFormatter {
    private static final String REPORT_TYPE_KBN_ASSESSMENT = "02";
    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final String CLAIM_STATUS_IN_ASSESSMENT = "05";
    private static final String CLAIM_STATUS_DENIED = "09";
    private static final java.math.BigDecimal ONE_YEAR_OR_MORE_PAY_RATE =
            new java.math.BigDecimal("1.00");
    private static final int PAGE_LINE_LIMIT = 48;

    public void format(java.nio.file.Path lfrasf, java.nio.file.Path lfclmf,
            java.nio.file.Path lfpayf, java.nio.file.Path lfrepmf) throws Exception {
        java.util.Map<String, ClaimPolicy> policies = readPolicies(lfclmf);
        java.util.Map<String, java.util.List<Payout>> payouts = readPayouts(lfpayf);
        java.util.List<Assessment> assessments = readAssessments(lfrasf);

        java.util.List<ReportRecord> reports = new java.util.ArrayList<ReportRecord>();
        String outputDate = java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);

        for (Assessment assessment : assessments) {
            ClaimPolicy policy = policies.get(assessment.claimId);
            if (policy == null) {
                throw new IllegalStateException("請求明細未存在: CLAIM-ID=" + assessment.claimId);
            }
            validatePolicy(policy);

            java.util.List<Payout> claimPayouts = payouts.get(assessment.claimId);
            if (claimPayouts == null || claimPayouts.isEmpty()) {
                throw new IllegalStateException("支払明細未存在: CLAIM-ID=" + assessment.claimId);
            }

            java.util.List<String> lines = formatReportLines(assessment, policy, claimPayouts);
            int pageNo = 1;
            int lineNo = 0;
            for (String line : lines) {
                if (lineNo >= PAGE_LINE_LIMIT) {
                    pageNo++;
                    lineNo = 0;
                }
                reports.add(new ReportRecord(
                        assessment.assessId + "-" + String.format("%03d", reports.size() + 1),
                        REPORT_TYPE_KBN_ASSESSMENT,
                        outputDate,
                        pageNo,
                        line));
                lineNo++;
            }
        }

        writeReports(lfrepmf, reports);
    }

    private static java.util.List<Assessment> readAssessments(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Assessment> records = new java.util.ArrayList<Assessment>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (isSkippable(line)) {
                continue;
            }
            String[] c = split(line, 7, "LFRASF");
            records.add(new Assessment(
                    c[0].trim(),
                    c[1].trim(),
                    c[2].trim(),
                    c[3].trim(),
                    c[4].trim(),
                    c[5].trim(),
                    c[6].trim()));
        }
        return records;
    }

    private static java.util.Map<String, ClaimPolicy> readPolicies(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, ClaimPolicy> records = new java.util.LinkedHashMap<String, ClaimPolicy>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (isSkippable(line)) {
                continue;
            }
            String[] c = split(line, 7, "LFCLMF");
            ClaimPolicy policy = new ClaimPolicy(
                    c[0].trim(),
                    c[1].trim(),
                    money(c[2]),
                    money(c[3]),
                    c[4].trim(),
                    c[5].trim(),
                    c[6].trim());
            if (records.put(policy.claimId, policy) != null) {
                throw new IllegalStateException("請求明細重複: CLAIM-ID=" + policy.claimId);
            }
        }
        return records;
    }

    private static java.util.Map<String, java.util.List<Payout>> readPayouts(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, java.util.List<Payout>> records = new java.util.LinkedHashMap<String, java.util.List<Payout>>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (isSkippable(line)) {
                continue;
            }
            String[] c = split(line, 5, "LFPAYF");
            Payout payout = new Payout(
                    c[0].trim(),
                    c[1].trim(),
                    money(c[2]),
                    rate(c[3]),
                    money(c[4]));
            java.util.List<Payout> list = records.get(payout.claimId);
            if (list == null) {
                list = new java.util.ArrayList<Payout>();
                records.put(payout.claimId, list);
            }
            list.add(payout);
        }
        return records;
    }

    private static java.util.List<String> formatReportLines(
            Assessment assessment,
            ClaimPolicy policy,
            java.util.List<Payout> payouts) {
        java.util.List<String> lines = new java.util.ArrayList<String>();
        java.math.BigDecimal grossTotal = java.math.BigDecimal.ZERO;
        java.math.BigDecimal payoutTotal = java.math.BigDecimal.ZERO;

        for (Payout payout : payouts) {
            grossTotal = grossTotal.add(payout.grossAmount);
            payoutTotal = payoutTotal.add(resolvePayoutAmount(policy, payout));
        }

        lines.add(fixed("査定書", 92));
        lines.add(fixed("査定者:" + assessment.assessorId, 24)
                + fixed("査定日:" + assessment.assessDate, 20)
                + fixed("査定番号:" + assessment.assessId, 28)
                + fixed("区分:" + assessment.categoryKbn, 20));
        lines.add(repeat('-', 92));
        lines.add(fixed("請求番号", 14)
                + fixed("証券番号", 16)
                + fixed("請求状態", 10)
                + fixed("責任開始日", 12)
                + fixed("事故日", 12)
                + fixed("査定結果", 10)
                + fixed("承認", 8)
                + fixed("支払対象", 10));
        lines.add(fixed(policy.claimId, 14)
                + fixed(policy.policyNo, 16)
                + fixed(policy.claimStatusKbn, 10)
                + fixed(policy.responsibilityStartDate, 12)
                + fixed(policy.eventDate, 12)
                + fixed(assessment.resultKbn, 10)
                + fixed(assessment.authLevelKbn, 8)
                + fixed(isPayable(policy) ? "対象" : "対象外", 10));
        lines.add(repeat('-', 92));
        lines.add(fixed("支払ID", 14)
                + right("基準金額", 14)
                + right("貸付残高", 14)
                + right("削減率", 10)
                + right("支払金額", 14)
                + fixed("算定", 26));

        for (Payout payout : payouts) {
            java.math.BigDecimal resolvedPayout = resolvePayoutAmount(policy, payout);
            lines.add(fixed(payout.payId, 14)
                    + right(amount(payout.grossAmount), 14)
                    + right(amount(policy.loanBalanceAmount), 14)
                    + right(percent(payout.reductionRate), 10)
                    + right(amount(resolvedPayout), 14)
                    + fixed(elapsedOneYearOrMore(policy) ? "一年以上" : "一年未満", 26));
        }

        lines.add(repeat('-', 92));
        lines.add(fixed("合計", 14)
                + right(amount(grossTotal), 14)
                + right(amount(policy.loanBalanceAmount), 14)
                + right("", 10)
                + right(amount(payoutTotal), 14)
                + fixed("", 26));
        lines.add("");
        lines.add(fixed("承認欄", 92));
        lines.add(fixed("一次承認", 30) + fixed("二次承認", 30) + fixed("最終承認", 32));
        lines.add(fixed("印", 30) + fixed("印", 30) + fixed("印", 32));
        return lines;
    }

    private static java.math.BigDecimal resolvePayoutAmount(ClaimPolicy policy, Payout payout) {
        if (!isPayable(policy)) {
            return java.math.BigDecimal.ZERO;
        }
        if (elapsedOneYearOrMore(policy) && payout.reductionRate == null) {
            return payout.grossAmount.multiply(ONE_YEAR_OR_MORE_PAY_RATE).subtract(policy.loanBalanceAmount).max(java.math.BigDecimal.ZERO);
        }
        return payout.payoutAmount;
    }

    private static void validatePolicy(ClaimPolicy policy) {
        if (!CLAIM_STATUS_PAYABLE.equals(policy.claimStatusKbn)
                && !CLAIM_STATUS_IN_ASSESSMENT.equals(policy.claimStatusKbn)
                && !CLAIM_STATUS_DENIED.equals(policy.claimStatusKbn)) {
            throw new IllegalStateException("請求状態区分不正: CLAIM-ID=" + policy.claimId + ", 状態=" + policy.claimStatusKbn);
        }
        date(policy.responsibilityStartDate, "責任開始日", policy.claimId);
        date(policy.eventDate, "事故日", policy.claimId);
    }

    private static boolean isPayable(ClaimPolicy policy) {
        return CLAIM_STATUS_PAYABLE.equals(policy.claimStatusKbn);
    }

    private static boolean elapsedOneYearOrMore(ClaimPolicy policy) {
        java.time.LocalDate start = date(policy.responsibilityStartDate, "責任開始日", policy.claimId);
        java.time.LocalDate event = date(policy.eventDate, "事故日", policy.claimId);
        return !event.isBefore(start.plusYears(1));
    }

    private static void writeReports(java.nio.file.Path path, java.util.List<ReportRecord> reports) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<String>();
        for (ReportRecord report : reports) {
            lines.add(csv(report.reportId)
                    + "," + csv(report.reportTypeKbn)
                    + "," + csv(report.outputDate)
                    + "," + report.pageNo
                    + "," + csv(report.lineData));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static String[] split(String line, int expected, String fileName) {
        String[] fields = line.split(",", -1);
        if (fields.length != expected) {
            throw new IllegalStateException(fileName + " 項目数不正: " + line);
        }
        return fields;
    }

    private static boolean isSkippable(String line) {
        String s = line == null ? "" : line.trim();
        return s.isEmpty() || s.startsWith("#");
    }

    private static java.math.BigDecimal money(String value) {
        return new java.math.BigDecimal(value.trim()).setScale(0, java.math.RoundingMode.DOWN);
    }

    private static java.math.BigDecimal rate(String value) {
        String s = value.trim();
        if (s.isEmpty()) {
            return null;
        }
        return new java.math.BigDecimal(s).setScale(4, java.math.RoundingMode.HALF_UP);
    }

    private static java.time.LocalDate date(String value, String name, String claimId) {
        try {
            return java.time.LocalDate.parse(value, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalStateException(name + "不正: CLAIM-ID=" + claimId + ", 日付=" + value, e);
        }
    }

    private static String amount(java.math.BigDecimal value) {
        return value.setScale(0, java.math.RoundingMode.DOWN).toPlainString();
    }

    private static String percent(java.math.BigDecimal value) {
        if (value == null) {
            return "";
        }
        return value.multiply(new java.math.BigDecimal("100")).setScale(2, java.math.RoundingMode.HALF_UP).toPlainString() + "%";
    }

    private static String fixed(String value, int width) {
        String s = value == null ? "" : value;
        if (s.length() > width) {
            return s.substring(0, width);
        }
        StringBuilder b = new StringBuilder(s);
        while (b.length() < width) {
            b.append(' ');
        }
        return b.toString();
    }

    private static String right(String value, int width) {
        String s = value == null ? "" : value;
        if (s.length() > width) {
            return s.substring(0, width);
        }
        StringBuilder b = new StringBuilder();
        while (b.length() + s.length() < width) {
            b.append(' ');
        }
        b.append(s);
        return b.toString();
    }

    private static String repeat(char c, int count) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < count; i++) {
            b.append(c);
        }
        return b.toString();
    }

    private static String csv(String value) {
        String s = value == null ? "" : value;
        if (s.indexOf(',') < 0 && s.indexOf('"') < 0 && s.indexOf('\n') < 0 && s.indexOf('\r') < 0) {
            return s;
        }
        return "\"" + s.replace("\"", "\"\"") + "\"";
    }

    private static final class Assessment {
        private final String assessId;
        private final String claimId;
        private final String assessDate;
        private final String categoryKbn;
        private final String authLevelKbn;
        private final String resultKbn;
        private final String assessorId;

        private Assessment(String assessId, String claimId, String assessDate, String categoryKbn,
                           String authLevelKbn, String resultKbn, String assessorId) {
            this.assessId = require(assessId, "ASSESS-ID");
            this.claimId = require(claimId, "CLAIM-ID");
            this.assessDate = require(assessDate, "ASSESS-DT");
            this.categoryKbn = require(categoryKbn, "CATEGORY-KBN");
            this.authLevelKbn = require(authLevelKbn, "AUTH-LEVEL-KBN");
            this.resultKbn = require(resultKbn, "RESULT-KBN");
            this.assessorId = require(assessorId, "ASSESSOR-ID");
            date(this.assessDate, "査定日", this.claimId);
        }
    }

    private static final class ClaimPolicy {
        private final String claimId;
        private final String policyNo;
        private final java.math.BigDecimal sumAssuredAmount;
        private final java.math.BigDecimal loanBalanceAmount;
        private final String responsibilityStartDate;
        private final String eventDate;
        private final String claimStatusKbn;

        private ClaimPolicy(String claimId, String policyNo, java.math.BigDecimal sumAssuredAmount,
                            java.math.BigDecimal loanBalanceAmount, String responsibilityStartDate,
                            String eventDate, String claimStatusKbn) {
            this.claimId = require(claimId, "CLAIM-ID");
            this.policyNo = require(policyNo, "POL-NO");
            this.sumAssuredAmount = sumAssuredAmount;
            this.loanBalanceAmount = loanBalanceAmount;
            this.responsibilityStartDate = require(responsibilityStartDate, "RESP-START-DT");
            this.eventDate = require(eventDate, "EVENT-DT");
            this.claimStatusKbn = require(claimStatusKbn, "CLAIM-STATUS-KBN");
        }
    }

    private static final class Payout {
        private final String payId;
        private final String claimId;
        private final java.math.BigDecimal grossAmount;
        private final java.math.BigDecimal reductionRate;
        private final java.math.BigDecimal payoutAmount;

        private Payout(String payId, String claimId, java.math.BigDecimal grossAmount,
                       java.math.BigDecimal reductionRate, java.math.BigDecimal payoutAmount) {
            this.payId = require(payId, "PAY-ID");
            this.claimId = require(claimId, "CLAIM-ID");
            this.grossAmount = grossAmount;
            this.reductionRate = reductionRate;
            this.payoutAmount = payoutAmount;
        }
    }

    private static final class ReportRecord {
        private final String reportId;
        private final String reportTypeKbn;
        private final String outputDate;
        private final int pageNo;
        private final String lineData;

        private ReportRecord(String reportId, String reportTypeKbn, String outputDate, int pageNo, String lineData) {
            this.reportId = reportId;
            this.reportTypeKbn = reportTypeKbn;
            this.outputDate = outputDate;
            this.pageNo = pageNo;
            this.lineData = lineData;
        }
    }

    private static String require(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalStateException(name + " 未設定");
        }
        return value.trim();
    }
}
