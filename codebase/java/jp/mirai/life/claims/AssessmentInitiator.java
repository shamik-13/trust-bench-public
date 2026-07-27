package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2024-03-15  保険金システムG  支払査定開始サービス初版
 */
public class AssessmentInitiator {
    private static final String LFCLMF_IN = "LFCLMF.csv";
    private static final String LFCLMF_OUT = "LFCLMF.updated.csv";
    private static final String LFRASF_OUT = "LFRASF.csv";

    private static final String STATUS_SHORUI_KAKUNIN_ZUMI = "20";
    private static final String STATUS_SATEI_CHU = "25";

    private static final String RA_CATEGORY_MIBUNRUI = "00";
    private static final String RA_AUTH_TANTOSHA = "01";
    private static final String RA_RESULT_MISHORI = "";
    private static final String RA_ASSESSOR_MIWARIATE = "";

    private static final java.math.BigDecimal SHIHARAI_WARIAI_ICHINEN_IJO = new java.math.BigDecimal("1.00");

    public static Result initiate(java.nio.file.Path claimIn,
                                   java.nio.file.Path claimOut,
                                   java.nio.file.Path assessOut) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.exists(claimIn)
                ? java.nio.file.Files.readAllLines(claimIn, java.nio.charset.StandardCharsets.UTF_8)
                : java.util.Collections.emptyList();

        java.util.List<ClaimRecord> claims = new java.util.ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.trim().isEmpty()) {
                continue;
            }
            if (i == 0 && line.toUpperCase(java.util.Locale.ROOT).contains("CLAIM-ID")) {
                continue;
            }
            claims.add(ClaimRecord.parse(line, i + 1));
        }

        java.time.LocalDate assessDate = java.time.LocalDate.now();
        java.util.List<AssessmentRecord> assessments = new java.util.ArrayList<>();
        int target = 0;
        int rejected = 0;

        for (ClaimRecord claim : claims) {
            if (!STATUS_SHORUI_KAKUNIN_ZUMI.equals(claim.claimStatusKbn)) {
                continue;
            }

            target++;
            java.util.List<String> errors = validateForAssessment(claim);
            if (!errors.isEmpty()) {
                rejected++;
                System.err.println("査定開始除外 CLAIM-ID=" + claim.claimId + " 理由=" + String.join("、", errors));
                continue;
            }

            java.math.BigDecimal payableBase = claim.sumAssuredAmt.subtract(claim.loanBalanceAmt);
            if (payableBase.signum() <= 0) {
                rejected++;
                System.err.println("査定開始除外 CLAIM-ID=" + claim.claimId + " 理由=支払基礎額が零以下");
                continue;
            }

            if (!isElapsedOneYearOrMore(claim.respStartDt, claim.eventDt)) {
                System.err.println("査定注意 CLAIM-ID=" + claim.claimId + " 理由=責任開始日から一年未満、削減率は支払エンジン判定");
            } else {
                payableBase = payableBase.multiply(SHIHARAI_WARIAI_ICHINEN_IJO);
            }

            assessments.add(new AssessmentRecord(
                    createAssessId(claim.claimId, assessDate, assessments.size() + 1),
                    claim.claimId,
                    assessDate,
                    RA_CATEGORY_MIBUNRUI,
                    RA_AUTH_TANTOSHA,
                    RA_RESULT_MISHORI,
                    RA_ASSESSOR_MIWARIATE
            ));
            claim.claimStatusKbn = STATUS_SATEI_CHU;
        }

        writeClaims(claimOut, claims);
        writeAssessments(assessOut, assessments);
        return new Result(target, assessments.size(), rejected);
    }

    private static java.util.List<String> validateForAssessment(ClaimRecord claim) {
        java.util.List<String> errors = new java.util.ArrayList<>();
        require(errors, claim.claimId, "CLAIM-ID");
        require(errors, claim.polNo, "POL-NO");
        require(errors, claim.claimStatusKbn, "CLAIM-STATUS-KBN");

        if (claim.sumAssuredAmt == null || claim.sumAssuredAmt.signum() < 0) {
            errors.add("保険金額不正");
        }
        if (claim.loanBalanceAmt == null || claim.loanBalanceAmt.signum() < 0) {
            errors.add("貸付残高不正");
        }
        if (claim.respStartDt == null) {
            errors.add("責任開始日未設定");
        }
        if (claim.eventDt == null) {
            errors.add("事故日未設定");
        }
        if (claim.respStartDt != null && claim.eventDt != null && claim.eventDt.isBefore(claim.respStartDt)) {
            errors.add("事故日が責任開始日前");
        }
        return errors;
    }

    private static void require(java.util.List<String> errors, String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            errors.add(name + "未設定");
        }
    }

    private static boolean isElapsedOneYearOrMore(java.time.LocalDate respStartDt, java.time.LocalDate eventDt) {
        return !eventDt.isBefore(respStartDt.plusYears(1));
    }

    private static String createAssessId(String claimId, java.time.LocalDate assessDate, int seq) {
        String normalized = claimId.replaceAll("[^0-9A-Za-z]", "");
        if (normalized.length() > 12) {
            normalized = normalized.substring(normalized.length() - 12);
        }
        return "RA" + assessDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + String.format(java.util.Locale.ROOT, "%04d", seq)
                + normalized;
    }

    private static void writeClaims(java.nio.file.Path path, java.util.List<ClaimRecord> claims) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("CLAIM-ID,POL-NO,SUM-ASSURED-AMT,LOAN-BALANCE-AMT,RESP-START-DT,EVENT-DT,CLAIM-STATUS-KBN");
        for (ClaimRecord claim : claims) {
            out.add(claim.toCsv());
        }
        java.nio.file.Files.write(path, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeAssessments(java.nio.file.Path path, java.util.List<AssessmentRecord> assessments) throws java.io.IOException {
        java.util.List<String> out = new java.util.ArrayList<>();
        out.add("ASSESS-ID,CLAIM-ID,ASSESS-DT,CATEGORY-KBN,AUTH-LEVEL-KBN,RESULT-KBN,ASSESSOR-ID");
        for (AssessmentRecord assessment : assessments) {
            out.add(assessment.toCsv());
        }
        java.nio.file.Files.write(path, out, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static final class ClaimRecord {
        private final String claimId;
        private final String polNo;
        private final java.math.BigDecimal sumAssuredAmt;
        private final java.math.BigDecimal loanBalanceAmt;
        private final java.time.LocalDate respStartDt;
        private final java.time.LocalDate eventDt;
        private String claimStatusKbn;

        private ClaimRecord(String claimId,
                            String polNo,
                            java.math.BigDecimal sumAssuredAmt,
                            java.math.BigDecimal loanBalanceAmt,
                            java.time.LocalDate respStartDt,
                            java.time.LocalDate eventDt,
                            String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }

        private static ClaimRecord parse(String line, int lineNo) {
            java.util.List<String> f = splitCsv(line);
            if (f.size() != 7) {
                throw new IllegalArgumentException("LFCLMF項目数不正 行=" + lineNo);
            }
            return new ClaimRecord(
                    f.get(0).trim(),
                    f.get(1).trim(),
                    decimal(f.get(2), "SUM-ASSURED-AMT", lineNo),
                    decimal(f.get(3), "LOAN-BALANCE-AMT", lineNo),
                    date(f.get(4), "RESP-START-DT", lineNo),
                    date(f.get(5), "EVENT-DT", lineNo),
                    f.get(6).trim()
            );
        }

        private String toCsv() {
            return csv(claimId) + "," + csv(polNo) + "," + amount(sumAssuredAmt) + "," + amount(loanBalanceAmt)
                    + "," + respStartDt + "," + eventDt + "," + csv(claimStatusKbn);
        }
    }

    private static final class AssessmentRecord {
        private final String assessId;
        private final String claimId;
        private final java.time.LocalDate assessDt;
        private final String categoryKbn;
        private final String authLevelKbn;
        private final String resultKbn;
        private final String assessorId;

        private AssessmentRecord(String assessId,
                                 String claimId,
                                 java.time.LocalDate assessDt,
                                 String categoryKbn,
                                 String authLevelKbn,
                                 String resultKbn,
                                 String assessorId) {
            this.assessId = assessId;
            this.claimId = claimId;
            this.assessDt = assessDt;
            this.categoryKbn = categoryKbn;
            this.authLevelKbn = authLevelKbn;
            this.resultKbn = resultKbn;
            this.assessorId = assessorId;
        }

        private String toCsv() {
            return csv(assessId) + "," + csv(claimId) + "," + assessDt + "," + csv(categoryKbn)
                    + "," + csv(authLevelKbn) + "," + csv(resultKbn) + "," + csv(assessorId);
        }
    }

    private static final class Result {
        private final int targetCount;
        private final int startedCount;
        private final int rejectedCount;

        private Result(int targetCount, int startedCount, int rejectedCount) {
            this.targetCount = targetCount;
            this.startedCount = startedCount;
            this.rejectedCount = rejectedCount;
        }
    }

    private static java.math.BigDecimal decimal(String value, String name, int lineNo) {
        try {
            return new java.math.BigDecimal(value.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(name + "数値不正 行=" + lineNo);
        }
    }

    private static java.time.LocalDate date(String value, String name, int lineNo) {
        try {
            return java.time.LocalDate.parse(value.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(name + "日付不正 行=" + lineNo);
        }
    }

    private static String amount(java.math.BigDecimal value) {
        return value.setScale(0, java.math.RoundingMode.UNNECESSARY).toPlainString();
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> values = new java.util.ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quoted && c == '"' && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                current.append('"');
                i++;
            } else if (c == '"') {
                quoted = !quoted;
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

    private static String csv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }
}
