package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.0   2024-03-15  保険金システムG  保険金支払明細生成サービスの初期実装
 */
public class ClaimPayoutDetailGenerator {

    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final String REPORT_TYPE_CLAIM_PAYOUT_DETAIL = "01";
    private static final int FULL_PAYOUT_RATE_PERCENT_AFTER_ONE_YEAR = 100;
    private static final int REPORT_PAGE_NO = 1;
    private static final java.math.BigDecimal ONE_HUNDRED = new java.math.BigDecimal("100");
    private static final java.nio.charset.Charset INPUT_CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    public static java.util.List<String> generateReportLines(
            String claimId,
            java.nio.file.Path lfclmfPath,
            java.nio.file.Path lfpayfPath,
            java.nio.file.Path lfbenfPath) throws java.io.IOException {

        requireText(claimId, "請求ID");
        requireReadable(lfclmfPath, "LFCLMF");
        requireReadable(lfpayfPath, "LFPAYF");
        requireReadable(lfbenfPath, "LFBENF");

        ClaimFileRecord claim = findClaim(claimId, lfclmfPath);
        if (!CLAIM_STATUS_PAYABLE.equals(claim.claimStatusKbn)) {
            throw new IllegalStateException("支払対象外の請求状態です: 請求ID=" + claimId + ", 状態=" + claim.claimStatusKbn);
        }

        PaymentFileRecord payment = findPayment(claimId, lfpayfPath);
        java.util.List<BeneficiaryFileRecord> beneficiaries = findBeneficiaries(claim.polNo, lfbenfPath);
        if (beneficiaries.isEmpty()) {
            throw new IllegalStateException("受取人が存在しません: 証券番号=" + claim.polNo);
        }

        beneficiaries.sort(java.util.Comparator
                .comparingInt((BeneficiaryFileRecord b) -> b.paymentPriority)
                .thenComparing(b -> b.beneficiaryId));

        validatePayment(claim, payment);

        java.time.LocalDate outputDate = java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo"));
        String reportId = "RP" + outputDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + "-" + claim.claimId + "-" + payment.payId;

        java.util.List<String> reportLines = new java.util.ArrayList<>();
        reportLines.add(toLfRepMf(reportId, outputDate, "保険金支払明細書"));
        reportLines.add(toLfRepMf(reportId, outputDate, "請求ID=" + claim.claimId
                + " 証券番号=" + claim.polNo
                + " 支払ID=" + payment.payId));
        reportLines.add(toLfRepMf(reportId, outputDate, "責任開始日=" + claim.respStartDt
                + " 事故日=" + claim.eventDt
                + " 経過判定=" + elapsedLabel(claim)));

        reportLines.add(toLfRepMf(reportId, outputDate, "【受取人】"));
        for (BeneficiaryFileRecord beneficiary : beneficiaries) {
            reportLines.add(toLfRepMf(reportId, outputDate,
                    beneficiary.paymentPriority + ". " + beneficiary.nameKana
                            + " 受取人ID=" + beneficiary.beneficiaryId
                            + " 続柄=" + beneficiary.relationshipKbn
                            + " 振込先=" + beneficiary.bankCd + "-" + beneficiary.branchCd + "-" + maskAccount(beneficiary.acctNo)));
        }

        java.math.BigDecimal reducedAmount = claim.sumAssuredAmt.subtract(payment.grossAmt.subtract(payment.payoutAmt).subtract(claim.loanBalanceAmt));
        if (reducedAmount.signum() < 0) {
            reducedAmount = java.math.BigDecimal.ZERO;
        }

        java.math.BigDecimal reductionDeduction = claim.sumAssuredAmt.subtract(reducedAmount);
        java.math.BigDecimal loanDeduction = reducedAmount.subtract(payment.payoutAmt);

        reportLines.add(toLfRepMf(reportId, outputDate, "【控除内訳】"));
        reportLines.add(toLfRepMf(reportId, outputDate, "保険金額 " + money(claim.sumAssuredAmt)));
        reportLines.add(toLfRepMf(reportId, outputDate, "支払削減後金額 " + money(reducedAmount)
                + " 削減額=" + money(reductionDeduction)
                + " 削減率=" + percent(payment.reductionRate)));
        reportLines.add(toLfRepMf(reportId, outputDate, "貸付控除後金額 " + money(payment.payoutAmt)
                + " 貸付控除額=" + money(loanDeduction)));
        reportLines.add(toLfRepMf(reportId, outputDate, "実支払金額 " + money(payment.payoutAmt)));

        return reportLines;
    }

    private static ClaimFileRecord findClaim(String claimId, java.nio.file.Path path) throws java.io.IOException {
        ClaimFileRecord found = null;
        for (String line : java.nio.file.Files.readAllLines(path, INPUT_CHARSET)) {
            if (isSkippable(line)) {
                continue;
            }
            ClaimFileRecord record = ClaimFileRecord.parse(line);
            if (claimId.equals(record.claimId)) {
                if (found != null) {
                    throw new IllegalStateException("LFCLMFに請求IDが重複しています: " + claimId);
                }
                found = record;
            }
        }
        if (found == null) {
            throw new IllegalStateException("LFCLMFに請求IDが存在しません: " + claimId);
        }
        return found;
    }

    private static PaymentFileRecord findPayment(String claimId, java.nio.file.Path path) throws java.io.IOException {
        PaymentFileRecord found = null;
        for (String line : java.nio.file.Files.readAllLines(path, INPUT_CHARSET)) {
            if (isSkippable(line)) {
                continue;
            }
            PaymentFileRecord record = PaymentFileRecord.parse(line);
            if (claimId.equals(record.claimId)) {
                if (found != null) {
                    throw new IllegalStateException("LFPAYFに請求IDが重複しています: " + claimId);
                }
                found = record;
            }
        }
        if (found == null) {
            throw new IllegalStateException("LFPAYFに請求IDが存在しません: " + claimId);
        }
        return found;
    }

    private static java.util.List<BeneficiaryFileRecord> findBeneficiaries(String polNo, java.nio.file.Path path) throws java.io.IOException {
        java.util.List<BeneficiaryFileRecord> beneficiaries = new java.util.ArrayList<>();
        java.util.Set<Integer> priorities = new java.util.HashSet<>();
        for (String line : java.nio.file.Files.readAllLines(path, INPUT_CHARSET)) {
            if (isSkippable(line)) {
                continue;
            }
            BeneficiaryFileRecord record = BeneficiaryFileRecord.parse(line);
            if (polNo.equals(record.polNo)) {
                if (!priorities.add(record.paymentPriority)) {
                    throw new IllegalStateException("LFBENFに支払優先順位が重複しています: 証券番号=" + polNo
                            + ", 優先順位=" + record.paymentPriority);
                }
                beneficiaries.add(record);
            }
        }
        return beneficiaries;
    }

    private static void validatePayment(ClaimFileRecord claim, PaymentFileRecord payment) {
        if (claim.sumAssuredAmt.signum() <= 0) {
            throw new IllegalStateException("保険金額が不正です: " + money(claim.sumAssuredAmt));
        }
        if (claim.loanBalanceAmt.signum() < 0) {
            throw new IllegalStateException("貸付残高が不正です: " + money(claim.loanBalanceAmt));
        }
        if (payment.grossAmt.signum() < 0 || payment.payoutAmt.signum() < 0) {
            throw new IllegalStateException("支払金額が不正です: 支払ID=" + payment.payId);
        }
        if (payment.reductionRate.signum() < 0 || payment.reductionRate.compareTo(ONE_HUNDRED) > 0) {
            throw new IllegalStateException("支払削減率が不正です: 支払ID=" + payment.payId + ", 削減率=" + percent(payment.reductionRate));
        }
        if (payment.payoutAmt.compareTo(payment.grossAmt) > 0) {
            throw new IllegalStateException("実支払金額が総支払額を超過しています: 支払ID=" + payment.payId);
        }
        java.math.BigDecimal expectedPayout = payment.grossAmt.subtract(claim.loanBalanceAmt);
        if (expectedPayout.signum() < 0) {
            expectedPayout = java.math.BigDecimal.ZERO;
        }
        if (expectedPayout.compareTo(payment.payoutAmt) != 0) {
            throw new IllegalStateException("貸付控除後金額が不整合です: 支払ID=" + payment.payId
                    + ", 期待=" + money(expectedPayout) + ", 実値=" + money(payment.payoutAmt));
        }
    }

    private static String elapsedLabel(ClaimFileRecord claim) {
        long days = java.time.temporal.ChronoUnit.DAYS.between(claim.respStartDt, claim.eventDt);
        if (days < 0) {
            throw new IllegalStateException("事故日が責任開始日より前です: 請求ID=" + claim.claimId);
        }
        if (days >= 365) {
            return "1年以上経過 支払割合=" + FULL_PAYOUT_RATE_PERCENT_AFTER_ONE_YEAR + "%";
        }
        return "1年未満 支払削減率は支払エンジン結果を使用";
    }

    private static String toLfRepMf(String reportId, java.time.LocalDate outputDate, String lineData) {
        return csv(reportId, REPORT_TYPE_CLAIM_PAYOUT_DETAIL, outputDate.toString(),
                String.valueOf(REPORT_PAGE_NO), lineData);
    }

    private static String csv(String... values) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                builder.append(',');
            }
            String value = values[i] == null ? "" : values[i];
            if (value.indexOf(',') >= 0 || value.indexOf('"') >= 0 || value.indexOf('\n') >= 0 || value.indexOf('\r') >= 0) {
                builder.append('"').append(value.replace("\"", "\"\"")).append('"');
            } else {
                builder.append(value);
            }
        }
        return builder.toString();
    }

    private static String[] splitRecord(String line, int expected, String fileName) {
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
            throw new IllegalArgumentException(fileName + "の引用符が閉じていません");
        }
        values.add(current.toString().trim());
        if (values.size() != expected) {
            throw new IllegalArgumentException(fileName + "の項目数が不正です: 期待=" + expected + ", 実際=" + values.size());
        }
        return values.toArray(new String[0]);
    }

    private static boolean isSkippable(String line) {
        String trimmed = line == null ? "" : line.trim();
        return trimmed.isEmpty() || trimmed.startsWith("#") || trimmed.startsWith("CLAIM-ID") || trimmed.startsWith("PAY-ID") || trimmed.startsWith("POL-NO");
    }

    private static void requireReadable(java.nio.file.Path path, String name) {
        if (path == null || !java.nio.file.Files.isRegularFile(path) || !java.nio.file.Files.isReadable(path)) {
            throw new IllegalArgumentException(name + "が読み込めません: " + path);
        }
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "が未設定です");
        }
    }

    private static java.math.BigDecimal amount(String value, String name) {
        requireText(value, name);
        try {
            return new java.math.BigDecimal(value).setScale(0, java.math.RoundingMode.UNNECESSARY);
        } catch (ArithmeticException | NumberFormatException e) {
            throw new IllegalArgumentException(name + "が金額形式ではありません: " + value, e);
        }
    }

    private static java.math.BigDecimal rate(String value, String name) {
        requireText(value, name);
        try {
            return new java.math.BigDecimal(value).setScale(2, java.math.RoundingMode.HALF_UP);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + "が率形式ではありません: " + value, e);
        }
    }

    private static int priority(String value) {
        requireText(value, "支払優先順位");
        try {
            int parsed = Integer.parseInt(value);
            if (parsed <= 0) {
                throw new IllegalArgumentException("支払優先順位が不正です: " + value);
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("支払優先順位が数値ではありません: " + value, e);
        }
    }

    private static java.time.LocalDate date(String value, String name) {
        requireText(value, name);
        return java.time.LocalDate.parse(value, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
    }

    private static String money(java.math.BigDecimal value) {
        return value.setScale(0, java.math.RoundingMode.UNNECESSARY).toPlainString() + "円";
    }

    private static String percent(java.math.BigDecimal value) {
        return value.stripTrailingZeros().toPlainString() + "%";
    }

    private static String maskAccount(String acctNo) {
        if (acctNo == null || acctNo.length() <= 3) {
            return "***";
        }
        return "***" + acctNo.substring(acctNo.length() - 3);
    }

    private static final class ClaimFileRecord {
        private final String claimId;
        private final String polNo;
        private final java.math.BigDecimal sumAssuredAmt;
        private final java.math.BigDecimal loanBalanceAmt;
        private final java.time.LocalDate respStartDt;
        private final java.time.LocalDate eventDt;
        private final String claimStatusKbn;

        private ClaimFileRecord(String claimId, String polNo, java.math.BigDecimal sumAssuredAmt,
                                java.math.BigDecimal loanBalanceAmt, java.time.LocalDate respStartDt,
                                java.time.LocalDate eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }

        private static ClaimFileRecord parse(String line) {
            String[] v = splitRecord(line, 7, "LFCLMF");
            return new ClaimFileRecord(v[0], v[1], amount(v[2], "保険金額"), amount(v[3], "貸付残高"),
                    date(v[4], "責任開始日"), date(v[5], "事故日"), v[6]);
        }
    }

    private static final class PaymentFileRecord {
        private final String payId;
        private final String claimId;
        private final java.math.BigDecimal grossAmt;
        private final java.math.BigDecimal reductionRate;
        private final java.math.BigDecimal payoutAmt;

        private PaymentFileRecord(String payId, String claimId, java.math.BigDecimal grossAmt,
                                  java.math.BigDecimal reductionRate, java.math.BigDecimal payoutAmt) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
        }

        private static PaymentFileRecord parse(String line) {
            String[] v = splitRecord(line, 5, "LFPAYF");
            return new PaymentFileRecord(v[0], v[1], amount(v[2], "総支払額"),
                    rate(v[3], "支払削減率"), amount(v[4], "実支払金額"));
        }
    }

    private static final class BeneficiaryFileRecord {
        private final String polNo;
        private final String beneficiaryId;
        private final String nameKana;
        private final String relationshipKbn;
        private final String bankCd;
        private final String branchCd;
        private final String acctNo;
        private final int paymentPriority;

        private BeneficiaryFileRecord(String polNo, String beneficiaryId, String nameKana, String relationshipKbn,
                                      String bankCd, String branchCd, String acctNo, int paymentPriority) {
            this.polNo = polNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.paymentPriority = paymentPriority;
        }

        private static BeneficiaryFileRecord parse(String line) {
            String[] v = splitRecord(line, 8, "LFBENF");
            return new BeneficiaryFileRecord(v[0], v[1], v[2], v[3], v[4], v[5], v[6], priority(v[7]));
        }
    }
}
