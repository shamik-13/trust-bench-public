package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数    年月日      担当            概要
 * 1.00    2024/03/15  保険金システムG  初版作成
 */
public class ClaimNotificationDispatcher {

    private static final java.nio.charset.Charset 入出力文字コード = java.nio.charset.StandardCharsets.UTF_8;
    private static final java.math.BigDecimal 一年以上経過支払割合 = new java.math.BigDecimal("1.00");
    private static final String 通知状態未送信 = "10";
    private static final String 通知区分書面郵送 = "01";
    private static final String 通知区分電子交付 = "02";

    private static java.util.Map<String, BeneficiaryRow> loadBeneficiaries(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, BeneficiaryRow> index = new java.util.HashMap<>();
        int lineNo = 0;

        for (String line : java.nio.file.Files.readAllLines(path, 入出力文字コード)) {
            lineNo++;
            if (isSkippable(line)) {
                continue;
            }

            String[] c = splitCsv(line, 8, "LFBENF", lineNo);
            BeneficiaryRow row = new BeneficiaryRow(
                    c[0].trim(),
                    c[1].trim(),
                    c[2].trim(),
                    c[3].trim(),
                    c[4].trim(),
                    c[5].trim(),
                    c[6].trim(),
                    c[7].trim());

            requireNonBlank(row.policyNo, "LFBENF", lineNo, "POL-NO");
            requireNonBlank(row.beneficiaryId, "LFBENF", lineNo, "BENEFICIARY-ID");

            index.put(row.policyNo, row);
        }

        return index;
    }

    private static DispatchResult dispatch(
            java.nio.file.Path paymentPath,
            java.nio.file.Path noticePath,
            java.util.Map<String, BeneficiaryRow> beneficiaries) throws java.io.IOException {

        int created = 0;
        int offsetSkipped = 0;
        int missingBeneficiary = 0;
        java.time.LocalDate noticeDate = java.time.LocalDate.now();
        java.util.List<String> notices = new java.util.ArrayList<>();
        int lineNo = 0;

        for (String line : java.nio.file.Files.readAllLines(paymentPath, 入出力文字コード)) {
            lineNo++;
            if (isSkippable(line)) {
                continue;
            }

            String[] c = splitCsv(line, 5, "LFPAYF", lineNo);
            PaymentRow payment = new PaymentRow(
                    c[0].trim(),
                    c[1].trim(),
                    parseAmount(c[2], "LFPAYF", lineNo, "GROSS-AMT"),
                    parseRate(c[3], "LFPAYF", lineNo, "REDUCTION-RATE"),
                    parseAmount(c[4], "LFPAYF", lineNo, "PAYOUT-AMT"));

            requireNonBlank(payment.payId, "LFPAYF", lineNo, "PAY-ID");
            requireNonBlank(payment.claimId, "LFPAYF", lineNo, "CLAIM-ID");

            if (payment.payoutAmount.compareTo(java.math.BigDecimal.ZERO) == 0) {
                offsetSkipped++;
                continue;
            }

            BeneficiaryRow beneficiary = beneficiaries.get(payment.claimId);
            if (beneficiary == null) {
                missingBeneficiary++;
                continue;
            }

            java.math.BigDecimal netCheck = payment.grossAmount
                    .multiply(java.math.BigDecimal.ONE.subtract(payment.reductionRate))
                    .setScale(0, java.math.RoundingMode.DOWN);
            if (netCheck.compareTo(payment.payoutAmount) < 0) {
                throw new IllegalArgumentException("LFPAYF " + lineNo + " 行目: 支払額が計算上限を超過しています。");
            }

            String noticeId = buildNoticeId(noticeDate, payment.payId, created + 1);
            String noticeType = decideNoticeType(beneficiary);
            notices.add(String.join(",",
                    noticeId,
                    payment.claimId,
                    beneficiary.beneficiaryId,
                    noticeDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE),
                    noticeType,
                    通知状態未送信));
            created++;
        }

        java.nio.file.Files.write(noticePath, notices, 入出力文字コード,
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.TRUNCATE_EXISTING,
                java.nio.file.StandardOpenOption.WRITE);

        return new DispatchResult(created, offsetSkipped, missingBeneficiary);
    }

    private static String decideNoticeType(BeneficiaryRow beneficiary) {
        if ("1".equals(beneficiary.paymentPriority)) {
            return 通知区分書面郵送;
        }
        return 通知区分電子交付;
    }

    private static String buildNoticeId(java.time.LocalDate noticeDate, String payId, int sequence) {
        String normalizedPayId = payId.replaceAll("[^0-9A-Za-z]", "");
        if (normalizedPayId.length() > 10) {
            normalizedPayId = normalizedPayId.substring(normalizedPayId.length() - 10);
        }
        return "NT" + noticeDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + String.format("%06d", sequence)
                + normalizedPayId;
    }

    private static boolean isSkippable(String line) {
        String trimmed = line == null ? "" : line.trim();
        return trimmed.isEmpty() || trimmed.startsWith("#");
    }

    private static String[] splitCsv(String line, int expected, String fileName, int lineNo) {
        String[] c = line.split(",", -1);
        if (c.length != expected) {
            throw new IllegalArgumentException(fileName + " " + lineNo + " 行目: 項目数が不正です。");
        }
        return c;
    }

    private static java.math.BigDecimal parseAmount(String value, String fileName, int lineNo, String itemName) {
        try {
            java.math.BigDecimal amount = new java.math.BigDecimal(value.trim());
            if (amount.compareTo(java.math.BigDecimal.ZERO) < 0) {
                throw new IllegalArgumentException(fileName + " " + lineNo + " 行目: " + itemName + " が負数です。");
            }
            return amount;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(fileName + " " + lineNo + " 行目: " + itemName + " が数値ではありません。");
        }
    }

    private static java.math.BigDecimal parseRate(String value, String fileName, int lineNo, String itemName) {
        try {
            java.math.BigDecimal rate = new java.math.BigDecimal(value.trim());
            if (rate.compareTo(java.math.BigDecimal.ZERO) < 0 || rate.compareTo(一年以上経過支払割合) > 0) {
                throw new IllegalArgumentException(fileName + " " + lineNo + " 行目: " + itemName + " が範囲外です。");
            }
            return rate;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(fileName + " " + lineNo + " 行目: " + itemName + " が数値ではありません。");
        }
    }

    private static void requireNonBlank(String value, String fileName, int lineNo, String itemName) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(fileName + " " + lineNo + " 行目: " + itemName + " が未設定です。");
        }
    }

    private static final class PaymentRow {
        private final String payId;
        private final String claimId;
        private final java.math.BigDecimal grossAmount;
        private final java.math.BigDecimal reductionRate;
        private final java.math.BigDecimal payoutAmount;

        private PaymentRow(String payId, String claimId, java.math.BigDecimal grossAmount,
                           java.math.BigDecimal reductionRate, java.math.BigDecimal payoutAmount) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmount = grossAmount;
            this.reductionRate = reductionRate;
            this.payoutAmount = payoutAmount;
        }
    }

    private static final class BeneficiaryRow {
        private final String policyNo;
        private final String beneficiaryId;
        private final String nameKana;
        private final String relationshipKbn;
        private final String bankCode;
        private final String branchCode;
        private final String accountNo;
        private final String paymentPriority;

        private BeneficiaryRow(String policyNo, String beneficiaryId, String nameKana, String relationshipKbn,
                               String bankCode, String branchCode, String accountNo, String paymentPriority) {
            this.policyNo = policyNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCode = bankCode;
            this.branchCode = branchCode;
            this.accountNo = accountNo;
            this.paymentPriority = paymentPriority;
        }
    }

    private static final class DispatchResult {
        private final int createdCount;
        private final int offsetSkipCount;
        private final int missingBeneficiaryCount;

        private DispatchResult(int createdCount, int offsetSkipCount, int missingBeneficiaryCount) {
            this.createdCount = createdCount;
            this.offsetSkipCount = offsetSkipCount;
            this.missingBeneficiaryCount = missingBeneficiaryCount;
        }
    }
}
