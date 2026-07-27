package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数   年月日       担当       概要
 * 1.00   2024-04-01   佐藤       初版作成
 * 1.01   2024-07-15   田中       医籍登録番号形式チェック追加
 */
public class ClaimDocumentValidator {

    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final int FULL_PAYMENT_AFTER_ONE_YEAR_PERCENT = 100;
    private static final int CONTRACTUAL_COVERAGE_TERM_YEARS = 10;

    private static final java.util.regex.Pattern DEATH_CERTIFICATE_REF_PATTERN =
            java.util.regex.Pattern.compile("DC-[0-9]{4}-[0-9]{6}");
    private static final java.util.regex.Pattern MEDICAL_EXAMINER_LICENSE_PATTERN =
            java.util.regex.Pattern.compile("[0-9]{6}");

    private static java.util.List<ValidationResult> validateClaims(
            java.util.List<LfclmfRecord> claims,
            BeneficiaryVerificationService beneficiaryService) {

        java.util.List<ValidationResult> results = new java.util.ArrayList<ValidationResult>();

        if (claims == null || claims.isEmpty()) {
            results.add(ValidationResult.error("", "", "E9001", "請求レコードがありません"));
            return results;
        }

        for (LfclmfRecord claim : claims) {
            if (claim == null) {
                results.add(ValidationResult.error("", "", "E9002", "請求レコードが不正です"));
                continue;
            }

            validateRequiredClaimItems(claim, results);
            validateSubmissionDocuments(claim, results);
            validateCoverageWindow(claim, results);
            validateBeneficiaryIdentity(claim, beneficiaryService, results);

            if (CLAIM_STATUS_PAYABLE.equals(claim.claimStatusKbn)
                    && !hasErrorForClaim(results, claim.claimId)) {
                int paymentPercent = calculatePaymentPercent(claim);
                results.add(ValidationResult.ok(
                        claim.claimId,
                        claim.polNo,
                        "I0000",
                        "請求書類検証が完了しました。支払割合=" + paymentPercent + "%"));
            }
        }

        return results;
    }

    private static void validateRequiredClaimItems(LfclmfRecord claim, java.util.List<ValidationResult> results) {
        if (isBlank(claim.claimId)) {
            results.add(ValidationResult.error("", claim.polNo, "E1001", "請求番号が未設定です"));
        }
        if (isBlank(claim.polNo)) {
            results.add(ValidationResult.error(claim.claimId, "", "E1002", "証券番号が未設定です"));
        }
        if (claim.sumAssuredAmt == null || claim.sumAssuredAmt.signum() <= 0) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E1003", "保険金額が不正です"));
        }
        if (claim.loanBalanceAmt == null || claim.loanBalanceAmt.signum() < 0) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E1004", "貸付残高が不正です"));
        }
        if (claim.respStartDt == null) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E1005", "責任開始日が未設定です"));
        }
        if (claim.eventDt == null) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E1006", "事故発生日が未設定です"));
        }
        if (!CLAIM_STATUS_PAYABLE.equals(claim.claimStatusKbn)
                && !"05".equals(claim.claimStatusKbn)
                && !"09".equals(claim.claimStatusKbn)) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E1007", "請求状態区分が不正です"));
        }
    }

    private static void validateSubmissionDocuments(LfclmfRecord claim, java.util.List<ValidationResult> results) {
        if (isBlank(claim.deathCertificateRefCode)) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E2001", "死亡診断書参照コードが未提出です"));
        } else if (!DEATH_CERTIFICATE_REF_PATTERN.matcher(claim.deathCertificateRefCode).matches()) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E2002", "死亡診断書参照コードの形式が不正です"));
        }

        if (!claim.beneficiaryConsentFlag) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E2003", "受取人同意が未取得です"));
        }

        if (isBlank(claim.medicalExaminerLicenseNo)) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E2004", "医師免許番号が未提出です"));
        } else if (!MEDICAL_EXAMINER_LICENSE_PATTERN.matcher(claim.medicalExaminerLicenseNo).matches()) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E2005", "医師免許番号の形式が不正です"));
        }
    }

    private static void validateCoverageWindow(LfclmfRecord claim, java.util.List<ValidationResult> results) {
        if (claim.respStartDt == null || claim.eventDt == null) {
            return;
        }

        java.time.LocalDate coverageEndDt = claim.respStartDt.plusYears(CONTRACTUAL_COVERAGE_TERM_YEARS);
        if (claim.eventDt.isBefore(claim.respStartDt)) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E3001", "事故発生日が責任開始日前です"));
        }
        if (claim.eventDt.isAfter(coverageEndDt)) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E3002", "事故発生日が保障期間外です"));
        }
    }

    private static void validateBeneficiaryIdentity(
            LfclmfRecord claim,
            BeneficiaryVerificationService beneficiaryService,
            java.util.List<ValidationResult> results) {

        if (isBlank(claim.beneficiaryId)) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, "E4001", "受取人番号が未設定です"));
            return;
        }

        BeneficiaryVerification verification = beneficiaryService.verify(claim.polNo, claim.beneficiaryId);
        if (!verification.verified) {
            results.add(ValidationResult.error(claim.claimId, claim.polNo, verification.errorCode, verification.message));
        }
    }

    private static int calculatePaymentPercent(LfclmfRecord claim) {
        if (claim.respStartDt == null || claim.eventDt == null) {
            return 0;
        }
        if (!claim.eventDt.isBefore(claim.respStartDt.plusYears(1))) {
            return FULL_PAYMENT_AFTER_ONE_YEAR_PERCENT;
        }

        return FULL_PAYMENT_AFTER_ONE_YEAR_PERCENT;
    }

    private static boolean hasErrorForClaim(java.util.List<ValidationResult> results, String claimId) {
        for (ValidationResult result : results) {
            if ("ERROR".equals(result.level) && equalsText(result.claimId, claimId)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static boolean equalsText(String left, String right) {
        return left == null ? right == null : left.equals(right);
    }

    private static final class LfclmfRecord {
        private final String claimId;
        private final String polNo;
        private final java.math.BigDecimal sumAssuredAmt;
        private final java.math.BigDecimal loanBalanceAmt;
        private final java.time.LocalDate respStartDt;
        private final java.time.LocalDate eventDt;
        private final String claimStatusKbn;
        private final String deathCertificateRefCode;
        private final boolean beneficiaryConsentFlag;
        private final String medicalExaminerLicenseNo;
        private final String beneficiaryId;

        private LfclmfRecord(
                String claimId,
                String polNo,
                java.math.BigDecimal sumAssuredAmt,
                java.math.BigDecimal loanBalanceAmt,
                java.time.LocalDate respStartDt,
                java.time.LocalDate eventDt,
                String claimStatusKbn,
                String deathCertificateRefCode,
                boolean beneficiaryConsentFlag,
                String medicalExaminerLicenseNo,
                String beneficiaryId) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
            this.deathCertificateRefCode = deathCertificateRefCode;
            this.beneficiaryConsentFlag = beneficiaryConsentFlag;
            this.medicalExaminerLicenseNo = medicalExaminerLicenseNo;
            this.beneficiaryId = beneficiaryId;
        }
    }

    private static final class ValidationResult {
        private final String claimId;
        private final String polNo;
        private final String level;
        private final String code;
        private final String message;

        private ValidationResult(String claimId, String polNo, String level, String code, String message) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.level = level;
            this.code = code;
            this.message = message;
        }

        private static ValidationResult ok(String claimId, String polNo, String code, String message) {
            return new ValidationResult(claimId, polNo, "INFO", code, message);
        }

        private static ValidationResult error(String claimId, String polNo, String code, String message) {
            return new ValidationResult(claimId, polNo, "ERROR", code, message);
        }

        private String toOperatorMessage() {
            return "請求番号=" + claimId
                    + " 証券番号=" + polNo
                    + " レベル=" + level
                    + " コード=" + code
                    + " 内容=" + message;
        }
    }

    private static final class BeneficiaryVerification {
        private final boolean verified;
        private final String errorCode;
        private final String message;

        private BeneficiaryVerification(boolean verified, String errorCode, String message) {
            this.verified = verified;
            this.errorCode = errorCode;
            this.message = message;
        }
    }

    private static final class BeneficiaryVerificationService {
        private BeneficiaryVerification verify(String polNo, String beneficiaryId) {
            if (isBlank(polNo)) {
                return new BeneficiaryVerification(false, "E4002", "証券番号未設定のため受取人確認ができません");
            }
            if (!beneficiaryId.startsWith("BEN")) {
                return new BeneficiaryVerification(false, "E4003", "受取人番号の形式が不正です");
            }
            return new BeneficiaryVerification(true, "", "受取人確認済み");
        }
    }
}
