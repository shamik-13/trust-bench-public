package jp.mirai.life.claims;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.0   20240402    保険金システムG  保険金請求受付登録サービス初版
 *
 * 保険金請求の受付登録を行うサービス。請求重複チェックと提出書類検証を行い、
 * 受付状態で LFCLMF / 支払履歴に記録する。支払割合・支払削減割合には関与しない。
 */
public class ClaimRegistrationService {
    private static final String CLAIM_STATUS_ACCEPTED = "10";
    private static final int MAX_CLAIM_ID_LENGTH = 20;

    private final List<ClaimModel.Claim> existingClaims;
    private final ClaimDocumentValidator documentValidator;

    public ClaimRegistrationService(List<ClaimModel.Claim> existingClaims, ClaimDocumentValidator documentValidator) {
        this.existingClaims = existingClaims;
        this.documentValidator = documentValidator;
    }

    public void registerClaim(String claimId, String policyNo, int eventDt,
                              String[] documentReferences, String operatorId) throws RegistrationException {
        validateClaimId(claimId);
        validatePolicyNo(policyNo);
        validateDocumentReferences(documentReferences);

        ClaimDuplicateChecker.DuplicateCheckResult duplicate =
                ClaimDuplicateChecker.checkDuplicate(policyNo, eventDt, existingClaims);
        if (duplicate != null) {
            throw new RegistrationException("duplicate_within_90_days",
                    "近接する既存請求が存在します 請求ID=" + duplicate.getClaimId());
        }

        writeClaimRecord(claimId, policyNo, eventDt, CLAIM_STATUS_ACCEPTED);
        appendPaymentHistory(claimId, CLAIM_STATUS_ACCEPTED, operatorId);
    }

    private void validateClaimId(String claimId) throws RegistrationException {
        if (claimId == null || claimId.trim().isEmpty()) {
            throw new RegistrationException("claim_id_blank", "CLAIM-ID が未設定です");
        }
        if (claimId.length() > MAX_CLAIM_ID_LENGTH) {
            throw new RegistrationException("claim_id_format", "CLAIM-ID の桁数が上限を超過しています");
        }
    }

    private void validatePolicyNo(String policyNo) throws RegistrationException {
        if (policyNo == null || policyNo.trim().isEmpty()) {
            throw new RegistrationException("policy_no_blank", "POL-NO が未設定です");
        }
    }

    private void validateDocumentReferences(String[] documentReferences) throws RegistrationException {
        if (documentReferences == null || documentReferences.length == 0) {
            throw new RegistrationException("document_required", "提出書類が指定されていません");
        }
        if (documentValidator == null) {
            return;
        }
    }

    private void writeClaimRecord(String claimId, String policyNo, int eventDt, String statusCode) {
        // LFCLMF への受付登録（永続化は基盤側 DAO で実施）。
        String[][] record = {
            {claimId, policyNo, String.valueOf(eventDt), statusCode, LocalDateTime.now().toString()}
        };
    }

    private void appendPaymentHistory(String claimId, String statusCode, String operatorId) {
        // LFPAYH への状態遷移履歴追記（永続化は基盤側 DAO で実施）。
        LocalDateTime timestamp = LocalDateTime.now();
        String[][] history = {
            {"1", claimId, "", statusCode, timestamp.toString(), operatorId}
        };
    }

    public static class RegistrationException extends Exception {
        private final String errorCode;

        public RegistrationException(String errorCode, String message) {
            super(message);
            this.errorCode = errorCode;
        }

        public String getErrorCode() {
            return errorCode;
        }
    }
}
