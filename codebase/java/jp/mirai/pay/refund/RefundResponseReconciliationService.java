package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-06-11  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundResponseReconciliationService {

    private static final String DECISION_ACCEPTED = "A";
    private static final String DECISION_DECLINED = "D";

    private static final String REASON_CUSTOMER = "10";
    private static final String REASON_MERCHANT = "20";
    private static final String REASON_CHARGEBACK = "30";

    private static final String DECLINE_WINDOW = "WIN";
    private static final String DECLINE_AMOUNT = "AMT";
    private static final String DECLINE_TRANSACTION = "TXN";

    public static void main(String[] a) {
        java.util.List<PrreqfRecord> requests = loadPrreqf();
        java.util.List<PrrspfRecord> responses = loadPrrspf();

        java.util.List<ReconciliationResult> results = reconcile(requests, responses);
        for (ReconciliationResult result : results) {
            System.out.println(result.toOperatorLine());
        }
    }

    private static java.util.List<ReconciliationResult> reconcile(
            java.util.List<PrreqfRecord> requests,
            java.util.List<PrrspfRecord> responses) {

        java.util.Map<String, java.util.List<PrrspfRecord>> responsesByRequestId = new java.util.LinkedHashMap<>();
        for (PrrspfRecord response : responses) {
            validateResponse(response);
            responsesByRequestId.computeIfAbsent(response.requestId, key -> new java.util.ArrayList<>()).add(response);
        }

        java.util.List<ReconciliationResult> results = new java.util.ArrayList<>();
        java.util.Set<String> requestedIds = new java.util.LinkedHashSet<>();

        for (PrreqfRecord request : requests) {
            validateRequest(request);
            requestedIds.add(request.requestId);

            java.util.List<PrrspfRecord> matchedResponses =
                    responsesByRequestId.getOrDefault(request.requestId, java.util.Collections.emptyList());

            java.util.List<String> warnings = new java.util.ArrayList<>();
            if (matchedResponses.isEmpty()) {
                warnings.add("応答なし");
                results.add(new ReconciliationResult(request.requestId, request.originalTransactionId,
                        request.refundAmount, null, 0, warnings));
                continue;
            }

            if (matchedResponses.size() > 1) {
                warnings.add("重複応答");
            }

            PrrspfRecord latestResponse = matchedResponses.get(matchedResponses.size() - 1);
            for (PrrspfRecord response : matchedResponses) {
                if (!request.originalTransactionId.equals(response.originalTransactionId)) {
                    warnings.add("原取引ID不一致");
                }
                if (DECISION_ACCEPTED.equals(response.decisionCode)
                        && request.refundAmount.compareTo(response.eligibleAmount) != 0) {
                    warnings.add("依頼額不一致");
                }
                if (DECISION_DECLINED.equals(response.decisionCode)
                        && response.declineReason == null) {
                    warnings.add("否認理由なし");
                }
            }

            results.add(new ReconciliationResult(request.requestId, request.originalTransactionId,
                    request.refundAmount, latestResponse, matchedResponses.size(), distinct(warnings)));
        }

        for (java.util.Map.Entry<String, java.util.List<PrrspfRecord>> entry : responsesByRequestId.entrySet()) {
            if (!requestedIds.contains(entry.getKey())) {
                PrrspfRecord latestResponse = entry.getValue().get(entry.getValue().size() - 1);
                java.util.List<String> warnings = new java.util.ArrayList<>();
                warnings.add("依頼なし応答");
                if (entry.getValue().size() > 1) {
                    warnings.add("重複応答");
                }
                results.add(new ReconciliationResult(entry.getKey(), latestResponse.originalTransactionId,
                        java.math.BigDecimal.ZERO, latestResponse, entry.getValue().size(), warnings));
            }
        }

        return results;
    }

    private static java.util.List<String> distinct(java.util.List<String> values) {
        return new java.util.ArrayList<>(new java.util.LinkedHashSet<>(values));
    }

    private static void validateRequest(PrreqfRecord request) {
        requireText(request.requestId, "REQ-ID");
        requireText(request.originalTransactionId, "ORIG-TXN-ID");
        if (request.refundAmount == null || request.refundAmount.signum() <= 0) {
            throw new IllegalArgumentException("REFUND-AMT不正: " + request.requestId);
        }
        requireText(request.requestDate, "REQ-DT");
        if (!REASON_CUSTOMER.equals(request.requestReason)
                && !REASON_MERCHANT.equals(request.requestReason)
                && !REASON_CHARGEBACK.equals(request.requestReason)) {
            throw new IllegalArgumentException("REQ-REASON不正: " + request.requestId);
        }
    }

    private static void validateResponse(PrrspfRecord response) {
        requireText(response.requestId, "REQ-ID");
        requireText(response.originalTransactionId, "ORIG-TXN-ID");
        if (!DECISION_ACCEPTED.equals(response.decisionCode)
                && !DECISION_DECLINED.equals(response.decisionCode)) {
            throw new IllegalArgumentException("DECISION-KBN不正: " + response.requestId);
        }
        if (DECISION_DECLINED.equals(response.decisionCode)
                && !DECLINE_WINDOW.equals(response.declineReason)
                && !DECLINE_AMOUNT.equals(response.declineReason)
                && !DECLINE_TRANSACTION.equals(response.declineReason)) {
            throw new IllegalArgumentException("DECLINE-REASON不正: " + response.requestId);
        }
        if (response.eligibleAmount == null || response.eligibleAmount.signum() < 0) {
            throw new IllegalArgumentException("ELIGIBLE-AMT不正: " + response.requestId);
        }
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "未設定");
        }
    }

    private static java.util.List<PrreqfRecord> loadPrreqf() {
        java.util.List<PrreqfRecord> rows = new java.util.ArrayList<>();
        rows.add(new PrreqfRecord("RQ-20260628-0001", "TX-20260620-9182",
                money("12800"), "20260628", REASON_CUSTOMER));
        rows.add(new PrreqfRecord("RQ-20260628-0002", "TX-20260618-1027",
                money("4500"), "20260628", REASON_MERCHANT));
        rows.add(new PrreqfRecord("RQ-20260628-0003", "TX-20260530-7751",
                money("21000"), "20260628", REASON_CHARGEBACK));
        rows.add(new PrreqfRecord("RQ-20260628-0004", "TX-20260622-3188",
                money("9800"), "20260628", REASON_CUSTOMER));
        return rows;
    }

    private static java.util.List<PrrspfRecord> loadPrrspf() {
        java.util.List<PrrspfRecord> rows = new java.util.ArrayList<>();
        rows.add(new PrrspfRecord("RQ-20260628-0001", "TX-20260620-9182",
                DECISION_ACCEPTED, null, money("12800")));
        rows.add(new PrrspfRecord("RQ-20260628-0002", "TX-20260618-1027",
                DECISION_ACCEPTED, null, money("4000")));
        rows.add(new PrrspfRecord("RQ-20260628-0003", "TX-20260530-7751",
                DECISION_DECLINED, DECLINE_WINDOW, money("0")));
        rows.add(new PrrspfRecord("RQ-20260628-0003", "TX-20260530-7751",
                DECISION_ACCEPTED, null, money("21000")));
        rows.add(new PrrspfRecord("RQ-20260628-9999", "TX-20260619-4470",
                DECISION_DECLINED, DECLINE_TRANSACTION, money("0")));
        return rows;
    }

    private static java.math.BigDecimal money(String value) {
        return new java.math.BigDecimal(value);
    }

    private static final class PrreqfRecord {
        private final String requestId;
        private final String originalTransactionId;
        private final java.math.BigDecimal refundAmount;
        private final String requestDate;
        private final String requestReason;

        private PrreqfRecord(String requestId, String originalTransactionId,
                             java.math.BigDecimal refundAmount, String requestDate, String requestReason) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.refundAmount = refundAmount;
            this.requestDate = requestDate;
            this.requestReason = requestReason;
        }
    }

    private static final class PrrspfRecord {
        private final String requestId;
        private final String originalTransactionId;
        private final String decisionCode;
        private final String declineReason;
        private final java.math.BigDecimal eligibleAmount;

        private PrrspfRecord(String requestId, String originalTransactionId,
                             String decisionCode, String declineReason, java.math.BigDecimal eligibleAmount) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.decisionCode = decisionCode;
            this.declineReason = declineReason;
            this.eligibleAmount = eligibleAmount;
        }
    }

    private static final class ReconciliationResult {
        private final String requestId;
        private final String originalTransactionId;
        private final java.math.BigDecimal requestAmount;
        private final PrrspfRecord latestResponse;
        private final int responseCount;
        private final java.util.List<String> warnings;

        private ReconciliationResult(String requestId, String originalTransactionId,
                                     java.math.BigDecimal requestAmount, PrrspfRecord latestResponse,
                                     int responseCount, java.util.List<String> warnings) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.requestAmount = requestAmount;
            this.latestResponse = latestResponse;
            this.responseCount = responseCount;
            this.warnings = warnings;
        }

        private String toOperatorLine() {
            String decision = latestResponse == null ? "-" : latestResponse.decisionCode;
            String decline = latestResponse == null || latestResponse.declineReason == null
                    ? "-"
                    : latestResponse.declineReason;
            String warningText = warnings.isEmpty() ? "正常" : String.join(",", warnings);
            return "REQ-ID=" + requestId
                    + " ORIG-TXN-ID=" + originalTransactionId
                    + " REFUND-AMT=" + requestAmount.toPlainString()
                    + " 最新判定=" + decision
                    + " 否認理由=" + decline
                    + " 応答件数=" + responseCount
                    + " 警告=" + warningText;
        }
    }
}
