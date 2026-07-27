package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.0   2025-03-18  みらいペイ システム部 返金・チャージバックチーム  返金受付ファイル生成サービス初版
 */
public class RefundRequestFileBuilder {
    private static final java.time.format.DateTimeFormatter DATE_FORMAT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
    private static final java.math.BigDecimal MAX_REFUND_AMOUNT =
            new java.math.BigDecimal("9999999999");

    private RefundRequestFileBuilder() {
    }

    public static void main(String[] a) {
        java.util.List<RequestRecord> requests = readPrreqf();
        java.util.Map<String, ReasonRecord> reasons = readPrrsnf();
        java.util.List<QueueRecord> queueRecords = buildQueue(requests, reasons);

        for (QueueRecord record : queueRecords) {
            System.out.println(record.toLine());
        }
    }

    private static java.util.List<RequestRecord> readPrreqf() {
        java.util.List<RequestRecord> records = new java.util.ArrayList<>();
        records.add(new RequestRecord("REQ-20260628-0001", "TXN-20260620-8801",
                new java.math.BigDecimal("12800"), java.time.LocalDate.of(2026, 6, 28), "10"));
        records.add(new RequestRecord("REQ-20260628-0002", "TXN-20260618-1420",
                new java.math.BigDecimal("98000"), java.time.LocalDate.of(2026, 6, 28), "30"));
        records.add(new RequestRecord("REQ-20260628-0003", "TXN-20260627-6011",
                new java.math.BigDecimal("4500"), java.time.LocalDate.of(2026, 6, 28), "20"));
        records.add(new RequestRecord("REQ-20260628-0004", "TXN-20260614-9022",
                new java.math.BigDecimal("-1200"), java.time.LocalDate.of(2026, 6, 28), "10"));
        records.add(new RequestRecord("REQ-20260628-0005", "TXN-20260601-5530",
                new java.math.BigDecimal("15000000000"), java.time.LocalDate.of(2026, 6, 28), "30"));
        records.add(new RequestRecord("REQ-20260628-0006", "TXN-20260625-7712",
                new java.math.BigDecimal("3200"), java.time.LocalDate.of(2026, 6, 28), "99"));
        return records;
    }

    private static java.util.Map<String, ReasonRecord> readPrrsnf() {
        java.util.Map<String, ReasonRecord> records = new java.util.HashMap<>();
        records.put("10", new ReasonRecord("10", "CUST", 10, "0"));
        records.put("20", new ReasonRecord("20", "MERCHANT", 30, "0"));
        records.put("30", new ReasonRecord("30", "CHARGEBACK", 80, "1"));
        return records;
    }

    private static java.util.List<QueueRecord> buildQueue(
            java.util.List<RequestRecord> requests,
            java.util.Map<String, ReasonRecord> reasons) {
        java.util.List<QueueSeed> accepted = new java.util.ArrayList<>();
        java.util.Map<String, Integer> merchantBacklog = new java.util.HashMap<>();
        int sequence = 0;

        for (RequestRecord request : requests) {
            sequence++;
            ValidationResult validation = validate(request, reasons);
            RefundModel refundModel = normalize(request);

            if (!validation.accepted) {
                System.err.println("否認," + request.requestId + "," + validation.declineReason);
                continue;
            }

            String merchantKey = merchantKey(request.originalTransactionId);
            int backlog = merchantBacklog.getOrDefault(merchantKey, 0) + 1;
            merchantBacklog.put(merchantKey, backlog);

            ReasonRecord reason = reasons.get(request.reasonCode);
            int priority = priority(sequence, reason.riskWeight, backlog);
            accepted.add(new QueueSeed(request, refundModel, reason, priority));
        }

        accepted.sort(java.util.Comparator
                .comparingInt((QueueSeed seed) -> seed.priority).reversed()
                .thenComparing(seed -> seed.request.requestDate)
                .thenComparing(seed -> seed.request.requestId));

        java.util.List<QueueRecord> queue = new java.util.ArrayList<>();
        int queueNumber = 1;
        for (QueueSeed seed : accepted) {
            String queueId = "PRQ" + DATE_FORMAT.format(seed.request.requestDate)
                    + String.format("%06d", queueNumber++);
            String queueKbn = "1".equals(seed.reason.autoReviewKbn) ? "R" : "N";
            queue.add(new QueueRecord(queueId, seed.request.requestId, queueKbn,
                    seed.priority, seed.request.requestDate, ""));
        }
        return queue;
    }

    private static ValidationResult validate(
            RequestRecord request,
            java.util.Map<String, ReasonRecord> reasons) {
        if (!reasons.containsKey(request.reasonCode)) {
            return ValidationResult.declined("TXN");
        }
        if (request.refundAmount.signum() <= 0 || request.refundAmount.scale() > 0) {
            return ValidationResult.declined("AMT");
        }
        if (request.refundAmount.precision() > 10
                || request.refundAmount.compareTo(MAX_REFUND_AMOUNT) > 0) {
            return ValidationResult.declined("AMT");
        }
        return ValidationResult.accepted();
    }

    private static RefundModel normalize(RequestRecord request) {
        try {
            java.lang.reflect.Constructor<RefundModel> constructor = RefundModel.class.getDeclaredConstructor(
                    String.class, String.class, java.math.BigDecimal.class,
                    java.time.LocalDate.class, String.class);
            constructor.setAccessible(true);
            return constructor.newInstance(request.requestId, request.originalTransactionId,
                    request.refundAmount, request.requestDate, request.reasonCode);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("RefundModel正規化失敗:" + request.requestId, e);
        }
    }

    private static int priority(int receiptSequence, int riskWeight, int merchantBacklog) {
        int receiptScore = Math.max(0, 1000 - receiptSequence);
        int riskScore = riskWeight * 20;
        int backlogScore = Math.min(merchantBacklog, 50) * 15;
        return receiptScore + riskScore + backlogScore;
    }

    private static String merchantKey(String originalTransactionId) {
        int lastHyphen = originalTransactionId.lastIndexOf('-');
        if (lastHyphen < 0) {
            return originalTransactionId;
        }
        return originalTransactionId.substring(0, lastHyphen);
    }

    private static final class RequestRecord {
        private final String requestId;
        private final String originalTransactionId;
        private final java.math.BigDecimal refundAmount;
        private final java.time.LocalDate requestDate;
        private final String reasonCode;

        private RequestRecord(String requestId, String originalTransactionId,
                java.math.BigDecimal refundAmount, java.time.LocalDate requestDate, String reasonCode) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.refundAmount = refundAmount;
            this.requestDate = requestDate;
            this.reasonCode = reasonCode;
        }
    }

    private static final class ReasonRecord {
        private final String reasonCode;
        private final String reasonGroup;
        private final int riskWeight;
        private final String autoReviewKbn;

        private ReasonRecord(String reasonCode, String reasonGroup, int riskWeight, String autoReviewKbn) {
            this.reasonCode = reasonCode;
            this.reasonGroup = reasonGroup;
            this.riskWeight = riskWeight;
            this.autoReviewKbn = autoReviewKbn;
        }
    }

    private static final class QueueRecord {
        private final String queueId;
        private final String requestId;
        private final String queueKbn;
        private final int priority;
        private final java.time.LocalDate enqueueDate;
        private final String lockOwner;

        private QueueRecord(String queueId, String requestId, String queueKbn,
                int priority, java.time.LocalDate enqueueDate, String lockOwner) {
            this.queueId = queueId;
            this.requestId = requestId;
            this.queueKbn = queueKbn;
            this.priority = priority;
            this.enqueueDate = enqueueDate;
            this.lockOwner = lockOwner;
        }

        private String toLine() {
            return queueId + "," + requestId + "," + queueKbn + ","
                    + priority + "," + DATE_FORMAT.format(enqueueDate) + "," + lockOwner;
        }
    }

    private static final class QueueSeed {
        private final RequestRecord request;
        private final RefundModel refundModel;
        private final ReasonRecord reason;
        private final int priority;

        private QueueSeed(RequestRecord request, RefundModel refundModel,
                ReasonRecord reason, int priority) {
            this.request = request;
            this.refundModel = refundModel;
            this.reason = reason;
            this.priority = priority;
        }
    }

    private static final class ValidationResult {
        private final boolean accepted;
        private final String declineReason;

        private ValidationResult(boolean accepted, String declineReason) {
            this.accepted = accepted;
            this.declineReason = declineReason;
        }

        private static ValidationResult accepted() {
            return new ValidationResult(true, "");
        }

        private static ValidationResult declined(String declineReason) {
            return new ValidationResult(false, declineReason);
        }
    }
}
