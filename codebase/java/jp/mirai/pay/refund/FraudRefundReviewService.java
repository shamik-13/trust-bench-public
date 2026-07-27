package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-11-07  みらいペイ システム部 返金・チャージバックチーム  不正返金審査振分の初版作成
 */
public class FraudRefundReviewService {
    private static final String QUEUE_MANUAL_REVIEW = "MANUAL";
    private static final String QUEUE_REJUDGE = "REJUDGE";
    private static final String QUEUE_RELEASE_HOLD = "RELEASE";
    private static final int SCORE_MANUAL_THRESHOLD = 80;
    private static final int SCORE_REJUDGE_THRESHOLD = 45;

    public static void main(String[] a) {
        java.util.List<PdfRdf> fraudRows = java.util.Arrays.asList(
                new PdfRdf("FRD-0001", "REQ-1001", "WLT-001", 92, "R90", null),
                new PdfRdf("FRD-0002", "REQ-1002", "WLT-002", 67, "R21", null),
                new PdfRdf("FRD-0003", "REQ-1003", "WLT-003", 18, "", null),
                new PdfRdf("FRD-0004", "REQ-1004", "WLT-004", 41, "R10", null),
                new PdfRdf("FRD-0005", "REQ-1005", "WLT-005", 99, "R99", java.time.LocalDate.of(2026, 6, 26)),
                new PdfRdf("FRD-0006", "REQ-1002", "WLT-002", 72, "R31", null),
                new PdfRdf("FRD-0007", "REQ-1006", "WLT-006", 55, "R20", null)
        );

        java.util.List<PrReqF> requestRows = java.util.Arrays.asList(
                new PrReqF("REQ-1001", "TXN-9001", 12800L, java.time.LocalDate.of(2026, 6, 27), "30"),
                new PrReqF("REQ-1002", "TXN-9002", 3400L, java.time.LocalDate.of(2026, 6, 27), "10"),
                new PrReqF("REQ-1003", "TXN-9003", 980L, java.time.LocalDate.of(2026, 6, 25), "20"),
                new PrReqF("REQ-1004", "TXN-9004", 51000L, java.time.LocalDate.of(2026, 6, 20), "10"),
                new PrReqF("REQ-1005", "TXN-9005", 7200L, java.time.LocalDate.of(2026, 6, 24), "30")
        );

        java.util.List<PrQueF> queueRows = route(fraudRows, requestRows, java.time.LocalDate.of(2026, 6, 28));
        System.out.println("QUEUE-ID,REQ-ID,QUEUE-KBN,PRIORITY,ENQUEUE-DT,LOCK-OWNER");
        for (PrQueF row : queueRows) {
            System.out.println(row.toCsv());
        }
    }

    private static java.util.List<PrQueF> route(
            java.util.List<PdfRdf> fraudRows,
            java.util.List<PrReqF> requestRows,
            java.time.LocalDate businessDate) {
        java.util.Map<String, PrReqF> requestsById = new java.util.HashMap<>();
        for (PrReqF request : requestRows) {
            if (request.reqId != null && !request.reqId.trim().isEmpty()) {
                requestsById.put(request.reqId, request);
            }
        }

        java.util.List<PdfRdf> orderedFraudRows = new java.util.ArrayList<>(fraudRows);
        orderedFraudRows.sort(new java.util.Comparator<PdfRdf>() {
            @Override
            public int compare(PdfRdf left, PdfRdf right) {
                int byReq = left.reqId.compareTo(right.reqId);
                if (byReq != 0) {
                    return byReq;
                }
                return Integer.compare(right.score, left.score);
            }
        });

        java.util.List<PrQueF> queueRows = new java.util.ArrayList<>();
        java.util.Set<String> acceptedReqIds = new java.util.HashSet<>();
        int sequence = 1;

        for (PdfRdf fraud : orderedFraudRows) {
            if (fraud.reqId == null || fraud.reqId.trim().isEmpty()) {
                continue;
            }
            if (fraud.judgeDt != null) {
                acceptedReqIds.add(fraud.reqId);
                continue;
            }
            if (acceptedReqIds.contains(fraud.reqId)) {
                continue;
            }

            PrReqF request = requestsById.get(fraud.reqId);
            RouteDecision decision = decide(fraud, request);
            queueRows.add(new PrQueF(
                    String.format("QUE-%06d", sequence++),
                    fraud.reqId,
                    decision.queueKbn,
                    decision.priority,
                    businessDate,
                    ""
            ));
            acceptedReqIds.add(fraud.reqId);
        }

        return queueRows;
    }

    private static RouteDecision decide(PdfRdf fraud, PrReqF request) {
        if (request == null || request.origTxnId == null || request.origTxnId.trim().isEmpty()) {
            return new RouteDecision(QUEUE_REJUDGE, 1);
        }

        boolean chargeback = "30".equals(request.reqReason);
        boolean hardRule = "R90".equals(fraud.ruleHitCd) || "R99".equals(fraud.ruleHitCd);
        boolean weakRule = fraud.ruleHitCd == null || fraud.ruleHitCd.trim().isEmpty();

        if (fraud.score >= SCORE_MANUAL_THRESHOLD || hardRule || chargeback && fraud.score >= SCORE_REJUDGE_THRESHOLD) {
            return new RouteDecision(QUEUE_MANUAL_REVIEW, fraud.score >= 95 ? 1 : 2);
        }
        if (fraud.score >= SCORE_REJUDGE_THRESHOLD || !weakRule) {
            return new RouteDecision(QUEUE_REJUDGE, 3);
        }
        return new RouteDecision(QUEUE_RELEASE_HOLD, 5);
    }

    private static final class PdfRdf {
        private final String fraudId;
        private final String reqId;
        private final String walletId;
        private final int score;
        private final String ruleHitCd;
        private final java.time.LocalDate judgeDt;

        private PdfRdf(String fraudId, String reqId, String walletId, int score, String ruleHitCd, java.time.LocalDate judgeDt) {
            this.fraudId = fraudId;
            this.reqId = reqId;
            this.walletId = walletId;
            this.score = score;
            this.ruleHitCd = ruleHitCd;
            this.judgeDt = judgeDt;
        }
    }

    private static final class PrReqF {
        private final String reqId;
        private final String origTxnId;
        private final long refundAmt;
        private final java.time.LocalDate reqDt;
        private final String reqReason;

        private PrReqF(String reqId, String origTxnId, long refundAmt, java.time.LocalDate reqDt, String reqReason) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.refundAmt = refundAmt;
            this.reqDt = reqDt;
            this.reqReason = reqReason;
        }
    }

    private static final class PrQueF {
        private final String queueId;
        private final String reqId;
        private final String queueKbn;
        private final int priority;
        private final java.time.LocalDate enqueueDt;
        private final String lockOwner;

        private PrQueF(String queueId, String reqId, String queueKbn, int priority, java.time.LocalDate enqueueDt, String lockOwner) {
            this.queueId = queueId;
            this.reqId = reqId;
            this.queueKbn = queueKbn;
            this.priority = priority;
            this.enqueueDt = enqueueDt;
            this.lockOwner = lockOwner;
        }

        private String toCsv() {
            return queueId + "," + reqId + "," + queueKbn + "," + priority + "," + enqueueDt + "," + lockOwner;
        }
    }

    private static final class RouteDecision {
        private final String queueKbn;
        private final int priority;

        private RouteDecision(String queueKbn, int priority) {
            this.queueKbn = queueKbn;
            this.priority = priority;
        }
    }
}
