package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-10-29  みらいペイ システム部 返金・チャージバックチーム  不正返金検知サービス初版
 */
public class FraudRefundScreeningService {
    private static final int SCORE_THRESHOLD = 70;

    public static void main(String[] a) {
        java.time.LocalDate businessDate = java.time.LocalDate.of(2026, 6, 28);
        java.util.List<RequestRow> requests = sampleRequests();
        java.util.Map<String, TransactionRow> transactions = sampleTransactions();
        java.util.Map<String, BalanceRow> balances = sampleBalances();
        java.util.List<FraudRow> fraudRows = sampleExistingFraudRows();

        ScreeningState state = new ScreeningState();
        loadExistingFraud(fraudRows, state);

        for (RequestRow request : requests) {
            TransactionRow transaction = transactions.get(request.origTxnId);
            if (transaction == null) {
                continue;
            }

            BalanceRow balance = balances.get(transaction.walletId);
            ScoreResult score = score(request, transaction, balance, state, businessDate);

            if (score.score >= SCORE_THRESHOLD) {
                FraudRow row = new FraudRow(
                    nextFraudId(fraudRows.size() + 1),
                    request.reqId,
                    transaction.walletId,
                    score.score,
                    score.ruleHitCd,
                    businessDate
                );
                fraudRows.add(row);
                state.register(row, request, transaction);
                System.out.println("不正検知記録: " + row.fraudId + " 要求=" + row.reqId + " ウォレット=" + row.walletId
                    + " スコア=" + row.score + " ルール=" + row.ruleHitCd);
            } else {
                state.registerScreened(request, transaction);
            }
        }

        System.out.println("PDFRDF件数=" + fraudRows.size());
    }

    private static ScoreResult score(
        RequestRow request,
        TransactionRow transaction,
        BalanceRow balance,
        ScreeningState state,
        java.time.LocalDate businessDate
    ) {
        int score = 0;
        java.util.List<String> hits = new java.util.ArrayList<String>();

        int walletBurst = state.walletRequestCount(transaction.walletId, request.reqDt.minusDays(3), request.reqDt);
        if (walletBurst >= 2) {
            score += 25 + Math.min(20, walletBurst * 5);
            hits.add("WLT");
        }

        int merchantBurst = state.merchantRequestCount(transaction.merchantCode, request.reqDt.minusDays(3), request.reqDt);
        if (merchantBurst >= 3) {
            score += 20 + Math.min(15, merchantBurst * 3);
            hits.add("MCH");
        }

        int originalTxnBurst = state.origTxnRequestCount(request.origTxnId, request.reqDt.minusDays(7), request.reqDt);
        if (originalTxnBurst >= 1) {
            score += 30 + Math.min(20, originalTxnBurst * 10);
            hits.add("OTX");
        }

        java.math.BigDecimal alreadyRefunded = state.refundedAmountByOrigTxn(request.origTxnId);
        java.math.BigDecimal totalRequested = alreadyRefunded.add(request.refundAmt);
        if (transaction.origTxnAmt.signum() > 0) {
            java.math.BigDecimal ratio = totalRequested.divide(transaction.origTxnAmt, 4, java.math.RoundingMode.HALF_UP);
            if (ratio.compareTo(new java.math.BigDecimal("0.80")) >= 0) {
                score += 20;
                hits.add("R80");
            }
            if (ratio.compareTo(new java.math.BigDecimal("1.00")) > 0) {
                score += 35;
                hits.add("R100");
            }
        }

        if (balance != null && balance.pendingRefundAmt.compareTo(balance.availableBal) > 0) {
            score += 15;
            hits.add("BAL");
        }

        if ("30".equals(request.reqReason)) {
            score += 10;
            hits.add("CBK");
        }

        int boundedScore = Math.min(100, score);
        String ruleHitCd = hits.isEmpty() ? "NON" : joinHits(hits);
        return new ScoreResult(boundedScore, ruleHitCd);
    }

    private static void loadExistingFraud(java.util.List<FraudRow> fraudRows, ScreeningState state) {
        for (FraudRow row : fraudRows) {
            state.registerFraudOnly(row);
        }
    }

    private static String joinHits(java.util.List<String> hits) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < hits.size(); i++) {
            if (i > 0) {
                builder.append('+');
            }
            builder.append(hits.get(i));
        }
        return builder.toString();
    }

    private static String nextFraudId(int sequence) {
        return String.format("FR%08d", Integer.valueOf(sequence));
    }

    private static java.util.List<RequestRow> sampleRequests() {
        java.util.List<RequestRow> rows = new java.util.ArrayList<RequestRow>();
        rows.add(new RequestRow("RQ2606280001", "TX2606219901", money("12800"), java.time.LocalDate.of(2026, 6, 28), "20"));
        rows.add(new RequestRow("RQ2606280002", "TX2606219901", money("6800"), java.time.LocalDate.of(2026, 6, 28), "10"));
        rows.add(new RequestRow("RQ2606280003", "TX2606204210", money("42000"), java.time.LocalDate.of(2026, 6, 28), "30"));
        rows.add(new RequestRow("RQ2606280004", "TX2606161188", money("9800"), java.time.LocalDate.of(2026, 6, 28), "20"));
        rows.add(new RequestRow("RQ2606280005", "TX2606161188", money("9400"), java.time.LocalDate.of(2026, 6, 28), "20"));
        rows.add(new RequestRow("RQ2606280006", "TX2606123344", money("1200"), java.time.LocalDate.of(2026, 6, 28), "10"));
        rows.add(new RequestRow("RQ2606280007", "TX2605017777", money("3000"), java.time.LocalDate.of(2026, 6, 28), "10"));
        return rows;
    }

    private static java.util.Map<String, TransactionRow> sampleTransactions() {
        java.util.Map<String, TransactionRow> rows = new java.util.HashMap<String, TransactionRow>();
        rows.put("TX2606219901", new TransactionRow("TX2606219901", "WL92880011", "MC104233", money("19800"), java.time.LocalDate.of(2026, 6, 21)));
        rows.put("TX2606204210", new TransactionRow("TX2606204210", "WL80550019", "MC771204", money("43000"), java.time.LocalDate.of(2026, 6, 20)));
        rows.put("TX2606161188", new TransactionRow("TX2606161188", "WL92880011", "MC104233", money("12000"), java.time.LocalDate.of(2026, 6, 16)));
        rows.put("TX2606123344", new TransactionRow("TX2606123344", "WL23001981", "MC550018", money("8800"), java.time.LocalDate.of(2026, 6, 12)));
        rows.put("TX2605017777", new TransactionRow("TX2605017777", "WL23001981", "MC550018", money("3000"), java.time.LocalDate.of(2026, 5, 1)));
        return rows;
    }

    private static java.util.Map<String, BalanceRow> sampleBalances() {
        java.util.Map<String, BalanceRow> rows = new java.util.HashMap<String, BalanceRow>();
        rows.put("WL92880011", new BalanceRow("WL92880011", money("5600"), money("11200"), java.time.LocalDate.of(2026, 6, 27)));
        rows.put("WL80550019", new BalanceRow("WL80550019", money("31000"), money("42000"), java.time.LocalDate.of(2026, 6, 27)));
        rows.put("WL23001981", new BalanceRow("WL23001981", money("9000"), money("0"), java.time.LocalDate.of(2026, 6, 20)));
        return rows;
    }

    private static java.util.List<FraudRow> sampleExistingFraudRows() {
        java.util.List<FraudRow> rows = new java.util.ArrayList<FraudRow>();
        rows.add(new FraudRow("FR00000001", "RQ2606270091", "WL92880011", 82, "WLT+OTX", java.time.LocalDate.of(2026, 6, 27)));
        rows.add(new FraudRow("FR00000002", "RQ2606270098", "WL80550019", 75, "MCH+R80", java.time.LocalDate.of(2026, 6, 27)));
        return rows;
    }

    private static java.math.BigDecimal money(String value) {
        return new java.math.BigDecimal(value);
    }

    private static final class ScreeningState {
        private final java.util.List<ScreenedRequest> screenedRequests = new java.util.ArrayList<ScreenedRequest>();
        private final java.util.Map<String, java.math.BigDecimal> refundedByOrigTxn = new java.util.HashMap<String, java.math.BigDecimal>();

        int walletRequestCount(String walletId, java.time.LocalDate from, java.time.LocalDate to) {
            int count = 0;
            for (ScreenedRequest request : screenedRequests) {
                if (walletId.equals(request.walletId) && inRange(request.reqDt, from, to)) {
                    count++;
                }
            }
            return count;
        }

        int merchantRequestCount(String merchantCode, java.time.LocalDate from, java.time.LocalDate to) {
            int count = 0;
            for (ScreenedRequest request : screenedRequests) {
                if (merchantCode.equals(request.merchantCode) && inRange(request.reqDt, from, to)) {
                    count++;
                }
            }
            return count;
        }

        int origTxnRequestCount(String origTxnId, java.time.LocalDate from, java.time.LocalDate to) {
            int count = 0;
            for (ScreenedRequest request : screenedRequests) {
                if (origTxnId.equals(request.origTxnId) && inRange(request.reqDt, from, to)) {
                    count++;
                }
            }
            return count;
        }

        java.math.BigDecimal refundedAmountByOrigTxn(String origTxnId) {
            java.math.BigDecimal amount = refundedByOrigTxn.get(origTxnId);
            return amount == null ? java.math.BigDecimal.ZERO : amount;
        }

        void register(FraudRow row, RequestRow request, TransactionRow transaction) {
            registerScreened(request, transaction);
        }

        void registerScreened(RequestRow request, TransactionRow transaction) {
            screenedRequests.add(new ScreenedRequest(
                request.reqId,
                request.origTxnId,
                transaction.walletId,
                transaction.merchantCode,
                request.refundAmt,
                request.reqDt
            ));
            java.math.BigDecimal current = refundedAmountByOrigTxn(request.origTxnId);
            refundedByOrigTxn.put(request.origTxnId, current.add(request.refundAmt));
        }

        void registerFraudOnly(FraudRow row) {
            screenedRequests.add(new ScreenedRequest(row.reqId, "", row.walletId, "", java.math.BigDecimal.ZERO, row.judgeDt));
        }

        private boolean inRange(java.time.LocalDate value, java.time.LocalDate from, java.time.LocalDate to) {
            return !value.isBefore(from) && !value.isAfter(to);
        }
    }

    private static final class RequestRow {
        private final String reqId;
        private final String origTxnId;
        private final java.math.BigDecimal refundAmt;
        private final java.time.LocalDate reqDt;
        private final String reqReason;

        private RequestRow(String reqId, String origTxnId, java.math.BigDecimal refundAmt, java.time.LocalDate reqDt, String reqReason) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.refundAmt = refundAmt;
            this.reqDt = reqDt;
            this.reqReason = reqReason;
        }
    }

    private static final class TransactionRow {
        private final String origTxnId;
        private final String walletId;
        private final String merchantCode;
        private final java.math.BigDecimal origTxnAmt;
        private final java.time.LocalDate origTxnDt;

        private TransactionRow(String origTxnId, String walletId, String merchantCode, java.math.BigDecimal origTxnAmt, java.time.LocalDate origTxnDt) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
        }
    }

    private static final class BalanceRow {
        private final String walletId;
        private final java.math.BigDecimal availableBal;
        private final java.math.BigDecimal pendingRefundAmt;
        private final java.time.LocalDate lastAdjDt;

        private BalanceRow(String walletId, java.math.BigDecimal availableBal, java.math.BigDecimal pendingRefundAmt, java.time.LocalDate lastAdjDt) {
            this.walletId = walletId;
            this.availableBal = availableBal;
            this.pendingRefundAmt = pendingRefundAmt;
            this.lastAdjDt = lastAdjDt;
        }
    }

    private static final class FraudRow {
        private final String fraudId;
        private final String reqId;
        private final String walletId;
        private final int score;
        private final String ruleHitCd;
        private final java.time.LocalDate judgeDt;

        private FraudRow(String fraudId, String reqId, String walletId, int score, String ruleHitCd, java.time.LocalDate judgeDt) {
            this.fraudId = fraudId;
            this.reqId = reqId;
            this.walletId = walletId;
            this.score = score;
            this.ruleHitCd = ruleHitCd;
            this.judgeDt = judgeDt;
        }
    }

    private static final class ScreenedRequest {
        private final String reqId;
        private final String origTxnId;
        private final String walletId;
        private final String merchantCode;
        private final java.math.BigDecimal refundAmt;
        private final java.time.LocalDate reqDt;

        private ScreenedRequest(
            String reqId,
            String origTxnId,
            String walletId,
            String merchantCode,
            java.math.BigDecimal refundAmt,
            java.time.LocalDate reqDt
        ) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.refundAmt = refundAmt;
            this.reqDt = reqDt;
        }
    }

    private static final class ScoreResult {
        private final int score;
        private final String ruleHitCd;

        private ScoreResult(int score, String ruleHitCd) {
            this.score = score;
            this.ruleHitCd = ruleHitCd;
        }
    }
}
