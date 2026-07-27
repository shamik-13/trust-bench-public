package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-07-09  みらいペイ システム部 返金・チャージバックチーム  初版作成
 * 1.01  2025-01-22  みらいペイ システム部 返金・チャージバックチーム  受付可否はPRRSPFの判定結果に従う方式へ整理
 */
public class RefundQueueDispatcher {
    private static final String QUEUE_READY = "N";
    private static final String QUEUE_WORK = "W";
    private static final String QUEUE_DONE = "D";
    private static final String QUEUE_ERROR = "E";
    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";
    private static final int MAX_RETRY = 3;

    public static void main(String[] a) {
        DispatcherState state = new DispatcherState("PRQDSP-" + java.time.LocalDate.now().toString().replace("-", ""));
        state.queueRows.add(new QueueRow("Q000001", "R20260628001", QUEUE_READY, 90, java.time.LocalDate.parse("2026-06-28"), ""));
        state.queueRows.add(new QueueRow("Q000002", "R20260628002", QUEUE_READY, 80, java.time.LocalDate.parse("2026-06-27"), ""));
        state.queueRows.add(new QueueRow("Q000003", "R20260628003", QUEUE_READY, 70, java.time.LocalDate.parse("2026-06-26"), "PRQDSP-OLD"));
        state.queueRows.add(new QueueRow("Q000004", "R20260628004", QUEUE_READY, 60, java.time.LocalDate.parse("2026-06-25"), ""));
        state.queueRows.add(new QueueRow("Q000005", "R20260628005", QUEUE_READY, 50, java.time.LocalDate.parse("2026-06-24"), ""));

        // PRRSPF: RefundEngine が確定した判定結果 (RS-DECISION-KBN / RS-DECLINE-REASON)。
        // ディスパッチャは受付可否を再判定せず、この判定区分に従ってキューを進める。
        state.decisions.put("R20260628001", new Decision("R20260628001", "T20260601001", DECISION_ACCEPT, "", 12800L));
        state.decisions.put("R20260628002", new Decision("R20260628002", "T20260501002", DECISION_DECLINE, "AMT", 0L));
        state.decisions.put("R20260628004", new Decision("R20260628004", "T20260401004", DECISION_DECLINE, "WIN", 0L));
        state.decisions.put("R20260628005", new Decision("R20260628005", "T20260601005", DECISION_ACCEPT, "", 70000L));

        DispatchResult result = dispatch(state);
        for (DispatchMessage message : result.messages) {
            System.out.println(message.text);
        }
    }

    private static DispatchResult dispatch(DispatcherState state) {
        DispatchResult result = new DispatchResult();

        java.util.List<QueueRow> targets = new java.util.ArrayList<>();
        for (QueueRow row : state.queueRows) {
            if ((row.lockOwner == null || row.lockOwner.isEmpty()) && QUEUE_READY.equals(row.queueKbn)) {
                targets.add(row);
            }
        }
        targets.sort(java.util.Comparator
                .comparingInt((QueueRow row) -> row.priority).reversed()
                .thenComparing(row -> row.enqueueDt)
                .thenComparing(row -> row.queueId));

        java.util.Map<String, Integer> declineCounts = new java.util.LinkedHashMap<>();
        for (QueueRow row : targets) {
            row.queueKbn = QUEUE_WORK;
            row.lockOwner = state.workerId;

            Decision decision = state.decisions.get(row.reqId);
            if (decision == null) {
                // 判定結果が未到着のキューは投入せず READY に戻す。
                row.queueKbn = QUEUE_READY;
                row.lockOwner = "";
                result.pending++;
                result.messages.add(new DispatchMessage("判定待ち: QUEUE-ID=" + row.queueId + " REQ-ID=" + row.reqId));
                continue;
            }

            if (DECISION_DECLINE.equals(decision.decisionKbn)) {
                reject(row, result, declineCounts, decision.declineReason, "判定結果=否認");
                continue;
            }
            if (!DECISION_ACCEPT.equals(decision.decisionKbn)) {
                reject(row, result, declineCounts, "ERR", "判定区分不正");
                continue;
            }

            row.queueKbn = QUEUE_DONE;
            row.lockOwner = "";
            result.accepted++;
            result.messages.add(new DispatchMessage("受付: QUEUE-ID=" + row.queueId + " REQ-ID=" + row.reqId));
        }

        for (java.util.Map.Entry<String, Integer> entry : declineCounts.entrySet()) {
            result.messages.add(new DispatchMessage("否認集計: 理由=" + entry.getKey() + " 件数=" + entry.getValue()));
        }
        result.messages.add(new DispatchMessage("処理件数: 受付=" + result.accepted + " 否認=" + result.declined
                + " 判定待ち=" + result.pending));
        return result;
    }

    private static void reject(QueueRow row, DispatchResult result, java.util.Map<String, Integer> declineCounts,
            String declineReason, String detail) {
        row.retryCount++;
        row.queueKbn = row.retryCount >= MAX_RETRY ? QUEUE_ERROR : QUEUE_READY;
        row.lockOwner = "";
        result.declined++;
        declineCounts.put(declineReason, declineCounts.getOrDefault(declineReason, 0) + 1);
        result.messages.add(new DispatchMessage("否認: QUEUE-ID=" + row.queueId + " REQ-ID=" + row.reqId
                + " 理由=" + declineReason + " 詳細=" + detail + " 戻し先=" + row.queueKbn));
    }

    private static final class DispatcherState {
        final String workerId;
        final java.util.List<QueueRow> queueRows = new java.util.ArrayList<>();
        final java.util.Map<String, Decision> decisions = new java.util.LinkedHashMap<>();

        DispatcherState(String workerId) {
            this.workerId = workerId;
        }
    }

    private static final class Decision {
        final String reqId;
        final String origTxnId;
        final String decisionKbn;
        final String declineReason;
        final long eligibleAmt;

        Decision(String reqId, String origTxnId, String decisionKbn, String declineReason, long eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class QueueRow {
        final String queueId;
        final String reqId;
        String queueKbn;
        final int priority;
        final java.time.LocalDate enqueueDt;
        String lockOwner;
        int retryCount;

        QueueRow(String queueId, String reqId, String queueKbn, int priority,
                java.time.LocalDate enqueueDt, String lockOwner) {
            this.queueId = queueId;
            this.reqId = reqId;
            this.queueKbn = queueKbn;
            this.priority = priority;
            this.enqueueDt = enqueueDt;
            this.lockOwner = lockOwner;
        }
    }

    private static final class DispatchResult {
        int accepted;
        int declined;
        int pending;
        final java.util.List<DispatchMessage> messages = new java.util.ArrayList<>();
    }

    private static final class DispatchMessage {
        final String text;

        DispatchMessage(String text) {
            this.text = text;
        }
    }
}
