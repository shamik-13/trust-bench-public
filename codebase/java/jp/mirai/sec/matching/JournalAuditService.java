/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2021/07/15  東京基盤部  ジャーナル監査サービス初版
 */

package jp.mirai.sec.matching;

public class JournalAuditService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final int DECISION_ACCEPT = 0;
    private static final int DECISION_REJECT_MARGIN = 4;
    private static final int DECISION_REJECT_NOTIONAL = 8;
    private static final int DECISION_REJECT_TICK = 12;

    private JournalAuditService() {
    }

    public static void main(String[] a) {
        java.util.List<JournalRecord> journals = syntheticJournal();
        java.util.Map<String, OrderRecord> orders = new java.util.LinkedHashMap<>();
        for (OrderRecord order : syntheticOrders()) {
            orders.put(order.orderId, order);
        }

        java.util.Map<String, java.util.List<ExecRecord>> execsByOrder = new java.util.LinkedHashMap<>();
        for (ExecRecord exec : syntheticExecs()) {
            execsByOrder.computeIfAbsent(exec.orderId, k -> new java.util.ArrayList<>()).add(exec);
        }

        java.util.List<LatencySample> samples = new java.util.ArrayList<>();
        AuditCounter counter = new AuditCounter();

        scanSequence(journals, counter, samples);
        scanDuplicatePayload(journals, counter, samples);
        scanStateTransitions(journals, orders, execsByOrder, counter, samples);
        writeSamples(samples, counter);
    }

    private static void scanSequence(java.util.List<JournalRecord> journals, AuditCounter counter,
            java.util.List<LatencySample> samples) {
        long expected = -1L;
        JournalRecord previous = null;
        for (JournalRecord journal : journals) {
            if (expected >= 0L && journal.seqNo != expected) {
                counter.sequenceGap++;
                samples.add(new LatencySample(nextSampleId(samples), journal.orderId, "SEQ-GAP",
                        previous == null ? journal.eventTs : previous.eventTs, journal.eventTs,
                        previous == null ? 0L : nanosBetween(previous.eventTs, journal.eventTs)));
            }
            expected = journal.seqNo + 1L;
            previous = journal;
        }
    }

    private static void scanDuplicatePayload(java.util.List<JournalRecord> journals, AuditCounter counter,
            java.util.List<LatencySample> samples) {
        java.util.Map<String, JournalRecord> firstByHash = new java.util.HashMap<>();
        for (JournalRecord journal : journals) {
            JournalRecord first = firstByHash.putIfAbsent(journal.payloadHash, journal);
            if (first != null) {
                counter.duplicateHash++;
                samples.add(new LatencySample(nextSampleId(samples), journal.orderId, "DUP-HASH",
                        first.eventTs, journal.eventTs, nanosBetween(first.eventTs, journal.eventTs)));
            }
        }
    }

    private static void scanStateTransitions(java.util.List<JournalRecord> journals,
            java.util.Map<String, OrderRecord> orders,
            java.util.Map<String, java.util.List<ExecRecord>> execsByOrder,
            AuditCounter counter,
            java.util.List<LatencySample> samples) {
        java.util.Map<String, String> stateByOrder = new java.util.HashMap<>();
        java.util.Map<String, java.time.Instant> lastEventTsByOrder = new java.util.HashMap<>();

        for (JournalRecord journal : journals) {
            OrderRecord order = orders.get(journal.orderId);
            if (order == null) {
                counter.stateMismatch++;
                samples.add(new LatencySample(nextSampleId(samples), journal.orderId, "ORD-NONE",
                        journal.eventTs, journal.eventTs, 0L));
                continue;
            }

            if (!order.instrCode.equals(journal.instrCode)) {
                counter.stateMismatch++;
                java.time.Instant start = lastEventTsByOrder.getOrDefault(journal.orderId, journal.eventTs);
                samples.add(new LatencySample(nextSampleId(samples), journal.orderId, "INS-MISM",
                        start, journal.eventTs, nanosBetween(start, journal.eventTs)));
            }

            String before = stateByOrder.getOrDefault(journal.orderId, "NONE");
            String after = transition(before, journal.eventKbn, order, execsByOrder.get(journal.orderId));
            if ("ERR".equals(after)) {
                counter.stateMismatch++;
                java.time.Instant start = lastEventTsByOrder.getOrDefault(journal.orderId, journal.eventTs);
                samples.add(new LatencySample(nextSampleId(samples), journal.orderId, "STATE",
                        start, journal.eventTs, nanosBetween(start, journal.eventTs)));
            } else {
                stateByOrder.put(journal.orderId, after);
            }

            java.time.Instant previousTs = lastEventTsByOrder.put(journal.orderId, journal.eventTs);
            if (previousTs != null) {
                samples.add(new LatencySample(nextSampleId(samples), journal.orderId, journal.eventKbn,
                        previousTs, journal.eventTs, nanosBetween(previousTs, journal.eventTs)));
            }
        }

        for (OrderRecord order : orders.values()) {
            long fillQty = 0L;
            long fillAmount = 0L;
            java.util.List<ExecRecord> execs = execsByOrder.get(order.orderId);
            if (execs != null) {
                for (ExecRecord exec : execs) {
                    if (!order.instrCode.equals(exec.instrCode)) {
                        counter.stateMismatch++;
                    }
                    fillQty += exec.fillQty;
                    fillAmount += exec.fillAmt;
                }
            }

            long expectedCumQty = fillQty;
            long expectedAvg = fillQty == 0L ? 0L : fillAmount / fillQty;
            if (order.cumQty != expectedCumQty || order.avgFillAmt != expectedAvg) {
                counter.stateMismatch++;
                java.time.Instant end = lastEventTsByOrder.getOrDefault(order.orderId, order.lastUpdTs);
                samples.add(new LatencySample(nextSampleId(samples), order.orderId, "EXEC-SUM",
                        order.lastUpdTs, end, Math.abs(nanosBetween(order.lastUpdTs, end))));
            }
        }
    }

    private static String transition(String before, String eventKbn, OrderRecord order, java.util.List<ExecRecord> execs) {
        if ("受付".equals(eventKbn)) {
            return "NONE".equals(before) ? "受付済" : "ERR";
        }
        if ("審査".equals(eventKbn)) {
            if (!"受付済".equals(before)) {
                return "ERR";
            }
            int decision = decisionCode(order);
            return decision == DECISION_ACCEPT ? "審査済" : "拒否";
        }
        if ("約定".equals(eventKbn)) {
            if (!"審査済".equals(before) && !"一部約定".equals(before)) {
                return "ERR";
            }
            long executedQty = 0L;
            if (execs != null) {
                for (ExecRecord exec : execs) {
                    executedQty += exec.fillQty;
                }
            }
            return executedQty >= order.cumQty && order.leavesQty == 0L ? "全部約定" : "一部約定";
        }
        if ("完了".equals(eventKbn)) {
            return ("全部約定".equals(before) || "拒否".equals(before)) ? "完了" : "ERR";
        }
        if ("取消".equals(eventKbn)) {
            return ("受付済".equals(before) || "審査済".equals(before) || "一部約定".equals(before)) ? "取消済" : "ERR";
        }
        return "ERR";
    }

    private static int decisionCode(OrderRecord order) {
        InstrumentSpec spec = spec(order.instrTier);
        long notional = order.cumQty * Math.max(order.avgFillAmt, 1L);
        if (notional > MIHFT_MAX_NOTIONAL) {
            return DECISION_REJECT_NOTIONAL;
        }
        if (order.avgFillAmt > 0L && order.avgFillAmt % spec.tick != 0L) {
            return DECISION_REJECT_TICK;
        }
        long margin = notional * spec.rateBp / 10000L;
        return margin <= MIHFT_MAX_NOTIONAL ? DECISION_ACCEPT : DECISION_REJECT_MARGIN;
    }

    private static InstrumentSpec spec(int tier) {
        if (tier == 1) {
            return new InstrumentSpec(1, 1000, 100);
        }
        if (tier == 2) {
            return new InstrumentSpec(2, 2000, 500);
        }
        return new InstrumentSpec(3, 4000, 1000);
    }

    private static void writeSamples(java.util.List<LatencySample> samples, AuditCounter counter) {
        System.out.println("SAMPLE-ID,ORDER-ID,STAGE-KBN,START-TS,END-TS,LATENCY-NS");
        for (LatencySample sample : samples) {
            System.out.println(sample.sampleId + "," + sample.orderId + "," + sample.stageKbn + ","
                    + sample.startTs + "," + sample.endTs + "," + sample.latencyNs);
        }
        System.out.println("監査件数,SEQ欠番=" + counter.sequenceGap + ",HASH重複=" + counter.duplicateHash
                + ",状態不整合=" + counter.stateMismatch);
    }

    private static long nanosBetween(java.time.Instant start, java.time.Instant end) {
        return java.time.Duration.between(start, end).toNanos();
    }

    private static String nextSampleId(java.util.List<LatencySample> samples) {
        return String.format("LAT%06d", samples.size() + 1);
    }

    private static java.util.List<JournalRecord> syntheticJournal() {
        java.util.List<JournalRecord> list = new java.util.ArrayList<>();
        list.add(new JournalRecord(1001L, ts("2025-01-15T00:00:00.000001Z"), "受付", "ORD0001", "7203", "H-A01"));
        list.add(new JournalRecord(1002L, ts("2025-01-15T00:00:00.000008Z"), "審査", "ORD0001", "7203", "H-A02"));
        list.add(new JournalRecord(1003L, ts("2025-01-15T00:00:00.000021Z"), "約定", "ORD0001", "7203", "H-A03"));
        list.add(new JournalRecord(1005L, ts("2025-01-15T00:00:00.000030Z"), "完了", "ORD0001", "7203", "H-A04"));
        list.add(new JournalRecord(1006L, ts("2025-01-15T00:00:00.000040Z"), "受付", "ORD0002", "9984", "H-B01"));
        list.add(new JournalRecord(1007L, ts("2025-01-15T00:00:00.000044Z"), "審査", "ORD0002", "9984", "H-B02"));
        list.add(new JournalRecord(1008L, ts("2025-01-15T00:00:00.000050Z"), "完了", "ORD0002", "9984", "H-B02"));
        list.add(new JournalRecord(1009L, ts("2025-01-15T00:00:00.000060Z"), "約定", "ORD0003", "6758", "H-C01"));
        return list;
    }

    private static java.util.List<OrderRecord> syntheticOrders() {
        java.util.List<OrderRecord> list = new java.util.ArrayList<>();
        list.add(new OrderRecord("ORD0001", "CIF10001", "7203", "全部約定", 1, 0L, 1000L, 2300L,
                ts("2025-01-15T00:00:00.000029Z")));
        list.add(new OrderRecord("ORD0002", "CIF10002", "9984", "拒否", 2, 0L, 0L, 0L,
                ts("2025-01-15T00:00:00.000049Z")));
        list.add(new OrderRecord("ORD0003", "CIF10003", "6758", "受付済", 1, 500L, 0L, 0L,
                ts("2025-01-15T00:00:00.000059Z")));
        return list;
    }

    private static java.util.List<ExecRecord> syntheticExecs() {
        java.util.List<ExecRecord> list = new java.util.ArrayList<>();
        list.add(new ExecRecord("EX00001", "ORD0001", "7203", "B", 400L, 920000L,
                ts("2025-01-15T00:00:00.000020Z")));
        list.add(new ExecRecord("EX00002", "ORD0001", "7203", "B", 600L, 1380000L,
                ts("2025-01-15T00:00:00.000024Z")));
        list.add(new ExecRecord("EX00003", "ORD0002", "9984", "S", 100L, 1000000L,
                ts("2025-01-15T00:00:00.000051Z")));
        return list;
    }

    private static java.time.Instant ts(String value) {
        return java.time.Instant.parse(value);
    }

    private static final class JournalRecord {
        final long seqNo;
        final java.time.Instant eventTs;
        final String eventKbn;
        final String orderId;
        final String instrCode;
        final String payloadHash;

        JournalRecord(long seqNo, java.time.Instant eventTs, String eventKbn, String orderId, String instrCode,
                String payloadHash) {
            this.seqNo = seqNo;
            this.eventTs = eventTs;
            this.eventKbn = eventKbn;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.payloadHash = payloadHash;
        }
    }

    private static final class OrderRecord {
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String stateKbn;
        final int instrTier;
        final long leavesQty;
        final long cumQty;
        final long avgFillAmt;
        final java.time.Instant lastUpdTs;

        OrderRecord(String orderId, String cifNo, String instrCode, String stateKbn, int instrTier, long leavesQty,
                long cumQty, long avgFillAmt, java.time.Instant lastUpdTs) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.stateKbn = stateKbn;
            this.instrTier = instrTier;
            this.leavesQty = leavesQty;
            this.cumQty = cumQty;
            this.avgFillAmt = avgFillAmt;
            this.lastUpdTs = lastUpdTs;
        }
    }

    private static final class ExecRecord {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final java.time.Instant execTs;

        ExecRecord(String execId, String orderId, String instrCode, String sideKbn, long fillQty, long fillAmt,
                java.time.Instant execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class LatencySample {
        final String sampleId;
        final String orderId;
        final String stageKbn;
        final java.time.Instant startTs;
        final java.time.Instant endTs;
        final long latencyNs;

        LatencySample(String sampleId, String orderId, String stageKbn, java.time.Instant startTs,
                java.time.Instant endTs, long latencyNs) {
            this.sampleId = sampleId;
            this.orderId = orderId;
            this.stageKbn = stageKbn;
            this.startTs = startTs;
            this.endTs = endTs;
            this.latencyNs = latencyNs;
        }
    }

    private static final class InstrumentSpec {
        final int tier;
        final int rateBp;
        final long tick;

        InstrumentSpec(int tier, int rateBp, long tick) {
            this.tier = tier;
            this.rateBp = rateBp;
            this.tick = tick;
        }
    }

    private static final class AuditCounter {
        int sequenceGap;
        int duplicateHash;
        int stateMismatch;
    }
}
