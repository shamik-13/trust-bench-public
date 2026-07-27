/**
 * 変更履歴
 * 版数    年月日        担当      概要
 * 1.00    2024-02-13    開発部    合成レイテンシ計測サービス初版
 */

package jp.mirai.sec.matching;

public class LatencyBenchmarkService {
    private static final java.time.ZoneId JST = java.time.ZoneId.of("Asia/Tokyo");
    private static final java.time.format.DateTimeFormatter TS_FMT =
            java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSS").withZone(JST);

    private LatencyBenchmarkService() {
    }

    public static void main(String[] a) {
        java.util.List<Scjrnf> journal = syntheticJournal();
        java.util.List<Sclatf> latencyFile = new java.util.ArrayList<Sclatf>();
        java.util.Map<String, RefData> refData = syntheticRefData();

        validateJournal(journal, refData);

        java.util.List<Sclatf> newSamples = buildStageSamples(journal);
        latencyFile.addAll(newSamples);

        java.util.Map<String, java.util.List<Long>> buckets = aggregate(latencyFile, refData);
        printReport(buckets);
        printLatencyFile(latencyFile);
    }

    private static void validateJournal(java.util.List<Scjrnf> journal, java.util.Map<String, RefData> refData) {
        java.util.Set<Long> seqNos = new java.util.HashSet<Long>();
        java.util.Map<String, java.time.Instant> lastByOrder = new java.util.HashMap<String, java.time.Instant>();

        for (Scjrnf r : journal) {
            if (r.seqNo <= 0) {
                throw new IllegalArgumentException("順序番号不正:" + r.seqNo);
            }
            if (!seqNos.add(r.seqNo)) {
                throw new IllegalArgumentException("順序番号重複:" + r.seqNo);
            }
            if (r.eventTs == null || r.eventKbn == null || r.orderId == null || r.instrCode == null) {
                throw new IllegalArgumentException("必須項目不足:" + r.seqNo);
            }
            if (!refData.containsKey(r.instrCode)) {
                throw new IllegalArgumentException("銘柄未登録:" + r.instrCode);
            }
            java.time.Instant prev = lastByOrder.get(r.orderId);
            if (prev != null && r.eventTs.isBefore(prev)) {
                throw new IllegalArgumentException("時刻逆転:" + r.orderId);
            }
            lastByOrder.put(r.orderId, r.eventTs);
        }
    }

    private static java.util.List<Sclatf> buildStageSamples(java.util.List<Scjrnf> journal) {
        java.util.Map<String, java.util.List<Scjrnf>> byOrder = new java.util.LinkedHashMap<String, java.util.List<Scjrnf>>();
        for (Scjrnf r : journal) {
            java.util.List<Scjrnf> rows = byOrder.get(r.orderId);
            if (rows == null) {
                rows = new java.util.ArrayList<Scjrnf>();
                byOrder.put(r.orderId, rows);
            }
            rows.add(r);
        }

        java.util.List<Sclatf> out = new java.util.ArrayList<Sclatf>();
        long sampleSeq = 1L;

        for (java.util.Map.Entry<String, java.util.List<Scjrnf>> e : byOrder.entrySet()) {
            java.util.List<Scjrnf> rows = e.getValue();
            rows.sort(new java.util.Comparator<Scjrnf>() {
                public int compare(Scjrnf x, Scjrnf y) {
                    return x.eventTs.compareTo(y.eventTs);
                }
            });

            for (int i = 1; i < rows.size(); i++) {
                Scjrnf start = rows.get(i - 1);
                Scjrnf end = rows.get(i);
                String stage = stageKbn(start.eventKbn, end.eventKbn);
                if (stage == null) {
                    continue;
                }

                long latencyNs = java.time.Duration.between(start.eventTs, end.eventTs).toNanos();
                if (latencyNs < 0L) {
                    throw new IllegalArgumentException("負レイテンシ:" + e.getKey());
                }
                if (latencyNs == 0L) {
                    continue;
                }

                out.add(new Sclatf("S" + zeroPad(sampleSeq++, 8), e.getKey(), stage, start.eventTs, end.eventTs, latencyNs));
            }
        }
        return out;
    }

    private static String stageKbn(String from, String to) {
        if ("受信".equals(from) && "検証済".equals(to)) {
            return "入力検証";
        }
        if ("検証済".equals(from) && "制御判定前".equals(to)) {
            return "制御準備";
        }
        if ("制御判定前".equals(from) && "送信済".equals(to)) {
            return "送信準備";
        }
        return null;
    }

    private static java.util.Map<String, java.util.List<Long>> aggregate(
            java.util.List<Sclatf> rows, java.util.Map<String, RefData> refData) {
        java.util.Map<String, String> orderInstr = new java.util.HashMap<String, String>();
        for (Scjrnf r : syntheticJournal()) {
            if ("受信".equals(r.eventKbn)) {
                orderInstr.put(r.orderId, r.instrCode);
            }
        }

        java.util.Map<String, java.util.List<Long>> buckets = new java.util.TreeMap<String, java.util.List<Long>>();
        for (Sclatf r : rows) {
            String instr = orderInstr.get(r.orderId);
            if (instr == null) {
                continue;
            }
            RefData ref = refData.get(instr);
            if (ref == null) {
                continue;
            }
            String key = ref.instrumentTier + "/" + ref.sessionType + "/" + r.stageKbn;
            java.util.List<Long> latencies = buckets.get(key);
            if (latencies == null) {
                latencies = new java.util.ArrayList<Long>();
                buckets.put(key, latencies);
            }
            latencies.add(r.latencyNs);
        }
        return buckets;
    }

    private static void printReport(java.util.Map<String, java.util.List<Long>> buckets) {
        System.out.println("区分,件数,P50ナノ秒,P95ナノ秒,P99ナノ秒,最大ナノ秒");
        for (java.util.Map.Entry<String, java.util.List<Long>> e : buckets.entrySet()) {
            java.util.List<Long> v = e.getValue();
            java.util.Collections.sort(v);
            System.out.println(e.getKey()
                    + "," + v.size()
                    + "," + percentile(v, 50)
                    + "," + percentile(v, 95)
                    + "," + percentile(v, 99)
                    + "," + v.get(v.size() - 1));
        }
    }

    private static void printLatencyFile(java.util.List<Sclatf> rows) {
        System.out.println("標本ID,注文ID,段階区分,開始時刻,終了時刻,レイテンシナノ秒");
        for (Sclatf r : rows) {
            System.out.println(r.sampleId + "," + r.orderId + "," + r.stageKbn + ","
                    + TS_FMT.format(r.startTs) + "," + TS_FMT.format(r.endTs) + "," + r.latencyNs);
        }
    }

    private static long percentile(java.util.List<Long> sorted, int pct) {
        if (sorted.isEmpty()) {
            return 0L;
        }
        double rank = (pct / 100.0d) * (sorted.size() - 1);
        int lo = (int) Math.floor(rank);
        int hi = (int) Math.ceil(rank);
        if (lo == hi) {
            return sorted.get(lo);
        }
        double weight = rank - lo;
        return Math.round(sorted.get(lo) * (1.0d - weight) + sorted.get(hi) * weight);
    }

    private static String zeroPad(long value, int width) {
        String s = Long.toString(value);
        StringBuilder b = new StringBuilder();
        for (int i = s.length(); i < width; i++) {
            b.append('0');
        }
        return b.append(s).toString();
    }

    private static java.util.Map<String, RefData> syntheticRefData() {
        java.util.Map<String, RefData> m = new java.util.HashMap<String, RefData>();
        m.put("7203", new RefData("主力", "前場"));
        m.put("9984", new RefData("主力", "前場"));
        m.put("8306", new RefData("主力", "後場"));
        m.put("4755", new RefData("準主力", "後場"));
        m.put("9432", new RefData("準主力", "前場"));
        return m;
    }

    private static java.util.List<Scjrnf> syntheticJournal() {
        java.util.List<Scjrnf> r = new java.util.ArrayList<Scjrnf>();
        java.time.Instant base = java.time.ZonedDateTime.of(2026, 6, 27, 9, 0, 0, 0, JST).toInstant();

        addOrder(r, 1, base.plusNanos(1_000_000L), "O202501150001", "7203", 21_000L, 58_000L, 34_000L);
        addOrder(r, 5, base.plusNanos(2_000_000L), "O202501150002", "9984", 25_000L, 64_000L, 39_000L);
        addOrder(r, 9, base.plusNanos(3_000_000L), "O202501150003", "9432", 19_000L, 51_000L, 31_000L);
        addOrder(r, 13, base.plusNanos(4_000_000L), "O202501150004", "8306", 32_000L, 82_000L, 45_000L);
        addOrder(r, 17, base.plusNanos(5_000_000L), "O202501150005", "4755", 37_000L, 96_000L, 52_000L);
        return r;
    }

    private static void addOrder(java.util.List<Scjrnf> r, long seq, java.time.Instant t, String orderId,
                                 String instr, long vNs, long pNs, long sNs) {
        r.add(new Scjrnf(seq, t, "受信", orderId, instr, "H" + seq));
        r.add(new Scjrnf(seq + 1, t.plusNanos(vNs), "検証済", orderId, instr, "H" + (seq + 1)));
        r.add(new Scjrnf(seq + 2, t.plusNanos(vNs + pNs), "制御判定前", orderId, instr, "H" + (seq + 2)));
        r.add(new Scjrnf(seq + 3, t.plusNanos(vNs + pNs + sNs), "送信済", orderId, instr, "H" + (seq + 3)));
    }

    private static final class Scjrnf {
        final long seqNo;
        final java.time.Instant eventTs;
        final String eventKbn;
        final String orderId;
        final String instrCode;
        final String payloadHash;

        Scjrnf(long seqNo, java.time.Instant eventTs, String eventKbn,
               String orderId, String instrCode, String payloadHash) {
            this.seqNo = seqNo;
            this.eventTs = eventTs;
            this.eventKbn = eventKbn;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.payloadHash = payloadHash;
        }
    }

    private static final class Sclatf {
        final String sampleId;
        final String orderId;
        final String stageKbn;
        final java.time.Instant startTs;
        final java.time.Instant endTs;
        final long latencyNs;

        Sclatf(String sampleId, String orderId, String stageKbn,
               java.time.Instant startTs, java.time.Instant endTs, long latencyNs) {
            this.sampleId = sampleId;
            this.orderId = orderId;
            this.stageKbn = stageKbn;
            this.startTs = startTs;
            this.endTs = endTs;
            this.latencyNs = latencyNs;
        }
    }

    private static final class RefData {
        final String instrumentTier;
        final String sessionType;

        RefData(String instrumentTier, String sessionType) {
            this.instrumentTier = instrumentTier;
            this.sessionType = sessionType;
        }
    }
}
