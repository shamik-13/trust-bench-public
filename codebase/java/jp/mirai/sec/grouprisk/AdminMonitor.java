package jp.mirai.sec.grouprisk;

public class AdminMonitor {
    /**
     * 変更履歴
     * 版数  年月日      担当        概要
     * 1.0   2023-04-18  岡本 涼 (E-294)    管理・監視ベンチマーク初版
     */
    public static void main(String[] a) {
        java.util.List<NodeSample> samples = java.util.Arrays.asList(
                new NodeSample("tokyo-gw-01", "注文入口", true, false, 420, 18, 42, 71, 0),
                new NodeSample("tokyo-gw-02", "注文入口", true, false, 780, 31, 64, 82, 0),
                new NodeSample("osaka-gw-01", "注文入口", true, false, 960, 48, 91, 86, 0),
                new NodeSample("tokyo-risk-01", "事前審査", true, false, 310, 22, 55, 77, 0),
                new NodeSample("tokyo-risk-02", "事前審査", true, false, 1180, 76, 142, 91, 2),
                new NodeSample("osaka-risk-01", "事前審査", true, false, 1320, 88, 166, 94, 4),
                new NodeSample("tokyo-oms-01", "発注制御", true, false, 540, 29, 73, 80, 0),
                new NodeSample("tokyo-oms-02", "発注制御", true, false, 1540, 94, 188, 96, 7),
                new NodeSample("osaka-oms-01", "発注制御", true, false, 1720, 110, 224, 97, 9),
                new NodeSample("tokyo-md-01", "市況配信", true, false, 260, 12, 35, 69, 0),
                new NodeSample("tokyo-md-02", "市況配信", false, false, 0, 0, 0, 0, 15),
                new NodeSample("osaka-md-01", "市況配信", true, false, 880, 45, 108, 88, 1)
        );

        ControlPolicy policy = new ControlPolicy(1000, 1500, 75, 100, 130, 180, 90, 95, 3, 8);
        Summary summary = aggregate(samples);

        System.out.println("管理・監視 ベンチマーク");
        System.out.println("対象ノード数=" + summary.count + " 稼働=" + summary.liveCount + " 停止=" + summary.downCount);
        System.out.println("総キュー深度=" + summary.totalQueueDepth
                + " 最大キュー深度=" + summary.maxQueueDepth
                + " 平均遅延=" + summary.averageLatencyMs() + "ミリ秒"
                + " 最大遅延=" + summary.maxLatencyMs + "ミリ秒"
                + " 平均ＣＰＵ=" + summary.averageCpuPercent() + "％");

        java.util.Map<String, Bucket> byRole = bucketByRole(samples);
        for (java.util.Map.Entry<String, Bucket> entry : byRole.entrySet()) {
            Bucket bucket = entry.getValue();
            System.out.println("系統=" + entry.getKey()
                    + " 台数=" + bucket.count
                    + " 総キュー=" + bucket.totalQueueDepth
                    + " 最大遅延=" + bucket.maxLatencyMs + "ミリ秒"
                    + " 障害数=" + bucket.totalErrorCount);
        }

        java.util.List<Action> actions = decide(samples, policy);
        for (Action action : actions) {
            System.out.println(action.nodeName + " 判定=" + action.kind + " 理由=" + action.reason);
        }

        int throttleCount = 0;
        int killSwitchCount = 0;
        for (Action action : actions) {
            if ("流量制限".equals(action.kind)) {
                throttleCount++;
            } else if ("停止遮断".equals(action.kind)) {
                killSwitchCount++;
            }
        }
        System.out.println("管理判断 集計 流量制限=" + throttleCount + " 停止遮断=" + killSwitchCount);
    }

    private static Summary aggregate(java.util.List<NodeSample> samples) {
        Summary summary = new Summary();
        for (NodeSample sample : samples) {
            summary.count++;
            if (sample.live) {
                summary.liveCount++;
            } else {
                summary.downCount++;
            }
            summary.totalQueueDepth += sample.queueDepth;
            summary.totalLatencyMs += sample.p95LatencyMs;
            summary.totalCpuPercent += sample.cpuPercent;
            summary.totalErrorCount += sample.errorCount;
            summary.maxQueueDepth = Math.max(summary.maxQueueDepth, sample.queueDepth);
            summary.maxLatencyMs = Math.max(summary.maxLatencyMs, sample.p99LatencyMs);
        }
        return summary;
    }

    private static java.util.Map<String, Bucket> bucketByRole(java.util.List<NodeSample> samples) {
        java.util.Map<String, Bucket> buckets = new java.util.LinkedHashMap<>();
        for (NodeSample sample : samples) {
            Bucket bucket = buckets.computeIfAbsent(sample.role, k -> new Bucket());
            bucket.count++;
            bucket.totalQueueDepth += sample.queueDepth;
            bucket.totalErrorCount += sample.errorCount;
            bucket.maxLatencyMs = Math.max(bucket.maxLatencyMs, sample.p99LatencyMs);
        }
        return buckets;
    }

    private static java.util.List<Action> decide(java.util.List<NodeSample> samples, ControlPolicy policy) {
        java.util.List<Action> actions = new java.util.ArrayList<>();
        for (NodeSample sample : samples) {
            validate(sample);

            if (!sample.live) {
                actions.add(new Action(sample.nodeName, "停止遮断", "稼働信号なし"));
                continue;
            }
            if (sample.killSwitchArmed) {
                actions.add(new Action(sample.nodeName, "停止遮断", "遮断準備中"));
                continue;
            }
            if (sample.queueDepth >= policy.killQueueDepth
                    || sample.p95LatencyMs >= policy.killLatencyMs
                    || sample.p99LatencyMs >= policy.killP99LatencyMs
                    || sample.cpuPercent >= policy.killCpuPercent
                    || sample.errorCount >= policy.killErrorCount) {
                actions.add(new Action(sample.nodeName, "停止遮断", severeReason(sample, policy)));
                continue;
            }
            if (sample.queueDepth >= policy.throttleQueueDepth
                    || sample.p95LatencyMs >= policy.throttleLatencyMs
                    || sample.p99LatencyMs >= policy.throttleP99LatencyMs
                    || sample.cpuPercent >= policy.throttleCpuPercent
                    || sample.errorCount >= policy.throttleErrorCount) {
                actions.add(new Action(sample.nodeName, "流量制限", throttleReason(sample, policy)));
            } else {
                actions.add(new Action(sample.nodeName, "継続監視", "閾値内"));
            }
        }
        return actions;
    }

    private static void validate(NodeSample sample) {
        if (sample.nodeName == null || sample.nodeName.trim().isEmpty()) {
            throw new IllegalArgumentException("ノード名が空です");
        }
        if (sample.role == null || sample.role.trim().isEmpty()) {
            throw new IllegalArgumentException("系統名が空です");
        }
        if (sample.queueDepth < 0 || sample.p95LatencyMs < 0 || sample.p99LatencyMs < 0
                || sample.cpuPercent < 0 || sample.cpuPercent > 100 || sample.errorCount < 0) {
            throw new IllegalArgumentException("監視値が範囲外です: " + sample.nodeName);
        }
        if (sample.p99LatencyMs < sample.p95LatencyMs) {
            throw new IllegalArgumentException("遅延分位が逆転しています: " + sample.nodeName);
        }
    }

    private static String severeReason(NodeSample sample, ControlPolicy policy) {
        java.util.List<String> reasons = new java.util.ArrayList<>();
        if (sample.queueDepth >= policy.killQueueDepth) {
            reasons.add("キュー深度過大");
        }
        if (sample.p95LatencyMs >= policy.killLatencyMs) {
            reasons.add("通常遅延過大");
        }
        if (sample.p99LatencyMs >= policy.killP99LatencyMs) {
            reasons.add("最大遅延過大");
        }
        if (sample.cpuPercent >= policy.killCpuPercent) {
            reasons.add("ＣＰＵ逼迫");
        }
        if (sample.errorCount >= policy.killErrorCount) {
            reasons.add("障害多発");
        }
        return String.join("・", reasons);
    }

    private static String throttleReason(NodeSample sample, ControlPolicy policy) {
        java.util.List<String> reasons = new java.util.ArrayList<>();
        if (sample.queueDepth >= policy.throttleQueueDepth) {
            reasons.add("キュー深度上昇");
        }
        if (sample.p95LatencyMs >= policy.throttleLatencyMs) {
            reasons.add("通常遅延上昇");
        }
        if (sample.p99LatencyMs >= policy.throttleP99LatencyMs) {
            reasons.add("最大遅延上昇");
        }
        if (sample.cpuPercent >= policy.throttleCpuPercent) {
            reasons.add("ＣＰＵ高負荷");
        }
        if (sample.errorCount >= policy.throttleErrorCount) {
            reasons.add("障害増加");
        }
        return String.join("・", reasons);
    }

    private static final class NodeSample {
        final String nodeName;
        final String role;
        final boolean live;
        final boolean killSwitchArmed;
        final int queueDepth;
        final int p95LatencyMs;
        final int p99LatencyMs;
        final int cpuPercent;
        final int errorCount;

        NodeSample(String nodeName, String role, boolean live, boolean killSwitchArmed,
                   int queueDepth, int p95LatencyMs, int p99LatencyMs, int cpuPercent, int errorCount) {
            this.nodeName = nodeName;
            this.role = role;
            this.live = live;
            this.killSwitchArmed = killSwitchArmed;
            this.queueDepth = queueDepth;
            this.p95LatencyMs = p95LatencyMs;
            this.p99LatencyMs = p99LatencyMs;
            this.cpuPercent = cpuPercent;
            this.errorCount = errorCount;
        }
    }

    private static final class ControlPolicy {
        final int throttleQueueDepth;
        final int killQueueDepth;
        final int throttleLatencyMs;
        final int throttleP99LatencyMs;
        final int killLatencyMs;
        final int killP99LatencyMs;
        final int throttleCpuPercent;
        final int killCpuPercent;
        final int throttleErrorCount;
        final int killErrorCount;

        ControlPolicy(int throttleQueueDepth, int killQueueDepth, int throttleLatencyMs,
                      int throttleP99LatencyMs, int killLatencyMs, int killP99LatencyMs,
                      int throttleCpuPercent, int killCpuPercent, int throttleErrorCount,
                      int killErrorCount) {
            this.throttleQueueDepth = throttleQueueDepth;
            this.killQueueDepth = killQueueDepth;
            this.throttleLatencyMs = throttleLatencyMs;
            this.throttleP99LatencyMs = throttleP99LatencyMs;
            this.killLatencyMs = killLatencyMs;
            this.killP99LatencyMs = killP99LatencyMs;
            this.throttleCpuPercent = throttleCpuPercent;
            this.killCpuPercent = killCpuPercent;
            this.throttleErrorCount = throttleErrorCount;
            this.killErrorCount = killErrorCount;
        }
    }

    private static final class Summary {
        int count;
        int liveCount;
        int downCount;
        int totalQueueDepth;
        int maxQueueDepth;
        int totalLatencyMs;
        int maxLatencyMs;
        int totalCpuPercent;
        int totalErrorCount;

        int averageLatencyMs() {
            return count == 0 ? 0 : totalLatencyMs / count;
        }

        int averageCpuPercent() {
            return count == 0 ? 0 : totalCpuPercent / count;
        }
    }

    private static final class Bucket {
        int count;
        int totalQueueDepth;
        int maxLatencyMs;
        int totalErrorCount;
    }

    private static final class Action {
        final String nodeName;
        final String kind;
        final String reason;

        Action(String nodeName, String kind, String reason) {
            this.nodeName = nodeName;
            this.kind = kind;
            this.reason = reason;
        }
    }
}
