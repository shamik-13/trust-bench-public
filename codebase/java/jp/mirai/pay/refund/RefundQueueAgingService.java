package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-10-15  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundQueueAgingService {
    private static final String QUEUE_NORMAL = "通常";
    private static final String QUEUE_REVIEW = "審査";
    private static final String QUEUE_RETRY = "再配分";

    private static final int PRIORITY_MIN = 1;
    private static final int PRIORITY_MAX = 9;
    private static final int NORMAL_STAY_LIMIT_MIN = 120;
    private static final int URGENT_STAY_LIMIT_MIN = 30;
    private static final int LOCK_LIMIT_MIN = 45;
    private static final long NOW_MIN = 202606281030L;

    public static void main(String[] a) {
        PrquefRecord[] prquef = startingPrquef();
        PdfrdfRecord[] pdfrdf = startingPdfrdf();

        Result result = redistribute(prquef, pdfrdf);

        System.out.println("返金キュー滞留監視サービス 処理日=2026/06/28 10:30");
        System.out.println("読込 PRQUEF=" + result.readPrquef + " PDFRDF=" + result.readPdfrdf);
        System.out.println("更新 PRQUEF=" + result.writePrquef + " 再配分=" + result.redistributed
                + " 審査優先度調整=" + result.reviewAdjusted + " ロック解除=" + result.lockReleased
                + " 除外=" + result.rejected);

        for (PrquefRecord record : result.records) {
            System.out.println(record.toLine());
        }
    }

    private static Result redistribute(PrquefRecord[] queueRecords, PdfrdfRecord[] fraudRecords) {
        Result result = new Result(queueRecords.length, fraudRecords.length, copy(queueRecords));
        for (int i = 0; i < result.records.length; i++) {
            PrquefRecord q = result.records[i];
            if (!validQueue(q)) {
                result.rejected++;
                continue;
            }

            boolean underReview = hasActiveReview(q.reqId, fraudRecords);
            int stayMinutes = minutesBetween(q.enqueueDt, NOW_MIN);
            boolean lockExpired = q.lockOwner.length() > 0 && stayMinutes >= LOCK_LIMIT_MIN;
            boolean stale = stayMinutes >= stayLimit(q.priority);

            if (underReview) {
                int adjusted = clamp(q.priority + reviewBoost(q.reqId, fraudRecords));
                if (adjusted != q.priority || lockExpired) {
                    result.records[i] = new PrquefRecord(q.queueId, q.reqId, QUEUE_REVIEW, adjusted, q.enqueueDt, "");
                    result.writePrquef++;
                    result.reviewAdjusted++;
                    if (lockExpired) {
                        result.lockReleased++;
                    }
                }
                continue;
            }

            if (lockExpired || stale) {
                int adjusted = clamp(q.priority + agingBoost(stayMinutes, lockExpired));
                result.records[i] = new PrquefRecord(q.queueId, q.reqId, QUEUE_RETRY, adjusted, q.enqueueDt, "");
                result.writePrquef++;
                result.redistributed++;
                if (lockExpired) {
                    result.lockReleased++;
                }
            }
        }
        return result;
    }

    private static boolean validQueue(PrquefRecord q) {
        return q != null
                && q.queueId.length() > 0
                && q.reqId.length() > 0
                && (QUEUE_NORMAL.equals(q.queueKbn) || QUEUE_REVIEW.equals(q.queueKbn) || QUEUE_RETRY.equals(q.queueKbn))
                && q.priority >= PRIORITY_MIN
                && q.priority <= PRIORITY_MAX
                && q.enqueueDt > 0L;
    }

    private static boolean hasActiveReview(String reqId, PdfrdfRecord[] fraudRecords) {
        for (PdfrdfRecord f : fraudRecords) {
            if (f != null && reqId.equals(f.reqId) && f.judgeDt == 0L) {
                return true;
            }
        }
        return false;
    }

    private static int reviewBoost(String reqId, PdfrdfRecord[] fraudRecords) {
        int maxScore = 0;
        for (PdfrdfRecord f : fraudRecords) {
            if (f != null && reqId.equals(f.reqId) && f.judgeDt == 0L && f.score > maxScore) {
                maxScore = f.score;
            }
        }
        if (maxScore >= 900) {
            return 3;
        }
        if (maxScore >= 750) {
            return 2;
        }
        return 1;
    }

    private static int agingBoost(int stayMinutes, boolean lockExpired) {
        int boost = stayMinutes >= 360 ? 3 : stayMinutes >= 180 ? 2 : 1;
        return lockExpired ? boost + 1 : boost;
    }

    private static int stayLimit(int priority) {
        return priority >= 7 ? URGENT_STAY_LIMIT_MIN : NORMAL_STAY_LIMIT_MIN;
    }

    private static int clamp(int priority) {
        if (priority < PRIORITY_MIN) {
            return PRIORITY_MIN;
        }
        if (priority > PRIORITY_MAX) {
            return PRIORITY_MAX;
        }
        return priority;
    }

    private static int minutesBetween(long from, long to) {
        int fromHour = (int) ((from / 100L) % 100L);
        int fromMin = (int) (from % 100L);
        int toHour = (int) ((to / 100L) % 100L);
        int toMin = (int) (to % 100L);
        return (toHour * 60 + toMin) - (fromHour * 60 + fromMin);
    }

    private static PrquefRecord[] copy(PrquefRecord[] source) {
        PrquefRecord[] copied = new PrquefRecord[source.length];
        System.arraycopy(source, 0, copied, 0, source.length);
        return copied;
    }

    private static PrquefRecord[] startingPrquef() {
        return new PrquefRecord[] {
                new PrquefRecord("Q260628001", "RF2606280001", QUEUE_NORMAL, 5, 202606280740L, ""),
                new PrquefRecord("Q260628002", "RF2606280002", QUEUE_NORMAL, 8, 202606280955L, "OPR042"),
                new PrquefRecord("Q260628003", "RF2606280003", QUEUE_NORMAL, 3, 202606280915L, "OPR017"),
                new PrquefRecord("Q260628004", "RF2606280004", QUEUE_REVIEW, 6, 202606280810L, "OPR031"),
                new PrquefRecord("Q260628005", "RF2606280005", QUEUE_NORMAL, 4, 202606280830L, ""),
                new PrquefRecord("Q260628006", "RF2606280006", QUEUE_NORMAL, 9, 202606281006L, ""),
                new PrquefRecord("Q260628007", "RF2606280007", QUEUE_REVIEW, 7, 202606280945L, "OPR055"),
                new PrquefRecord("Q260628008", "RF2606280008", QUEUE_NORMAL, 2, 202606281015L, "")
        };
    }

    private static PdfrdfRecord[] startingPdfrdf() {
        return new PdfrdfRecord[] {
                new PdfrdfRecord("F260628011", "RF2606280004", "WLT730044", 930, "多重返金", 0L),
                new PdfrdfRecord("F260628012", "RF2606280005", "WLT310219", 770, "端末相違", 0L),
                new PdfrdfRecord("F260628013", "RF2606280007", "WLT883102", 820, "口座相違", 0L),
                new PdfrdfRecord("F260627099", "RF2606280003", "WLT118002", 640, "既判定", 202606280925L)
        };
    }

    private static final class Result {
        private final int readPrquef;
        private final int readPdfrdf;
        private final PrquefRecord[] records;
        private int writePrquef;
        private int redistributed;
        private int reviewAdjusted;
        private int lockReleased;
        private int rejected;

        private Result(int readPrquef, int readPdfrdf, PrquefRecord[] records) {
            this.readPrquef = readPrquef;
            this.readPdfrdf = readPdfrdf;
            this.records = records;
        }
    }

    private static final class PrquefRecord {
        private final String queueId;
        private final String reqId;
        private final String queueKbn;
        private final int priority;
        private final long enqueueDt;
        private final String lockOwner;

        private PrquefRecord(String queueId, String reqId, String queueKbn, int priority, long enqueueDt, String lockOwner) {
            this.queueId = queueId;
            this.reqId = reqId;
            this.queueKbn = queueKbn;
            this.priority = priority;
            this.enqueueDt = enqueueDt;
            this.lockOwner = lockOwner;
        }

        private String toLine() {
            return "PRQUEF QUEUE-ID=" + queueId + " REQ-ID=" + reqId + " QUEUE-KBN=" + queueKbn
                    + " PRIORITY=" + priority + " ENQUEUE-DT=" + enqueueDt + " LOCK-OWNER=" + lockOwner;
        }
    }

    private static final class PdfrdfRecord {
        private final String fraudId;
        private final String reqId;
        private final String walletId;
        private final int score;
        private final String ruleHitCd;
        private final long judgeDt;

        private PdfrdfRecord(String fraudId, String reqId, String walletId, int score, String ruleHitCd, long judgeDt) {
            this.fraudId = fraudId;
            this.reqId = reqId;
            this.walletId = walletId;
            this.score = score;
            this.ruleHitCd = ruleHitCd;
            this.judgeDt = judgeDt;
        }
    }
}
