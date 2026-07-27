/**
 * 変更履歴
 * 版数  年月日    担当       概要
 * 1.00  20240219  会員系保守  ポイント残高サービス新規作成
 */
public class PointBalanceService {
    private static final String CARD_STATUS_ACTIVE = "01";
    private static final String CARD_STATUS_DELINQUENT = "09";

    private static final String CAPTURE_CONFIRMED = "C";
    private static final String CAPTURE_HOLD = "H";
    private static final String CAPTURE_SKIP = "S";

    private static final long POINT_UNIT_YEN = 100L;

    public PointBalanceResponse inquire(String memberId) {
        if (memberId == null || memberId.trim().isEmpty()) {
            throw new IllegalArgumentException("memberId is required");
        }

        String normalizedMemberId = memberId.trim();
        PointRecord pointRecord = findPointRecord(normalizedMemberId);

        if (pointRecord == null) {
            throw new IllegalArgumentException("point balance does not exist: " + normalizedMemberId);
        }

        long pendingEarnPoint = 0L;
        int excludedSalesCount = 0;
        int billableCardCount = 0;
        String nextEarnDate = "";

        for (CardRecord card : CARDS) {
            if (!card.memberId.equals(normalizedMemberId)) {
                continue;
            }

            if (!isBillableCard(card)) {
                continue;
            }

            billableCardCount++;

            for (SalesRecord sale : SALES) {
                if (!sale.cardNo.equals(card.cardNo)) {
                    continue;
                }

                if (CAPTURE_CONFIRMED.equals(sale.captureStatus)) {
                    long pointBase = sale.salesAmount - sale.taxAmount;
                    if (pointBase > 0L) {
                        pendingEarnPoint += pointBase / POINT_UNIT_YEN;
                    }

                    if (nextEarnDate.isEmpty() || sale.postingDate.compareTo(nextEarnDate) < 0) {
                        nextEarnDate = sale.postingDate;
                    }
                } else {
                    excludedSalesCount++;
                }
            }
        }

        return new PointBalanceResponse(
                normalizedMemberId,
                pointRecord.pointBalance,
                pendingEarnPoint,
                nextEarnDate.isEmpty() ? "none" : nextEarnDate,
                pointRecord.lastEarnDate,
                pointRecord.lastRedeemDate,
                pointRecord.pointStatus,
                excludedSalesCount,
                billableCardCount
        );
    }

    private static PointRecord findPointRecord(String memberId) {
        for (PointRecord point : POINTS) {
            if (point.memberId.equals(memberId)) {
                return point;
            }
        }
        return null;
    }

    private static boolean isBillableCard(CardRecord card) {
        return CARD_STATUS_ACTIVE.equals(card.cardStatus)
                || CARD_STATUS_DELINQUENT.equals(card.cardStatus);
    }

    private static final PointRecord[] POINTS = {
            new PointRecord("M0000001", 12840L, "2026-06-15", "2026-05-28", "ACTIVE"),
            new PointRecord("M0000002", 320L, "2026-04-30", "2026-06-01", "ACTIVE"),
            new PointRecord("M0000003", 0L, "2026-01-10", "2026-02-11", "STOPPED")
    };

    private static final CardRecord[] CARDS = {
            new CardRecord("4980000000000001", "M0000001", CARD_STATUS_ACTIVE),
            new CardRecord("4980000000000002", "M0000001", CARD_STATUS_ACTIVE),
            new CardRecord("4980000000000003", "M0000001", "02"),
            new CardRecord("4980000000000101", "M0000002", CARD_STATUS_DELINQUENT),
            new CardRecord("4980000000000201", "M0000003", "03")
    };

    private static final SalesRecord[] SALES = {
            new SalesRecord("4980000000000001", "2026-06-03", 12500L, 1136L, CAPTURE_CONFIRMED),
            new SalesRecord("4980000000000001", "2026-06-05", 8400L, 763L, CAPTURE_CONFIRMED),
            new SalesRecord("4980000000000002", "2026-06-09", 33100L, 3009L, CAPTURE_CONFIRMED),
            new SalesRecord("4980000000000002", "2026-06-12", 4550L, 413L, CAPTURE_HOLD),
            new SalesRecord("4980000000000003", "2026-06-13", 20000L, 1818L, CAPTURE_CONFIRMED),
            new SalesRecord("4980000000000101", "2026-06-16", 11800L, 1072L, CAPTURE_CONFIRMED),
            new SalesRecord("4980000000000101", "2026-06-19", 500000L, 0L, CAPTURE_SKIP)
    };

    private static final class PointRecord {
        private final String memberId;
        private final long pointBalance;
        private final String lastEarnDate;
        private final String lastRedeemDate;
        private final String pointStatus;

        private PointRecord(String memberId, long pointBalance, String lastEarnDate,
                            String lastRedeemDate, String pointStatus) {
            this.memberId = memberId;
            this.pointBalance = pointBalance;
            this.lastEarnDate = lastEarnDate;
            this.lastRedeemDate = lastRedeemDate;
            this.pointStatus = pointStatus;
        }
    }

    private static final class CardRecord {
        private final String cardNo;
        private final String memberId;
        private final String cardStatus;

        private CardRecord(String cardNo, String memberId, String cardStatus) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
        }
    }

    private static final class SalesRecord {
        private final String cardNo;
        private final String postingDate;
        private final long salesAmount;
        private final long taxAmount;
        private final String captureStatus;

        private SalesRecord(String cardNo, String postingDate, long salesAmount,
                            long taxAmount, String captureStatus) {
            this.cardNo = cardNo;
            this.postingDate = postingDate;
            this.salesAmount = salesAmount;
            this.taxAmount = taxAmount;
            this.captureStatus = captureStatus;
        }
    }

    public static final class PointBalanceResponse {
        public final String memberId;
        public final long pointBalance;
        public final long pendingEarnPoint;
        public final String nextEarnDate;
        public final String lastEarnDate;
        public final String lastRedeemDate;
        public final String pointStatus;
        public final int excludedSalesCount;
        public final int billableCardCount;

        public PointBalanceResponse(String memberId, long pointBalance, long pendingEarnPoint,
                                    String nextEarnDate, String lastEarnDate, String lastRedeemDate,
                                    String pointStatus, int excludedSalesCount, int billableCardCount) {
            this.memberId = memberId;
            this.pointBalance = pointBalance;
            this.pendingEarnPoint = pendingEarnPoint;
            this.nextEarnDate = nextEarnDate;
            this.lastEarnDate = lastEarnDate;
            this.lastRedeemDate = lastRedeemDate;
            this.pointStatus = pointStatus;
            this.excludedSalesCount = excludedSalesCount;
            this.billableCardCount = billableCardCount;
        }
    }
}
