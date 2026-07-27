/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2022-08-01  開発担当  初版作成
 */
public class TemporaryLimitService {
    private static final String CARD_STATUS_ACTIVE = "01";
    private static final String DECISION_APPROVE = "A";
    private static final String DECISION_DENY = "D";
    private static final String NOTICE_APPROVED = "01";
    private static final String NOTICE_DENIED = "02";
    private static final String NOTICE_CANCELLED = "03";
    private static final String NOTICE_EXPIRED = "04";
    private static final String CHANNEL_POST = "01";
    private static final String CHANNEL_MAIL = "02";
    private static final String STATUS_APPROVED = "20";
    private static final String STATUS_DENIED = "30";
    private static final String STATUS_CANCELLED = "40";
    private static final String STATUS_EXPIRED = "50";
    private static final String BASE_CURRENCY = "JPY";
    private static final long MAX_TEMP_LIMIT_MULTIPLIER = 2L;
    private static final long MIN_TEMP_LIMIT_AMOUNT = 10_000L;
    private static final long MAX_TERM_DAYS = 90L;

    public static void main(String[] a) {
        DataStore store = new DataStore();

        store.cards.put("4900000000000001", new CardRecord(
                "4900000000000001", "M000001", "01", 1_000_000L, "ヤマダタロウ"));
        store.cards.put("4900000000000002", new CardRecord(
                "4900000000000002", "M000002", "09", 800_000L, "サトウハナコ"));
        store.cards.put("4900000000000003", new CardRecord(
                "4900000000000003", "M000003", "01", 500_000L, "スズキイチロウ"));

        store.balances.add(new BalanceRecord("4900000000000001", 230_000L, 180_000L, 20260710));
        store.balances.add(new BalanceRecord("4900000000000001", 20_000L, 0L, 20260710));
        store.balances.add(new BalanceRecord("4900000000000002", 120_000L, 90_000L, 20260710));
        store.balances.add(new BalanceRecord("4900000000000003", 490_000L, 450_000L, 20260710));

        TemporaryLimitService service = new TemporaryLimitService(store);

        ApplicationResult r1 = service.applyTemporaryLimit(new TemporaryLimitApplication(
                "4900000000000001", 1_500_000L, 20260701, 20260731, "JPY"));
        ApplicationResult r2 = service.applyTemporaryLimit(new TemporaryLimitApplication(
                "4900000000000002", 1_000_000L, 20260701, 20260715, "JPY"));
        ApplicationResult r3 = service.applyTemporaryLimit(new TemporaryLimitApplication(
                "4900000000000001", 2_500_000L, 20260801, 20260831, "JPY"));
        CancelResult r4 = service.cancelTemporaryLimit(r1.approvalId, 20260710);
        ExpireResult r5 = service.expireTemporaryLimits(20260801);

        System.out.println("処理結果=" + r1.summary());
        System.out.println("処理結果=" + r2.summary());
        System.out.println("処理結果=" + r3.summary());
        System.out.println("処理結果=" + r4.summary());
        System.out.println("期限切れ件数=" + r5.expiredCount);
    }

    private final DataStore store;

    public TemporaryLimitService(DataStore store) {
        if (store == null) {
            throw new IllegalArgumentException("データストア未設定");
        }
        this.store = store;
    }

    public synchronized ApplicationResult applyTemporaryLimit(TemporaryLimitApplication app) {
        validateApplicationShape(app);

        CardRecord card = store.cards.get(app.cardNo);
        String approvalId = store.nextApprovalId();
        long currentBalance = aggregateCurrentBalance(app.cardNo);
        String decision;
        String reason = "";

        if (card == null) {
            decision = DECISION_DENY;
            reason = "STS";
        } else if (!CARD_STATUS_ACTIVE.equals(card.cardStatus)) {
            decision = DECISION_DENY;
            reason = "STS";
        } else if (!BASE_CURRENCY.equals(app.currencyCd)) {
            decision = DECISION_DENY;
            reason = "CUR";
        } else if (app.tempLimitAmount > card.creditLimit * MAX_TEMP_LIMIT_MULTIPLIER) {
            decision = DECISION_DENY;
            reason = "LIM";
        } else if (app.tempLimitAmount <= currentBalance) {
            decision = DECISION_DENY;
            reason = "LIM";
        } else if (hasOverlappedApprovedTerm(app.cardNo, app.startDt, app.endDt)) {
            decision = DECISION_DENY;
            reason = "LIM";
        } else {
            decision = DECISION_APPROVE;
        }

        String status = DECISION_APPROVE.equals(decision) ? STATUS_APPROVED : STATUS_DENIED;
        LimitRecord written = new LimitRecord(app.cardNo, app.tempLimitAmount,
                app.startDt, app.endDt, approvalId, status);
        store.limits.put(approvalId, written);

        NoticeRecord notice = createApplicationNotice(card, written, decision, reason, currentBalance);
        store.notices.add(notice);

        return new ApplicationResult(approvalId, decision, reason, status, notice.noticeId);
    }

    public synchronized CancelResult cancelTemporaryLimit(String approvalId, int requestDt) {
        if (isBlank(approvalId)) {
            throw new IllegalArgumentException("承認番号未設定");
        }
        validateDate(requestDt, "取消日");

        LimitRecord current = store.limits.get(approvalId);
        if (current == null) {
            return new CancelResult(approvalId, false, "対象なし", "");
        }
        if (!STATUS_APPROVED.equals(current.status)) {
            return new CancelResult(approvalId, false, "取消不可状態", current.status);
        }
        if (current.endDt < requestDt) {
            return new CancelResult(approvalId, false, "期限経過", current.status);
        }

        LimitRecord cancelled = new LimitRecord(current.cardNo, current.tempLimitAmount,
                current.startDt, current.endDt, current.approvalId, STATUS_CANCELLED);
        store.limits.put(approvalId, cancelled);

        CardRecord card = store.cards.get(current.cardNo);
        NoticeRecord notice = new NoticeRecord(store.nextNoticeId(), current.cardNo,
                NOTICE_CANCELLED, CHANNEL_MAIL, store.nextTimestamp(),
                buildMemberPrefix(card) + "一時増枠の取消を受け付けました。承認番号=" + approvalId);
        store.notices.add(notice);

        return new CancelResult(approvalId, true, "取消済", STATUS_CANCELLED);
    }

    public synchronized ExpireResult expireTemporaryLimits(int businessDt) {
        validateDate(businessDt, "営業日");
        int count = 0;

        for (java.util.Map.Entry<String, LimitRecord> entry : new java.util.ArrayList<java.util.Map.Entry<String, LimitRecord>>(store.limits.entrySet())) {
            LimitRecord current = entry.getValue();
            if (STATUS_APPROVED.equals(current.status) && current.endDt < businessDt) {
                LimitRecord expired = new LimitRecord(current.cardNo, current.tempLimitAmount,
                        current.startDt, current.endDt, current.approvalId, STATUS_EXPIRED);
                store.limits.put(entry.getKey(), expired);

                CardRecord card = store.cards.get(current.cardNo);
                store.notices.add(new NoticeRecord(store.nextNoticeId(), current.cardNo,
                        NOTICE_EXPIRED, CHANNEL_MAIL, store.nextTimestamp(),
                        buildMemberPrefix(card) + "一時増枠の適用期間が終了しました。承認番号=" + current.approvalId));
                count++;
            }
        }

        return new ExpireResult(count);
    }

    private void validateApplicationShape(TemporaryLimitApplication app) {
        if (app == null) {
            throw new IllegalArgumentException("申込情報未設定");
        }
        if (isBlank(app.cardNo)) {
            throw new IllegalArgumentException("カード番号未設定");
        }
        if (app.tempLimitAmount < MIN_TEMP_LIMIT_AMOUNT) {
            throw new IllegalArgumentException("一時増枠金額下限未満");
        }
        validateDate(app.startDt, "開始日");
        validateDate(app.endDt, "終了日");
        if (app.startDt > app.endDt) {
            throw new IllegalArgumentException("適用期間不正");
        }
        if (daysBetween(app.startDt, app.endDt) + 1L > MAX_TERM_DAYS) {
            throw new IllegalArgumentException("適用期間上限超過");
        }
        if (isBlank(app.currencyCd)) {
            throw new IllegalArgumentException("通貨コード未設定");
        }
    }

    private long aggregateCurrentBalance(String cardNo) {
        long total = 0L;
        for (BalanceRecord balance : store.balances) {
            if (cardNo.equals(balance.cardNo)) {
                total = Math.addExact(total, balance.currentBalAmount);
            }
        }
        return total;
    }

    private boolean hasOverlappedApprovedTerm(String cardNo, int startDt, int endDt) {
        for (LimitRecord limit : store.limits.values()) {
            if (cardNo.equals(limit.cardNo)
                    && STATUS_APPROVED.equals(limit.status)
                    && startDt <= limit.endDt
                    && limit.startDt <= endDt) {
                return true;
            }
        }
        return false;
    }

    private NoticeRecord createApplicationNotice(CardRecord card, LimitRecord limit,
                                                 String decision, String reason, long currentBalance) {
        String noticeKbn = DECISION_APPROVE.equals(decision) ? NOTICE_APPROVED : NOTICE_DENIED;
        String text;
        if (DECISION_APPROVE.equals(decision)) {
            text = buildMemberPrefix(card)
                    + "一時増枠を承認しました。承認番号=" + limit.approvalId
                    + "、増枠後限度額=" + limit.tempLimitAmount
                    + "円、期間=" + limit.startDt + "から" + limit.endDt + "まで";
        } else {
            text = buildMemberPrefix(card)
                    + "一時増枠を承認できませんでした。理由=" + reason
                    + "、申込金額=" + limit.tempLimitAmount
                    + "円、現在残高=" + currentBalance + "円";
        }
        return new NoticeRecord(store.nextNoticeId(), limit.cardNo, noticeKbn,
                CHANNEL_POST, store.nextTimestamp(), text);
    }

    private static String buildMemberPrefix(CardRecord card) {
        if (card == null || isBlank(card.memberNameKana)) {
            return "";
        }
        return card.memberNameKana + "様、";
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static void validateDate(int yyyymmdd, String name) {
        int year = yyyymmdd / 10000;
        int month = (yyyymmdd / 100) % 100;
        int day = yyyymmdd % 100;
        try {
            java.time.LocalDate.of(year, month, day);
        } catch (java.time.DateTimeException ex) {
            throw new IllegalArgumentException(name + "不正:" + yyyymmdd);
        }
    }

    private static long daysBetween(int fromYmd, int toYmd) {
        return java.time.temporal.ChronoUnit.DAYS.between(toDate(fromYmd), toDate(toYmd));
    }

    private static java.time.LocalDate toDate(int yyyymmdd) {
        return java.time.LocalDate.of(yyyymmdd / 10000, (yyyymmdd / 100) % 100, yyyymmdd % 100);
    }

    public static final class DataStore {
        private final java.util.Map<String, CardRecord> cards = new java.util.LinkedHashMap<String, CardRecord>();
        private final java.util.Map<String, LimitRecord> limits = new java.util.LinkedHashMap<String, LimitRecord>();
        private final java.util.List<BalanceRecord> balances = new java.util.ArrayList<BalanceRecord>();
        private final java.util.List<NoticeRecord> notices = new java.util.ArrayList<NoticeRecord>();
        private long approvalSeq = 100000L;
        private long noticeSeq = 500000L;
        private long timestampSeq = 20260628090000L;

        private String nextApprovalId() {
            approvalSeq++;
            return "TL" + approvalSeq;
        }

        private String nextNoticeId() {
            noticeSeq++;
            return "NT" + noticeSeq;
        }

        private long nextTimestamp() {
            timestampSeq++;
            return timestampSeq;
        }
    }

    public static final class TemporaryLimitApplication {
        public final String cardNo;
        public final long tempLimitAmount;
        public final int startDt;
        public final int endDt;
        public final String currencyCd;

        public TemporaryLimitApplication(String cardNo, long tempLimitAmount,
                                         int startDt, int endDt, String currencyCd) {
            this.cardNo = cardNo;
            this.tempLimitAmount = tempLimitAmount;
            this.startDt = startDt;
            this.endDt = endDt;
            this.currencyCd = currencyCd;
        }
    }

    private static final class CardRecord {
        private final String cardNo;
        private final String memberId;
        private final String cardStatus;
        private final long creditLimit;
        private final String memberNameKana;

        private CardRecord(String cardNo, String memberId, String cardStatus,
                           long creditLimit, String memberNameKana) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
            this.creditLimit = creditLimit;
            this.memberNameKana = memberNameKana;
        }
    }

    private static final class LimitRecord {
        private final String cardNo;
        private final long tempLimitAmount;
        private final int startDt;
        private final int endDt;
        private final String approvalId;
        private final String status;

        private LimitRecord(String cardNo, long tempLimitAmount, int startDt,
                            int endDt, String approvalId, String status) {
            this.cardNo = cardNo;
            this.tempLimitAmount = tempLimitAmount;
            this.startDt = startDt;
            this.endDt = endDt;
            this.approvalId = approvalId;
            this.status = status;
        }
    }

    private static final class BalanceRecord {
        private final String cardNo;
        private final long currentBalAmount;
        private final long lastStmtAmount;
        private final int cycleDt;

        private BalanceRecord(String cardNo, long currentBalAmount, long lastStmtAmount, int cycleDt) {
            this.cardNo = cardNo;
            this.currentBalAmount = currentBalAmount;
            this.lastStmtAmount = lastStmtAmount;
            this.cycleDt = cycleDt;
        }
    }

    private static final class NoticeRecord {
        private final String noticeId;
        private final String cardNo;
        private final String noticeKbn;
        private final String channelCd;
        private final long noticeTs;
        private final String noticeText;

        private NoticeRecord(String noticeId, String cardNo, String noticeKbn,
                             String channelCd, long noticeTs, String noticeText) {
            this.noticeId = noticeId;
            this.cardNo = cardNo;
            this.noticeKbn = noticeKbn;
            this.channelCd = channelCd;
            this.noticeTs = noticeTs;
            this.noticeText = noticeText;
        }
    }

    public static final class ApplicationResult {
        public final String approvalId;
        public final String decisionKbn;
        public final String declineReason;
        public final String status;
        public final String noticeId;

        private ApplicationResult(String approvalId, String decisionKbn,
                                  String declineReason, String status, String noticeId) {
            this.approvalId = approvalId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.status = status;
            this.noticeId = noticeId;
        }

        public String summary() {
            return "承認番号=" + approvalId + ",判定=" + decisionKbn
                    + ",理由=" + declineReason + ",状態=" + status + ",通知番号=" + noticeId;
        }
    }

    public static final class CancelResult {
        public final String approvalId;
        public final boolean cancelled;
        public final String message;
        public final String status;

        private CancelResult(String approvalId, boolean cancelled, String message, String status) {
            this.approvalId = approvalId;
            this.cancelled = cancelled;
            this.message = message;
            this.status = status;
        }

        public String summary() {
            return "承認番号=" + approvalId + ",取消=" + cancelled + ",結果=" + message + ",状態=" + status;
        }
    }

    public static final class ExpireResult {
        public final int expiredCount;

        private ExpireResult(int expiredCount) {
            this.expiredCount = expiredCount;
        }
    }
}
