public class RevoCourseApplicationService {
    /**
     * 変更履歴
     * 版数  年月日      担当    概要
     * 1.00  2018/11/26  情シス  Web会員リボ申込、支払コース変更、リボ停止受付の判定処理を新規作成
     * 1.01  2020/06/15  情シス  締日判定による当月/次回適用の振り分けを追加
     * 1.02  2023/10/02  情シス  解約済みコース申込時の対象外判定を厳格化
     */

    private static final java.math.BigDecimal REVO_MONTHLY_FEE_RATE = new java.math.BigDecimal("0.0125");

    private static final String REV_STATUS_ACTIVE = "01";
    private static final String REV_STATUS_SUSPENDED = "02";
    private static final String REV_STATUS_CANCELLED = "03";

    private static final String NOTICE_STATUS_CONFIRMED = "C";
    private static final String NOTICE_STATUS_SKIPPED = "S";

    private static final String COURSE_STATUS_VALID = "1";
    private static final String MEMBER_STATUS_VALID = "1";

    private static final String APPLY_NEW = "01";
    private static final String APPLY_CHANGE = "02";
    private static final String APPLY_STOP = "03";

    private static final int MONTHLY_CUTOFF_DAY = 25;

    /**
     * 1件の申込レコードを判定し、リボ更新内容と通知内容を返す。
     * 通知レコードの登録やリボ原簿の更新は呼出側で {@link Decision} を用いて行う。
     */
    public Decision decide(DataStore store, ApplicationRecord application, java.time.LocalDate businessDate) {
        MemberRecord member = store.cdmemf.get(application.memberId);
        if (member == null) {
            return skipped(application, "S", "会員未登録");
        }
        if (!member.cardNo.equals(application.cardNo)) {
            return skipped(application, "S", "カード番号不一致");
        }
        if (!MEMBER_STATUS_VALID.equals(member.memberStatus)) {
            return skipped(application, "S", "会員状態対象外");
        }

        RevoRecord current = store.cdrevf.get(application.cardNo);
        if (current != null && (REV_STATUS_SUSPENDED.equals(current.revStatus) || REV_STATUS_CANCELLED.equals(current.revStatus))) {
            return skipped(application, "S", "リボ状態対象外");
        }

        if (!APPLY_STOP.equals(application.applyType)) {
            CourseRecord course = store.cdcourf.get(application.requestCourseCd);
            if (course == null) {
                return skipped(application, "S", "コース未登録");
            }
            if (!COURSE_STATUS_VALID.equals(course.courseStatus)) {
                return skipped(application, "S", "コース停止中");
            }
            if (application.requestAmount < course.minPayAmount) {
                return skipped(application, "S", "支払指定額不足");
            }
        }

        java.time.LocalDate startDate = effectiveStartDate(application.applyDate);
        long feeAmount = calculateFee(application.revolvingBalance);
        String nextStatus = APPLY_STOP.equals(application.applyType) ? REV_STATUS_SUSPENDED : REV_STATUS_ACTIVE;
        String nextCourse = APPLY_STOP.equals(application.applyType)
                ? (current == null ? application.requestCourseCd : current.revCourseCd)
                : application.requestCourseCd;
        String kana = current == null ? application.memberNameKana : current.memberNameKana;

        RevoRecord updated = new RevoRecord(
                application.cardNo,
                application.memberId,
                nextStatus,
                nextCourse,
                kana,
                startDate
        );

        String noticeType = startDate.getMonth().equals(application.applyDate.getMonth()) ? "当月適用" : "次回適用";
        String message = "受付済 カード=" + maskCard(application.cardNo)
                + " 申込区分=" + application.applyType
                + " 通知区分=" + noticeType
                + " 適用日=" + formatDate(startDate)
                + " 手数料=" + feeAmount;

        return new Decision(noticeType, feeAmount, NOTICE_STATUS_CONFIRMED, updated, message);
    }

    /**
     * 判定結果を原簿・通知ファイルへ反映する。判定で更新されたリボ原簿を書き戻し、
     * 通知レコードを採番のうえ追加する。
     */
    public NoticeRecord register(DataStore store, ApplicationRecord application, Decision decision, java.time.LocalDate businessDate) {
        if (decision.updateRevoRecord != null) {
            store.cdrevf.put(decision.updateRevoRecord.cardNo, decision.updateRevoRecord);
        }
        NoticeRecord notice = new NoticeRecord(
                nextNoticeId(businessDate, store.cdnotif.size() + 1),
                application.cardNo,
                businessDate,
                decision.noticeType,
                decision.noticeAmount,
                decision.noticeStatus
        );
        store.cdnotif.add(notice);
        return notice;
    }

    private Decision skipped(ApplicationRecord application, String noticeType, String reason) {
        String message = "対象外 カード=" + maskCard(application.cardNo)
                + " 申込区分=" + application.applyType
                + " 理由=" + reason
                + " 手数料=0";
        return new Decision(noticeType, 0L, NOTICE_STATUS_SKIPPED, null, message);
    }

    private java.time.LocalDate effectiveStartDate(java.time.LocalDate applyDate) {
        if (applyDate.getDayOfMonth() <= MONTHLY_CUTOFF_DAY) {
            return applyDate.withDayOfMonth(1);
        }
        return applyDate.plusMonths(1).withDayOfMonth(1);
    }

    private long calculateFee(long revolvingBalance) {
        return new java.math.BigDecimal(revolvingBalance)
                .multiply(REVO_MONTHLY_FEE_RATE)
                .setScale(0, java.math.RoundingMode.DOWN)
                .longValue();
    }

    private String nextNoticeId(java.time.LocalDate businessDate, int sequence) {
        return "N" + businessDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + String.format("%05d", sequence);
    }

    private String maskCard(String cardNo) {
        if (cardNo.length() < 8) {
            return cardNo;
        }
        return cardNo.substring(0, 4) + "********" + cardNo.substring(cardNo.length() - 4);
    }

    private String formatDate(java.time.LocalDate date) {
        return date.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
    }

    public static final class DataStore {
        private final java.util.Map<String, MemberRecord> cdmemf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, RevoRecord> cdrevf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, CourseRecord> cdcourf = new java.util.LinkedHashMap<>();
        private final java.util.List<NoticeRecord> cdnotif = new java.util.ArrayList<>();

        void putMember(MemberRecord record) {
            cdmemf.put(record.memberId, record);
        }

        void putRevolving(RevoRecord record) {
            cdrevf.put(record.cardNo, record);
        }

        void putCourse(CourseRecord record) {
            cdcourf.put(record.courseCd, record);
        }

        java.util.List<NoticeRecord> notices() {
            return java.util.Collections.unmodifiableList(cdnotif);
        }
    }

    static final class MemberRecord {
        private final String memberId;
        private final String cardNo;
        private final String memberStatus;
        private final java.time.LocalDate joinDate;
        private final String annualFeeCd;
        private final java.time.LocalDate lastStatusDate;

        MemberRecord(String memberId, String cardNo, String memberStatus, java.time.LocalDate joinDate, String annualFeeCd, java.time.LocalDate lastStatusDate) {
            this.memberId = memberId;
            this.cardNo = cardNo;
            this.memberStatus = memberStatus;
            this.joinDate = joinDate;
            this.annualFeeCd = annualFeeCd;
            this.lastStatusDate = lastStatusDate;
        }
    }

    static final class RevoRecord {
        private final String cardNo;
        private final String memberId;
        private final String revStatus;
        private final String revCourseCd;
        private final String memberNameKana;
        private final java.time.LocalDate revStartDate;

        RevoRecord(String cardNo, String memberId, String revStatus, String revCourseCd, String memberNameKana, java.time.LocalDate revStartDate) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.revStatus = revStatus;
            this.revCourseCd = revCourseCd;
            this.memberNameKana = memberNameKana;
            this.revStartDate = revStartDate;
        }
    }

    static final class CourseRecord {
        private final String courseCd;
        private final String courseName;
        private final java.math.BigDecimal feeRate;
        private final long minPayAmount;
        private final String courseStatus;

        CourseRecord(String courseCd, String courseName, java.math.BigDecimal feeRate, long minPayAmount, String courseStatus) {
            this.courseCd = courseCd;
            this.courseName = courseName;
            this.feeRate = feeRate;
            this.minPayAmount = minPayAmount;
            this.courseStatus = courseStatus;
        }
    }

    static final class NoticeRecord {
        private final String noticeId;
        private final String cardNo;
        private final java.time.LocalDate noticeDate;
        private final String noticeType;
        private final long noticeAmount;
        private final String noticeStatus;

        NoticeRecord(String noticeId, String cardNo, java.time.LocalDate noticeDate, String noticeType, long noticeAmount, String noticeStatus) {
            this.noticeId = noticeId;
            this.cardNo = cardNo;
            this.noticeDate = noticeDate;
            this.noticeType = noticeType;
            this.noticeAmount = noticeAmount;
            this.noticeStatus = noticeStatus;
        }
    }

    static final class ApplicationRecord {
        private final String memberId;
        private final String cardNo;
        private final String applyType;
        private final String requestCourseCd;
        private final long requestAmount;
        private final long revolvingBalance;
        private final java.time.LocalDate applyDate;
        private final String memberNameKana;

        ApplicationRecord(String memberId, String cardNo, String applyType, String requestCourseCd, long requestAmount, long revolvingBalance, java.time.LocalDate applyDate, String memberNameKana) {
            this.memberId = memberId;
            this.cardNo = cardNo;
            this.applyType = applyType;
            this.requestCourseCd = requestCourseCd;
            this.requestAmount = requestAmount;
            this.revolvingBalance = revolvingBalance;
            this.applyDate = applyDate;
            this.memberNameKana = memberNameKana;
        }
    }

    static final class Decision {
        private final String noticeType;
        private final long noticeAmount;
        private final String noticeStatus;
        private final RevoRecord updateRevoRecord;
        private final String message;

        private Decision(String noticeType, long noticeAmount, String noticeStatus, RevoRecord updateRevoRecord, String message) {
            this.noticeType = noticeType;
            this.noticeAmount = noticeAmount;
            this.noticeStatus = noticeStatus;
            this.updateRevoRecord = updateRevoRecord;
            this.message = message;
        }
    }
}
