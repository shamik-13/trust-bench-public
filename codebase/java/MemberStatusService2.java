public class MemberStatusService2 {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2019-05-20  第一開発  会員ステータス照会および住所変更後再開要求の処理を新規作成
     * 1.01  2021-12-03  第一開発  延滞日数による審査待ち遷移の判定を追加
     * 1.02  2024-08-19  第一開発  督促抽出済み会員の再開判定を見直し
     */

    private static final java.math.BigDecimal REVOLVING_MONTHLY_RATE = new java.math.BigDecimal("0.0125");
    private static final int REVIEW_LIMIT_DAYS = 30;
    private static final int REJECT_LIMIT_DAYS = 60;

    private static final String REV_ACTIVE = "01";
    private static final String REV_PAUSED = "02";
    private static final String REV_CLOSED = "03";

    private static final String STMT_CONFIRMED = "C";
    private static final String STMT_SKIPPED = "S";

    private static final String MEMBER_ACTIVE = "01";
    private static final String MEMBER_PAUSED = "02";
    private static final String MEMBER_CLOSED = "03";
    private static final String MEMBER_REVIEW = "04";

    private final TableStore store;

    public MemberStatusService2(TableStore store) {
        this.store = store;
    }

    public ServiceReply handle(InquiryRequest request) {
        if (request == null || isBlank(request.memberId)) {
            return ServiceReply.error("入力不備", "会員番号が未設定です");
        }

        Cdmemf mem = store.findMember(request.memberId);
        if (mem == null) {
            return ServiceReply.error("該当なし", "会員情報が見つかりません");
        }

        Cdrevf rev = store.findRevolving(mem.cardNo);
        if (rev == null || !mem.memberId.equals(rev.memberId)) {
            return ServiceReply.error("参照不整合", "リボ情報と会員情報が一致しません");
        }

        if (!REV_ACTIVE.equals(rev.revStatus)) {
            return skippedReply(mem, rev);
        }

        Delinquency delinquency = inspectStatements(mem.cardNo);
        long feeAmount = calcFeeAmount(delinquency.billAmount);

        if (!request.resumeAfterAddressChange) {
            return buildInquiryReply(mem, rev, delinquency, feeAmount);
        }

        return handleResume(mem, rev, delinquency, feeAmount);
    }

    private ServiceReply handleResume(Cdmemf mem, Cdrevf rev, Delinquency delinquency, long feeAmount) {
        if (MEMBER_CLOSED.equals(mem.memberStatus)) {
            return new ServiceReply(
                    "再開不可",
                    false,
                    "解約済み会員のため再開できません",
                    mem.memberId,
                    mem.cardNo,
                    rev.memberNameKana,
                    mem.memberStatus,
                    rev.revStatus,
                    rev.revCourseCd,
                    delinquency.maxDelinqDays,
                    delinquency.billAmount,
                    feeAmount
            );
        }

        if (delinquency.maxDelinqDays >= REJECT_LIMIT_DAYS) {
            return new ServiceReply(
                    "再開不可",
                    false,
                    "延滞日数が基準超過のため再開を拒否します",
                    mem.memberId,
                    mem.cardNo,
                    rev.memberNameKana,
                    mem.memberStatus,
                    rev.revStatus,
                    rev.revCourseCd,
                    delinquency.maxDelinqDays,
                    delinquency.billAmount,
                    feeAmount
            );
        }

        if (delinquency.maxDelinqDays >= REVIEW_LIMIT_DAYS || delinquency.hasCollectionExtracted) {
            Cdmemf updated = mem.withStatus(MEMBER_REVIEW, today());
            store.writeMember(updated);
            return new ServiceReply(
                    "審査待ち",
                    false,
                    "延滞または督促抽出済みのため審査待ちへ更新しました",
                    updated.memberId,
                    updated.cardNo,
                    rev.memberNameKana,
                    updated.memberStatus,
                    rev.revStatus,
                    rev.revCourseCd,
                    delinquency.maxDelinqDays,
                    delinquency.billAmount,
                    feeAmount
            );
        }

        Cdmemf updated = mem.withStatus(MEMBER_ACTIVE, today());
        store.writeMember(updated);
        return new ServiceReply(
                "再開可",
                true,
                "住所変更後再開を受け付けました",
                updated.memberId,
                updated.cardNo,
                rev.memberNameKana,
                updated.memberStatus,
                rev.revStatus,
                rev.revCourseCd,
                delinquency.maxDelinqDays,
                delinquency.billAmount,
                feeAmount
        );
    }

    private ServiceReply buildInquiryReply(Cdmemf mem, Cdrevf rev, Delinquency delinquency, long feeAmount) {
        boolean operable = MEMBER_ACTIVE.equals(mem.memberStatus) && delinquency.maxDelinqDays < REVIEW_LIMIT_DAYS;
        String displayStatus;
        String message;

        if (MEMBER_CLOSED.equals(mem.memberStatus)) {
            displayStatus = "解約済み";
            message = "解約済み会員です";
            operable = false;
        } else if (delinquency.maxDelinqDays >= REJECT_LIMIT_DAYS) {
            displayStatus = "延滞停止";
            message = "長期延滞のため操作不可です";
            operable = false;
        } else if (delinquency.maxDelinqDays >= REVIEW_LIMIT_DAYS || delinquency.hasCollectionExtracted) {
            displayStatus = "要確認";
            message = "延滞または督促抽出済みです";
            operable = false;
        } else if (MEMBER_PAUSED.equals(mem.memberStatus)) {
            displayStatus = "一時停止";
            message = "会員は一時停止中です";
            operable = false;
        } else {
            displayStatus = "通常";
            message = "操作可能です";
        }

        return new ServiceReply(
                displayStatus,
                operable,
                message,
                mem.memberId,
                mem.cardNo,
                rev.memberNameKana,
                mem.memberStatus,
                rev.revStatus,
                rev.revCourseCd,
                delinquency.maxDelinqDays,
                delinquency.billAmount,
                feeAmount
        );
    }

    private ServiceReply skippedReply(Cdmemf mem, Cdrevf rev) {
        String statusName;
        if (REV_PAUSED.equals(rev.revStatus)) {
            statusName = "リボ一時停止";
        } else if (REV_CLOSED.equals(rev.revStatus)) {
            statusName = "リボ解約";
        } else {
            statusName = "リボ状態不明";
        }

        return new ServiceReply(
                statusName,
                false,
                "リボ状態が有効ではないため処理対象外です",
                mem.memberId,
                mem.cardNo,
                rev.memberNameKana,
                mem.memberStatus,
                rev.revStatus,
                rev.revCourseCd,
                0,
                0,
                0
        );
    }

    private Delinquency inspectStatements(String cardNo) {
        int maxDays = 0;
        long billAmount = 0;
        boolean extracted = false;

        for (Cdstmtf2 stmt : store.findStatements(cardNo)) {
            if (!STMT_CONFIRMED.equals(stmt.stmtStatus)) {
                continue;
            }
            if (stmt.delinqDays > maxDays) {
                maxDays = stmt.delinqDays;
            }
            if (stmt.delinqDays > 0) {
                billAmount += Math.max(0, stmt.billAmount);
            }
            if (stmt.delinqDays >= REVIEW_LIMIT_DAYS && stmt.minPayAmount > 0) {
                extracted = true;
            }
        }

        return new Delinquency(maxDays, billAmount, extracted);
    }

    private long calcFeeAmount(long revBalance) {
        if (revBalance <= 0) {
            return 0;
        }
        java.math.BigDecimal fee = java.math.BigDecimal.valueOf(revBalance).multiply(REVOLVING_MONTHLY_RATE);
        return fee.setScale(0, java.math.RoundingMode.DOWN).longValue();
    }

    private static String today() {
        return java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    static final class InquiryRequest {
        private final String memberId;
        private final boolean resumeAfterAddressChange;

        InquiryRequest(String memberId, boolean resumeAfterAddressChange) {
            this.memberId = memberId;
            this.resumeAfterAddressChange = resumeAfterAddressChange;
        }
    }

    static final class ServiceReply {
        private final String displayStatus;
        private final boolean operable;
        private final String message;
        private final String memberId;
        private final String cardNo;
        private final String memberNameKana;
        private final String memberStatus;
        private final String revStatus;
        private final String revCourseCd;
        private final int delinqDays;
        private final long billAmount;
        private final long feeAmount;

        private ServiceReply(
                String displayStatus,
                boolean operable,
                String message,
                String memberId,
                String cardNo,
                String memberNameKana,
                String memberStatus,
                String revStatus,
                String revCourseCd,
                int delinqDays,
                long billAmount,
                long feeAmount
        ) {
            this.displayStatus = displayStatus;
            this.operable = operable;
            this.message = message;
            this.memberId = memberId;
            this.cardNo = cardNo;
            this.memberNameKana = memberNameKana;
            this.memberStatus = memberStatus;
            this.revStatus = revStatus;
            this.revCourseCd = revCourseCd;
            this.delinqDays = delinqDays;
            this.billAmount = billAmount;
            this.feeAmount = feeAmount;
        }

        private static ServiceReply error(String displayStatus, String message) {
            return new ServiceReply(displayStatus, false, message, "", "", "", "", "", "", 0, 0, 0);
        }

        private String toDisplayLine() {
            return "表示状態=" + displayStatus
                    + ", 操作可否=" + (operable ? "可" : "不可")
                    + ", 会員番号=" + memberId
                    + ", カード番号=" + mask(cardNo)
                    + ", カナ氏名=" + memberNameKana
                    + ", 会員状態=" + memberStatus
                    + ", リボ状態=" + revStatus
                    + ", 区分=" + revCourseCd
                    + ", 延滞日数=" + delinqDays
                    + ", 請求額=" + billAmount
                    + ", 手数料=" + feeAmount
                    + ", メッセージ=" + message;
        }

        private static String mask(String cardNo) {
            if (cardNo == null || cardNo.length() < 8) {
                return cardNo == null ? "" : cardNo;
            }
            return cardNo.substring(0, 6) + "******" + cardNo.substring(cardNo.length() - 4);
        }
    }

    private static final class Delinquency {
        private final int maxDelinqDays;
        private final long billAmount;
        private final boolean hasCollectionExtracted;

        private Delinquency(int maxDelinqDays, long billAmount, boolean hasCollectionExtracted) {
            this.maxDelinqDays = maxDelinqDays;
            this.billAmount = billAmount;
            this.hasCollectionExtracted = hasCollectionExtracted;
        }
    }

    static final class TableStore {
        private final java.util.Map<String, Cdmemf> cdmemf = new java.util.LinkedHashMap<String, Cdmemf>();
        private final java.util.Map<String, Cdrevf> cdrevf = new java.util.LinkedHashMap<String, Cdrevf>();
        private final java.util.Map<String, java.util.List<Cdstmtf2>> cdstmtf2 =
                new java.util.LinkedHashMap<String, java.util.List<Cdstmtf2>>();

        Cdmemf findMember(String memberId) {
            return cdmemf.get(memberId);
        }

        Cdrevf findRevolving(String cardNo) {
            return cdrevf.get(cardNo);
        }

        java.util.List<Cdstmtf2> findStatements(String cardNo) {
            java.util.List<Cdstmtf2> list = cdstmtf2.get(cardNo);
            if (list == null) {
                return java.util.Collections.emptyList();
            }
            return java.util.Collections.unmodifiableList(list);
        }

        void writeMember(Cdmemf record) {
            cdmemf.put(record.memberId, record);
        }

        void writeRevolving(Cdrevf record) {
            cdrevf.put(record.cardNo, record);
        }

        void writeStatement(Cdstmtf2 record) {
            java.util.List<Cdstmtf2> list = cdstmtf2.get(record.cardNo);
            if (list == null) {
                list = new java.util.ArrayList<Cdstmtf2>();
                cdstmtf2.put(record.cardNo, list);
            }
            list.add(record);
        }
    }

    static final class Cdmemf {
        private final String memberId;
        private final String cardNo;
        private final String memberStatus;
        private final String joinDt;
        private final String annualFeeCd;
        private final String lastStatusDt;

        Cdmemf(String memberId, String cardNo, String memberStatus, String joinDt, String annualFeeCd, String lastStatusDt) {
            this.memberId = memberId;
            this.cardNo = cardNo;
            this.memberStatus = memberStatus;
            this.joinDt = joinDt;
            this.annualFeeCd = annualFeeCd;
            this.lastStatusDt = lastStatusDt;
        }

        private Cdmemf withStatus(String nextStatus, String statusDt) {
            return new Cdmemf(memberId, cardNo, nextStatus, joinDt, annualFeeCd, statusDt);
        }
    }

    static final class Cdrevf {
        private final String cardNo;
        private final String memberId;
        private final String revStatus;
        private final String revCourseCd;
        private final String memberNameKana;
        private final String revStartDt;

        Cdrevf(String cardNo, String memberId, String revStatus, String revCourseCd, String memberNameKana, String revStartDt) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.revStatus = revStatus;
            this.revCourseCd = revCourseCd;
            this.memberNameKana = memberNameKana;
            this.revStartDt = revStartDt;
        }
    }

    static final class Cdstmtf2 {
        private final String cardNo;
        private final String cycleDt;
        private final long billAmount;
        private final long minPayAmount;
        private final String dueDt;
        private final String stmtStatus;
        private final int delinqDays;

        Cdstmtf2(
                String cardNo,
                String cycleDt,
                long billAmount,
                long minPayAmount,
                String dueDt,
                String stmtStatus,
                int delinqDays
        ) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.billAmount = billAmount;
            this.minPayAmount = minPayAmount;
            this.dueDt = dueDt;
            this.stmtStatus = stmtStatus;
            this.delinqDays = delinqDays;
        }
    }
}
