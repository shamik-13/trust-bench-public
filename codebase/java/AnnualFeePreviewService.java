/**
 * 変更履歴
 * 版数  年月日    担当       概要
 * 1.00  20210917  会員系開発  年会費予定照会サービス新規作成
 */
public class AnnualFeePreviewService {
    private static final String CARD_STATUS_ACTIVE = "01";
    private static final String CARD_STATUS_STOPPED = "02";
    private static final String CARD_STATUS_CLOSED = "03";
    private static final String CARD_STATUS_DELINQUENT = "09";

    private static final String BILL_STATUS_CONFIRMED = "C";
    private static final String BILL_STATUS_SKIP = "S";

    private static final String POST_STATUS_POSTED = "P";
    private static final String POST_STATUS_PENDING = "N";

    private static final String FEE_TYPE_ANNUAL = "ANN";
    private static final int ANNUAL_FEE_AMOUNT = 1320;

    public static PreviewResult preview(
            String cardNo,
            int inquiryDate,
            CdCardf[] cards,
            CdFeef[] fees,
            CdMemStatf[] memberStatuses) {

        CdCardf card = findCard(cardNo, cards);
        if (card == null) {
            return new PreviewResult(cardNo, "", BILL_STATUS_SKIP, 0, 0, "カード未登録", "");
        }

        CdMemStatf memberStatus = findMemberStatus(card.memberId, memberStatuses);

        if (CARD_STATUS_STOPPED.equals(card.cardStatus) || CARD_STATUS_CLOSED.equals(card.cardStatus)) {
            return new PreviewResult(
                    card.cardNo,
                    card.memberId,
                    BILL_STATUS_SKIP,
                    0,
                    0,
                    "カード状態対象外:" + safe(card.cardStatus),
                    "");
        }

        if (!CARD_STATUS_ACTIVE.equals(card.cardStatus) && !CARD_STATUS_DELINQUENT.equals(card.cardStatus)) {
            return new PreviewResult(
                    card.cardNo,
                    card.memberId,
                    BILL_STATUS_SKIP,
                    0,
                    0,
                    "カード状態不明:" + safe(card.cardStatus),
                    "");
        }

        int nextFeeDate = nextAnniversaryOnOrAfter(card.openDate, inquiryDate);
        CdFeef existing = findAnnualFee(card.cardNo, yearOf(nextFeeDate), fees);

        if (existing != null && POST_STATUS_POSTED.equals(existing.postStatus)) {
            return new PreviewResult(
                    card.cardNo,
                    card.memberId,
                    BILL_STATUS_CONFIRMED,
                    existing.feeDate,
                    existing.feeAmount,
                    memberReason(memberStatus) + " 請求投稿済",
                    statementReference(existing));
        }

        if (existing != null && POST_STATUS_PENDING.equals(existing.postStatus)) {
            return new PreviewResult(
                    card.cardNo,
                    card.memberId,
                    BILL_STATUS_CONFIRMED,
                    existing.feeDate,
                    existing.feeAmount,
                    memberReason(memberStatus) + " 請求候補作成済",
                    "");
        }

        return new PreviewResult(
                card.cardNo,
                card.memberId,
                BILL_STATUS_CONFIRMED,
                nextFeeDate,
                ANNUAL_FEE_AMOUNT,
                memberReason(memberStatus) + " 次回請求候補",
                "");
    }

    private static CdCardf findCard(String cardNo, CdCardf[] cards) {
        if (cards == null) {
            return null;
        }

        for (int i = 0; i < cards.length; i++) {
            CdCardf card = cards[i];
            if (card != null && equals(cardNo, card.cardNo)) {
                return card;
            }
        }

        return null;
    }

    private static CdMemStatf findMemberStatus(String memberId, CdMemStatf[] statuses) {
        if (statuses == null) {
            return null;
        }

        CdMemStatf latest = null;
        for (int i = 0; i < statuses.length; i++) {
            CdMemStatf status = statuses[i];
            if (status != null
                    && equals(memberId, status.memberId)
                    && (latest == null || status.effectiveDate > latest.effectiveDate)) {
                latest = status;
            }
        }

        return latest;
    }

    private static CdFeef findAnnualFee(String cardNo, int feeYear, CdFeef[] fees) {
        if (fees == null) {
            return null;
        }

        CdFeef selected = null;
        for (int i = 0; i < fees.length; i++) {
            CdFeef fee = fees[i];
            if (fee != null
                    && equals(cardNo, fee.cardNo)
                    && FEE_TYPE_ANNUAL.equals(fee.feeType)
                    && yearOf(fee.feeDate) == feeYear
                    && (selected == null || isBetterFee(fee, selected))) {
                selected = fee;
            }
        }

        return selected;
    }

    private static boolean isBetterFee(CdFeef candidate, CdFeef current) {
        int candidatePriority = postingPriority(candidate.postStatus);
        int currentPriority = postingPriority(current.postStatus);

        if (candidatePriority != currentPriority) {
            return candidatePriority > currentPriority;
        }

        if (candidate.feeDate != current.feeDate) {
            return candidate.feeDate > current.feeDate;
        }

        return safe(candidate.feeId).compareTo(safe(current.feeId)) > 0;
    }

    private static int postingPriority(String postStatus) {
        if (POST_STATUS_POSTED.equals(postStatus)) {
            return 2;
        }
        if (POST_STATUS_PENDING.equals(postStatus)) {
            return 1;
        }
        return 0;
    }

    private static int nextAnniversaryOnOrAfter(int openDate, int inquiryDate) {
        int month = monthOf(openDate);
        int day = dayOf(openDate);
        int year = yearOf(inquiryDate);

        int candidate = normalizeDate(year, month, day);
        if (candidate < inquiryDate) {
            candidate = normalizeDate(year + 1, month, day);
        }

        return candidate;
    }

    private static int normalizeDate(int year, int month, int day) {
        int normalizedMonth = month;
        if (normalizedMonth < 1) {
            normalizedMonth = 1;
        } else if (normalizedMonth > 12) {
            normalizedMonth = 12;
        }

        int maxDay = lastDayOfMonth(year, normalizedMonth);
        int normalizedDay = day;
        if (normalizedDay < 1) {
            normalizedDay = 1;
        } else if (normalizedDay > maxDay) {
            normalizedDay = maxDay;
        }

        return year * 10000 + normalizedMonth * 100 + normalizedDay;
    }

    private static int lastDayOfMonth(int year, int month) {
        switch (month) {
            case 2:
                return isLeapYear(year) ? 29 : 28;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            default:
                return 31;
        }
    }

    private static boolean isLeapYear(int year) {
        return year % 400 == 0 || (year % 4 == 0 && year % 100 != 0);
    }

    private static int yearOf(int yyyymmdd) {
        return yyyymmdd / 10000;
    }

    private static int monthOf(int yyyymmdd) {
        return (yyyymmdd / 100) % 100;
    }

    private static int dayOf(int yyyymmdd) {
        return yyyymmdd % 100;
    }

    private static String memberReason(CdMemStatf status) {
        if (status == null) {
            return "会員状態未取得";
        }
        return "会員状態:" + safe(status.statusCode) + "/" + safe(status.statusReason);
    }

    private static String statementReference(CdFeef fee) {
        return "StatementInquiryService:cardNo=" + safe(fee.cardNo) + ",feeId=" + safe(fee.feeId);
    }

    private static boolean equals(String left, String right) {
        return left == null ? right == null : left.equals(right);
    }

    private static String safe(String value) {
        return value == null ? "" : value;
    }

    static class CdCardf {
        String cardNo;
        String memberId;
        String cardStatus;
        int openDate;
    }

    static class CdFeef {
        String cardNo;
        String feeId;
        String feeType;
        int feeDate;
        int feeAmount;
        String postStatus;
    }

    static class CdMemStatf {
        String memberId;
        int effectiveDate;
        String statusCode;
        String statusReason;
    }

    static class PreviewResult {
        final String cardNo, memberId, billStatus, message, reference;
        final int feeDate, feeAmount;

        PreviewResult(String cardNo, String memberId, String billStatus, int feeDate,
                      int feeAmount, String message, String reference) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.billStatus = billStatus;
            this.feeDate = feeDate;
            this.feeAmount = feeAmount;
            this.message = message;
            this.reference = reference;
        }
    }
}
