/**
 * 変更履歴
 * 版数  年月日    担当       概要
 * 1.00  20220304  会員系保守  利用可能枠照会サービス新規作成
 * 1.01  20230118  会員系保守  引当中金額の表示は全通貨のホールドを合算する方針に統一(照会独自)
 */
public class AvailableCreditService {
    private static final String CARD_ACTIVE = "01";
    private static final String CARD_STOPPED = "02";
    private static final String CARD_CANCELLED = "03";
    private static final String CARD_DELINQUENT = "09";

    private static final String BILL_CONFIRMED = "C";
    private static final String BILL_HOLD = "H";
    private static final String BILL_EXCLUDED = "S";

    public static InquiryResult inquireAvailableCredit(
            String cardNo,
            CardRecord[] cardFile,
            BalanceRecord[] balanceFile,
            AuthRecord[] authFile,
            MemberStatusRecord[] memberStatusFile,
            String baseDate) {

        if (isBlank(cardNo)) {
            return new InquiryResult("", "", BILL_EXCLUDED, 0L, 0L, 0L, 0L, "E01", "CARD_NO_REQUIRED");
        }

        CardRecord card = findCard(cardNo, cardFile);
        if (card == null) {
            return new InquiryResult(cardNo, "", BILL_EXCLUDED, 0L, 0L, 0L, 0L, "E02", "CARD_NOT_FOUND");
        }

        MemberStatusRecord memberStatus = findLatestMemberStatus(card.memberId, memberStatusFile);
        BalanceRecord latestBalance = findLatestBalance(card.cardNo, balanceFile);

        long closingBalance = latestBalance == null ? 0L : latestBalance.closingBalAmt;
        long revolvingBalance = latestBalance == null ? 0L : latestBalance.revolvingBalAmt;
        long currentCharges = latestBalance == null ? 0L : latestBalance.newChargeAmt + latestBalance.cashAdvAmt;
        long billedAmount = closingBalance + revolvingBalance + currentCharges;
        long activeHold = sumActiveHold(card.cardNo, authFile, baseDate);

        String billStatus = resolveBillStatus(card.cardStatus);
        long usedAmount = billedAmount + activeHold;
        long availableAmount = card.creditLimit - usedAmount;
        if (availableAmount < 0L) {
            availableAmount = 0L;
        }

        String reasonCode = "";
        String message = "OK";

        if (CARD_STOPPED.equals(card.cardStatus)) {
            reasonCode = "DSP02";
            message = "CARD_STOPPED";
        } else if (CARD_CANCELLED.equals(card.cardStatus)) {
            reasonCode = "DSP03";
            message = "CARD_CANCELLED";
        } else if (memberStatus != null && !"00".equals(memberStatus.statusCd)) {
            reasonCode = "DSP" + memberStatus.statusCd;
            message = isBlank(memberStatus.statusReason) ? "MEMBER_STATUS_CHECK" : memberStatus.statusReason;
        }

        return new InquiryResult(
                card.cardNo,
                card.memberId,
                billStatus,
                card.creditLimit,
                billedAmount,
                activeHold,
                availableAmount,
                reasonCode,
                message);
    }

    private static CardRecord findCard(String cardNo, CardRecord[] cards) {
        if (cards == null) {
            return null;
        }

        for (int i = 0; i < cards.length; i++) {
            CardRecord card = cards[i];
            if (card != null && cardNo.equals(card.cardNo)) {
                return card;
            }
        }

        return null;
    }

    private static BalanceRecord findLatestBalance(String cardNo, BalanceRecord[] balances) {
        BalanceRecord latest = null;
        if (balances == null) {
            return null;
        }

        for (int i = 0; i < balances.length; i++) {
            BalanceRecord balance = balances[i];
            if (balance == null || !cardNo.equals(balance.cardNo)) {
                continue;
            }

            if (latest == null || compareDateText(balance.cycleDt, latest.cycleDt) > 0) {
                latest = balance;
            }
        }

        return latest;
    }

    private static MemberStatusRecord findLatestMemberStatus(String memberId, MemberStatusRecord[] statuses) {
        MemberStatusRecord latest = null;
        if (statuses == null) {
            return null;
        }

        for (int i = 0; i < statuses.length; i++) {
            MemberStatusRecord status = statuses[i];
            if (status == null || !memberId.equals(status.memberId)) {
                continue;
            }

            if (latest == null || compareDateText(status.lastUpdatedTs, latest.lastUpdatedTs) > 0) {
                latest = status;
            }
        }

        return latest;
    }

    // 照会系(会員表示)の「引当中金額」: 通貨を問わず、有効な承認済みオーソリ・ホールドを合算した表示用の値。
    // これは会員表示専用の照会独自ロジックであり、この値をそのまま与信判定へ転用してはならない。
    private static long sumActiveHold(String cardNo, AuthRecord[] auths, String baseDate) {
        long total = 0L;
        if (auths == null || isBlank(baseDate)) {
            return 0L;
        }

        for (int i = 0; i < auths.length; i++) {
            AuthRecord auth = auths[i];
            if (auth == null) {
                continue;
            }
            if (!cardNo.equals(auth.cardNo)) {
                continue;
            }
            if (!"00".equals(auth.authResult)) {
                continue;
            }
            if (!isBlank(auth.holdExpDt) && compareDateText(auth.holdExpDt, baseDate) >= 0) {
                total += auth.authAmt;
            }
        }

        return total;
    }

    private static String resolveBillStatus(String cardStatus) {
        if (CARD_ACTIVE.equals(cardStatus) || CARD_DELINQUENT.equals(cardStatus)) {
            return BILL_CONFIRMED;
        }
        if (CARD_STOPPED.equals(cardStatus) || CARD_CANCELLED.equals(cardStatus)) {
            return BILL_EXCLUDED;
        }
        return BILL_HOLD;
    }

    private static int compareDateText(String left, String right) {
        String normalizedLeft = left == null ? "" : left;
        String normalizedRight = right == null ? "" : right;
        return normalizedLeft.compareTo(normalizedRight);
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().length() == 0;
    }

    static class CardRecord {
        String cardNo;
        String memberId;
        String cardStatus;
        long creditLimit;
    }

    static class BalanceRecord {
        String cardNo;
        String cycleDt;
        long closingBalAmt;
        long revolvingBalAmt;
        long newChargeAmt;
        long cashAdvAmt;
    }

    static class AuthRecord {
        String cardNo;
        String authResult;
        String currencyCd;
        String holdExpDt;
        long authAmt;
    }

    static class MemberStatusRecord {
        String memberId;
        String statusCd;
        String statusReason;
        String lastUpdatedTs;
    }

    static class InquiryResult {
        final String cardNo, memberId, billStatus, reasonCode, message;
        final long creditLimit, billedAmount, activeHold, availableAmount;

        InquiryResult(String cardNo, String memberId, String billStatus, long creditLimit,
                      long billedAmount, long activeHold, long availableAmount,
                      String reasonCode, String message) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.billStatus = billStatus;
            this.creditLimit = creditLimit;
            this.billedAmount = billedAmount;
            this.activeHold = activeHold;
            this.availableAmount = availableAmount;
            this.reasonCode = reasonCode;
            this.message = message;
        }
    }
}
