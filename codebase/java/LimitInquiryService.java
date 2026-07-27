public class LimitInquiryService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2022/06/20  開発担当  初版作成
     */

    private static final String STATUS_ACTIVE = "01";
    private static final String TEMP_LIMIT_ACTIVE = "01";
    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] a) {
        CardMaster card = new CardMaster("4980123400000001", "M000000001", "01", 800000L, "ヤマダタロウ");
        BalanceFile balance = new BalanceFile("4980123400000001", 125000L, 118000L, 20260625);
        TempLimitFile[] limits = {
                new TempLimitFile("4980123400000001", 300000L, 20260601, 20260630, "TL20260601001", "01"),
                new TempLimitFile("4980123400000001", 150000L, 20260501, 20260531, "TL20260501009", "01"),
                new TempLimitFile("4980123400000001", 100000L, 20260610, 20260710, "TL20260610003", "20")
        };

        InquiryResult r = inquire(card, balance, limits, 20260628, BASE_CURRENCY);
        System.out.println(r.toDisplayLine());
    }

    public static InquiryResult inquire(CardMaster card,
                                        BalanceFile balance,
                                        TempLimitFile[] tempLimits,
                                        int inquiryDate,
                                        String currency) {
        validateCardRecord(card);
        validateBalanceRecord(balance);

        if (!card.cardNo.equals(balance.cardNo)) {
            throw new IllegalArgumentException("カード番号不一致");
        }
        if (!BASE_CURRENCY.equals(currency)) {
            return new InquiryResult(card.cardNo, card.memberId, card.memberNameKana, card.cardStatus,
                    card.creditLimit, balance.currentBalanceAmount, 0L,
                    card.creditLimit - balance.currentBalanceAmount,
                    false, "CUR", "取扱通貨対象外");
        }

        long activeTempLimit = aggregateActiveTempLimit(card.cardNo, tempLimits, inquiryDate);
        long referenceLimit = card.creditLimit + activeTempLimit;
        long availableAmount = referenceLimit - balance.currentBalanceAmount;

        if (!STATUS_ACTIVE.equals(card.cardStatus)) {
            return new InquiryResult(card.cardNo, card.memberId, card.memberNameKana, card.cardStatus,
                    card.creditLimit, balance.currentBalanceAmount, activeTempLimit,
                    availableAmount, false, "STS", "カード状態不正");
        }

        return new InquiryResult(card.cardNo, card.memberId, card.memberNameKana, card.cardStatus,
                card.creditLimit, balance.currentBalanceAmount, activeTempLimit,
                availableAmount, true, "", "照会正常");
    }

    private static long aggregateActiveTempLimit(String cardNo, TempLimitFile[] tempLimits, int inquiryDate) {
        if (tempLimits == null) {
            return 0L;
        }

        long amount = 0L;
        for (TempLimitFile limit : tempLimits) {
            if (limit == null) {
                continue;
            }
            validateTempLimitRecord(limit);
            if (cardNo.equals(limit.cardNo)
                    && TEMP_LIMIT_ACTIVE.equals(limit.status)
                    && limit.startDate <= inquiryDate
                    && inquiryDate <= limit.endDate) {
                amount += limit.tempLimitAmount;
            }
        }
        return amount;
    }

    private static void validateCardRecord(CardMaster card) {
        if (card == null) {
            throw new IllegalArgumentException("カード基本情報なし");
        }
        requireDigits(card.cardNo, 16, "カード番号不正");
        requireNotBlank(card.memberId, "会員番号不正");
        requireNotBlank(card.memberNameKana, "会員カナ氏名不正");
        if (!"01".equals(card.cardStatus)
                && !"02".equals(card.cardStatus)
                && !"03".equals(card.cardStatus)
                && !"09".equals(card.cardStatus)) {
            throw new IllegalArgumentException("カード状態区分不正");
        }
        if (card.creditLimit < 0L) {
            throw new IllegalArgumentException("基本限度額不正");
        }
    }

    private static void validateBalanceRecord(BalanceFile balance) {
        if (balance == null) {
            throw new IllegalArgumentException("請求残高情報なし");
        }
        requireDigits(balance.cardNo, 16, "カード番号不正");
        if (balance.currentBalanceAmount < 0L || balance.lastStatementAmount < 0L) {
            throw new IllegalArgumentException("請求残高不正");
        }
        requireDate(balance.cycleDate, "請求サイクル日不正");
    }

    private static void validateTempLimitRecord(TempLimitFile limit) {
        requireDigits(limit.cardNo, 16, "カード番号不正");
        if (limit.tempLimitAmount < 0L) {
            throw new IllegalArgumentException("一時増枠額不正");
        }
        requireDate(limit.startDate, "一時増枠開始日不正");
        requireDate(limit.endDate, "一時増枠終了日不正");
        if (limit.startDate > limit.endDate) {
            throw new IllegalArgumentException("一時増枠期間不正");
        }
        requireNotBlank(limit.approvalId, "増枠承認番号不正");
        requireNotBlank(limit.status, "一時増枠状態不正");
    }

    private static void requireDigits(String value, int length, String message) {
        if (value == null || value.length() != length) {
            throw new IllegalArgumentException(message);
        }
        for (int i = 0; i < value.length(); i++) {
            if (!Character.isDigit(value.charAt(i))) {
                throw new IllegalArgumentException(message);
            }
        }
    }

    private static void requireNotBlank(String value, String message) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(message);
        }
    }

    private static void requireDate(int yyyymmdd, String message) {
        int y = yyyymmdd / 10000;
        int m = (yyyymmdd / 100) % 100;
        int d = yyyymmdd % 100;
        if (y < 1900 || y > 2099 || m < 1 || m > 12 || d < 1 || d > daysInMonth(y, m)) {
            throw new IllegalArgumentException(message);
        }
    }

    private static int daysInMonth(int y, int m) {
        switch (m) {
            case 2:
                return isLeapYear(y) ? 29 : 28;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            default:
                return 31;
        }
    }

    private static boolean isLeapYear(int y) {
        return y % 400 == 0 || (y % 4 == 0 && y % 100 != 0);
    }

    public static final class CardMaster {
        public final String cardNo;
        public final String memberId;
        public final String cardStatus;
        public final long creditLimit;
        public final String memberNameKana;

        public CardMaster(String cardNo, String memberId, String cardStatus, long creditLimit, String memberNameKana) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
            this.creditLimit = creditLimit;
            this.memberNameKana = memberNameKana;
        }
    }

    public static final class BalanceFile {
        public final String cardNo;
        public final long currentBalanceAmount;
        public final long lastStatementAmount;
        public final int cycleDate;

        public BalanceFile(String cardNo, long currentBalanceAmount, long lastStatementAmount, int cycleDate) {
            this.cardNo = cardNo;
            this.currentBalanceAmount = currentBalanceAmount;
            this.lastStatementAmount = lastStatementAmount;
            this.cycleDate = cycleDate;
        }
    }

    public static final class TempLimitFile {
        public final String cardNo;
        public final long tempLimitAmount;
        public final int startDate;
        public final int endDate;
        public final String approvalId;
        public final String status;

        public TempLimitFile(String cardNo, long tempLimitAmount, int startDate, int endDate, String approvalId, String status) {
            this.cardNo = cardNo;
            this.tempLimitAmount = tempLimitAmount;
            this.startDate = startDate;
            this.endDate = endDate;
            this.approvalId = approvalId;
            this.status = status;
        }
    }

    public static final class InquiryResult {
        public final String cardNo;
        public final String memberId;
        public final String memberNameKana;
        public final String cardStatus;
        public final long baseLimitAmount;
        public final long currentBalanceAmount;
        public final long activeTempLimitAmount;
        public final long availableAmount;
        public final boolean referenceUsable;
        public final String declineReason;
        public final String message;

        public InquiryResult(String cardNo,
                             String memberId,
                             String memberNameKana,
                             String cardStatus,
                             long baseLimitAmount,
                             long currentBalanceAmount,
                             long activeTempLimitAmount,
                             long availableAmount,
                             boolean referenceUsable,
                             String declineReason,
                             String message) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.memberNameKana = memberNameKana;
            this.cardStatus = cardStatus;
            this.baseLimitAmount = baseLimitAmount;
            this.currentBalanceAmount = currentBalanceAmount;
            this.activeTempLimitAmount = activeTempLimitAmount;
            this.availableAmount = availableAmount;
            this.referenceUsable = referenceUsable;
            this.declineReason = declineReason;
            this.message = message;
        }

        public String toDisplayLine() {
            return "カード番号=" + maskCardNo(cardNo)
                    + ", 会員番号=" + memberId
                    + ", カード状態=" + cardStatus
                    + ", 基本限度額=" + baseLimitAmount
                    + ", 請求残高=" + currentBalanceAmount
                    + ", 有効一時増枠=" + activeTempLimitAmount
                    + ", 参考利用可能枠=" + availableAmount
                    + ", 参照可否=" + (referenceUsable ? "可" : "不可")
                    + ", 理由=" + declineReason
                    + ", メッセージ=" + message;
        }

        private static String maskCardNo(String cardNo) {
            if (cardNo == null || cardNo.length() < 8) {
                return "****************";
            }
            return cardNo.substring(0, 6) + "******" + cardNo.substring(cardNo.length() - 4);
        }
    }
}
