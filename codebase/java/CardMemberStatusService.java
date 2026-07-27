/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/04/01  基盤      初版作成。会員状態照会の基本応答を作成。
 * 1.01    2024/07/16  会員      API応答項目に表示状態コードを追加。
 * 1.02    2024/10/03  会員      API応答項目に利用可能枠を追加。
 * 1.03    2025/01/22  監査      照会監査イベントの出力項目を追加。
 */
public class CardMemberStatusService {

    private static final String PROGRAM_ID = "CDMBSTS";
    private static final String EVENT_INQUIRY = "照会";
    private static final String DETAIL_OK = "正常";
    private static final String DETAIL_WARN = "注意";
    private static final String DETAIL_NG = "拒否";

    private final java.util.List<Cdaccfc> accounts;
    private final java.util.List<Cddelfc> delays;
    private final java.util.List<Cdlogfc> logRows = new java.util.ArrayList<>();

    public CardMemberStatusService(java.util.List<Cdaccfc> accounts, java.util.List<Cddelfc> delays) {
        this.accounts = accounts;
        this.delays = delays;
    }

    public java.util.List<Cdlogfc> logRows() {
        return java.util.Collections.unmodifiableList(logRows);
    }

    public InquiryResponse inquire(String key, boolean withLimit) {
        String normalizedKey = normalizeKey(key);
        Cdaccfc account = findAccount(normalizedKey);
        if (account == null) {
            writeLog("", DETAIL_NG);
            throw new IllegalArgumentException("会員番号またはカード番号が存在しません");
        }

        java.util.List<Cddelfc> activeDelays = findActiveDelays(account.acCardNo());
        DisplayStatus status = decideDisplayStatus(account, activeDelays);
        long availableLimit = withLimit ? calculateAvailableLimit(account, activeDelays) : -1L;

        String detail = status == DisplayStatus.NORMAL ? DETAIL_OK : DETAIL_WARN;
        writeLog(account.acCardNo(), detail);

        return new InquiryResponse(
                account.acMemberId(),
                maskCardNo(account.acCardNo()),
                status.code,
                status.label,
                availableLimit,
                sumDueAmount(activeDelays),
                account.acLastUpdDt()
        );
    }

    private String normalizeKey(String key) {
        if (key == null) {
            throw new IllegalArgumentException("照会キーが未設定です");
        }
        String value = key.trim();
        if (value.isEmpty()) {
            throw new IllegalArgumentException("照会キーが未設定です");
        }
        if (!value.matches("[0-9A-Za-z]+")) {
            throw new IllegalArgumentException("照会キーに使用できない文字があります");
        }
        return value;
    }

    private Cdaccfc findAccount(String key) {
        for (Cdaccfc record : accounts) {
            if (record.acCardNo().equals(key) || record.acMemberId().equals(key)) {
                return record;
            }
        }
        return null;
    }

    private java.util.List<Cddelfc> findActiveDelays(String cardNo) {
        java.util.List<Cddelfc> result = new java.util.ArrayList<>();
        for (Cddelfc record : delays) {
            if (record.dlCardNo().equals(cardNo) && delayDays(record) > 0 && record.dlDueAmt() > 0) {
                result.add(record);
            }
        }
        result.sort(java.util.Comparator.comparingInt(CardMemberStatusService::delayDays).reversed());
        return result;
    }

    private DisplayStatus decideDisplayStatus(Cdaccfc account, java.util.List<Cddelfc> activeDelays) {
        int maxDelayDays = 0;
        for (Cddelfc delay : activeDelays) {
            if (delayDays(delay) > maxDelayDays) {
                maxDelayDays = delayDays(delay);
            }
        }

        if ("2".equals(account.acStatusKbn()) || maxDelayDays >= 45) {
            return DisplayStatus.SUSPENDED;
        }
        if ("1".equals(account.acStatusKbn()) || "1".equals(account.acDelayKbn()) || maxDelayDays > 0) {
            return DisplayStatus.DELAYED;
        }
        return DisplayStatus.NORMAL;
    }

    private long calculateAvailableLimit(Cdaccfc account, java.util.List<Cddelfc> activeDelays) {
        long baseLimit = account.acCreditLimit() + account.acCashLimit();
        long delayedHold = sumDueAmount(activeDelays);
        long available = baseLimit - account.acUsedAmt() - delayedHold;
        return Math.max(0L, available);
    }

    private static long sumDueAmount(java.util.List<Cddelfc> activeDelays) {
        long total = 0L;
        for (Cddelfc delay : activeDelays) {
            total += delay.dlDueAmt();
        }
        return total;
    }

    private static int delayDays(Cddelfc record) {
        String value = record.dlDelayDays();
        if (value == null || value.trim().isEmpty()) {
            return 0;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException ex) {
            return 0;
        }
    }

    private void writeLog(String cardNo, String detailCode) {
        String logId = "L" + String.format("%08d", logRows.size() + 1);
        int eventDate = Integer.parseInt(java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE));
        logRows.add(new Cdlogfc(logId, PROGRAM_ID, cardNo, EVENT_INQUIRY, eventDate, detailCode));
    }

    private static String maskCardNo(String cardNo) {
        if (cardNo.length() < 8) {
            return "****";
        }
        return cardNo.substring(0, 4) + "********" + cardNo.substring(cardNo.length() - 4);
    }

    private enum DisplayStatus {
        DELAYED("01", "延滞中"),
        SUSPENDED("02", "利用停止"),
        NORMAL("03", "通常利用可");

        private final String code;
        private final String label;

        DisplayStatus(String code, String label) {
            this.code = code;
            this.label = label;
        }
    }

    public static final class InquiryResponse {
        private final String memberId;
        private final String cardNoMasked;
        private final String displayStatusCode;
        private final String displayStatusName;
        private final long availableLimit;
        private final long overdueAmount;
        private final int lastUpdateDate;

        private InquiryResponse(String memberId, String cardNoMasked, String displayStatusCode,
                                String displayStatusName, long availableLimit, long overdueAmount,
                                int lastUpdateDate) {
            this.memberId = memberId;
            this.cardNoMasked = cardNoMasked;
            this.displayStatusCode = displayStatusCode;
            this.displayStatusName = displayStatusName;
            this.availableLimit = availableLimit;
            this.overdueAmount = overdueAmount;
            this.lastUpdateDate = lastUpdateDate;
        }

        public String toDisplayLine() {
            String limitText = availableLimit >= 0 ? String.valueOf(availableLimit) : "未照会";
            return "会員番号=" + memberId
                    + ", カード番号=" + cardNoMasked
                    + ", 表示状態コード=" + displayStatusCode
                    + ", 表示状態=" + displayStatusName
                    + ", 利用可能枠=" + limitText
                    + ", 延滞金額=" + overdueAmount
                    + ", 最終更新日=" + lastUpdateDate;
        }
    }
}
