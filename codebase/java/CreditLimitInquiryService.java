/**
 * 変更履歴
 * 版数  年月日      担当    概要
 * 1.00  2019/04/01  開発一課  初版作成。Web会員向け利用可能枠照会を追加。
 * 1.01  2021/09/15  開発二課  未確定オーソリ集計と停止会員の表示マスクを追加。
 * 1.02  2023/06/20  開発二課  スマートフォン表示対応として表示ラベルの短文化を追加。
 */
public class CreditLimitInquiryService {
    private static final String PROGRAM_ID = "CDLIMINQ";
    private static final String STATUS_NORMAL = "0";
    private static final String STATUS_STOPPED = "9";
    private static final String DELAY_NONE = "0";
    private static final String DISP_SHOPPING = "S";
    private static final String DISP_CASHING = "K";
    private static final String AUTH_HOLD = "H";

    private final java.util.List<Cdaccfc> accounts;
    private final java.util.List<Cdauthf4c> authorizations;
    private final java.util.List<Cdmvwfc> displayRows = new java.util.ArrayList<>();
    private final java.util.List<Cdlogfc> logRows = new java.util.ArrayList<>();
    private int logSeq = 1;

    public CreditLimitInquiryService(java.util.List<Cdaccfc> accounts,
                                     java.util.List<Cdauthf4c> authorizations) {
        this.accounts = accounts;
        this.authorizations = authorizations;
    }

    public java.util.List<Cdmvwfc> displayRows() {
        return java.util.Collections.unmodifiableList(displayRows);
    }

    public java.util.List<Cdlogfc> logRows() {
        return java.util.Collections.unmodifiableList(logRows);
    }

    public void inquire(String cardNo, String txnId) {
        if (!isCardNo(cardNo) || !isTxnId(txnId)) {
            writeLog(cardNo, "入力不正", "E001");
            return;
        }

        Cdaccfc account = findByCardNo(cardNo);
        if (account == null) {
            writeLog(cardNo, "口座なし", "E404");
            return;
        }

        long pending = sumPendingApproved(cardNo);
        if (STATUS_STOPPED.equals(account.acStatusKbn())) {
            writeMaskedRows(cardNo, txnId, "停止中");
            writeLog(cardNo, "照会マスク", "SUSP");
            return;
        }
        if (!DELAY_NONE.equals(account.acDelayKbn())) {
            writeMaskedRows(cardNo, txnId, "延滞中");
            writeLog(cardNo, "照会マスク", "DELAY");
            return;
        }
        if (!STATUS_NORMAL.equals(account.acStatusKbn())) {
            writeMaskedRows(cardNo, txnId, "取扱不可");
            writeLog(cardNo, "照会マスク", "STAT");
            return;
        }

        long used = Math.max(0, account.acUsedAmt());
        long shoppingAvailable = account.acCreditLimit() - used - pending;
        long cashingAvailable = account.acCashLimit() - pending;
        if (shoppingAvailable < 0) {
            shoppingAvailable = 0;
        }
        if (cashingAvailable < 0) {
            cashingAvailable = 0;
        }

        displayRows.add(new Cdmvwfc(cardNo, txnId, DISP_SHOPPING, shoppingAvailable, "ショッピング利用可能額"));
        displayRows.add(new Cdmvwfc(cardNo, txnId, DISP_CASHING, cashingAvailable, "キャッシング利用可能額"));
        writeLog(cardNo, "照会正常", "OK");
    }

    private void writeMaskedRows(String cardNo, String txnId, String label) {
        displayRows.add(new Cdmvwfc(cardNo, txnId, DISP_SHOPPING, 0, label));
        displayRows.add(new Cdmvwfc(cardNo, txnId, DISP_CASHING, 0, label));
    }

    private void writeLog(String cardNo, String eventKbn, String detailCd) {
        logRows.add(new Cdlogfc("L" + String.format("%09d", logSeq++), PROGRAM_ID, cardNo, eventKbn, today(), detailCd));
    }

    private Cdaccfc findByCardNo(String cardNo) {
        for (Cdaccfc record : accounts) {
            if (record.acCardNo().equals(cardNo)) {
                return record;
            }
        }
        return null;
    }

    private long sumPendingApproved(String cardNo) {
        long sum = 0;
        for (Cdauthf4c record : authorizations) {
            if (record.auCardNo().equals(cardNo) && AUTH_HOLD.equals(record.auAuthKbn()) && "00".equals(record.auReasonCd())) {
                sum += Math.max(0, record.auApprovedAmt());
            }
        }
        return sum;
    }

    private boolean isCardNo(String value) {
        if (value == null || value.length() != 16) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            if (!Character.isDigit(value.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private boolean isTxnId(String value) {
        return value != null && value.length() >= 8 && value.length() <= 20;
    }

    private int today() {
        return Integer.parseInt(java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE));
    }
}
