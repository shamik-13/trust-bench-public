/****
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2023-05-29  営業管理部  売上不一致明細の分類および調査通知作成を追加
 */
public class SalesPostingService {

    private static final String AUTH_RESULT_APPROVED_HOLD = "00";
    private static final String AUTH_RESULT_CANCELLED = "20";
    private static final String AUTH_RESULT_POSTED = "30";
    private static final String BASE_CURRENCY = "JPY";

    private static final long HIGH_DIFF_THRESHOLD = 10_000L;
    private static final int NOTICE_LIMIT = 200;

    public static void main(String[] a) {
        SalesRecord[] salesFile = {
                new SalesRecord("S202606280001", "A202606270101", "4111110000000001", 12000L, "2026-06-28T09:01:12", "M10001"),
                new SalesRecord("S202606280002", "A202606270102", "4111110000000002", 84500L, "2026-06-28T09:02:41", "M10002"),
                new SalesRecord("S202606280003", "A202606270103", "4111110000000003", 4800L, "2026-06-28T09:04:09", "M10003"),
                new SalesRecord("S202606280004", "A202606270104", "4111110000000004", 177000L, "2026-06-28T09:08:33", "M10004"),
                new SalesRecord("S202606280005", "A202606270105", "4111110000000005", 3300L, "2026-06-28T09:09:54", "M10005"),
                new SalesRecord("S202606280006", "A202606270102", "4111110000000002", 84500L, "2026-06-28T09:13:15", "M10002")
        };

        AuthRecord[] authFile = {
                new AuthRecord("A202606270101", "4111110000000001", 12000L, "00", "M10001", "JPY", "2026-06-27T18:12:22", "2026-07-04"),
                new AuthRecord("A202606270102", "4111110000000002", 84500L, "30", "M10002", "JPY", "2026-06-27T18:31:48", "2026-07-04"),
                new AuthRecord("A202606270103", "4111110000000003", 3000L, "00", "M10003", "JPY", "2026-06-27T18:44:01", "2026-07-04"),
                new AuthRecord("A202606270104", "4111110000000004", 165000L, "00", "M10004", "JPY", "2026-06-27T19:01:39", "2026-07-04"),
                new AuthRecord("A202606270106", "4111110000000006", 9200L, "20", "M10006", "JPY", "2026-06-27T19:11:02", "2026-07-04")
        };

        BalanceRecord[] balanceFile = {
                new BalanceRecord("4111110000000001", 128000L, 91300L, "2026-06-20"),
                new BalanceRecord("4111110000000002", 462000L, 305000L, "2026-06-20"),
                new BalanceRecord("4111110000000003", 67000L, 55800L, "2026-06-20"),
                new BalanceRecord("4111110000000004", 809000L, 702000L, "2026-06-20"),
                new BalanceRecord("4111110000000005", 21000L, 19400L, "2026-06-20")
        };

        NoticeRecord[] noticeFile = new NoticeRecord[NOTICE_LIMIT];

        int noticeCount = process(salesFile, authFile, balanceFile, noticeFile);

        System.out.println("売上確定連携サービスを終了しました。通知件数=" + noticeCount);
        for (int i = 0; i < noticeCount; i++) {
            System.out.println(noticeFile[i].toOutputLine());
        }
    }

    private static int process(SalesRecord[] salesFile, AuthRecord[] authFile,
                               BalanceRecord[] balanceFile, NoticeRecord[] noticeFile) {
        int noticeCount = 0;
        int unauthorizedCount = 0;
        int overAmountCount = 0;
        int duplicateCount = 0;

        for (int i = 0; i < salesFile.length; i++) {
            SalesRecord sales = salesFile[i];
            AuthRecord auth = findAuth(authFile, sales.authId);
            BalanceRecord balance = findBalance(balanceFile, sales.cardNo);

            if (auth == null) {
                unauthorizedCount++;
                continue;
            }

            if (!sales.cardNo.equals(auth.cardNo) || !sales.merchantCode.equals(auth.merchantCode)) {
                unauthorizedCount++;
                continue;
            }

            if (!BASE_CURRENCY.equals(auth.currencyCd) || AUTH_RESULT_CANCELLED.equals(auth.authResult)) {
                unauthorizedCount++;
                continue;
            }

            if (AUTH_RESULT_POSTED.equals(auth.authResult)) {
                duplicateCount++;
                if (noticeCount < noticeFile.length) {
                    noticeFile[noticeCount] = createNotice(noticeCount + 1, sales.cardNo, "DUP", "01",
                            "重複確定調査: 売上ID=" + sales.salesId + " 承認ID=" + sales.authId);
                    noticeCount++;
                }
                continue;
            }

            if (AUTH_RESULT_APPROVED_HOLD.equals(auth.authResult) && sales.salesAmt > auth.authAmt) {
                long diff = sales.salesAmt - auth.authAmt;
                overAmountCount++;

                if (diff >= HIGH_DIFF_THRESHOLD && noticeCount < noticeFile.length) {
                    long currentBalance = balance == null ? 0L : balance.currentBalAmt;
                    noticeFile[noticeCount] = createNotice(noticeCount + 1, sales.cardNo, "HAM", "01",
                            "高額差異調査: 売上ID=" + sales.salesId + " 差額=" + diff + " 残高=" + currentBalance);
                    noticeCount++;
                }
            }
        }

        System.out.println("分類結果: 承認なし売上=" + unauthorizedCount
                + " 金額超過=" + overAmountCount
                + " 重複確定=" + duplicateCount);

        return noticeCount;
    }

    private static AuthRecord findAuth(AuthRecord[] authFile, String authId) {
        for (AuthRecord auth : authFile) {
            if (auth.authId.equals(authId)) {
                return auth;
            }
        }
        return null;
    }

    private static BalanceRecord findBalance(BalanceRecord[] balanceFile, String cardNo) {
        for (BalanceRecord balance : balanceFile) {
            if (balance.cardNo.equals(cardNo)) {
                return balance;
            }
        }
        return null;
    }

    private static NoticeRecord createNotice(int seq, String cardNo, String noticeKbn,
                                             String channelCd, String noticeText) {
        String noticeId = "N20260628" + String.format("%04d", seq);
        return new NoticeRecord(noticeId, cardNo, noticeKbn, channelCd,
                "2026-06-28T09:30:00", noticeText);
    }

    private static final class SalesRecord {
        final String salesId;
        final String authId;
        final String cardNo;
        final long salesAmt;
        final String salesTs;
        final String merchantCode;

        SalesRecord(String salesId, String authId, String cardNo, long salesAmt,
                    String salesTs, String merchantCode) {
            this.salesId = salesId;
            this.authId = authId;
            this.cardNo = cardNo;
            this.salesAmt = salesAmt;
            this.salesTs = salesTs;
            this.merchantCode = merchantCode;
        }
    }

    private static final class AuthRecord {
        final String authId;
        final String cardNo;
        final long authAmt;
        final String authResult;
        final String merchantCode;
        final String currencyCd;
        final String authTs;
        final String holdExpDt;

        AuthRecord(String authId, String cardNo, long authAmt, String authResult,
                   String merchantCode, String currencyCd, String authTs, String holdExpDt) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.authAmt = authAmt;
            this.authResult = authResult;
            this.merchantCode = merchantCode;
            this.currencyCd = currencyCd;
            this.authTs = authTs;
            this.holdExpDt = holdExpDt;
        }
    }

    private static final class BalanceRecord {
        final String cardNo;
        final long currentBalAmt;
        final long lastStmtAmt;
        final String cycleDt;

        BalanceRecord(String cardNo, long currentBalAmt, long lastStmtAmt, String cycleDt) {
            this.cardNo = cardNo;
            this.currentBalAmt = currentBalAmt;
            this.lastStmtAmt = lastStmtAmt;
            this.cycleDt = cycleDt;
        }
    }

    private static final class NoticeRecord {
        final String noticeId;
        final String cardNo;
        final String noticeKbn;
        final String channelCd;
        final String noticeTs;
        final String noticeText;

        NoticeRecord(String noticeId, String cardNo, String noticeKbn,
                     String channelCd, String noticeTs, String noticeText) {
            this.noticeId = noticeId;
            this.cardNo = cardNo;
            this.noticeKbn = noticeKbn;
            this.channelCd = channelCd;
            this.noticeTs = noticeTs;
            this.noticeText = noticeText;
        }

        String toOutputLine() {
            return noticeId + "," + cardNo + "," + noticeKbn + ","
                    + channelCd + "," + noticeTs + "," + noticeText;
        }
    }
}
