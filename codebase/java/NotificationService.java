public class NotificationService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.0   2022/10/04  開発担当  初版作成
     */

    private static final String CARD_STATUS_ACTIVE = "01";
    private static final String CARD_STATUS_STOPPED = "02";
    private static final String CARD_STATUS_CLOSED = "03";
    private static final String CARD_STATUS_DELINQUENT = "09";

    private static final String NOTICE_RISK = "RISK";
    private static final String NOTICE_LIMIT = "LIM";
    private static final String NOTICE_STATUS = "STS";

    private static final String CHANNEL_MAIL = "MAIL";
    private static final String CHANNEL_SMS = "SMS";
    private static final String CHANNEL_APP = "APP";

    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] a) {
        CdcCardf[] cardFile = new CdcCardf[] {
            new CdcCardf("4980123412341001", "M000001", "01", 300000L, "ヤマダタロウ"),
            new CdcCardf("4980123412341002", "M000002", "09", 120000L, "サトウハナコ"),
            new CdcCardf("4980123412341003", "M000003", "02", 50000L, "タナカイチロウ"),
            new CdcCardf("4980123412341004", "M000004", "03", 800000L, "スズキミカ")
        };

        CdntffStore noticeFile = new CdntffStore(new Cdntff[] {
            new Cdntff("N202606280001", "4980123412341002", NOTICE_STATUS, CHANNEL_MAIL,
                    "20260628080500", "カード状態によりお取引を確認できません。カード番号：************1002")
        });

        String businessDate = "20260628";
        String processTime = "20260628093000";
        String channel = CHANNEL_MAIL;
        String currency = BASE_CURRENCY;

        int written = 0;
        for (int i = 0; i < cardFile.length; i++) {
            CdcCardf card = cardFile[i];
            NotificationDecision decision = judge(card, currency, channel, processTime);
            if (!decision.sendable) {
                System.out.println("通知対象外 会員ＩＤ：" + card.memberId + " 理由：" + decision.reason);
                continue;
            }

            String noticeKey = makeSuppressKey(card.cardNo, decision.noticeKbn, channel, businessDate);
            if (noticeFile.existsSuppression(noticeKey)) {
                System.out.println("重複抑止 会員ＩＤ：" + card.memberId + " 抑止キー：" + noticeKey);
                continue;
            }

            String noticeId = "N" + businessDate + leftPad(String.valueOf(noticeFile.size() + 1), 4, '0');
            String text = buildText(card, decision.noticeKbn, channel);
            Cdntff notice = new Cdntff(noticeId, card.cardNo, decision.noticeKbn, channel, processTime, text);
            notice.suppressKey = noticeKey;
            noticeFile.write(notice);
            written++;
            System.out.println("通知登録 会員ＩＤ：" + card.memberId + " 通知ＩＤ：" + noticeId);
        }

        System.out.println("登録件数：" + written);
    }

    private static NotificationDecision judge(CdcCardf card, String currency, String channel, String noticeTs) {
        if (!BASE_CURRENCY.equals(currency)) {
            return new NotificationDecision(false, "", "取扱通貨対象外");
        }
        if (!isSendableTime(channel, noticeTs)) {
            return new NotificationDecision(false, "", "送信時間帯外");
        }
        if (CARD_STATUS_DELINQUENT.equals(card.cardStatus)) {
            return new NotificationDecision(true, NOTICE_STATUS, "延滞通知");
        }
        if (CARD_STATUS_STOPPED.equals(card.cardStatus) || CARD_STATUS_CLOSED.equals(card.cardStatus)) {
            return new NotificationDecision(true, NOTICE_STATUS, "状態通知");
        }
        if (CARD_STATUS_ACTIVE.equals(card.cardStatus) && card.creditLimit <= 100000L) {
            return new NotificationDecision(true, NOTICE_LIMIT, "枠低下通知");
        }
        return new NotificationDecision(false, "", "通知条件なし");
    }

    private static boolean isSendableTime(String channel, String noticeTs) {
        int hour = Integer.parseInt(noticeTs.substring(8, 10));
        if (CHANNEL_SMS.equals(channel)) {
            return hour >= 9 && hour < 20;
        }
        if (CHANNEL_MAIL.equals(channel)) {
            return hour >= 8 && hour < 22;
        }
        if (CHANNEL_APP.equals(channel)) {
            return hour >= 7 && hour < 23;
        }
        return false;
    }

    private static String buildText(CdcCardf card, String noticeKbn, String channel) {
        String masked = maskCardNo(card.cardNo);
        String prefix;
        if (CHANNEL_SMS.equals(channel)) {
            prefix = card.memberNameKana + "様 ";
        } else if (CHANNEL_APP.equals(channel)) {
            prefix = "【会員通知】" + card.memberNameKana + "様 ";
        } else {
            prefix = card.memberNameKana + "様\n";
        }

        if (NOTICE_STATUS.equals(noticeKbn)) {
            if (CARD_STATUS_DELINQUENT.equals(card.cardStatus)) {
                return prefix + "お支払状況の確認が必要です。カード番号：" + masked;
            }
            if (CARD_STATUS_STOPPED.equals(card.cardStatus)) {
                return prefix + "カードは利用停止中です。カード番号：" + masked;
            }
            return prefix + "カードは解約済です。カード番号：" + masked;
        }
        if (NOTICE_LIMIT.equals(noticeKbn)) {
            return prefix + "ご利用可能枠が少なくなっています。カード番号：" + masked;
        }
        return prefix + "お取引に関するお知らせです。カード番号：" + masked;
    }

    private static String makeSuppressKey(String cardNo, String noticeKbn, String channel, String businessDate) {
        return cardNo + ":" + noticeKbn + ":" + channel + ":" + businessDate;
    }

    private static String maskCardNo(String cardNo) {
        if (cardNo == null || cardNo.length() < 4) {
            return "****";
        }
        String last4 = cardNo.substring(cardNo.length() - 4);
        return "************" + last4;
    }

    private static String leftPad(String value, int length, char pad) {
        StringBuilder b = new StringBuilder();
        for (int i = value.length(); i < length; i++) {
            b.append(pad);
        }
        b.append(value);
        return b.toString();
    }

    private static final class CdcCardf {
        private final String cardNo;
        private final String memberId;
        private final String cardStatus;
        private final long creditLimit;
        private final String memberNameKana;

        private CdcCardf(String cardNo, String memberId, String cardStatus, long creditLimit, String memberNameKana) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
            this.creditLimit = creditLimit;
            this.memberNameKana = memberNameKana;
        }
    }

    private static final class Cdntff {
        private final String noticeId;
        private final String cardNo;
        private final String noticeKbn;
        private final String channelCd;
        private final String noticeTs;
        private final String noticeText;
        private String suppressKey;

        private Cdntff(String noticeId, String cardNo, String noticeKbn, String channelCd,
                       String noticeTs, String noticeText) {
            this.noticeId = noticeId;
            this.cardNo = cardNo;
            this.noticeKbn = noticeKbn;
            this.channelCd = channelCd;
            this.noticeTs = noticeTs;
            this.noticeText = noticeText;
            this.suppressKey = makeSuppressKey(cardNo, noticeKbn, channelCd, noticeTs.substring(0, 8));
        }
    }

    private static final class CdntffStore {
        private Cdntff[] rows;
        private int count;

        private CdntffStore(Cdntff[] initialRows) {
            this.rows = new Cdntff[Math.max(16, initialRows.length * 2)];
            for (int i = 0; i < initialRows.length; i++) {
                this.rows[i] = initialRows[i];
            }
            this.count = initialRows.length;
        }

        private boolean existsSuppression(String suppressKey) {
            for (int i = 0; i < count; i++) {
                if (rows[i].suppressKey.equals(suppressKey)) {
                    return true;
                }
            }
            return false;
        }

        private void write(Cdntff notice) {
            if (count == rows.length) {
                Cdntff[] expanded = new Cdntff[rows.length * 2];
                System.arraycopy(rows, 0, expanded, 0, rows.length);
                rows = expanded;
            }
            rows[count] = notice;
            count++;
        }

        private int size() {
            return count;
        }
    }

    private static final class NotificationDecision {
        private final boolean sendable;
        private final String noticeKbn;
        private final String reason;

        private NotificationDecision(boolean sendable, String noticeKbn, String reason) {
            this.sendable = sendable;
            this.noticeKbn = noticeKbn;
            this.reason = reason;
        }
    }
}
