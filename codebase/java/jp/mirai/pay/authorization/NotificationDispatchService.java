package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025-02-14  みらいペイ システム部  初版作成
 */
public class NotificationDispatchService {
    private static final String WALLET_STATUS_ACTIVE = "01";
    private static final String SEND_STATUS_UNSENT = "00";
    private static final String SEND_STATUS_SENT = "10";
    private static final String SEND_STATUS_EXCLUDED = "90";

    public static void main(String[] a) {
        java.util.List<Pyntff> pyntff = loadPyntff();
        java.util.Map<String, Pywalf> pywalfByWalletId = loadPywalfByWalletId();

        java.util.Set<String> duplicatedNoticeIds = findDuplicatedNoticeIds(pyntff);
        java.util.List<Pyntff> updated = new java.util.ArrayList<Pyntff>();

        int sentCount = 0;
        int excludedCount = 0;

        for (Pyntff notice : pyntff) {
            DispatchJudgement judgement = judge(notice, pywalfByWalletId, duplicatedNoticeIds);

            if (!judgement.dispatchable) {
                updated.add(notice.withSendStatus(SEND_STATUS_EXCLUDED));
                excludedCount++;
                System.out.println("通知除外 NOTICE-ID=" + notice.noticeId + " 理由=" + judgement.reason);
                continue;
            }

            Pywalf wallet = pywalfByWalletId.get(notice.walletId);
            String body = buildBody(notice, wallet);
            String channel = resolveChannel(notice.noticeKbn);

            if (body.trim().isEmpty()) {
                updated.add(notice.withSendStatus(SEND_STATUS_EXCLUDED));
                excludedCount++;
                System.out.println("通知除外 NOTICE-ID=" + notice.noticeId + " 理由=本文生成結果空");
                continue;
            }

            updated.add(notice.withSendStatus(SEND_STATUS_SENT));
            sentCount++;
            System.out.println("通知送信 NOTICE-ID=" + notice.noticeId
                    + " 利用者=" + wallet.userId
                    + " チャネル=" + channel
                    + " 本文=" + body);
        }

        System.out.println("配信集計 送信=" + sentCount + " 除外=" + excludedCount + " 更新件数=" + updated.size());

        for (Pyntff out : updated) {
            System.out.println("PYNTFF更新 NOTICE-ID=" + out.noticeId
                    + " WALLET-ID=" + out.walletId
                    + " NOTICE-KBN=" + out.noticeKbn
                    + " SEND-STATUS=" + out.sendStatus
                    + " CREATE-TS=" + out.createTs);
        }
    }

    private static DispatchJudgement judge(Pyntff notice,
                                           java.util.Map<String, Pywalf> pywalfByWalletId,
                                           java.util.Set<String> duplicatedNoticeIds) {
        if (!SEND_STATUS_UNSENT.equals(notice.sendStatus)) {
            return new DispatchJudgement(false, "未送信以外");
        }
        if (duplicatedNoticeIds.contains(notice.noticeId)) {
            return new DispatchJudgement(false, "重複NOTICE-ID");
        }
        if (notice.noticeText == null || notice.noticeText.trim().isEmpty()) {
            return new DispatchJudgement(false, "空本文");
        }

        Pywalf wallet = pywalfByWalletId.get(notice.walletId);
        if (wallet == null) {
            return new DispatchJudgement(false, "ウォレット未登録");
        }
        if (!WALLET_STATUS_ACTIVE.equals(wallet.walletStatus)) {
            return new DispatchJudgement(false, "停止中ウォレット");
        }

        return new DispatchJudgement(true, "送信対象");
    }

    private static java.util.Set<String> findDuplicatedNoticeIds(java.util.List<Pyntff> notices) {
        java.util.Set<String> seen = new java.util.HashSet<String>();
        java.util.Set<String> duplicated = new java.util.HashSet<String>();

        for (Pyntff notice : notices) {
            if (!seen.add(notice.noticeId)) {
                duplicated.add(notice.noticeId);
            }
        }
        return duplicated;
    }

    private static String buildBody(Pyntff notice, Pywalf wallet) {
        String template;
        if ("01".equals(notice.noticeKbn)) {
            template = "%s様、決済関連のお知らせです。%s";
        } else if ("02".equals(notice.noticeKbn)) {
            template = "%s様、入出金関連のお知らせです。%s";
        } else if ("03".equals(notice.noticeKbn)) {
            template = "%s様、重要なお知らせです。%s";
        } else {
            template = "%s様、お知らせです。%s";
        }

        return String.format(java.util.Locale.JAPAN, template, wallet.userNameKana, notice.noticeText.trim());
    }

    private static String resolveChannel(String noticeKbn) {
        if ("01".equals(noticeKbn)) {
            return "アプリ";
        }
        if ("02".equals(noticeKbn)) {
            return "メール";
        }
        return "アプリ";
    }

    private static java.util.List<Pyntff> loadPyntff() {
        java.util.List<Pyntff> rows = new java.util.ArrayList<Pyntff>();
        rows.add(new Pyntff("NT202606280001", "WL00010001", "01", "QR決済が承認されました。", "00", "2026-06-28T09:00:12"));
        rows.add(new Pyntff("NT202606280002", "WL00010002", "02", "出金依頼を受け付けました。", "00", "2026-06-28T09:01:35"));
        rows.add(new Pyntff("NT202606280003", "WL00010003", "03", "暗証番号の変更を検知しました。", "00", "2026-06-28T09:02:18"));
        rows.add(new Pyntff("NT202606280004", "WL00010004", "01", "加盟店での売上が確定しました。", "00", "2026-06-28T09:03:44"));
        rows.add(new Pyntff("NT202606280004", "WL00010005", "01", "重複検証用通知です。", "00", "2026-06-28T09:04:03"));
        rows.add(new Pyntff("NT202606280006", "WL00010006", "02", "   ", "00", "2026-06-28T09:05:21"));
        rows.add(new Pyntff("NT202606280007", "WL99999999", "03", "登録情報を確認してください。", "00", "2026-06-28T09:06:09"));
        rows.add(new Pyntff("NT202606280008", "WL00010001", "01", "チャージの受付を開始しました。", "10", "2026-06-28T09:07:55"));
        return rows;
    }

    private static java.util.Map<String, Pywalf> loadPywalfByWalletId() {
        java.util.Map<String, Pywalf> rows = new java.util.LinkedHashMap<String, Pywalf>();
        rows.put("WL00010001", new Pywalf("WL00010001", "US000001", "01", "G", "ヤマダタロウ"));
        rows.put("WL00010002", new Pywalf("WL00010002", "US000002", "02", "S", "サトウハナコ"));
        rows.put("WL00010003", new Pywalf("WL00010003", "US000003", "01", "P", "タナカイチロウ"));
        rows.put("WL00010004", new Pywalf("WL00010004", "US000004", "09", "G", "スズキミカ"));
        rows.put("WL00010005", new Pywalf("WL00010005", "US000005", "01", "S", "イトウケンジ"));
        rows.put("WL00010006", new Pywalf("WL00010006", "US000006", "01", "S", "コバヤシユイ"));
        return rows;
    }

    private static final class Pyntff {
        private final String noticeId;
        private final String walletId;
        private final String noticeKbn;
        private final String noticeText;
        private final String sendStatus;
        private final String createTs;

        private Pyntff(String noticeId,
                       String walletId,
                       String noticeKbn,
                       String noticeText,
                       String sendStatus,
                       String createTs) {
            this.noticeId = noticeId;
            this.walletId = walletId;
            this.noticeKbn = noticeKbn;
            this.noticeText = noticeText;
            this.sendStatus = sendStatus;
            this.createTs = createTs;
        }

        private Pyntff withSendStatus(String nextSendStatus) {
            return new Pyntff(noticeId, walletId, noticeKbn, noticeText, nextSendStatus, createTs);
        }
    }

    private static final class Pywalf {
        private final String walletId;
        private final String userId;
        private final String walletStatus;
        private final String walletTier;
        private final String userNameKana;

        private Pywalf(String walletId,
                       String userId,
                       String walletStatus,
                       String walletTier,
                       String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    private static final class DispatchJudgement {
        private final boolean dispatchable;
        private final String reason;

        private DispatchJudgement(boolean dispatchable, String reason) {
            this.dispatchable = dispatchable;
            this.reason = reason;
        }
    }
}
