package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2024-12-03  みらいペイ システム部    初版。外部会員状態とウォレット状態の差分通知生成。
 */
public class WalletStatusSyncService {
    private static final String STATUS_ACTIVE = "01";
    private static final String STATUS_STOPPED = "02";
    private static final String STATUS_CLOSED = "03";
    private static final String STATUS_LIMITED = "09";

    private static final String TOPUP_WAITING = "10";
    private static final String TOPUP_SETTLED = "30";

    private static final int RISK_HOLD_SCORE = 800;

    public static void main(String[] a) {
        java.time.LocalDateTime baseTs = java.time.LocalDateTime.of(2026, 6, 28, 9, 0, 0);

        java.util.List<Wallet> wallets = java.util.Arrays.asList(
                new Wallet("WLT000001", "USR000001", STATUS_ACTIVE, "通常", "ヤマダタロウ"),
                new Wallet("WLT000002", "USR000002", STATUS_ACTIVE, "通常", "サトウハナコ"),
                new Wallet("WLT000003", "USR000003", STATUS_STOPPED, "通常", "スズキイチロウ"),
                new Wallet("WLT000004", "USR000004", STATUS_ACTIVE, "簡易", "タナカミカ"),
                new Wallet("WLT000005", "USR000005", STATUS_LIMITED, "通常", "イトウケンジ"),
                new Wallet("WLT000006", "USR000006", STATUS_CLOSED, "通常", "コバヤシレイ")
        );

        java.util.List<Topup> topups = java.util.Arrays.asList(
                new Topup("TPU900001", "WLT000001", 20000L, "銀行振込", TOPUP_SETTLED, baseTs.minusHours(8)),
                new Topup("TPU900002", "WLT000002", 75000L, "コンビニ", TOPUP_WAITING, baseTs.minusMinutes(40)),
                new Topup("TPU900003", "WLT000004", 15000L, "銀行振込", TOPUP_SETTLED, baseTs.minusDays(1)),
                new Topup("TPU900004", "WLT000005", 30000L, "カード", TOPUP_WAITING, baseTs.minusMinutes(12))
        );

        java.util.List<RiskScore> scores = java.util.Arrays.asList(
                new RiskScore("SCR700001", "WLT000001", "MRC001", 210, "通常利用", baseTs.minusDays(1)),
                new RiskScore("SCR700002", "WLT000002", "MRC019", 420, "入金確認待ち", baseTs.minusHours(2)),
                new RiskScore("SCR700003", "WLT000003", "MRC044", 180, "再開審査済", baseTs.minusHours(3)),
                new RiskScore("SCR700004", "WLT000004", "MRC087", 650, "本人確認不足", baseTs.minusMinutes(55)),
                new RiskScore("SCR700005", "WLT000005", "MRC091", 930, "高頻度取引", baseTs.minusMinutes(10))
        );

        java.util.List<MemberChange> changes = java.util.Arrays.asList(
                new MemberChange("USR000001", "停止", true, baseTs.minusMinutes(30)),
                new MemberChange("USR000002", "停止", true, baseTs.minusMinutes(25)),
                new MemberChange("USR000003", "再開", true, baseTs.minusMinutes(20)),
                new MemberChange("USR000004", "有効", false, baseTs.minusMinutes(18)),
                new MemberChange("USR000005", "再開", true, baseTs.minusMinutes(15)),
                new MemberChange("USR000006", "再開", true, baseTs.minusMinutes(10))
        );

        java.util.List<Notice> notices = synchronize(wallets, topups, scores, changes, baseTs);
        for (Notice n : notices) {
            System.out.println(n.toLine());
        }
    }

    private static java.util.List<Notice> synchronize(
            java.util.List<Wallet> wallets,
            java.util.List<Topup> topups,
            java.util.List<RiskScore> scores,
            java.util.List<MemberChange> changes,
            java.time.LocalDateTime createTs) {
        java.util.Map<String, Wallet> walletByUser = new java.util.HashMap<>();
        for (Wallet w : wallets) {
            walletByUser.put(w.userId, w);
        }

        java.util.Map<String, Long> pendingAmountByWallet = new java.util.HashMap<>();
        for (Topup t : topups) {
            if (TOPUP_WAITING.equals(t.topupStatus)) {
                pendingAmountByWallet.merge(t.walletId, t.topupAmt, Long::sum);
            }
        }

        java.util.Map<String, RiskScore> latestScoreByWallet = new java.util.HashMap<>();
        for (RiskScore s : scores) {
            RiskScore current = latestScoreByWallet.get(s.walletId);
            if (current == null || s.scoreAsOfTs.isAfter(current.scoreAsOfTs)) {
                latestScoreByWallet.put(s.walletId, s);
            }
        }

        java.util.List<Notice> notices = new java.util.ArrayList<>();
        int seq = 1;
        for (MemberChange c : changes) {
            Wallet w = walletByUser.get(c.userId);
            if (w == null) {
                continue;
            }

            NoticeDecision decision = decide(w, c);
            if (decision == null) {
                continue;
            }

            long pendingAmount = pendingAmountByWallet.getOrDefault(w.walletId, 0L);
            RiskScore risk = latestScoreByWallet.get(w.walletId);
            boolean highRisk = risk != null && risk.riskScore >= RISK_HOLD_SCORE;
            boolean hasPendingTopup = pendingAmount > 0L;
            boolean hold = highRisk || hasPendingTopup;

            String noticeId = "NTF" + createTs.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")) + String.format("%04d", seq++);
            String sendStatus = hold ? "保留" : "送信待ち";
            String text = buildNoticeText(w, decision, c, pendingAmount, risk, hold);
            notices.add(new Notice(noticeId, w.walletId, decision.noticeKbn, text, sendStatus, createTs));
        }
        return notices;
    }

    private static NoticeDecision decide(Wallet w, MemberChange c) {
        if (STATUS_CLOSED.equals(w.walletStatus)) {
            return null;
        }
        if ("停止".equals(c.externalStatus) && !STATUS_STOPPED.equals(w.walletStatus)) {
            return new NoticeDecision("停止", "外部会員基盤の停止指示を検知");
        }
        if ("再開".equals(c.externalStatus) && STATUS_STOPPED.equals(w.walletStatus)) {
            return new NoticeDecision("再開", "外部会員基盤の再開指示を検知");
        }
        if (!c.kycCompleted && STATUS_ACTIVE.equals(w.walletStatus)) {
            return new NoticeDecision("ＫＹＣ不足", "本人確認未完了を検知");
        }
        return null;
    }

    private static String buildNoticeText(
            Wallet w,
            NoticeDecision d,
            MemberChange c,
            long pendingAmount,
            RiskScore risk,
            boolean hold) {
        StringBuilder b = new StringBuilder();
        b.append(d.reason)
                .append("。利用者=").append(w.userNameKana)
                .append("、会員状態=").append(c.externalStatus)
                .append("、ウォレット状態=").append(statusName(w.walletStatus))
                .append("、通知区分=").append(d.noticeKbn);

        if (pendingAmount > 0L) {
            b.append("、未確定チャージ=").append(pendingAmount).append("円");
        }
        if (risk != null) {
            b.append("、リスクスコア=").append(risk.riskScore)
                    .append("、理由=").append(risk.scoreReason);
        }
        if (hold) {
            b.append("。後続審査のため通知を保留");
        }
        return b.toString();
    }

    private static String statusName(String status) {
        if (STATUS_ACTIVE.equals(status)) {
            return "有効";
        }
        if (STATUS_STOPPED.equals(status)) {
            return "利用停止";
        }
        if (STATUS_CLOSED.equals(status)) {
            return "解約";
        }
        if (STATUS_LIMITED.equals(status)) {
            return "制限中";
        }
        return "不明";
    }

    private static final class Wallet {
        final String walletId;
        final String userId;
        final String walletStatus;
        final String walletTier;
        final String userNameKana;

        Wallet(String walletId, String userId, String walletStatus, String walletTier, String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    private static final class Topup {
        final String topupId;
        final String walletId;
        final long topupAmt;
        final String paymentMethod;
        final String topupStatus;
        final java.time.LocalDateTime requestTs;

        Topup(String topupId, String walletId, long topupAmt, String paymentMethod, String topupStatus, java.time.LocalDateTime requestTs) {
            this.topupId = topupId;
            this.walletId = walletId;
            this.topupAmt = topupAmt;
            this.paymentMethod = paymentMethod;
            this.topupStatus = topupStatus;
            this.requestTs = requestTs;
        }
    }

    private static final class RiskScore {
        final String scoreId;
        final String walletId;
        final String merchantCode;
        final int riskScore;
        final String scoreReason;
        final java.time.LocalDateTime scoreAsOfTs;

        RiskScore(String scoreId, String walletId, String merchantCode, int riskScore, String scoreReason, java.time.LocalDateTime scoreAsOfTs) {
            this.scoreId = scoreId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.riskScore = riskScore;
            this.scoreReason = scoreReason;
            this.scoreAsOfTs = scoreAsOfTs;
        }
    }

    private static final class MemberChange {
        final String userId;
        final String externalStatus;
        final boolean kycCompleted;
        final java.time.LocalDateTime changedTs;

        MemberChange(String userId, String externalStatus, boolean kycCompleted, java.time.LocalDateTime changedTs) {
            this.userId = userId;
            this.externalStatus = externalStatus;
            this.kycCompleted = kycCompleted;
            this.changedTs = changedTs;
        }
    }

    private static final class NoticeDecision {
        final String noticeKbn;
        final String reason;

        NoticeDecision(String noticeKbn, String reason) {
            this.noticeKbn = noticeKbn;
            this.reason = reason;
        }
    }

    private static final class Notice {
        final String noticeId;
        final String walletId;
        final String noticeKbn;
        final String noticeText;
        final String sendStatus;
        final java.time.LocalDateTime createTs;

        Notice(String noticeId, String walletId, String noticeKbn, String noticeText, String sendStatus, java.time.LocalDateTime createTs) {
            this.noticeId = noticeId;
            this.walletId = walletId;
            this.noticeKbn = noticeKbn;
            this.noticeText = noticeText;
            this.sendStatus = sendStatus;
            this.createTs = createTs;
        }

        String toLine() {
            return noticeId + "," + walletId + "," + noticeKbn + "," + noticeText + "," + sendStatus + "," + createTs;
        }
    }
}
