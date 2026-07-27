package jp.mirai.pay.authorization;

/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2024-10-07  みらいペイ システム部  トップアップ受付サービス初版
 */
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class TopupReceptionService {
    private static final String WALLET_STATUS_ACTIVE = "01";
    private static final String WALLET_STATUS_STOPPED = "02";
    private static final String WALLET_STATUS_CLOSED = "03";
    private static final String WALLET_STATUS_RESTRICTED = "09";

    private static final String BASE_CURRENCY = "JPY";

    private static final String TOPUP_STATUS_ACCEPTED = "10";
    private static final String TOPUP_STATUS_PENDING = "20";
    private static final String TOPUP_STATUS_REJECTED = "90";

    private static final String NOTICE_KBN_HOLD = "TOPUP_HOLD";
    private static final String NOTICE_KBN_REJECT = "TOPUP_REJECT";
    private static final String SEND_STATUS_NOT_SENT = "00";

    private static final BigDecimal SHORT_INTERVAL_TOTAL_LIMIT = new BigDecimal("150000");
    private static final int SHORT_INTERVAL_MINUTES = 10;

    public static void main(String[] a) {
        Repository repository = Repository.synthetic();
        TopupReceptionService service = new TopupReceptionService(repository, new RiskScreening());

        List<TopupRequest> requests = Arrays.asList(
                new TopupRequest("REQ-20260628-0001", "WLT000001", new BigDecimal("30000"), "BANK", BASE_CURRENCY, "ミズホ", "1234567", "", LocalDateTime.of(2026, 6, 28, 9, 10, 12)),
                new TopupRequest("REQ-20260628-0002", "WLT000002", new BigDecimal("250000"), "CARD", BASE_CURRENCY, "", "", "411111******1111", LocalDateTime.of(2026, 6, 28, 9, 12, 45)),
                new TopupRequest("REQ-20260628-0003", "WLT000003", new BigDecimal("10000"), "BANK", BASE_CURRENCY, "ミツイスミトモ", "7654321", "", LocalDateTime.of(2026, 6, 28, 9, 14, 3)),
                new TopupRequest("REQ-20260628-0004", "WLT000001", new BigDecimal("80000"), "BANK", BASE_CURRENCY, "ミズホ", "1234567", "", LocalDateTime.of(2026, 6, 28, 9, 16, 2)),
                new TopupRequest("REQ-20260628-0005", "WLT000004", new BigDecimal("5000"), "CARD", "USD", "", "", "555555******4444", LocalDateTime.of(2026, 6, 28, 9, 18, 22)),
                new TopupRequest("REQ-20260628-0006", "WLT000001", new BigDecimal("90000"), "BANK", BASE_CURRENCY, "ミズホ", "1234567", "", LocalDateTime.of(2026, 6, 28, 9, 19, 40))
        );

        for (TopupRequest request : requests) {
            service.accept(request);
        }

        repository.printPyTopf();
        repository.printPyNtff();
    }

    private final Repository repository;
    private final RiskScreening riskScreening;

    private TopupReceptionService(Repository repository, RiskScreening riskScreening) {
        this.repository = repository;
        this.riskScreening = riskScreening;
    }

    private void accept(TopupRequest request) {
        Wallet wallet = repository.findWallet(request.walletId);
        LocalDateTime now = request.requestTs;

        if (wallet == null) {
            reject(request, "STS", "ウォレット未登録");
            return;
        }

        if (!WALLET_STATUS_ACTIVE.equals(wallet.walletStatus)) {
            reject(request, "STS", "ウォレット状態不正:" + wallet.walletStatus);
            return;
        }

        if (!BASE_CURRENCY.equals(request.currency)) {
            reject(request, "CUR", "取扱通貨対象外:" + request.currency);
            return;
        }

        TierLimit limit = repository.findTierLimit(wallet.walletTier);
        if (limit == null) {
            reject(request, "STS", "ティア未登録:" + wallet.walletTier);
            return;
        }

        String requiredError = validateRequiredItems(request);
        if (requiredError != null) {
            reject(request, "STS", requiredError);
            return;
        }

        if (request.amount.compareTo(BigDecimal.ZERO) <= 0) {
            reject(request, "LIM", "金額不正");
            return;
        }

        if (request.amount.compareTo(limit.perTxnLimitAmt) > 0) {
            reject(request, "LIM", "一回上限超過");
            return;
        }

        BigDecimal dailyTotal = repository.sumTopups(wallet.walletId, now.toLocalDate(), null).add(request.amount);
        if (dailyTotal.compareTo(limit.dailyLimitAmt) > 0) {
            reject(request, "LIM", "日次上限超過");
            return;
        }

        BigDecimal monthlyTotal = repository.sumTopups(wallet.walletId, null, YearMonth.from(now)).add(request.amount);
        if (monthlyTotal.compareTo(limit.monthlyLimitAmt) > 0) {
            reject(request, "LIM", "月次上限超過");
            return;
        }

        BigDecimal shortTotal = repository.sumTopupsAfter(wallet.walletId, now.minusMinutes(SHORT_INTERVAL_MINUTES)).add(request.amount);
        boolean riskRequired = request.amount.compareTo(limit.alertThresholdAmt) >= 0
                || shortTotal.compareTo(SHORT_INTERVAL_TOTAL_LIMIT) >= 0;

        if (riskRequired) {
            RiskResult result = riskScreening.screen(request, wallet, shortTotal);
            if (!"A".equals(result.decisionKbn)) {
                repository.writeNotice(new Notice(nextNoticeId(), wallet.walletId, NOTICE_KBN_HOLD,
                        "トップアップを保留しました。理由=" + result.reason, SEND_STATUS_NOT_SENT, now));
                repository.writeTopup(new Topup(nextTopupId(), wallet.walletId, request.amount,
                        request.paymentMethod, TOPUP_STATUS_PENDING, now));
                return;
            }
        }

        repository.writeTopup(new Topup(nextTopupId(), wallet.walletId, request.amount,
                request.paymentMethod, TOPUP_STATUS_ACCEPTED, now));
    }

    private String validateRequiredItems(TopupRequest request) {
        if ("BANK".equals(request.paymentMethod)) {
            if (isBlank(request.bankCode) || isBlank(request.accountNo)) {
                return "銀行入金必須項目不足";
            }
            return null;
        }
        if ("CARD".equals(request.paymentMethod)) {
            if (isBlank(request.cardToken)) {
                return "カード入金必須項目不足";
            }
            return null;
        }
        if ("ATM".equals(request.paymentMethod)) {
            return null;
        }
        return "入金手段対象外:" + request.paymentMethod;
    }

    private void reject(TopupRequest request, String declineReason, String text) {
        repository.writeNotice(new Notice(nextNoticeId(), request.walletId, NOTICE_KBN_REJECT,
                "トップアップ否認。理由=" + declineReason + "/" + text,
                SEND_STATUS_NOT_SENT, request.requestTs));
    }

    private String nextTopupId() {
        return "TP" + String.format(Locale.ROOT, "%010d", repository.nextTopupSeq());
    }

    private String nextNoticeId() {
        return "NT" + String.format(Locale.ROOT, "%010d", repository.nextNoticeSeq());
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static final class RiskScreening {
        private RiskResult screen(TopupRequest request, Wallet wallet, BigDecimal shortTotal) {
            if (WALLET_STATUS_RESTRICTED.equals(wallet.walletStatus)) {
                return new RiskResult("D", "ウォレット制限中");
            }
            if (request.amount.compareTo(new BigDecimal("300000")) >= 0) {
                return new RiskResult("D", "高額チャージ確認要");
            }
            if (shortTotal.compareTo(new BigDecimal("180000")) >= 0) {
                return new RiskResult("D", "短時間連続チャージ確認要");
            }
            return new RiskResult("A", "承認");
        }
    }

    private static final class Repository {
        private final Map<String, Wallet> wallets = new HashMap<>();
        private final Map<String, TierLimit> tierLimits = new HashMap<>();
        private final List<Topup> topups = new ArrayList<>();
        private final List<Notice> notices = new ArrayList<>();
        private long topupSeq = 1000;
        private long noticeSeq = 7000;

        private static Repository synthetic() {
            Repository repository = new Repository();
            repository.wallets.put("WLT000001", new Wallet("WLT000001", "USR000001", WALLET_STATUS_ACTIVE, "T2", "ヤマダタロウ"));
            repository.wallets.put("WLT000002", new Wallet("WLT000002", "USR000002", WALLET_STATUS_ACTIVE, "T3", "サトウハナコ"));
            repository.wallets.put("WLT000003", new Wallet("WLT000003", "USR000003", WALLET_STATUS_STOPPED, "T1", "スズキジロウ"));
            repository.wallets.put("WLT000004", new Wallet("WLT000004", "USR000004", WALLET_STATUS_ACTIVE, "T1", "タナカミカ"));

            repository.tierLimits.put("T1", new TierLimit("T1", new BigDecimal("50000"), new BigDecimal("100000"), new BigDecimal("500000"), new BigDecimal("40000")));
            repository.tierLimits.put("T2", new TierLimit("T2", new BigDecimal("200000"), new BigDecimal("300000"), new BigDecimal("1500000"), new BigDecimal("100000")));
            repository.tierLimits.put("T3", new TierLimit("T3", new BigDecimal("500000"), new BigDecimal("1000000"), new BigDecimal("5000000"), new BigDecimal("250000")));

            repository.topups.add(new Topup("TP0000000991", "WLT000001", new BigDecimal("20000"), "BANK", TOPUP_STATUS_ACCEPTED, LocalDateTime.of(2026, 6, 28, 8, 55, 10)));
            repository.topups.add(new Topup("TP0000000992", "WLT000001", new BigDecimal("40000"), "BANK", TOPUP_STATUS_ACCEPTED, LocalDateTime.of(2026, 6, 28, 9, 8, 30)));
            repository.topups.add(new Topup("TP0000000993", "WLT000002", new BigDecimal("100000"), "CARD", TOPUP_STATUS_ACCEPTED, LocalDateTime.of(2026, 6, 27, 17, 40, 0)));
            return repository;
        }

        private Wallet findWallet(String walletId) {
            return wallets.get(walletId);
        }

        private TierLimit findTierLimit(String tierCode) {
            return tierLimits.get(tierCode);
        }

        private void writeTopup(Topup topup) {
            topups.add(topup);
        }

        private void writeNotice(Notice notice) {
            notices.add(notice);
        }

        private long nextTopupSeq() {
            topupSeq++;
            return topupSeq;
        }

        private long nextNoticeSeq() {
            noticeSeq++;
            return noticeSeq;
        }

        private BigDecimal sumTopups(String walletId, LocalDate date, YearMonth month) {
            BigDecimal sum = BigDecimal.ZERO;
            for (Topup topup : topups) {
                if (!walletId.equals(topup.walletId) || TOPUP_STATUS_REJECTED.equals(topup.topupStatus)) {
                    continue;
                }
                if (date != null && !topup.requestTs.toLocalDate().equals(date)) {
                    continue;
                }
                if (month != null && !YearMonth.from(topup.requestTs).equals(month)) {
                    continue;
                }
                sum = sum.add(topup.amount);
            }
            return sum;
        }

        private BigDecimal sumTopupsAfter(String walletId, LocalDateTime fromTs) {
            BigDecimal sum = BigDecimal.ZERO;
            for (Topup topup : topups) {
                if (walletId.equals(topup.walletId)
                        && !TOPUP_STATUS_REJECTED.equals(topup.topupStatus)
                        && !topup.requestTs.isBefore(fromTs)) {
                    sum = sum.add(topup.amount);
                }
            }
            return sum;
        }

        private void printPyTopf() {
            List<Topup> sorted = new ArrayList<>(topups);
            Collections.sort(sorted, Comparator.comparing(t -> t.topupId));
            for (Topup topup : sorted) {
                System.out.println("PYTOPF "
                        + topup.topupId + ","
                        + topup.walletId + ","
                        + topup.amount.toPlainString() + ","
                        + topup.paymentMethod + ","
                        + topup.topupStatus + ","
                        + topup.requestTs.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
            }
        }

        private void printPyNtff() {
            for (Notice notice : notices) {
                System.out.println("PYNTFF "
                        + notice.noticeId + ","
                        + notice.walletId + ","
                        + notice.noticeKbn + ","
                        + notice.noticeText + ","
                        + notice.sendStatus + ","
                        + notice.createTs.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
            }
        }
    }

    private static final class Wallet {
        private final String walletId;
        private final String userId;
        private final String walletStatus;
        private final String walletTier;
        private final String userNameKana;

        private Wallet(String walletId, String userId, String walletStatus, String walletTier, String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    private static final class TierLimit {
        private final String tierCode;
        private final BigDecimal perTxnLimitAmt;
        private final BigDecimal dailyLimitAmt;
        private final BigDecimal monthlyLimitAmt;
        private final BigDecimal alertThresholdAmt;

        private TierLimit(String tierCode, BigDecimal perTxnLimitAmt, BigDecimal dailyLimitAmt,
                          BigDecimal monthlyLimitAmt, BigDecimal alertThresholdAmt) {
            this.tierCode = tierCode;
            this.perTxnLimitAmt = perTxnLimitAmt;
            this.dailyLimitAmt = dailyLimitAmt;
            this.monthlyLimitAmt = monthlyLimitAmt;
            this.alertThresholdAmt = alertThresholdAmt;
        }
    }

    private static final class TopupRequest {
        private final String requestId;
        private final String walletId;
        private final BigDecimal amount;
        private final String paymentMethod;
        private final String currency;
        private final String bankCode;
        private final String accountNo;
        private final String cardToken;
        private final LocalDateTime requestTs;

        private TopupRequest(String requestId, String walletId, BigDecimal amount, String paymentMethod,
                             String currency, String bankCode, String accountNo, String cardToken,
                             LocalDateTime requestTs) {
            this.requestId = requestId;
            this.walletId = walletId;
            this.amount = amount;
            this.paymentMethod = paymentMethod;
            this.currency = currency;
            this.bankCode = bankCode;
            this.accountNo = accountNo;
            this.cardToken = cardToken;
            this.requestTs = requestTs;
        }
    }

    private static final class Topup {
        private final String topupId;
        private final String walletId;
        private final BigDecimal amount;
        private final String paymentMethod;
        private final String topupStatus;
        private final LocalDateTime requestTs;

        private Topup(String topupId, String walletId, BigDecimal amount, String paymentMethod,
                      String topupStatus, LocalDateTime requestTs) {
            this.topupId = topupId;
            this.walletId = walletId;
            this.amount = amount;
            this.paymentMethod = paymentMethod;
            this.topupStatus = topupStatus;
            this.requestTs = requestTs;
        }
    }

    private static final class Notice {
        private final String noticeId;
        private final String walletId;
        private final String noticeKbn;
        private final String noticeText;
        private final String sendStatus;
        private final LocalDateTime createTs;

        private Notice(String noticeId, String walletId, String noticeKbn, String noticeText,
                       String sendStatus, LocalDateTime createTs) {
            this.noticeId = noticeId;
            this.walletId = walletId;
            this.noticeKbn = noticeKbn;
            this.noticeText = noticeText;
            this.sendStatus = sendStatus;
            this.createTs = createTs;
        }
    }

    private static final class RiskResult {
        private final String decisionKbn;
        private final String reason;

        private RiskResult(String decisionKbn, String reason) {
            this.decisionKbn = decisionKbn;
            this.reason = reason;
        }
    }
}
