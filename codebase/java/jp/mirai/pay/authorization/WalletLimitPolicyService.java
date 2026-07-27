package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024-11-05  みらいペイ システム部      初版作成
 */
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class WalletLimitPolicyService {
    private static final String STATUS_ACTIVE = "01";
    private static final String STATUS_SUSPENDED = "02";
    private static final String STATUS_CLOSED = "03";
    private static final String STATUS_RESTRICTED = "09";

    private static final String DECISION_APPROVE = "A";
    private static final String DECISION_DENY = "D";

    private static final String REASON_LIMIT = "LIM";
    private static final String REASON_STATUS = "STS";
    private static final String REASON_CURRENCY = "CUR";

    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] a) {
        WalletLimitPolicyService service = new WalletLimitPolicyService();

        List<Pylmtf> limits = Arrays.asList(
                new Pylmtf("T1", yen("50000"), yen("150000"), yen("800000"), yen("120000")),
                new Pylmtf("T2", yen("200000"), yen("700000"), yen("3000000"), yen("550000")),
                new Pylmtf("T3", yen("1000000"), yen("3000000"), yen("12000000"), yen("2500000"))
        );

        List<Pywalf> wallets = Arrays.asList(
                new Pywalf("WL00000001", "U000000001", STATUS_ACTIVE, "T1", "ヤマダタロウ"),
                new Pywalf("WL00000002", "U000000002", STATUS_RESTRICTED, "T2", "サトウハナコ"),
                new Pywalf("WL00000003", "U000000003", STATUS_SUSPENDED, "T3", "スズキイチロウ"),
                new Pywalf("WL00000004", "U000000004", STATUS_CLOSED, "T1", "タナカミカ")
        );

        List<Pyvelf> velocities = Arrays.asList(
                new Pyvelf("WL00000001", LocalDateTime.of(2026, 6, 28, 0, 0), 3, yen("73000"), 0,
                        LocalDateTime.of(2026, 6, 28, 10, 12, 31)),
                new Pyvelf("WL00000001", LocalDateTime.of(2026, 6, 1, 0, 0), 24, yen("401000"), 1,
                        LocalDateTime.of(2026, 6, 27, 18, 44, 9)),
                new Pyvelf("WL00000002", LocalDateTime.of(2026, 6, 28, 0, 0), 7, yen("630000"), 2,
                        LocalDateTime.of(2026, 6, 28, 9, 30, 0))
        );

        List<AuthRequest> requests = Arrays.asList(
                new AuthRequest("REQ000001", "WL00000001", yen("25000"), BASE_CURRENCY,
                        LocalDateTime.of(2026, 6, 28, 11, 5, 0)),
                new AuthRequest("REQ000002", "WL00000001", yen("90000"), BASE_CURRENCY,
                        LocalDateTime.of(2026, 6, 28, 11, 6, 0)),
                new AuthRequest("REQ000003", "WL00000002", yen("20000"), BASE_CURRENCY,
                        LocalDateTime.of(2026, 6, 28, 11, 7, 0)),
                new AuthRequest("REQ000004", "WL00000001", yen("1000"), "USD",
                        LocalDateTime.of(2026, 6, 28, 11, 8, 0))
        );

        Map<String, Pylmtf> limitByTier = indexLimitByTier(limits);
        Map<String, Pywalf> walletById = indexWalletById(wallets);

        for (AuthRequest request : requests) {
            AuthResult result = service.decide(request, walletById, limitByTier, velocities);
            System.out.println(result.toLine());
        }
    }

    private AuthResult decide(AuthRequest request, Map<String, Pywalf> walletById,
                              Map<String, Pylmtf> limitByTier, List<Pyvelf> velocities) {
        if (!BASE_CURRENCY.equals(request.currency)) {
            return AuthResult.deny(request.requestId, REASON_CURRENCY, "取扱通貨対象外");
        }

        Pywalf wallet = walletById.get(request.walletId);
        if (wallet == null) {
            return AuthResult.deny(request.requestId, REASON_STATUS, "ウォレット未登録");
        }

        if (!STATUS_ACTIVE.equals(wallet.walletStatus)) {
            return AuthResult.deny(request.requestId, REASON_STATUS, statusMessage(wallet.walletStatus));
        }

        Pylmtf limit = limitByTier.get(wallet.walletTier);
        if (limit == null) {
            return AuthResult.deny(request.requestId, REASON_LIMIT, "ティア限度額未登録");
        }

        if (request.amount.compareTo(limit.perTxnLimitAmount) > 0) {
            return AuthResult.deny(request.requestId, REASON_LIMIT, "一回限度額超過");
        }

        BigDecimal dailyUsed = sumWindow(velocities, request.walletId, request.requestTime, WindowKind.DAY);
        BigDecimal monthlyUsed = sumWindow(velocities, request.walletId, request.requestTime, WindowKind.MONTH);

        BigDecimal dailyAfter = dailyUsed.add(request.amount);
        if (dailyAfter.compareTo(limit.dailyLimitAmount) > 0) {
            return AuthResult.deny(request.requestId, REASON_LIMIT, "日次限度額超過");
        }

        BigDecimal monthlyAfter = monthlyUsed.add(request.amount);
        if (monthlyAfter.compareTo(limit.monthlyLimitAmount) > 0) {
            return AuthResult.deny(request.requestId, REASON_LIMIT, "月次限度額超過");
        }

        boolean alert = dailyAfter.compareTo(limit.alertThresholdAmount) >= 0;
        return AuthResult.approve(request.requestId, dailyAfter, monthlyAfter, alert);
    }

    private static BigDecimal sumWindow(List<Pyvelf> velocities, String walletId,
                                        LocalDateTime requestTime, WindowKind kind) {
        BigDecimal sum = BigDecimal.ZERO;
        for (Pyvelf velocity : velocities) {
            if (!walletId.equals(velocity.walletId)) {
                continue;
            }
            if (inWindow(velocity.windowStartTs, requestTime, kind)) {
                sum = sum.add(velocity.authSumAmount);
            }
        }
        return sum;
    }

    private static boolean inWindow(LocalDateTime windowStart, LocalDateTime requestTime, WindowKind kind) {
        LocalDate windowDate = windowStart.toLocalDate();
        LocalDate requestDate = requestTime.toLocalDate();

        if (kind == WindowKind.DAY) {
            return windowDate.equals(requestDate);
        }
        return windowDate.getYear() == requestDate.getYear()
                && windowDate.getMonthValue() == requestDate.getMonthValue();
    }

    private static Map<String, Pylmtf> indexLimitByTier(List<Pylmtf> limits) {
        Map<String, Pylmtf> indexed = new HashMap<String, Pylmtf>();
        for (Pylmtf limit : limits) {
            indexed.put(limit.tierCode, limit);
        }
        return indexed;
    }

    private static Map<String, Pywalf> indexWalletById(List<Pywalf> wallets) {
        Map<String, Pywalf> indexed = new HashMap<String, Pywalf>();
        for (Pywalf wallet : wallets) {
            indexed.put(wallet.walletId, wallet);
        }
        return indexed;
    }

    private static String statusMessage(String status) {
        if (STATUS_SUSPENDED.equals(status)) {
            return "利用停止";
        }
        if (STATUS_CLOSED.equals(status)) {
            return "解約";
        }
        if (STATUS_RESTRICTED.equals(status)) {
            return "制限中";
        }
        return "ウォレット状態不正";
    }

    private static BigDecimal yen(String amount) {
        return new BigDecimal(amount);
    }

    private enum WindowKind {
        DAY,
        MONTH
    }

    private static final class Pylmtf {
        private final String tierCode;
        private final BigDecimal perTxnLimitAmount;
        private final BigDecimal dailyLimitAmount;
        private final BigDecimal monthlyLimitAmount;
        private final BigDecimal alertThresholdAmount;

        private Pylmtf(String tierCode, BigDecimal perTxnLimitAmount, BigDecimal dailyLimitAmount,
                       BigDecimal monthlyLimitAmount, BigDecimal alertThresholdAmount) {
            this.tierCode = tierCode;
            this.perTxnLimitAmount = perTxnLimitAmount;
            this.dailyLimitAmount = dailyLimitAmount;
            this.monthlyLimitAmount = monthlyLimitAmount;
            this.alertThresholdAmount = alertThresholdAmount;
        }
    }

    private static final class Pywalf {
        private final String walletId;
        private final String userId;
        private final String walletStatus;
        private final String walletTier;
        private final String userNameKana;

        private Pywalf(String walletId, String userId, String walletStatus, String walletTier, String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    private static final class Pyvelf {
        private final String walletId;
        private final LocalDateTime windowStartTs;
        private final int authCount;
        private final BigDecimal authSumAmount;
        private final int denyCount;
        private final LocalDateTime lastReqTs;

        private Pyvelf(String walletId, LocalDateTime windowStartTs, int authCount,
                       BigDecimal authSumAmount, int denyCount, LocalDateTime lastReqTs) {
            this.walletId = walletId;
            this.windowStartTs = windowStartTs;
            this.authCount = authCount;
            this.authSumAmount = authSumAmount;
            this.denyCount = denyCount;
            this.lastReqTs = lastReqTs;
        }
    }

    private static final class AuthRequest {
        private final String requestId;
        private final String walletId;
        private final BigDecimal amount;
        private final String currency;
        private final LocalDateTime requestTime;

        private AuthRequest(String requestId, String walletId, BigDecimal amount,
                            String currency, LocalDateTime requestTime) {
            this.requestId = requestId;
            this.walletId = walletId;
            this.amount = amount;
            this.currency = currency;
            this.requestTime = requestTime;
        }
    }

    private static final class AuthResult {
        private final String requestId;
        private final String decision;
        private final String declineReason;
        private final String message;
        private final BigDecimal dailyAfterAmount;
        private final BigDecimal monthlyAfterAmount;
        private final boolean alert;

        private AuthResult(String requestId, String decision, String declineReason, String message,
                           BigDecimal dailyAfterAmount, BigDecimal monthlyAfterAmount, boolean alert) {
            this.requestId = requestId;
            this.decision = decision;
            this.declineReason = declineReason;
            this.message = message;
            this.dailyAfterAmount = dailyAfterAmount;
            this.monthlyAfterAmount = monthlyAfterAmount;
            this.alert = alert;
        }

        private static AuthResult approve(String requestId, BigDecimal dailyAfterAmount,
                                          BigDecimal monthlyAfterAmount, boolean alert) {
            return new AuthResult(requestId, DECISION_APPROVE, "", "受付可能",
                    dailyAfterAmount, monthlyAfterAmount, alert);
        }

        private static AuthResult deny(String requestId, String reason, String message) {
            return new AuthResult(requestId, DECISION_DENY, reason, message,
                    BigDecimal.ZERO, BigDecimal.ZERO, false);
        }

        private String toLine() {
            List<String> values = new ArrayList<String>();
            values.add("要求ID=" + requestId);
            values.add("判定=" + decision);
            if (!declineReason.isEmpty()) {
                values.add("否認理由=" + declineReason);
            }
            values.add("内容=" + message);
            if (DECISION_APPROVE.equals(decision)) {
                values.add("日次利用後=" + dailyAfterAmount.toPlainString());
                values.add("月次利用後=" + monthlyAfterAmount.toPlainString());
                values.add("警告=" + (alert ? "対象" : "対象外"));
            }
            return String.join(" ", values);
        }
    }
}
