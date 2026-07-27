package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2024-08-19  みらいペイ システム部    残高スナップショット組立ヘルパ初版
 *
 * 表示用の残高スナップショットを組み立てる。利用可能残高 (AVAIL-AMT) と
 * 判定結果はオーソリ判定 (PYARSPF) で確定済みの値を転記するだけで、
 * ここでは再計算しない。ホールド・未確定決済は参考情報として件数のみ付す。
 */
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BalanceSnapshotAssembler {
    private static final String STATUS_ACTIVE = "01";
    private static final String BASE_CURRENCY = "JPY";
    private static final String HOLD_RESULT_APPROVED = "00";
    private static final String PENDING_STATUS_OPEN = "10";

    public static void main(String[] a) {
        List<PyBalf> balances = Arrays.asList(
                new PyBalf("WL000001", "01", new BigDecimal("125000.40"), new BigDecimal("30000"), LocalDate.of(2026, 6, 28)),
                new PyBalf("WL000002", "09", new BigDecimal("8000.00"), new BigDecimal("5000"), LocalDate.of(2026, 6, 28)),
                new PyBalf("WL000003", "01", new BigDecimal("43120.70"), new BigDecimal("10000"), LocalDate.of(2026, 6, 28))
        );

        List<PyHoldf> holds = Arrays.asList(
                new PyHoldf("HD000001", "WL000001", new BigDecimal("2500.45"), "00", "JP-SHOP-01", "JPY", LocalDate.of(2026, 7, 1)),
                new PyHoldf("HD000002", "WL000001", new BigDecimal("1400.10"), "20", "JP-SHOP-02", "JPY", LocalDate.of(2026, 7, 3)),
                new PyHoldf("HD000003", "WL000001", new BigDecimal("999.90"), "00", "JP-SHOP-03", "JPY", LocalDate.of(2026, 6, 20)),
                new PyHoldf("HD000004", "WL000003", new BigDecimal("3000.00"), "30", "JP-SHOP-04", "JPY", LocalDate.of(2026, 6, 29)),
                new PyHoldf("HD000005", "WL000003", new BigDecimal("750.80"), "00", "JP-SHOP-05", "USD", LocalDate.of(2026, 7, 2))
        );

        List<PyPendf> pendings = Arrays.asList(
                new PyPendf("PN000001", "WL000001", new BigDecimal("4500.60"), "10", LocalDate.of(2026, 6, 28)),
                new PyPendf("PN000002", "WL000001", new BigDecimal("1200.00"), "30", LocalDate.of(2026, 6, 27)),
                new PyPendf("PN000003", "WL000003", new BigDecimal("300.25"), "10", LocalDate.of(2026, 6, 28))
        );

        // オーソリ判定 (AuthEngine) が確定させた利用可能残高・判定結果。
        List<PyArspf> authResults = Arrays.asList(
                new PyArspf("REQ000001", "WL000001", "A", new BigDecimal("118000"), new BigDecimal("12000"), ""),
                new PyArspf("REQ000002", "WL000002", "D", new BigDecimal("8000"), new BigDecimal("1000"), "STS"),
                new PyArspf("REQ000003", "WL000003", "A", new BigDecimal("39820"), new BigDecimal("5000"), "")
        );

        List<BalanceSnapshot> snapshots = assemble(balances, holds, pendings, authResults);
        for (BalanceSnapshot snapshot : snapshots) {
            System.out.println(snapshot.toLine());
        }
    }

    /**
     * 表示用スナップショットを組み立てる。AVAIL-AMT と判定区分は PYARSPF の
     * 確定値をそのまま転記し、当ヘルパでは残高計算を行わない。
     */
    private static List<BalanceSnapshot> assemble(List<PyBalf> balances, List<PyHoldf> holds,
                                                  List<PyPendf> pendings, List<PyArspf> authResults) {
        List<BalanceSnapshot> snapshots = new ArrayList<>();
        for (PyBalf balance : balances) {
            BigDecimal ledgerAmount = yen(balance.ledgerBalanceAmount);
            int approvedHoldCount = countApprovedHolds(balance.walletId, holds);
            int openPendingCount = countOpenPendings(balance.walletId, pendings);

            PyArspf result = latestResult(balance.walletId, authResults);
            BigDecimal availableAmount = result != null ? yen(result.availAmount) : BigDecimal.ZERO;
            String decision = result != null ? result.decisionKbn
                    : (STATUS_ACTIVE.equals(balance.walletStatus) ? "" : "D");
            String declineReason = result != null ? result.declineReason
                    : (STATUS_ACTIVE.equals(balance.walletStatus) ? "" : "STS");

            snapshots.add(new BalanceSnapshot(
                    balance.walletId,
                    balance.walletStatus,
                    BASE_CURRENCY,
                    ledgerAmount,
                    approvedHoldCount,
                    openPendingCount,
                    availableAmount,
                    balance.lastTopupAmount,
                    balance.balanceAsOfDate,
                    decision,
                    declineReason
            ));
        }
        return snapshots;
    }

    private static int countApprovedHolds(String walletId, List<PyHoldf> holds) {
        int count = 0;
        for (PyHoldf hold : holds) {
            if (!walletId.equals(hold.walletId)) {
                continue;
            }
            if (!BASE_CURRENCY.equals(hold.currencyCode)) {
                continue;
            }
            if (!HOLD_RESULT_APPROVED.equals(hold.holdResult)) {
                continue;
            }
            count++;
        }
        return count;
    }

    private static int countOpenPendings(String walletId, List<PyPendf> pendings) {
        int count = 0;
        for (PyPendf pending : pendings) {
            if (walletId.equals(pending.walletId) && PENDING_STATUS_OPEN.equals(pending.pendingStatus)) {
                count++;
            }
        }
        return count;
    }

    private static PyArspf latestResult(String walletId, List<PyArspf> authResults) {
        PyArspf latest = null;
        for (PyArspf result : authResults) {
            if (walletId.equals(result.walletId)) {
                latest = result;
            }
        }
        return latest;
    }

    private static BigDecimal yen(BigDecimal amount) {
        return amount.setScale(0, RoundingMode.HALF_UP);
    }

    private static final class PyBalf {
        private final String walletId;
        private final String walletStatus;
        private final BigDecimal ledgerBalanceAmount;
        private final BigDecimal lastTopupAmount;
        private final LocalDate balanceAsOfDate;

        private PyBalf(String walletId, String walletStatus, BigDecimal ledgerBalanceAmount, BigDecimal lastTopupAmount, LocalDate balanceAsOfDate) {
            this.walletId = walletId;
            this.walletStatus = walletStatus;
            this.ledgerBalanceAmount = ledgerBalanceAmount;
            this.lastTopupAmount = lastTopupAmount;
            this.balanceAsOfDate = balanceAsOfDate;
        }
    }

    private static final class PyHoldf {
        private final String holdId;
        private final String walletId;
        private final BigDecimal holdAmount;
        private final String holdResult;
        private final String merchantCode;
        private final String currencyCode;
        private final LocalDate holdExpireDate;

        private PyHoldf(String holdId, String walletId, BigDecimal holdAmount, String holdResult, String merchantCode, String currencyCode, LocalDate holdExpireDate) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.holdAmount = holdAmount;
            this.holdResult = holdResult;
            this.merchantCode = merchantCode;
            this.currencyCode = currencyCode;
            this.holdExpireDate = holdExpireDate;
        }
    }

    private static final class PyPendf {
        private final String pendingId;
        private final String walletId;
        private final BigDecimal pendingAmount;
        private final String pendingStatus;
        private final LocalDate captureDate;

        private PyPendf(String pendingId, String walletId, BigDecimal pendingAmount, String pendingStatus, LocalDate captureDate) {
            this.pendingId = pendingId;
            this.walletId = walletId;
            this.pendingAmount = pendingAmount;
            this.pendingStatus = pendingStatus;
            this.captureDate = captureDate;
        }
    }

    private static final class PyArspf {
        private final String reqId;
        private final String walletId;
        private final String decisionKbn;
        private final BigDecimal availAmount;
        private final BigDecimal reqAmount;
        private final String declineReason;

        private PyArspf(String reqId, String walletId, String decisionKbn, BigDecimal availAmount,
                        BigDecimal reqAmount, String declineReason) {
            this.reqId = reqId;
            this.walletId = walletId;
            this.decisionKbn = decisionKbn;
            this.availAmount = availAmount;
            this.reqAmount = reqAmount;
            this.declineReason = declineReason;
        }
    }

    private static final class BalanceSnapshot {
        private final String walletId;
        private final String walletStatus;
        private final String currencyCode;
        private final BigDecimal ledgerAmount;
        private final int approvedHoldCount;
        private final int openPendingCount;
        private final BigDecimal availableAmount;
        private final BigDecimal lastTopupAmount;
        private final LocalDate balanceAsOfDate;
        private final String decision;
        private final String declineReason;

        private BalanceSnapshot(String walletId, String walletStatus, String currencyCode, BigDecimal ledgerAmount,
                                int approvedHoldCount, int openPendingCount, BigDecimal availableAmount,
                                BigDecimal lastTopupAmount, LocalDate balanceAsOfDate,
                                String decision, String declineReason) {
            this.walletId = walletId;
            this.walletStatus = walletStatus;
            this.currencyCode = currencyCode;
            this.ledgerAmount = ledgerAmount;
            this.approvedHoldCount = approvedHoldCount;
            this.openPendingCount = openPendingCount;
            this.availableAmount = availableAmount;
            this.lastTopupAmount = lastTopupAmount;
            this.balanceAsOfDate = balanceAsOfDate;
            this.decision = decision;
            this.declineReason = declineReason;
        }

        private String toLine() {
            return walletId
                    + "," + walletStatus
                    + "," + currencyCode
                    + "," + ledgerAmount
                    + ",ホールド件数=" + approvedHoldCount
                    + ",未確定件数=" + openPendingCount
                    + "," + availableAmount
                    + "," + yen(lastTopupAmount)
                    + "," + balanceAsOfDate
                    + "," + decision
                    + "," + declineReason;
        }
    }
}
