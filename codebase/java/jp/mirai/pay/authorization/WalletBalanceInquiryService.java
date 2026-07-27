package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.0     2024-09-26  みらいペイ システム部    ウォレット残高照会サービス初版作成
 *
 * ウォレットの残高照会画面向けに、台帳残高とオーソリ判定 (PYARSPF) で確定済みの
 * 利用可能残高 (AVAIL-AMT) を転記して表示する。利用可能残高はここで再計算せず、
 * ホールド・未確定決済は参考表示の明細としてのみ扱う。
 */
public class WalletBalanceInquiryService {

    private static final String WALLET_STATUS_ACTIVE = "01";
    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] a) {
        java.util.List<PywalfRecord> wallets = java.util.Arrays.asList(
                new PywalfRecord("W000000001", "U000001", "01", "STD", "ヤマダ タロウ"),
                new PywalfRecord("W000000002", "U000002", "09", "VIP", "サトウ ハナコ")
        );
        java.util.List<PybalfRecord> balances = java.util.Arrays.asList(
                new PybalfRecord("W000000001", bd("125000"), bd("30000"), "20260628"),
                new PybalfRecord("W000000002", bd("520000"), bd("100000"), "20260628")
        );
        java.util.List<PyholdfRecord> holds = java.util.Arrays.asList(
                new PyholdfRecord("H000001", "W000000001", bd("8500"), "00", "M10001", "JPY", "20260705"),
                new PyholdfRecord("H000002", "W000000001", bd("1200"), "20", "M10002", "JPY", "20260629"),
                new PyholdfRecord("H000003", "W000000002", bd("75000"), "00", "M20001", "JPY", "20260701")
        );
        java.util.List<PypendfRecord> pendings = java.util.Arrays.asList(
                new PypendfRecord("P000001", "W000000001", bd("4200"), "10", "20260628"),
                new PypendfRecord("P000002", "W000000001", bd("1800"), "30", "20260627")
        );
        // オーソリ判定 (AuthEngine) が確定させた利用可能残高 (AVAIL-AMT)。
        java.util.List<PyarspfRecord> authResults = java.util.Arrays.asList(
                new PyarspfRecord("R000001", "W000000001", "A", bd("112300"), bd("5000"), ""),
                new PyarspfRecord("R000002", "W000000002", "D", bd("445000"), bd("9000"), "STS")
        );

        DisplayModel model = inquire("W000000001", "20260628", wallets, balances, holds, pendings, authResults);
        System.out.println(model.toDisplayLine());
    }

    /**
     * 表示用モデルを組み立てる。利用可能残高はオーソリ判定の確定値を転記し、
     * ホールド・未確定決済は参考明細として付すのみで残高計算には用いない。
     */
    public static DisplayModel inquire(
            String walletId,
            String inquiryDate,
            java.util.List<PywalfRecord> wallets,
            java.util.List<PybalfRecord> balances,
            java.util.List<PyholdfRecord> holds,
            java.util.List<PypendfRecord> pendings,
            java.util.List<PyarspfRecord> authResults) {

        requireText(walletId, "ウォレットID未指定");
        requireText(inquiryDate, "照会日未指定");

        PywalfRecord wallet = findWallet(walletId, wallets);
        PybalfRecord balance = findBalance(walletId, balances);

        java.util.List<HoldDetail> holdDetails = new java.util.ArrayList<HoldDetail>();
        for (PyholdfRecord hold : nullSafe(holds)) {
            if (!walletId.equals(hold.walletId)) {
                continue;
            }
            if (BASE_CURRENCY.equals(hold.currencyCode)) {
                holdDetails.add(new HoldDetail(
                        hold.holdId,
                        hold.holdAmount,
                        hold.holdResult,
                        hold.merchantCode,
                        hold.currencyCode,
                        hold.holdExpireDate,
                        isExpired(hold.holdExpireDate, inquiryDate)));
            }
        }

        int pendingCount = 0;
        for (PypendfRecord pending : nullSafe(pendings)) {
            if (walletId.equals(pending.walletId)) {
                pendingCount++;
            }
        }

        java.math.BigDecimal ledgerBalance = nonNullAmount(balance.ledgerBalanceAmount, "台帳残高不正");
        PyarspfRecord result = latestResult(walletId, authResults);
        java.math.BigDecimal availableBalance = result != null
                ? nonNullAmount(result.availAmount, "利用可能残高不正")
                : ledgerBalance;
        String decision = result != null ? result.decisionKbn : "";

        return new DisplayModel(
                wallet.walletId,
                wallet.userId,
                wallet.walletStatus,
                wallet.walletTier,
                wallet.userNameKana,
                balance.balanceAsOfDate,
                ledgerBalance,
                holdDetails.size(),
                pendingCount,
                availableBalance,
                decision,
                WALLET_STATUS_ACTIVE.equals(wallet.walletStatus),
                holdDetails);
    }

    private static PywalfRecord findWallet(String walletId, java.util.List<PywalfRecord> wallets) {
        for (PywalfRecord wallet : nullSafe(wallets)) {
            if (walletId.equals(wallet.walletId)) {
                return wallet;
            }
        }
        throw new IllegalArgumentException("ウォレット未登録: " + walletId);
    }

    private static PybalfRecord findBalance(String walletId, java.util.List<PybalfRecord> balances) {
        for (PybalfRecord balance : nullSafe(balances)) {
            if (walletId.equals(balance.walletId)) {
                return balance;
            }
        }
        throw new IllegalArgumentException("残高未登録: " + walletId);
    }

    private static PyarspfRecord latestResult(String walletId, java.util.List<PyarspfRecord> authResults) {
        PyarspfRecord latest = null;
        for (PyarspfRecord result : nullSafe(authResults)) {
            if (walletId.equals(result.walletId)) {
                latest = result;
            }
        }
        return latest;
    }

    private static boolean isExpired(String yyyymmdd, String inquiryDate) {
        return yyyymmdd != null && yyyymmdd.compareTo(inquiryDate) < 0;
    }

    private static void requireText(String value, String message) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(message);
        }
    }

    private static java.math.BigDecimal nonNullAmount(java.math.BigDecimal amount, String message) {
        if (amount == null) {
            throw new IllegalArgumentException(message);
        }
        return amount;
    }

    private static java.math.BigDecimal bd(String value) {
        return new java.math.BigDecimal(value);
    }

    private static <T> java.util.List<T> nullSafe(java.util.List<T> list) {
        return list == null ? java.util.Collections.<T>emptyList() : list;
    }

    public static final class PywalfRecord {
        public final String walletId;
        public final String userId;
        public final String walletStatus;
        public final String walletTier;
        public final String userNameKana;

        public PywalfRecord(String walletId, String userId, String walletStatus, String walletTier, String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    public static final class PybalfRecord {
        public final String walletId;
        public final java.math.BigDecimal ledgerBalanceAmount;
        public final java.math.BigDecimal lastTopupAmount;
        public final String balanceAsOfDate;

        public PybalfRecord(String walletId, java.math.BigDecimal ledgerBalanceAmount,
                            java.math.BigDecimal lastTopupAmount, String balanceAsOfDate) {
            this.walletId = walletId;
            this.ledgerBalanceAmount = ledgerBalanceAmount;
            this.lastTopupAmount = lastTopupAmount;
            this.balanceAsOfDate = balanceAsOfDate;
        }
    }

    public static final class PyholdfRecord {
        public final String holdId;
        public final String walletId;
        public final java.math.BigDecimal holdAmount;
        public final String holdResult;
        public final String merchantCode;
        public final String currencyCode;
        public final String holdExpireDate;

        public PyholdfRecord(String holdId, String walletId, java.math.BigDecimal holdAmount, String holdResult,
                             String merchantCode, String currencyCode, String holdExpireDate) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.holdAmount = holdAmount;
            this.holdResult = holdResult;
            this.merchantCode = merchantCode;
            this.currencyCode = currencyCode;
            this.holdExpireDate = holdExpireDate;
        }
    }

    public static final class PypendfRecord {
        public final String pendId;
        public final String walletId;
        public final java.math.BigDecimal pendingAmount;
        public final String pendingStatus;
        public final String captureDate;

        public PypendfRecord(String pendId, String walletId, java.math.BigDecimal pendingAmount,
                             String pendingStatus, String captureDate) {
            this.pendId = pendId;
            this.walletId = walletId;
            this.pendingAmount = pendingAmount;
            this.pendingStatus = pendingStatus;
            this.captureDate = captureDate;
        }
    }

    public static final class PyarspfRecord {
        public final String reqId;
        public final String walletId;
        public final String decisionKbn;
        public final java.math.BigDecimal availAmount;
        public final java.math.BigDecimal reqAmount;
        public final String declineReason;

        public PyarspfRecord(String reqId, String walletId, String decisionKbn, java.math.BigDecimal availAmount,
                             java.math.BigDecimal reqAmount, String declineReason) {
            this.reqId = reqId;
            this.walletId = walletId;
            this.decisionKbn = decisionKbn;
            this.availAmount = availAmount;
            this.reqAmount = reqAmount;
            this.declineReason = declineReason;
        }
    }

    public static final class HoldDetail {
        public final String holdId;
        public final java.math.BigDecimal holdAmount;
        public final String holdResult;
        public final String merchantCode;
        public final String currencyCode;
        public final String holdExpireDate;
        public final boolean expiredNotice;

        public HoldDetail(String holdId, java.math.BigDecimal holdAmount, String holdResult, String merchantCode,
                          String currencyCode, String holdExpireDate, boolean expiredNotice) {
            this.holdId = holdId;
            this.holdAmount = holdAmount;
            this.holdResult = holdResult;
            this.merchantCode = merchantCode;
            this.currencyCode = currencyCode;
            this.holdExpireDate = holdExpireDate;
            this.expiredNotice = expiredNotice;
        }
    }

    public static final class DisplayModel {
        public final String walletId;
        public final String userId;
        public final String walletStatus;
        public final String walletTier;
        public final String userNameKana;
        public final String balanceAsOfDate;
        public final java.math.BigDecimal ledgerBalanceAmount;
        public final int activeHoldCount;
        public final int pendingCount;
        public final java.math.BigDecimal availableBalanceAmount;
        public final String decisionKbn;
        public final boolean activeWallet;
        public final java.util.List<HoldDetail> holdDetails;

        public DisplayModel(String walletId, String userId, String walletStatus, String walletTier, String userNameKana,
                            String balanceAsOfDate, java.math.BigDecimal ledgerBalanceAmount,
                            int activeHoldCount, int pendingCount, java.math.BigDecimal availableBalanceAmount,
                            String decisionKbn, boolean activeWallet, java.util.List<HoldDetail> holdDetails) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
            this.balanceAsOfDate = balanceAsOfDate;
            this.ledgerBalanceAmount = ledgerBalanceAmount;
            this.activeHoldCount = activeHoldCount;
            this.pendingCount = pendingCount;
            this.availableBalanceAmount = availableBalanceAmount;
            this.decisionKbn = decisionKbn;
            this.activeWallet = activeWallet;
            this.holdDetails = java.util.Collections.unmodifiableList(new java.util.ArrayList<HoldDetail>(holdDetails));
        }

        public String toDisplayLine() {
            return "ウォレットID=" + walletId
                    + ", 状態=" + walletStatus
                    + ", 台帳残高=" + ledgerBalanceAmount
                    + ", ホールド件数=" + activeHoldCount
                    + ", 未確定件数=" + pendingCount
                    + ", 利用可能残高=" + availableBalanceAmount
                    + ", 直近判定=" + decisionKbn;
        }
    }
}
