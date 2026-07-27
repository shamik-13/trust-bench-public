/**
 * 変更履歴
 * 版数  年月日      担当    概要
 * 1.00  2024/04/01  業務一課  初版作成。入金履歴照会、表示区分変換、照会ログ出力を追加。
 * 1.01  2024/09/17  業務一課  表示ラベルを「入金済」から「消込済み」へ変更。
 * 1.02  2025/02/03  業務二課  未消込入金の表示ラベルを「未充当」から「未消込」へ変更。
 * 1.03  2025/11/28  業務二課  返金予定の表示ラベルを追加。
 */
public class PaymentHistoryQueryService2 {
    private static final String PROGRAM_ID = "CDPAYHIS";

    private static final String TXN_KBN_DOMESTIC_SHOPPING = "P1";
    private static final String TXN_KBN_OVERSEAS_SHOPPING = "P2";
    private static final String TXN_KBN_DOMESTIC_CASHING = "C1";
    private static final String TXN_KBN_OVERSEAS_CASHING = "C2";
    private static final String TXN_KBN_ANNUAL_FEE = "A1";

    private static final String FEE_KBN_NONE = "00";
    private static final String FEE_KBN_OVERSEAS_ATM = "FA";
    private static final String FEE_KBN_BRAND_ADVANCE = "FB";

    private static final String SETL_KBN_FIXED = "D";
    private static final String SETL_KBN_HOLD = "H";

    private static final String DISP_KBN_SHOPPING = "S";
    private static final String DISP_KBN_CASHING = "K";

    private static final String ALLOC_KBN_ALLOCATED = "1";
    private static final String ALLOC_KBN_UNAPPLIED = "2";
    private static final String ALLOC_KBN_REFUND = "3";

    private static final String MEMBER_STOPPED = "9";

    private final java.util.List<Cdpymfc> payments;
    private final java.util.List<Cdovsfc> overviews;
    private final java.util.List<Cdmvwfc> displayRows = new java.util.ArrayList<>();
    private final java.util.List<Cdlogfc> logs = new java.util.ArrayList<>();

    public PaymentHistoryQueryService2(java.util.List<Cdpymfc> payments, java.util.List<Cdovsfc> overviews) {
        this.payments = payments;
        this.overviews = overviews;
    }

    public java.util.List<Cdmvwfc> displayRows() {
        return java.util.Collections.unmodifiableList(displayRows);
    }

    public java.util.List<Cdlogfc> logRows() {
        return java.util.Collections.unmodifiableList(logs);
    }

    public void inquire(String targetCardNo, String memberStatus) {
        java.util.Map<String, Cdovsfc> latestTxnByCard = latestSettledTxnByCard(overviews);

        payments.stream()
                .filter(r -> targetCardNo.equals(r.pyCardNo()))
                .sorted((l, r) -> Integer.compare(r.pyPayDt(), l.pyPayDt()))
                .forEach(payment -> {
                    validatePayment(payment);
                    Cdovsfc txn = latestTxnByCard.get(payment.pyCardNo());
                    if (txn == null) {
                        logs.add(new Cdlogfc(nextLogId(), PROGRAM_ID, payment.pyCardNo(), "E",
                                businessDate(), "対象取引なし"));
                        return;
                    }

                    String dispKbn = toDispKbn(txn);
                    long dispAmt = toDisplayAmount(payment, txn);
                    String dispLabel = toDisplayLabel(payment);
                    long memberAmt = MEMBER_STOPPED.equals(memberStatus) ? maskAmount(dispAmt) : dispAmt;

                    displayRows.add(new Cdmvwfc(payment.pyCardNo(), txn.ovTxnId(), dispKbn, memberAmt, dispLabel));
                    logs.add(new Cdlogfc(nextLogId(), PROGRAM_ID, payment.pyCardNo(), "Q",
                            businessDate(), dispLabel));
                });
    }

    private java.util.Map<String, Cdovsfc> latestSettledTxnByCard(java.util.List<Cdovsfc> records) {
        java.util.Map<String, Cdovsfc> result = new java.util.HashMap<>();
        for (Cdovsfc record : records) {
            validateOverview(record);
            if (!SETL_KBN_FIXED.equals(record.ovSetlKbn())) {
                continue;
            }
            Cdovsfc current = result.get(record.ovCardNo());
            if (current == null || record.ovIntStartDt() > current.ovIntStartDt()) {
                result.put(record.ovCardNo(), record);
            }
        }
        return result;
    }

    private static long toDisplayAmount(Cdpymfc payment, Cdovsfc txn) {
        long availableAmount = payment.pyPayAmt() - payment.pyUnappliedAmt();
        if (availableAmount < 0) {
            throw new IllegalArgumentException("入金額と未消込額の整合性不正: " + payment.pyPaymentId());
        }

        long feeAmount = feeIncluded(txn.ovFeeKbn()) ? txn.ovFeeAmt() : 0L;
        long settlementLimit = Math.max(0L, txn.ovSetlAmt() + feeAmount);
        if (ALLOC_KBN_UNAPPLIED.equals(payment.pyAllocKbn())) {
            return payment.pyUnappliedAmt();
        }
        if (ALLOC_KBN_REFUND.equals(payment.pyAllocKbn())) {
            return Math.min(payment.pyUnappliedAmt(), payment.pyPayAmt());
        }
        return Math.min(availableAmount, settlementLimit);
    }

    private static boolean feeIncluded(String feeKbn) {
        return FEE_KBN_OVERSEAS_ATM.equals(feeKbn) || FEE_KBN_BRAND_ADVANCE.equals(feeKbn);
    }

    private static String toDisplayLabel(Cdpymfc payment) {
        if (ALLOC_KBN_ALLOCATED.equals(payment.pyAllocKbn())) {
            return "消込済み";
        }
        if (ALLOC_KBN_UNAPPLIED.equals(payment.pyAllocKbn())) {
            return "未消込";
        }
        if (ALLOC_KBN_REFUND.equals(payment.pyAllocKbn())) {
            return "返金予定";
        }
        throw new IllegalArgumentException("消込区分不正: " + payment.pyAllocKbn());
    }

    private static String toDispKbn(Cdovsfc txn) {
        if (TXN_KBN_DOMESTIC_SHOPPING.equals(txn.ovTxnKbn())
                || TXN_KBN_OVERSEAS_SHOPPING.equals(txn.ovTxnKbn())
                || TXN_KBN_ANNUAL_FEE.equals(txn.ovTxnKbn())) {
            return DISP_KBN_SHOPPING;
        }
        if (TXN_KBN_DOMESTIC_CASHING.equals(txn.ovTxnKbn())
                || TXN_KBN_OVERSEAS_CASHING.equals(txn.ovTxnKbn())) {
            return DISP_KBN_CASHING;
        }
        throw new IllegalArgumentException("取引区分不正: " + txn.ovTxnKbn());
    }

    private static long maskAmount(long amount) {
        if (amount < 1000L) {
            return 0L;
        }
        return (amount / 1000L) * 1000L;
    }

    private static void validatePayment(Cdpymfc r) {
        require(r.pyPaymentId(), "入金番号");
        require(r.pyCardNo(), "カード番号");
        if (r.pyPayDt() <= 0) {
            throw new IllegalArgumentException("入金日未設定");
        }
        if (r.pyPayAmt() <= 0L) {
            throw new IllegalArgumentException("入金額不正: " + r.pyPaymentId());
        }
        if (r.pyUnappliedAmt() < 0L) {
            throw new IllegalArgumentException("未消込額不正: " + r.pyPaymentId());
        }
        if (!ALLOC_KBN_ALLOCATED.equals(r.pyAllocKbn())
                && !ALLOC_KBN_UNAPPLIED.equals(r.pyAllocKbn())
                && !ALLOC_KBN_REFUND.equals(r.pyAllocKbn())) {
            throw new IllegalArgumentException("消込区分不正: " + r.pyAllocKbn());
        }
    }

    private static void validateOverview(Cdovsfc r) {
        require(r.ovTxnId(), "取引番号");
        require(r.ovCardNo(), "カード番号");
        require(r.ovTxnKbn(), "取引区分");
        require(r.ovFeeKbn(), "手数料区分");
        require(r.ovSetlKbn(), "確定区分");
        require(r.ovProgramId(), "プログラム番号");

        if (!FEE_KBN_NONE.equals(r.ovFeeKbn())
                && !FEE_KBN_OVERSEAS_ATM.equals(r.ovFeeKbn())
                && !FEE_KBN_BRAND_ADVANCE.equals(r.ovFeeKbn())) {
            throw new IllegalArgumentException("手数料区分不正: " + r.ovFeeKbn());
        }
        if (!SETL_KBN_FIXED.equals(r.ovSetlKbn()) && !SETL_KBN_HOLD.equals(r.ovSetlKbn())) {
            throw new IllegalArgumentException("確定区分不正: " + r.ovSetlKbn());
        }
        if (r.ovFeeAmt() < 0L || r.ovSetlAmt() < 0L) {
            throw new IllegalArgumentException("金額不正: " + r.ovTxnId());
        }
    }

    private static void require(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "未設定");
        }
    }

    private String nextLogId() {
        return String.format("L%08d", logs.size() + 1);
    }

    private static int businessDate() {
        return Integer.parseInt(java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE));
    }
}
