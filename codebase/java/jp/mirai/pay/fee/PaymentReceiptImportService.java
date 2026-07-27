package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2025/04/01  料金基盤T    初版作成
 * 1.01  2025/09/16  料金基盤T    入金日妥当性と請求候補抽出条件を追加
 * 1.02  2026/02/12  料金基盤T    金額不一致時の保留登録を追加
 */
public class PaymentReceiptImportService {

    private static final String 請求状態_未収 = "0";
    private static final String 請求状態_一部入金 = "1";
    private static final String 照合状態_照合待ち = "01";
    private static final String 照合状態_金額不一致保留 = "81";
    private static final String 照合状態_請求不明保留 = "82";
    private static final String 照合状態_依頼人不正保留 = "83";
    private static final java.math.BigDecimal 許容差額 = new java.math.BigDecimal("0");

    public static void main(String[] a) {
        java.util.List<BillingRecord> 請求 = java.util.Arrays.asList(
                new BillingRecord("B2026040001", "M10001", "202604", new java.math.BigDecimal("33000"), new java.math.BigDecimal("3000"), 請求状態_未収, java.time.LocalDate.of(2026, 5, 31)),
                new BillingRecord("B2026040002", "M10002", "202604", new java.math.BigDecimal("52800"), new java.math.BigDecimal("4800"), 請求状態_未収, java.time.LocalDate.of(2026, 5, 31)),
                new BillingRecord("B2026040003", "M10003", "202604", new java.math.BigDecimal("11000"), new java.math.BigDecimal("1000"), "9", java.time.LocalDate.of(2026, 5, 31))
        );

        java.util.List<BankReceiptRecord> 入金 = java.util.Arrays.asList(
                new BankReceiptRecord("BK260500001", "M10001", java.time.LocalDate.of(2026, 5, 20), new java.math.BigDecimal("33000")),
                new BankReceiptRecord("BK260500002", "M10002", java.time.LocalDate.of(2026, 5, 22), new java.math.BigDecimal("52000")),
                new BankReceiptRecord("BK260500003", "M99999", java.time.LocalDate.of(2026, 5, 23), new java.math.BigDecimal("10000"))
        );

        ImportResult 結果 = importReceipts(請求, 入金);
        for (PaymentRecord 明細 : 結果.paymentRecords) {
            System.out.println(明細.toSequentialText());
        }
        System.err.println("取込件数=" + 結果.readCount + " 登録件数=" + 結果.writeCount + " 完全一致=" + 結果.matchedCount + " 保留=" + 結果.pendingCount);
    }

    public static ImportResult importReceipts(java.util.List<BillingRecord> pfbilf, java.util.List<BankReceiptRecord> bankReceipts) {
        if (pfbilf == null) {
            throw new IllegalArgumentException("PFBILFが未指定です");
        }
        if (bankReceipts == null) {
            throw new IllegalArgumentException("銀行入金データが未指定です");
        }

        java.util.Map<String, java.util.List<BillingRecord>> 請求索引 = buildBillingIndex(pfbilf);
        java.util.List<PaymentRecord> 登録一覧 = new java.util.ArrayList<>();
        int 完全一致件数 = 0;
        int 保留件数 = 0;

        for (BankReceiptRecord 入金 : bankReceipts) {
            validateReceipt(入金);
            String 加盟店コード = normalizeMerchantCode(入金.remitterCode);
            String 照合状態;
            BillingRecord 候補 = null;

            if (加盟店コード.isEmpty()) {
                照合状態 = 照合状態_依頼人不正保留;
            } else {
                候補 = selectBillingCandidate(請求索引.get(加盟店コード), 入金.paymentDate, 入金.amount);
                if (候補 == null) {
                    照合状態 = 照合状態_請求不明保留;
                } else if (候補.feeTotalAmount.subtract(入金.amount).abs().compareTo(許容差額) == 0) {
                    照合状態 = 照合状態_照合待ち;
                    完全一致件数++;
                } else {
                    照合状態 = 照合状態_金額不一致保留;
                }
            }

            if (!照合状態_照合待ち.equals(照合状態)) {
                保留件数++;
            }

            String 支払番号 = createPaymentId(入金.bankRefNo, 登録一覧.size() + 1);
            登録一覧.add(new PaymentRecord(
                    支払番号,
                    候補 == null ? 加盟店コード : 候補.merchantCode,
                    入金.paymentDate,
                    入金.amount,
                    入金.bankRefNo,
                    照合状態
            ));
        }

        return new ImportResult(bankReceipts.size(), 登録一覧.size(), 完全一致件数, 保留件数, java.util.Collections.unmodifiableList(登録一覧));
    }

    private static java.util.Map<String, java.util.List<BillingRecord>> buildBillingIndex(java.util.List<BillingRecord> pfbilf) {
        java.util.Map<String, java.util.List<BillingRecord>> 索引 = new java.util.HashMap<>();
        for (BillingRecord 請求 : pfbilf) {
            validateBilling(請求);
            if (!請求状態_未収.equals(請求.status) && !請求状態_一部入金.equals(請求.status)) {
                continue;
            }
            索引.computeIfAbsent(請求.merchantCode, k -> new java.util.ArrayList<>()).add(請求);
        }
        for (java.util.List<BillingRecord> 候補 : 索引.values()) {
            候補.sort(java.util.Comparator
                    .comparing((BillingRecord b) -> b.dueDate)
                    .thenComparing(b -> b.billingMonth)
                    .thenComparing(b -> b.billId));
        }
        return 索引;
    }

    private static BillingRecord selectBillingCandidate(java.util.List<BillingRecord> candidates, java.time.LocalDate paymentDate, java.math.BigDecimal amount) {
        if (candidates == null || candidates.isEmpty()) {
            return null;
        }

        BillingRecord 金額一致 = null;
        BillingRecord 最古候補 = null;
        for (BillingRecord 候補 : candidates) {
            if (候補.dueDate.plusDays(45).isBefore(paymentDate)) {
                continue;
            }
            if (最古候補 == null) {
                最古候補 = 候補;
            }
            if (候補.feeTotalAmount.compareTo(amount) == 0) {
                金額一致 = 候補;
                break;
            }
        }
        return 金額一致 != null ? 金額一致 : 最古候補;
    }

    private static String normalizeMerchantCode(String remitterCode) {
        if (remitterCode == null) {
            return "";
        }
        String 値 = remitterCode.trim().toUpperCase(java.util.Locale.ROOT);
        if (値.startsWith("JP")) {
            値 = 値.substring(2);
        }
        if (!値.matches("M[0-9]{5}")) {
            return "";
        }
        return 値;
    }

    private static String createPaymentId(String bankRefNo, int sequence) {
        String 参照番号 = bankRefNo == null ? "" : bankRefNo.replaceAll("[^0-9A-Za-z]", "");
        if (参照番号.length() > 10) {
            参照番号 = 参照番号.substring(参照番号.length() - 10);
        }
        return String.format(java.util.Locale.ROOT, "P%06d%s", sequence, 参照番号);
    }

    private static void validateBilling(BillingRecord billing) {
        if (billing == null) {
            throw new IllegalArgumentException("PFBILFに空レコードがあります");
        }
        if (isBlank(billing.billId) || isBlank(billing.merchantCode) || isBlank(billing.billingMonth) || isBlank(billing.status)) {
            throw new IllegalArgumentException("PFBILFの必須項目が不足しています");
        }
        if (billing.feeTotalAmount == null || billing.taxAmount == null || billing.dueDate == null) {
            throw new IllegalArgumentException("PFBILFの金額または期日が不足しています: " + billing.billId);
        }
        if (billing.feeTotalAmount.signum() < 0 || billing.taxAmount.signum() < 0 || billing.taxAmount.compareTo(billing.feeTotalAmount) > 0) {
            throw new IllegalArgumentException("PFBILFの金額が不正です: " + billing.billId);
        }
        if (!billing.billingMonth.matches("[0-9]{6}")) {
            throw new IllegalArgumentException("PFBILFの請求月が不正です: " + billing.billId);
        }
    }

    private static void validateReceipt(BankReceiptRecord receipt) {
        if (receipt == null) {
            throw new IllegalArgumentException("銀行入金データに空レコードがあります");
        }
        if (isBlank(receipt.bankRefNo)) {
            throw new IllegalArgumentException("銀行照会番号が未設定です");
        }
        if (receipt.paymentDate == null) {
            throw new IllegalArgumentException("入金日が未設定です: " + receipt.bankRefNo);
        }
        if (receipt.amount == null || receipt.amount.signum() <= 0) {
            throw new IllegalArgumentException("入金額が不正です: " + receipt.bankRefNo);
        }
        if (receipt.paymentDate.isAfter(java.time.LocalDate.now().plusDays(1))) {
            throw new IllegalArgumentException("入金日が未来日です: " + receipt.bankRefNo);
        }
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    public static final class BillingRecord {
        public final String billId;
        public final String merchantCode;
        public final String billingMonth;
        public final java.math.BigDecimal feeTotalAmount;
        public final java.math.BigDecimal taxAmount;
        public final String status;
        public final java.time.LocalDate dueDate;

        public BillingRecord(String billId, String merchantCode, String billingMonth, java.math.BigDecimal feeTotalAmount, java.math.BigDecimal taxAmount, String status, java.time.LocalDate dueDate) {
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.billingMonth = billingMonth;
            this.feeTotalAmount = feeTotalAmount;
            this.taxAmount = taxAmount;
            this.status = status;
            this.dueDate = dueDate;
        }
    }

    public static final class BankReceiptRecord {
        public final String bankRefNo;
        public final String remitterCode;
        public final java.time.LocalDate paymentDate;
        public final java.math.BigDecimal amount;

        public BankReceiptRecord(String bankRefNo, String remitterCode, java.time.LocalDate paymentDate, java.math.BigDecimal amount) {
            this.bankRefNo = bankRefNo;
            this.remitterCode = remitterCode;
            this.paymentDate = paymentDate;
            this.amount = amount;
        }
    }

    public static final class PaymentRecord {
        public final String paymentId;
        public final String merchantCode;
        public final java.time.LocalDate paymentDate;
        public final java.math.BigDecimal paymentAmount;
        public final String bankRefNo;
        public final String matchStatus;

        public PaymentRecord(String paymentId, String merchantCode, java.time.LocalDate paymentDate, java.math.BigDecimal paymentAmount, String bankRefNo, String matchStatus) {
            this.paymentId = paymentId;
            this.merchantCode = merchantCode;
            this.paymentDate = paymentDate;
            this.paymentAmount = paymentAmount;
            this.bankRefNo = bankRefNo;
            this.matchStatus = matchStatus;
        }

        public String toSequentialText() {
            return paymentId + "," + merchantCode + "," + paymentDate + "," + paymentAmount.toPlainString() + "," + bankRefNo + "," + matchStatus;
        }
    }

    public static final class ImportResult {
        public final int readCount;
        public final int writeCount;
        public final int matchedCount;
        public final int pendingCount;
        public final java.util.List<PaymentRecord> paymentRecords;

        public ImportResult(int readCount, int writeCount, int matchedCount, int pendingCount, java.util.List<PaymentRecord> paymentRecords) {
            this.readCount = readCount;
            this.writeCount = writeCount;
            this.matchedCount = matchedCount;
            this.pendingCount = pendingCount;
            this.paymentRecords = paymentRecords;
        }
    }
}
