package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/12/03  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class InvoiceGenerationService {
    private static final String STATUS_ACTIVE = "01";
    private static final String STATUS_CONFIRMED = "確定";
    private static final String STATUS_ISSUED = "発行済";
    private static final String STATUS_REISSUED = "再発行";
    private static final int TAX_RATE_PERCENT = 10;

    private static final SummaryRecord[] PFSUMF = {
            new SummaryRecord("SUM-202605-0001", "M000001", "202605", 1240, 186_430_000L, 5_592_900L, 180_837_100L),
            new SummaryRecord("SUM-202605-0002", "M000002", "202605", 452, 42_910_000L, 1_287_300L, 41_622_700L),
            new SummaryRecord("SUM-202605-0003", "M000003", "202605", 88, 9_120_000L, 91_200L, 9_028_800L),
            new SummaryRecord("SUM-202605-0004", "M000004", "202605", 2310, 311_004_000L, 12_440_160L, 298_563_840L),
            new SummaryRecord("SUM-202605-0005", "M000005", "202605", 37, 2_590_000L, 155_400L, 2_434_600L),
            new SummaryRecord("SUM-202606-0001", "M000001", "202606", 1318, 197_608_000L, 5_928_240L, 191_679_760L),
            new SummaryRecord("SUM-202606-0002", "M000004", "202606", 2384, 327_600_000L, 13_104_000L, 314_496_000L)
    };

    private static final MerchantRecord[] PFMERF = {
            new MerchantRecord("M000001", "青葉物産株式会社", "C1", "01"),
            new MerchantRecord("M000002", "みらい食堂株式会社", "C2", "01"),
            new MerchantRecord("M000003", "東都水道料金センター", "C3", "01"),
            new MerchantRecord("M000004", "桜オンラインマーケット", "C4", "01"),
            new MerchantRecord("M000005", "湾岸アミューズメント", "C5", "02")
    };

    private static final java.util.List<BillRecord> PFBILF = new java.util.ArrayList<>();

    static {
        PFBILF.add(new BillRecord("BIL-202605-M000002", "M000002", "202605", 1_287_300L, 128_730L, STATUS_ISSUED, "20260630"));
        PFBILF.add(new BillRecord("BIL-202604-M000001", "M000001", "202604", 5_402_100L, 540_210L, STATUS_CONFIRMED, "20260531"));
    }

    public static void main(String[] a) {
        String billingMonth = a.length == 0 ? "202605" : a[0];
        Result result = createInvoices(billingMonth);
        System.out.println(result.message);
        for (BillRecord bill : result.bills) {
            System.out.println(bill.toLine());
        }
    }

    private static Result createInvoices(String billingMonth) {
        validateBillingMonth(billingMonth);

        // 確定済み請求が同一月に存在する場合は月次再生成を拒否する。
        for (BillRecord bill : PFBILF) {
            if (bill.billingMonth.equals(billingMonth) && STATUS_CONFIRMED.equals(bill.status)) {
                throw new IllegalStateException("確定済み請求あり 月=" + billingMonth);
            }
        }

        java.util.Map<String, MerchantRecord> merchants = new java.util.HashMap<>();
        for (MerchantRecord merchant : PFMERF) {
            merchants.put(merchant.merchantCode, merchant);
        }

        java.util.Map<String, FeeAggregate> aggregates = new java.util.TreeMap<>();
        for (SummaryRecord summary : PFSUMF) {
            if (!billingMonth.equals(summary.settleMonth)) {
                continue;
            }
            validateSummary(summary);
            MerchantRecord merchant = merchants.get(summary.merchantCode);
            if (merchant == null) {
                throw new IllegalStateException("加盟店未登録 SUMMARY-ID=" + summary.summaryId);
            }
            if (!STATUS_ACTIVE.equals(merchant.status)) {
                continue;
            }
            FeeAggregate aggregate = aggregates.computeIfAbsent(summary.merchantCode, FeeAggregate::new);
            aggregate.add(summary);
        }

        java.util.List<BillRecord> created = new java.util.ArrayList<>();
        for (FeeAggregate aggregate : aggregates.values()) {
            BillRecord existing = findBill(aggregate.merchantCode, billingMonth);
            long taxAmount = calculateTax(aggregate.feeTotalAmount);
            String dueDate = calculateDueDate(billingMonth);

            if (existing != null) {
                existing.feeTotalAmount = aggregate.feeTotalAmount;
                existing.taxAmount = taxAmount;
                existing.status = STATUS_REISSUED;
                existing.dueDate = dueDate;
                created.add(existing);
            } else {
                BillRecord bill = new BillRecord(
                        createBillId(billingMonth, aggregate.merchantCode),
                        aggregate.merchantCode,
                        billingMonth,
                        aggregate.feeTotalAmount,
                        taxAmount,
                        STATUS_ISSUED,
                        dueDate);
                PFBILF.add(bill);
                created.add(bill);
            }
        }

        return new Result("請求書生成完了 月=" + billingMonth + " 件数=" + created.size(), created);
    }

    private static void validateBillingMonth(String billingMonth) {
        if (billingMonth == null || !billingMonth.matches("[0-9]{6}")) {
            throw new IllegalArgumentException("請求月不正 値=" + billingMonth);
        }
        int month = Integer.parseInt(billingMonth.substring(4, 6));
        if (month < 1 || month > 12) {
            throw new IllegalArgumentException("請求月不正 値=" + billingMonth);
        }
    }

    private static void validateSummary(SummaryRecord summary) {
        if (summary.txnCount < 0 || summary.txnTotalAmount < 0 || summary.feeTotalAmount < 0) {
            throw new IllegalStateException("月次精算サマリ金額不正 SUMMARY-ID=" + summary.summaryId);
        }
        long expectedNet = summary.txnTotalAmount - summary.feeTotalAmount;
        if (expectedNet != summary.netSettleAmount) {
            throw new IllegalStateException("月次精算サマリ差引不一致 SUMMARY-ID=" + summary.summaryId);
        }
    }

    private static BillRecord findBill(String merchantCode, String billingMonth) {
        for (BillRecord bill : PFBILF) {
            if (bill.merchantCode.equals(merchantCode) && bill.billingMonth.equals(billingMonth)) {
                return bill;
            }
        }
        return null;
    }

    private static long calculateTax(long feeTotalAmount) {
        return feeTotalAmount * TAX_RATE_PERCENT / 100;
    }

    private static String calculateDueDate(String billingMonth) {
        java.time.YearMonth month = java.time.YearMonth.of(
                Integer.parseInt(billingMonth.substring(0, 4)),
                Integer.parseInt(billingMonth.substring(4, 6)));
        java.time.LocalDate dueDate = month.plusMonths(1).atEndOfMonth();
        return dueDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
    }

    private static String createBillId(String billingMonth, String merchantCode) {
        return "BIL-" + billingMonth + "-" + merchantCode;
    }

    private static final class SummaryRecord {
        private final String summaryId;
        private final String merchantCode;
        private final String settleMonth;
        private final int txnCount;
        private final long txnTotalAmount;
        private final long feeTotalAmount;
        private final long netSettleAmount;

        private SummaryRecord(String summaryId, String merchantCode, String settleMonth, int txnCount,
                              long txnTotalAmount, long feeTotalAmount, long netSettleAmount) {
            this.summaryId = summaryId;
            this.merchantCode = merchantCode;
            this.settleMonth = settleMonth;
            this.txnCount = txnCount;
            this.txnTotalAmount = txnTotalAmount;
            this.feeTotalAmount = feeTotalAmount;
            this.netSettleAmount = netSettleAmount;
        }
    }

    private static final class MerchantRecord {
        private final String merchantCode;
        private final String merchantName;
        private final String category;
        private final String status;

        private MerchantRecord(String merchantCode, String merchantName, String category, String status) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.category = category;
            this.status = status;
        }
    }

    private static final class BillRecord {
        private final String billId;
        private final String merchantCode;
        private final String billingMonth;
        private long feeTotalAmount;
        private long taxAmount;
        private String status;
        private String dueDate;

        private BillRecord(String billId, String merchantCode, String billingMonth, long feeTotalAmount,
                           long taxAmount, String status, String dueDate) {
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.billingMonth = billingMonth;
            this.feeTotalAmount = feeTotalAmount;
            this.taxAmount = taxAmount;
            this.status = status;
            this.dueDate = dueDate;
        }

        private String toLine() {
            return billId + "," + merchantCode + "," + billingMonth + "," + feeTotalAmount + ","
                    + taxAmount + "," + status + "," + dueDate;
        }
    }

    private static final class FeeAggregate {
        private final String merchantCode;
        private long feeTotalAmount;

        private FeeAggregate(String merchantCode) {
            this.merchantCode = merchantCode;
        }

        private void add(SummaryRecord summary) {
            feeTotalAmount += summary.feeTotalAmount;
        }
    }

    private static final class Result {
        private final String message;
        private final java.util.List<BillRecord> bills;

        private Result(String message, java.util.List<BillRecord> bills) {
            this.message = message;
            this.bills = bills;
        }
    }
}
