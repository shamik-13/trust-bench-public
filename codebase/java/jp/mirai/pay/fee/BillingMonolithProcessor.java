package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-05-13  みらいペイ システム部 加盟店・手数料チーム  加盟店請求一括処理の初版作成
 */
public class BillingMonolithProcessor {
    private static final String STATUS_ACTIVE = "01";
    private static final String REPORT_TYPE_DETAIL = "MEISAI";
    private static final String REPORT_TYPE_SUMMARY = "SEISAN";
    private static final String BILL_STATUS_CREATED = "01";
    private static final long TAX_RATE_BP = 1000L;

    public static void main(String[] args) {
        java.time.LocalDate businessDate = args.length > 0
                ? java.time.LocalDate.parse(args[0])
                : java.time.LocalDate.now(java.time.ZoneId.of("Asia/Tokyo"));
        java.time.YearMonth billingMonth = args.length > 1
                ? java.time.YearMonth.parse(args[1])
                : java.time.YearMonth.from(businessDate.minusMonths(1));

        new Batch(createFeeModel(), businessDate, billingMonth).execute();
    }

    private static FeeModel createFeeModel() {
        try {
            return FeeModel.class.getDeclaredConstructor().newInstance();
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("FeeModelを生成できません", e);
        }
    }

    private static final class Batch {
        private final FeeModel feeModel;
        private final java.time.LocalDate businessDate;
        private final java.time.YearMonth billingMonth;
        private final java.util.Map<String, MerchantRecord> pfmerf = new java.util.LinkedHashMap<>();
        private final java.util.List<TxnRecord> pftxnf = new java.util.ArrayList<>();
        private final java.util.List<FeeRecord> pffeef = new java.util.ArrayList<>();
        private final java.util.Map<String, SummaryRecord> pfsumf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, BillRecord> pfbilf = new java.util.LinkedHashMap<>();
        private final java.util.List<PaymentRecord> pfpayf = new java.util.ArrayList<>();
        private final java.util.Map<String, CategoryRecord> pmcatf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, RatePlanRecord> pmratf = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, InvoiceRecord> pfinvf = new java.util.LinkedHashMap<>();
        private final java.util.List<ReportRecord> prrptf = new java.util.ArrayList<>();

        Batch(FeeModel feeModel, java.time.LocalDate businessDate, java.time.YearMonth billingMonth) {
            this.feeModel = java.util.Objects.requireNonNull(feeModel, "feeModel");
            this.businessDate = java.util.Objects.requireNonNull(businessDate, "businessDate");
            this.billingMonth = java.util.Objects.requireNonNull(billingMonth, "billingMonth");
            loadSyntheticFiles();
        }

        void execute() {
            validateMasters();

            java.util.Map<String, java.util.List<TxnRecord>> targetTransactions = selectUnratedTransactions();
            for (MerchantRecord merchant : pfmerf.values()) {
                if (!STATUS_ACTIVE.equals(merchant.merchantStatus)) {
                    continue;
                }

                RestartPoint restartPoint = restoreRestartPoint(merchant.merchantCode);
                if (restartPoint.completed) {
                    continue;
                }

                java.util.List<TxnRecord> transactions = targetTransactions.getOrDefault(
                        merchant.merchantCode, java.util.Collections.emptyList());
                if (transactions.isEmpty() && restartPoint.hasBill) {
                    continue;
                }

                MerchantResult result = calculateFees(merchant, transactions);
                updateSummary(merchant, result);
                BillRecord bill = updateBill(merchant, result);
                updateInvoice(merchant, bill, result);
                writeReports(merchant, bill);
            }

            validateTotals();
            printOperatorResult();
        }

        private void loadSyntheticFiles() {
            pfmerf.put("M0001001", new MerchantRecord("M0001001", "東京日用品ストア", "C1", "01"));
            pfmerf.put("M0001002", new MerchantRecord("M0001002", "大阪食堂", "C2", "01"));
            pfmerf.put("M0001003", new MerchantRecord("M0001003", "横浜水道料金", "C3", "01"));
            pfmerf.put("M0001004", new MerchantRecord("M0001004", "札幌通信販売", "C4", "01"));
            pfmerf.put("M0001005", new MerchantRecord("M0001005", "福岡予約代行", "C5", "02"));

            pftxnf.add(new TxnRecord("T2026050001", "M0001001", 128000L, java.time.LocalDate.of(2026, 5, 2)));
            pftxnf.add(new TxnRecord("T2026050002", "M0001001", 42000L, java.time.LocalDate.of(2026, 5, 19)));
            pftxnf.add(new TxnRecord("T2026050003", "M0001002", 86000L, java.time.LocalDate.of(2026, 5, 8)));
            pftxnf.add(new TxnRecord("T2026050004", "M0001003", 230000L, java.time.LocalDate.of(2026, 5, 12)));
            pftxnf.add(new TxnRecord("T2026050005", "M0001004", 97000L, java.time.LocalDate.of(2026, 5, 21)));
            pftxnf.add(new TxnRecord("T2026060001", "M0001001", 50000L, java.time.LocalDate.of(2026, 6, 1)));

            pffeef.add(new FeeRecord("F2026050000", "M0001002", "T2026049999", 70000L, "規程:C2:MdrFeeEngine", 1820L, "既存"));

            pfsumf.put("S-M0001002-2026-04", new SummaryRecord("S-M0001002-2026-04", "M0001002",
                    "2026-04", 31L, 2100000L, 54600L, 2045400L));

            pfbilf.put("B-M0001002-2026-04", new BillRecord("B-M0001002-2026-04", "M0001002",
                    "2026-04", 54600L, 5460L, "04", java.time.LocalDate.of(2026, 5, 31)));

            pfpayf.add(new PaymentRecord("P2026053101", "M0001002", java.time.LocalDate.of(2026, 5, 31),
                    59960L, "BK2605310001", "01"));

            pmcatf.put("C1", new CategoryRecord("C1", "一般物販", "B", true, true, java.time.LocalDate.of(2026, 4, 1)));
            pmcatf.put("C2", new CategoryRecord("C2", "飲食", "B", true, true, java.time.LocalDate.of(2026, 4, 1)));
            pmcatf.put("C3", new CategoryRecord("C3", "公共・公金", "A", false, true, java.time.LocalDate.of(2026, 4, 1)));
            pmcatf.put("C4", new CategoryRecord("C4", "EC・通信販売", "C", true, true, java.time.LocalDate.of(2026, 4, 1)));
            pmcatf.put("C5", new CategoryRecord("C5", "高リスク業種", "D", true, true, java.time.LocalDate.of(2026, 4, 1)));

            pmratf.put("R-C1-20260401", new RatePlanRecord("R-C1-20260401", "C1",
                    java.time.LocalDate.of(2026, 4, 1), "N26040101", "01", "RH-C1-260401"));
            pmratf.put("R-C2-20260401", new RatePlanRecord("R-C2-20260401", "C2",
                    java.time.LocalDate.of(2026, 4, 1), "N26040102", "01", "RH-C2-260401"));
            pmratf.put("R-C3-20260401", new RatePlanRecord("R-C3-20260401", "C3",
                    java.time.LocalDate.of(2026, 4, 1), "N26040103", "01", "RH-C3-260401"));
            pmratf.put("R-C4-20260401", new RatePlanRecord("R-C4-20260401", "C4",
                    java.time.LocalDate.of(2026, 4, 1), "N26040104", "01", "RH-C4-260401"));
            pmratf.put("R-C5-20260401", new RatePlanRecord("R-C5-20260401", "C5",
                    java.time.LocalDate.of(2026, 4, 1), "N26040105", "01", "RH-C5-260401"));
        }

        private void validateMasters() {
            java.util.Objects.requireNonNull(feeModel, "feeModel");

            for (MerchantRecord merchant : pfmerf.values()) {
                CategoryRecord category = pmcatf.get(merchant.merchantCategory);
                if (category == null || !category.activeFlag) {
                    throw new IllegalStateException("業種マスタ不正：" + merchant.merchantCode);
                }
                if (!hasApprovedRatePlan(merchant.merchantCategory)) {
                    throw new IllegalStateException("料率規程未承認：" + merchant.merchantCategory);
                }
                if (!"01".equals(merchant.merchantStatus)
                        && !"02".equals(merchant.merchantStatus)
                        && !"09".equals(merchant.merchantStatus)) {
                    throw new IllegalStateException("加盟店状態不正：" + merchant.merchantCode);
                }
            }
        }

        private boolean hasApprovedRatePlan(String category) {
            for (RatePlanRecord ratePlan : pmratf.values()) {
                if (ratePlan.categoryCode.equals(category)
                        && "01".equals(ratePlan.approvalStatus)
                        && !ratePlan.effectiveDate.isAfter(billingMonth.atEndOfMonth())) {
                    return true;
                }
            }
            return false;
        }

        private java.util.Map<String, java.util.List<TxnRecord>> selectUnratedTransactions() {
            java.util.Set<String> ratedTxnIds = new java.util.HashSet<>();
            for (FeeRecord fee : pffeef) {
                ratedTxnIds.add(fee.txnId);
            }

            java.util.Map<String, java.util.List<TxnRecord>> selected = new java.util.LinkedHashMap<>();
            for (TxnRecord txn : pftxnf) {
                if (!java.time.YearMonth.from(txn.txnDate).equals(billingMonth)) {
                    continue;
                }

                MerchantRecord merchant = pfmerf.get(txn.merchantCode);
                if (merchant == null) {
                    throw new IllegalStateException("取引加盟店未登録：" + txn.txnId);
                }
                if (!STATUS_ACTIVE.equals(merchant.merchantStatus) || ratedTxnIds.contains(txn.txnId)) {
                    continue;
                }

                selected.computeIfAbsent(txn.merchantCode, k -> new java.util.ArrayList<>()).add(txn);
            }
            return selected;
        }

        private RestartPoint restoreRestartPoint(String merchantCode) {
            boolean hasDoneReport = false;
            for (ReportRecord report : prrptf) {
                if (report.merchantCode.equals(merchantCode)
                        && report.businessDate.equals(businessDate)
                        && "04".equals(report.status)) {
                    hasDoneReport = true;
                }
            }

            boolean hasBill = pfbilf.containsKey(billId(merchantCode));
            return new RestartPoint(hasBill, hasDoneReport && hasBill);
        }

        private MerchantResult calculateFees(MerchantRecord merchant, java.util.List<TxnRecord> transactions) {
            long txnTotal = 0L;
            long feeTotal = 0L;
            String auditKey = auditKey(merchant.merchantCategory);

            // MDR料率は規程に基づき MdrFeeEngine が業種区分から適用する。当バッチは料率値を保持せず、
            // 算定結果(手数料額)のみを受け取り PFFEEF(FE-FEE-AMT)へ反映する。
            MdrFeeEngine engine = new MdrFeeEngine();

            for (TxnRecord txn : transactions) {
                if (txn.txnAmount <= 0L) {
                    throw new IllegalStateException("取引金額不正：" + txn.txnId);
                }

                FeeModel.Txn modelTxn = new FeeModel.Txn(txn.txnId, txn.merchantCode, txn.txnAmount,
                        toCompactDate(txn.txnDate));
                long feeAmount = engine.feeFor(modelTxn, merchant.merchantCategory);
                if (feeAmount < 0L || feeAmount > txn.txnAmount) {
                    throw new IllegalStateException("手数料金額不正：" + txn.txnId);
                }

                // FE-MDR-RATE は料率値を当バッチで決定せず、規程・MdrFeeEngine 由来である旨の参照のみ保持する。
                pffeef.add(new FeeRecord(feeId(txn.txnId), merchant.merchantCode, txn.txnId,
                        txn.txnAmount, mdrRateRef(merchant.merchantCategory), feeAmount, auditKey));
                txnTotal += txn.txnAmount;
                feeTotal += feeAmount;
            }

            return new MerchantResult(transactions.size(), txnTotal, feeTotal, auditKey);
        }

        private int toCompactDate(java.time.LocalDate date) {
            return date.getYear() * 10000 + date.getMonthValue() * 100 + date.getDayOfMonth();
        }

        private String mdrRateRef(String category) {
            // 料率の数値は保持しない。業種区分と料率出所(規程/MdrFeeEngine)のみを記録する。
            return "規程:" + category + ":MdrFeeEngine";
        }

        private void updateSummary(MerchantRecord merchant, MerchantResult result) {
            String id = summaryId(merchant.merchantCode);
            SummaryRecord current = pfsumf.get(id);
            long count = result.txnCount + (current == null ? 0L : current.txnCount);
            long txnTotal = result.txnTotalAmount + (current == null ? 0L : current.txnTotalAmount);
            long feeTotal = result.feeTotalAmount + (current == null ? 0L : current.feeTotalAmount);

            pfsumf.put(id, new SummaryRecord(id, merchant.merchantCode, billingMonth.toString(),
                    count, txnTotal, feeTotal, txnTotal - feeTotal));
        }

        private BillRecord updateBill(MerchantRecord merchant, MerchantResult result) {
            CategoryRecord category = pmcatf.get(merchant.merchantCategory);
            long taxAmount = category.taxableFlag ? roundHalfUp(result.feeTotalAmount * TAX_RATE_BP, 10000L) : 0L;

            BillRecord bill = new BillRecord(billId(merchant.merchantCode), merchant.merchantCode,
                    billingMonth.toString(), result.feeTotalAmount, taxAmount, BILL_STATUS_CREATED,
                    billingMonth.plusMonths(1).atEndOfMonth());
            pfbilf.put(bill.billId, bill);
            return bill;
        }

        private void updateInvoice(MerchantRecord merchant, BillRecord bill, MerchantResult result) {
            String taxBreakdown = "課税対象=" + bill.feeTotalAmount
                    + ";消費税=" + bill.taxAmount
                    + ";監査キー=" + result.auditKey;

            InvoiceRecord invoice = new InvoiceRecord(invoiceId(merchant.merchantCode), bill.billId,
                    merchant.merchantCode, qualifiedInvoiceNo(merchant.merchantCode),
                    businessDate, taxBreakdown);
            pfinvf.put(invoice.invoiceId, invoice);
        }

        private void writeReports(MerchantRecord merchant, BillRecord bill) {
            prrptf.add(new ReportRecord(reportId(merchant.merchantCode, REPORT_TYPE_DETAIL),
                    REPORT_TYPE_DETAIL, businessDate, merchant.merchantCode,
                    "/jp/mirai/pay/fee/" + billingMonth + "/" + merchant.merchantCode + "-meisai.dat", "04"));
            prrptf.add(new ReportRecord(reportId(merchant.merchantCode, REPORT_TYPE_SUMMARY),
                    REPORT_TYPE_SUMMARY, businessDate, merchant.merchantCode,
                    "/jp/mirai/pay/fee/" + billingMonth + "/" + bill.billId + "-seisan.dat", "04"));
        }

        private void validateTotals() {
            long feeFileTotal = 0L;
            for (FeeRecord fee : pffeef) {
                if (java.time.YearMonth.from(pftxnfDate(fee.txnId)).equals(billingMonth)) {
                    feeFileTotal += fee.feeAmount;
                }
            }

            long billTotal = 0L;
            for (BillRecord bill : pfbilf.values()) {
                if (billingMonth.toString().equals(bill.billingMonth)) {
                    billTotal += bill.feeTotalAmount;
                }
            }

            if (billTotal > feeFileTotal) {
                throw new IllegalStateException("請求合計が手数料明細合計を超過");
            }
        }

        private java.time.LocalDate pftxnfDate(String txnId) {
            for (TxnRecord txn : pftxnf) {
                if (txn.txnId.equals(txnId)) {
                    return txn.txnDate;
                }
            }
            return billingMonth.atDay(1);
        }

        private void printOperatorResult() {
            System.out.println("処理日=" + businessDate
                    + " 締月=" + billingMonth
                    + " 手数料明細件数=" + pffeef.size()
                    + " 集計件数=" + pfsumf.size()
                    + " 請求件数=" + pfbilf.size()
                    + " 入金件数=" + pfpayf.size()
                    + " 適格請求書件数=" + pfinvf.size()
                    + " レポート件数=" + prrptf.size());
        }

        private String auditKey(String category) {
            RatePlanRecord selected = null;
            for (RatePlanRecord ratePlan : pmratf.values()) {
                if (ratePlan.categoryCode.equals(category)
                        && "01".equals(ratePlan.approvalStatus)
                        && !ratePlan.effectiveDate.isAfter(billingMonth.atEndOfMonth())
                        && (selected == null || ratePlan.effectiveDate.isAfter(selected.effectiveDate))) {
                    selected = ratePlan;
                }
            }

            if (selected == null) {
                throw new IllegalStateException("監査キー未決定：" + category);
            }
            return selected.noticeId + ":" + selected.ruleHash;
        }

        private long roundHalfUp(long numerator, long denominator) {
            return (numerator + denominator / 2L) / denominator;
        }

        private String feeId(String txnId) {
            return "F-" + txnId;
        }

        private String summaryId(String merchantCode) {
            return "S-" + merchantCode + "-" + billingMonth;
        }

        private String billId(String merchantCode) {
            return "B-" + merchantCode + "-" + billingMonth;
        }

        private String invoiceId(String merchantCode) {
            return "I-" + merchantCode + "-" + billingMonth;
        }

        private String reportId(String merchantCode, String reportType) {
            return "R-" + reportType + "-" + merchantCode + "-" + businessDate;
        }

        private String qualifiedInvoiceNo(String merchantCode) {
            return "T9010000000" + merchantCode.substring(1);
        }
    }

    private static final class MerchantRecord {
        final String merchantCode;
        final String merchantName;
        final String merchantCategory;
        final String merchantStatus;

        MerchantRecord(String merchantCode, String merchantName, String merchantCategory, String merchantStatus) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merchantCategory = merchantCategory;
            this.merchantStatus = merchantStatus;
        }
    }

    private static final class TxnRecord {
        final String txnId;
        final String merchantCode;
        final long txnAmount;
        final java.time.LocalDate txnDate;

        TxnRecord(String txnId, String merchantCode, long txnAmount, java.time.LocalDate txnDate) {
            this.txnId = txnId;
            this.merchantCode = merchantCode;
            this.txnAmount = txnAmount;
            this.txnDate = txnDate;
        }
    }

    private static final class FeeRecord {
        final String feeId;
        final String merchantCode;
        final String txnId;
        final long txnAmount;
        final String mdrRate;
        final long feeAmount;
        final String auditKey;

        FeeRecord(String feeId, String merchantCode, String txnId, long txnAmount, String mdrRate,
                long feeAmount, String auditKey) {
            this.feeId = feeId;
            this.merchantCode = merchantCode;
            this.txnId = txnId;
            this.txnAmount = txnAmount;
            this.mdrRate = mdrRate;
            this.feeAmount = feeAmount;
            this.auditKey = auditKey;
        }
    }

    private static final class SummaryRecord {
        final String summaryId;
        final String merchantCode;
        final String settleMonth;
        final long txnCount;
        final long txnTotalAmount;
        final long feeTotalAmount;
        final long netSettleAmount;

        SummaryRecord(String summaryId, String merchantCode, String settleMonth, long txnCount,
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

    private static final class BillRecord {
        final String billId;
        final String merchantCode;
        final String billingMonth;
        final long feeTotalAmount;
        final long taxAmount;
        final String status;
        final java.time.LocalDate dueDate;

        BillRecord(String billId, String merchantCode, String billingMonth, long feeTotalAmount,
                long taxAmount, String status, java.time.LocalDate dueDate) {
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.billingMonth = billingMonth;
            this.feeTotalAmount = feeTotalAmount;
            this.taxAmount = taxAmount;
            this.status = status;
            this.dueDate = dueDate;
        }
    }

    private static final class PaymentRecord {
        final String paymentId;
        final String merchantCode;
        final java.time.LocalDate paymentDate;
        final long paymentAmount;
        final String bankRefNo;
        final String matchStatus;

        PaymentRecord(String paymentId, String merchantCode, java.time.LocalDate paymentDate,
                long paymentAmount, String bankRefNo, String matchStatus) {
            this.paymentId = paymentId;
            this.merchantCode = merchantCode;
            this.paymentDate = paymentDate;
            this.paymentAmount = paymentAmount;
            this.bankRefNo = bankRefNo;
            this.matchStatus = matchStatus;
        }
    }

    private static final class CategoryRecord {
        final String categoryCode;
        final String categoryName;
        final String riskRank;
        final boolean taxableFlag;
        final boolean activeFlag;
        final java.time.LocalDate lastUpdateDate;

        CategoryRecord(String categoryCode, String categoryName, String riskRank, boolean taxableFlag,
                boolean activeFlag, java.time.LocalDate lastUpdateDate) {
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDate = lastUpdateDate;
        }
    }

    private static final class RatePlanRecord {
        final String ratePlanId;
        final String categoryCode;
        final java.time.LocalDate effectiveDate;
        final String noticeId;
        final String approvalStatus;
        final String ruleHash;

        RatePlanRecord(String ratePlanId, String categoryCode, java.time.LocalDate effectiveDate,
                String noticeId, String approvalStatus, String ruleHash) {
            this.ratePlanId = ratePlanId;
            this.categoryCode = categoryCode;
            this.effectiveDate = effectiveDate;
            this.noticeId = noticeId;
            this.approvalStatus = approvalStatus;
            this.ruleHash = ruleHash;
        }
    }

    private static final class InvoiceRecord {
        final String invoiceId;
        final String billId;
        final String merchantCode;
        final String qualifiedInvoiceNo;
        final java.time.LocalDate issueDate;
        final String taxBreakdown;

        InvoiceRecord(String invoiceId, String billId, String merchantCode, String qualifiedInvoiceNo,
                java.time.LocalDate issueDate, String taxBreakdown) {
            this.invoiceId = invoiceId;
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.qualifiedInvoiceNo = qualifiedInvoiceNo;
            this.issueDate = issueDate;
            this.taxBreakdown = taxBreakdown;
        }
    }

    private static final class ReportRecord {
        final String reportId;
        final String reportType;
        final java.time.LocalDate businessDate;
        final String merchantCode;
        final String outputPath;
        final String status;

        ReportRecord(String reportId, String reportType, java.time.LocalDate businessDate,
                String merchantCode, String outputPath, String status) {
            this.reportId = reportId;
            this.reportType = reportType;
            this.businessDate = businessDate;
            this.merchantCode = merchantCode;
            this.outputPath = outputPath;
            this.status = status;
        }
    }

    private static final class RestartPoint {
        final boolean hasBill;
        final boolean completed;

        RestartPoint(boolean hasBill, boolean completed) {
            this.hasBill = hasBill;
            this.completed = completed;
        }
    }

    private static final class MerchantResult {
        final long txnCount;
        final long txnTotalAmount;
        final long feeTotalAmount;
        final String auditKey;

        MerchantResult(long txnCount, long txnTotalAmount, long feeTotalAmount, String auditKey) {
            this.txnCount = txnCount;
            this.txnTotalAmount = txnTotalAmount;
            this.feeTotalAmount = feeTotalAmount;
            this.auditKey = auditKey;
        }
    }
}
