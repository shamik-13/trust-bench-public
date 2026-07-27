package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025/02/12  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class MerchantBillingAuditReporter {
    private static final String CHARGEABLE_STATUS = "01";
    private static final String REPORT_TYPE_DIFF = "請求差異";
    private static final String REPORT_TYPE_UNISSUED = "未発行";
    private static final String REPORT_TYPE_UNMATCHED = "未消込";
    private static final String REPORT_TYPE_DUPLICATE = "重複出力";

    public static void main(String[] a) {
        String businessDt = a != null && a.length > 0 ? a[0] : "2026-06-28";
        java.util.List<BillingRecord> bills = seedBills();
        java.util.List<FeeRecord> fees = seedFees();
        java.util.List<SummaryRecord> summaries = seedSummaries();
        java.util.List<PaymentRecord> payments = seedPayments();
        java.util.List<ReportRecord> existingReports = seedReports();

        java.util.List<ReportRecord> written = audit(businessDt, bills, fees, summaries, payments, existingReports);
        for (ReportRecord r : written) {
            System.out.println(r.toLine());
        }
    }

    private static java.util.List<ReportRecord> audit(
            String businessDt,
            java.util.List<BillingRecord> bills,
            java.util.List<FeeRecord> fees,
            java.util.List<SummaryRecord> summaries,
            java.util.List<PaymentRecord> payments,
            java.util.List<ReportRecord> existingReports) {
        java.util.Map<String, Long> feeTotalByMerchant = new java.util.HashMap<String, Long>();
        for (FeeRecord f : fees) {
            add(feeTotalByMerchant, f.merchantCode, f.feeAmt);
        }

        java.util.Map<String, SummaryRecord> summaryByMerchantMonth = new java.util.HashMap<String, SummaryRecord>();
        for (SummaryRecord s : summaries) {
            summaryByMerchantMonth.put(key(s.merchantCode, s.settleMonth), s);
        }

        java.util.Map<String, Long> paymentMatchedByMerchant = new java.util.HashMap<String, Long>();
        java.util.Set<String> paymentSeen = new java.util.HashSet<String>();
        java.util.Set<String> duplicatePaymentKeys = new java.util.TreeSet<String>();
        for (PaymentRecord p : payments) {
            String paymentKey = p.merchantCode + "|" + p.bankRefNo + "|" + p.paymentDt + "|" + p.paymentAmt;
            if (!paymentSeen.add(paymentKey)) {
                duplicatePaymentKeys.add(p.merchantCode);
            }
            if ("消込済".equals(p.matchStatus)) {
                add(paymentMatchedByMerchant, p.merchantCode, p.paymentAmt);
            }
        }

        java.util.Set<String> reportSeen = new java.util.HashSet<String>();
        java.util.Set<String> duplicateReportMerchants = new java.util.TreeSet<String>();
        for (ReportRecord r : existingReports) {
            String reportKey = r.reportType + "|" + r.businessDt + "|" + r.merchantCode + "|" + r.outputPath;
            if (!reportSeen.add(reportKey)) {
                duplicateReportMerchants.add(r.merchantCode);
            }
        }

        java.util.List<ReportRecord> out = new java.util.ArrayList<ReportRecord>();
        int seq = existingReports.size() + 1;
        for (BillingRecord b : bills) {
            if (!CHARGEABLE_STATUS.equals(b.status)) {
                continue;
            }

            long feeDetailTotal = value(feeTotalByMerchant, b.merchantCode);
            SummaryRecord summary = summaryByMerchantMonth.get(key(b.merchantCode, b.billingMonth));
            long summaryFeeTotal = summary == null ? 0L : summary.feeTotalAmt;
            long billGross = b.feeTotalAmt + b.taxAmt;

            if (summary == null) {
                out.add(report(seq++, REPORT_TYPE_UNISSUED, businessDt, b.merchantCode, b.billId));
                continue;
            }

            if (b.feeTotalAmt != feeDetailTotal || b.feeTotalAmt != summaryFeeTotal) {
                out.add(report(seq++, REPORT_TYPE_DIFF, businessDt, b.merchantCode, b.billId));
            }

            long paid = value(paymentMatchedByMerchant, b.merchantCode);
            if (paid < billGross && compareDate(b.dueDt, businessDt) < 0) {
                out.add(report(seq++, REPORT_TYPE_UNMATCHED, businessDt, b.merchantCode, b.billId));
            }
        }

        for (String merchantCode : duplicatePaymentKeys) {
            out.add(report(seq++, REPORT_TYPE_DUPLICATE, businessDt, merchantCode, "PFPAYF"));
        }
        for (String merchantCode : duplicateReportMerchants) {
            out.add(report(seq++, REPORT_TYPE_DUPLICATE, businessDt, merchantCode, "PRRPTF"));
        }

        return out;
    }

    private static ReportRecord report(int seq, String type, String businessDt, String merchantCode, String subjectId) {
        String reportId = "RPT-" + businessDt.replace("-", "") + "-" + String.format("%04d", Integer.valueOf(seq));
        String outputPath = "/audit/merchant-billing/" + businessDt + "/" + merchantCode + "/" + subjectId + ".dat";
        return new ReportRecord(reportId, type, businessDt, merchantCode, outputPath, "登録");
    }

    private static void add(java.util.Map<String, Long> map, String key, long amount) {
        Long current = map.get(key);
        map.put(key, Long.valueOf((current == null ? 0L : current.longValue()) + amount));
    }

    private static long value(java.util.Map<String, Long> map, String key) {
        Long v = map.get(key);
        return v == null ? 0L : v.longValue();
    }

    private static String key(String merchantCode, String month) {
        return merchantCode + "|" + month;
    }

    private static int compareDate(String left, String right) {
        return left.compareTo(right);
    }

    private static java.util.List<BillingRecord> seedBills() {
        java.util.List<BillingRecord> list = new java.util.ArrayList<BillingRecord>();
        list.add(new BillingRecord("BIL-202605-0001", "MRC000001", "2026-05", 18400L, 1840L, "01", "2026-06-25"));
        list.add(new BillingRecord("BIL-202605-0002", "MRC000002", "2026-05", 9320L, 932L, "01", "2026-06-25"));
        list.add(new BillingRecord("BIL-202605-0003", "MRC000003", "2026-05", 12600L, 1260L, "01", "2026-06-20"));
        list.add(new BillingRecord("BIL-202605-0004", "MRC000004", "2026-05", 7710L, 771L, "02", "2026-06-25"));
        return list;
    }

    private static java.util.List<FeeRecord> seedFees() {
        java.util.List<FeeRecord> list = new java.util.ArrayList<FeeRecord>();
        list.add(new FeeRecord("FEE-000001", "MRC000001", 600000L, "C1", 9000L));
        list.add(new FeeRecord("FEE-000002", "MRC000001", 470000L, "C1", 9400L));
        list.add(new FeeRecord("FEE-000003", "MRC000002", 310000L, "C2", 6200L));
        list.add(new FeeRecord("FEE-000004", "MRC000002", 156000L, "C2", 3120L));
        list.add(new FeeRecord("FEE-000005", "MRC000003", 420000L, "C4", 12550L));
        return list;
    }

    private static java.util.List<SummaryRecord> seedSummaries() {
        java.util.List<SummaryRecord> list = new java.util.ArrayList<SummaryRecord>();
        list.add(new SummaryRecord("SUM-202605-0001", "MRC000001", "2026-05", 128, 1070000L, 18400L, 1051600L));
        list.add(new SummaryRecord("SUM-202605-0002", "MRC000002", "2026-05", 44, 466000L, 9320L, 456680L));
        list.add(new SummaryRecord("SUM-202605-0003", "MRC000003", "2026-05", 53, 420000L, 12600L, 407400L));
        return list;
    }

    private static java.util.List<PaymentRecord> seedPayments() {
        java.util.List<PaymentRecord> list = new java.util.ArrayList<PaymentRecord>();
        list.add(new PaymentRecord("PAY-000001", "MRC000001", "2026-06-24", 20240L, "BK202606240001", "消込済"));
        list.add(new PaymentRecord("PAY-000002", "MRC000002", "2026-06-24", 10252L, "BK202606240002", "未消込"));
        list.add(new PaymentRecord("PAY-000003", "MRC000003", "2026-06-19", 10000L, "BK202606190003", "消込済"));
        list.add(new PaymentRecord("PAY-000004", "MRC000003", "2026-06-19", 10000L, "BK202606190003", "消込済"));
        return list;
    }

    private static java.util.List<ReportRecord> seedReports() {
        java.util.List<ReportRecord> list = new java.util.ArrayList<ReportRecord>();
        list.add(new ReportRecord("RPT-20260628-0001", "請求差異", "2026-06-28", "MRC000003",
                "/audit/merchant-billing/2026-06-28/MRC000003/BIL-202605-0003.dat", "登録"));
        list.add(new ReportRecord("RPT-20260628-0002", "請求差異", "2026-06-28", "MRC000003",
                "/audit/merchant-billing/2026-06-28/MRC000003/BIL-202605-0003.dat", "登録"));
        return list;
    }

    private static final class BillingRecord {
        final String billId;
        final String merchantCode;
        final String billingMonth;
        final long feeTotalAmt;
        final long taxAmt;
        final String status;
        final String dueDt;

        BillingRecord(String billId, String merchantCode, String billingMonth, long feeTotalAmt, long taxAmt,
                String status, String dueDt) {
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.billingMonth = billingMonth;
            this.feeTotalAmt = feeTotalAmt;
            this.taxAmt = taxAmt;
            this.status = status;
            this.dueDt = dueDt;
        }
    }

    private static final class FeeRecord {
        final String feeId;
        final String merchantCode;
        final long txnAmt;
        final String mdrRate;
        final long feeAmt;

        FeeRecord(String feeId, String merchantCode, long txnAmt, String mdrRate, long feeAmt) {
            this.feeId = feeId;
            this.merchantCode = merchantCode;
            this.txnAmt = txnAmt;
            this.mdrRate = mdrRate;
            this.feeAmt = feeAmt;
        }
    }

    private static final class SummaryRecord {
        final String summaryId;
        final String merchantCode;
        final String settleMonth;
        final int txnCount;
        final long txnTotalAmt;
        final long feeTotalAmt;
        final long netSettleAmt;

        SummaryRecord(String summaryId, String merchantCode, String settleMonth, int txnCount, long txnTotalAmt,
                long feeTotalAmt, long netSettleAmt) {
            this.summaryId = summaryId;
            this.merchantCode = merchantCode;
            this.settleMonth = settleMonth;
            this.txnCount = txnCount;
            this.txnTotalAmt = txnTotalAmt;
            this.feeTotalAmt = feeTotalAmt;
            this.netSettleAmt = netSettleAmt;
        }
    }

    private static final class PaymentRecord {
        final String paymentId;
        final String merchantCode;
        final String paymentDt;
        final long paymentAmt;
        final String bankRefNo;
        final String matchStatus;

        PaymentRecord(String paymentId, String merchantCode, String paymentDt, long paymentAmt, String bankRefNo,
                String matchStatus) {
            this.paymentId = paymentId;
            this.merchantCode = merchantCode;
            this.paymentDt = paymentDt;
            this.paymentAmt = paymentAmt;
            this.bankRefNo = bankRefNo;
            this.matchStatus = matchStatus;
        }
    }

    private static final class ReportRecord {
        final String reportId;
        final String reportType;
        final String businessDt;
        final String merchantCode;
        final String outputPath;
        final String status;

        ReportRecord(String reportId, String reportType, String businessDt, String merchantCode, String outputPath,
                String status) {
            this.reportId = reportId;
            this.reportType = reportType;
            this.businessDt = businessDt;
            this.merchantCode = merchantCode;
            this.outputPath = outputPath;
            this.status = status;
        }

        String toLine() {
            return reportId + "," + reportType + "," + businessDt + "," + merchantCode + "," + outputPath + ","
                    + status;
        }
    }
}
