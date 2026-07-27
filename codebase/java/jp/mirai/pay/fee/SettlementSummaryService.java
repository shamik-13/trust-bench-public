package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当    概要
 * 1.00  2024-04-01  精算課  初版作成
 * 1.01  2024-09-18  精算課  過入金および不足入金の翌月繰越判定を追加
 * 1.02  2025-02-07  精算課  請求再発行対象額の出力を追加
 */

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class SettlementSummaryService {
    private static final DateTimeFormatter YM_FORMAT = DateTimeFormatter.ofPattern("yyyyMM", Locale.ROOT);
    private static final BigDecimal ZERO = BigDecimal.ZERO.setScale(0, RoundingMode.UNNECESSARY);

    public static void main(String[] a) {
        YearMonth targetMonth = a.length == 0 ? YearMonth.of(2025, 3) : YearMonth.parse(a[0], YM_FORMAT);
        List<PfSumf> summaries = loadPfSumf();
        List<PfBilf> bills = loadPfBilf();
        List<PfPayf> payments = loadPfPayf();

        Map<String, SettlementWork> works = new LinkedHashMap<>();
        for (PfSumf summary : summaries) {
            if (summary.settleMonth.equals(targetMonth)) {
                works.put(summary.merchantCode, new SettlementWork(summary));
            }
        }

        for (PfBilf bill : bills) {
            if (bill.billingMonth.equals(targetMonth)) {
                SettlementWork work = works.computeIfAbsent(bill.merchantCode, k -> new SettlementWork(emptySummary(k, targetMonth)));
                if ("請求済".equals(bill.status) || "一部入金".equals(bill.status) || "再発行待".equals(bill.status)) {
                    work.billedFee = work.billedFee.add(nvl(bill.feeTotalAmt));
                    work.billedTax = work.billedTax.add(nvl(bill.taxAmt));
                    if (bill.dueDate.isBefore(LocalDate.of(targetMonth.getYear(), targetMonth.getMonth(), 1).plusMonths(1))) {
                        work.dueBillCount++;
                    }
                }
            }
        }

        for (PfPayf payment : payments) {
            YearMonth paymentMonth = YearMonth.from(payment.paymentDate);
            if (paymentMonth.equals(targetMonth) && ("未照合".equals(payment.matchStatus) || "照合済".equals(payment.matchStatus))) {
                SettlementWork work = works.computeIfAbsent(payment.merchantCode, k -> new SettlementWork(emptySummary(k, targetMonth)));
                work.paidAmount = work.paidAmount.add(nvl(payment.paymentAmt));
                if ("未照合".equals(payment.matchStatus)) {
                    work.unmatchedPaymentCount++;
                }
            }
        }

        List<PfSumf> updated = new ArrayList<>();
        for (SettlementWork work : works.values()) {
            BigDecimal billedTotal = work.billedFee.add(work.billedTax);
            BigDecimal shortage = positive(billedTotal.subtract(work.paidAmount));
            BigDecimal overpaid = positive(work.paidAmount.subtract(billedTotal));
            BigDecimal nextCarry = overpaid.subtract(shortage);
            BigDecimal reissueAmount = shortage;

            PfSumf source = work.summary;
            PfSumf result = new PfSumf(
                    source.summaryId,
                    source.merchantCode,
                    source.settleMonth,
                    source.txnCount,
                    source.txnTotalAmt,
                    billedTotal,
                    source.txnTotalAmt.subtract(billedTotal).add(nextCarry)
            );
            updated.add(result);

            String state = shortage.signum() > 0 ? "不足入金" : overpaid.signum() > 0 ? "過入金" : "精算一致";
            System.out.println(
                    "精算月=" + targetMonth.format(YM_FORMAT)
                            + ", 加盟店=" + source.merchantCode
                            + ", 状態=" + state
                            + ", 請求額=" + yen(billedTotal)
                            + ", 入金額=" + yen(work.paidAmount)
                            + ", 翌月繰越額=" + yen(nextCarry)
                            + ", 再発行対象額=" + yen(reissueAmount)
                            + ", 期限到来請求数=" + work.dueBillCount
                            + ", 未照合入金数=" + work.unmatchedPaymentCount
            );
        }

        updated.sort(Comparator.comparing((PfSumf s) -> s.merchantCode).thenComparing(s -> s.summaryId));
        Map<String, PfSumf> pfSumfIndex = new HashMap<>();
        for (PfSumf row : summaries) {
            pfSumfIndex.put(row.summaryId, row);
        }
        for (PfSumf row : updated) {
            pfSumfIndex.put(row.summaryId, row);
        }

        System.out.println("PFSUMF更新件数=" + updated.size());
    }

    private static List<PfSumf> loadPfSumf() {
        List<PfSumf> rows = new ArrayList<>();
        rows.add(new PfSumf("SUM-202503-1001", "M1001", YearMonth.of(2025, 3), 18240, bd("421893200"), bd("0"), bd("421893200")));
        rows.add(new PfSumf("SUM-202503-1002", "M1002", YearMonth.of(2025, 3), 9044, bd("98110200"), bd("0"), bd("98110200")));
        rows.add(new PfSumf("SUM-202503-1003", "M1003", YearMonth.of(2025, 3), 2731, bd("33770650"), bd("0"), bd("33770650")));
        rows.add(new PfSumf("SUM-202502-1001", "M1001", YearMonth.of(2025, 2), 16920, bd("398104000"), bd("1129880"), bd("396974120")));
        return rows;
    }

    private static List<PfBilf> loadPfBilf() {
        List<PfBilf> rows = new ArrayList<>();
        rows.add(new PfBilf("BIL-202503-90001", "M1001", YearMonth.of(2025, 3), bd("1265679"), bd("126568"), "請求済", LocalDate.of(2025, 4, 25)));
        rows.add(new PfBilf("BIL-202503-90002", "M1002", YearMonth.of(2025, 3), bd("392441"), bd("39244"), "一部入金", LocalDate.of(2025, 4, 25)));
        rows.add(new PfBilf("BIL-202503-90003", "M1003", YearMonth.of(2025, 3), bd("135082"), bd("13508"), "請求済", LocalDate.of(2025, 4, 25)));
        rows.add(new PfBilf("BIL-202503-90004", "M1003", YearMonth.of(2025, 3), bd("18000"), bd("1800"), "取消", LocalDate.of(2025, 4, 25)));
        return rows;
    }

    private static List<PfPayf> loadPfPayf() {
        List<PfPayf> rows = new ArrayList<>();
        rows.add(new PfPayf("PAY-202503-70001", "M1001", LocalDate.of(2025, 3, 29), bd("1392247"), "BTMU250329001", "照合済"));
        rows.add(new PfPayf("PAY-202503-70002", "M1002", LocalDate.of(2025, 3, 30), bd("400000"), "SMBC250330118", "未照合"));
        rows.add(new PfPayf("PAY-202503-70003", "M1003", LocalDate.of(2025, 3, 31), bd("120000"), "MZBK250331552", "照合済"));
        rows.add(new PfPayf("PAY-202504-70004", "M1001", LocalDate.of(2025, 4, 1), bd("50000"), "BTMU250401044", "未照合"));
        return rows;
    }

    private static PfSumf emptySummary(String merchantCode, YearMonth month) {
        return new PfSumf("SUM-" + month.format(YM_FORMAT) + "-" + merchantCode, merchantCode, month, 0, ZERO, ZERO, ZERO);
    }

    private static BigDecimal positive(BigDecimal value) {
        return value.signum() > 0 ? value : ZERO;
    }

    private static BigDecimal nvl(BigDecimal value) {
        return value == null ? ZERO : value;
    }

    private static BigDecimal bd(String value) {
        return new BigDecimal(value).setScale(0, RoundingMode.UNNECESSARY);
    }

    private static String yen(BigDecimal value) {
        return value.setScale(0, RoundingMode.UNNECESSARY).toPlainString();
    }

    private static final class SettlementWork {
        private final PfSumf summary;
        private BigDecimal billedFee = ZERO;
        private BigDecimal billedTax = ZERO;
        private BigDecimal paidAmount = ZERO;
        private int dueBillCount;
        private int unmatchedPaymentCount;

        private SettlementWork(PfSumf summary) {
            this.summary = summary;
        }
    }

    private static final class PfSumf {
        private final String summaryId;
        private final String merchantCode;
        private final YearMonth settleMonth;
        private final int txnCount;
        private final BigDecimal txnTotalAmt;
        private final BigDecimal feeTotalAmt;
        private final BigDecimal netSettleAmt;

        private PfSumf(String summaryId, String merchantCode, YearMonth settleMonth, int txnCount,
                       BigDecimal txnTotalAmt, BigDecimal feeTotalAmt, BigDecimal netSettleAmt) {
            this.summaryId = summaryId;
            this.merchantCode = merchantCode;
            this.settleMonth = settleMonth;
            this.txnCount = txnCount;
            this.txnTotalAmt = txnTotalAmt;
            this.feeTotalAmt = feeTotalAmt;
            this.netSettleAmt = netSettleAmt;
        }
    }

    private static final class PfBilf {
        private final String billId;
        private final String merchantCode;
        private final YearMonth billingMonth;
        private final BigDecimal feeTotalAmt;
        private final BigDecimal taxAmt;
        private final String status;
        private final LocalDate dueDate;

        private PfBilf(String billId, String merchantCode, YearMonth billingMonth, BigDecimal feeTotalAmt,
                       BigDecimal taxAmt, String status, LocalDate dueDate) {
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.billingMonth = billingMonth;
            this.feeTotalAmt = feeTotalAmt;
            this.taxAmt = taxAmt;
            this.status = status;
            this.dueDate = dueDate;
        }
    }

    private static final class PfPayf {
        private final String paymentId;
        private final String merchantCode;
        private final LocalDate paymentDate;
        private final BigDecimal paymentAmt;
        private final String bankRefNo;
        private final String matchStatus;

        private PfPayf(String paymentId, String merchantCode, LocalDate paymentDate, BigDecimal paymentAmt,
                       String bankRefNo, String matchStatus) {
            this.paymentId = paymentId;
            this.merchantCode = merchantCode;
            this.paymentDate = paymentDate;
            this.paymentAmt = paymentAmt;
            this.bankRefNo = bankRefNo;
            this.matchStatus = matchStatus;
        }
    }
}
