package jp.mirai.pay.settlement;

/**
 * 変更履歴
 * 版数    年月日        担当        概要
 * 1.00    2024/07/02    加盟店精算チーム    初版作成
 * 1.01    2025/02/17    加盟店精算チーム    手数料は格納値を参照する方式に変更
 */

import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class MonthlySettlementReportService {
    private static final String MER_STATUS_SETTLEABLE = "01";
    private static final String MER_STATUS_HOLD = "02";
    private static final String MER_STATUS_CLOSED = "09";
    private static final String REPORT_KBN_MONTHLY = "M";
    private static final String STATUS_CREATED = "2";
    private static final String STATUS_REQUESTED = "0";
    private static final DateTimeFormatter YM_FORMAT = DateTimeFormatter.ofPattern("yyyyMM");

    public static void main(String[] a) {
        Service service = new Service(new Repository());
        Request request = Request.fromArgs(a);
        Response response = service.inquire(request);
        System.out.println(response.toOperatorText());
    }

    private static final class Service {
        private final Repository repository;

        private Service(Repository repository) {
            this.repository = repository;
        }

        private Response inquire(Request request) {
            List<String> notices = new ArrayList<String>();
            Merchant merchant = repository.findMerchant(request.merchantCode);
            if (merchant == null) {
                throw new IllegalArgumentException("加盟店が存在しません: " + request.merchantCode);
            }
            if (MER_STATUS_CLOSED.equals(merchant.status)) {
                throw new IllegalStateException("加盟店は解約済です: " + request.merchantCode);
            }
            if (!MER_STATUS_SETTLEABLE.equals(merchant.status)) {
                notices.add("加盟店は精算保留中です");
            }

            LocalDate from = request.period.atDay(1);
            LocalDate to = request.period.atEndOfMonth();
            Report report = repository.findReport(request.merchantCode, REPORT_KBN_MONTHLY, from, to);

            boolean recreateRequested = false;
            if (report == null || !STATUS_CREATED.equals(report.createStatus)) {
                String reportId = report == null ? repository.nextReportId(request.period) : report.reportId;
                String path = "/psrptf/" + request.period.format(YM_FORMAT) + "/" + request.merchantCode + ".pdf";
                Report requestReport = new Report(reportId, request.merchantCode, REPORT_KBN_MONTHLY, from, to, path, STATUS_REQUESTED);
                repository.writeReport(requestReport);
                report = requestReport;
                recreateRequested = true;
                notices.add("月次レポート再作成要求を登録しました");
            }

            List<Settlement> settlements = repository.findSettlements(request.merchantCode, from, to);
            List<Detail> details = repository.findDetails(request.merchantCode, settlements);

            Summary summary = aggregate(settlements, details);
            Links links = new Links(
                    "/settlement/summary?merchant=" + request.merchantCode + "&period=" + request.period.format(YM_FORMAT),
                    "/settlement/charges?merchant=" + request.merchantCode + "&period=" + request.period.format(YM_FORMAT),
                    "/settlement/adjustments?merchant=" + request.merchantCode + "&period=" + request.period.format(YM_FORMAT)
            );

            return new Response(merchant, report, summary, links, recreateRequested, notices);
        }

        private Summary aggregate(List<Settlement> settlements, List<Detail> details) {
            long netAmount = 0L;
            long recordedCharge = 0L;
            long payoutAmount = 0L;
            for (Settlement s : settlements) {
                netAmount += s.netAmount;
                recordedCharge += s.chargeAmount;
                payoutAmount += s.payoutAmount;
            }

            long salesAmount = 0L;
            long refundAmount = 0L;
            long adjustmentAmount = 0L;
            long detailCharge = 0L;
            for (Detail d : details) {
                if ("C".equals(d.lineKbn)) {
                    salesAmount += d.txnAmount;
                } else if ("R".equals(d.lineKbn)) {
                    refundAmount += d.txnAmount;
                } else {
                    adjustmentAmount += d.txnAmount;
                }
                // 手数料は明細に格納済みの値（PSDTLFのCHARGE-AMT）を集計する。
                detailCharge += d.chargeAmount;
            }

            // 精算ヘッダ（PSSETF）の手数料と明細（PSDTLF）の手数料合計の差。
            long chargeDiff = recordedCharge - detailCharge;
            return new Summary(settlements.size(), details.size(), salesAmount, refundAmount,
                    adjustmentAmount, netAmount, recordedCharge, detailCharge, chargeDiff, payoutAmount);
        }
    }

    private static final class Request {
        private final String merchantCode;
        private final YearMonth period;

        private Request(String merchantCode, YearMonth period) {
            this.merchantCode = merchantCode;
            this.period = period;
        }

        private static Request fromArgs(String[] a) {
            String merchantCode = a.length >= 1 ? a[0] : "M10001";
            YearMonth period = YearMonth.of(2026, 5);
            if (a.length >= 2) {
                try {
                    period = YearMonth.parse(a[1], YM_FORMAT);
                } catch (DateTimeParseException e) {
                    throw new IllegalArgumentException("対象年月はyyyyMMで指定してください: " + a[1], e);
                }
            }
            if (!merchantCode.matches("M[0-9]{5}")) {
                throw new IllegalArgumentException("加盟店コード形式が不正です: " + merchantCode);
            }
            return new Request(merchantCode, period);
        }
    }

    private static final class Repository {
        private final List<Report> reports = new ArrayList<Report>();
        private final List<Settlement> settlements = new ArrayList<Settlement>();
        private final List<Detail> details = new ArrayList<Detail>();
        private final Map<String, Merchant> merchants = new LinkedHashMap<String, Merchant>();

        private Repository() {
            loadMerchants();
            loadReports();
            loadSettlements();
            loadDetails();
        }

        private Merchant findMerchant(String merchantCode) {
            return merchants.get(merchantCode);
        }

        private Report findReport(String merchantCode, String reportKbn, LocalDate from, LocalDate to) {
            List<Report> matched = new ArrayList<Report>();
            for (Report r : reports) {
                if (r.merchantCode.equals(merchantCode)
                        && r.reportKbn.equals(reportKbn)
                        && r.periodFrom.equals(from)
                        && r.periodTo.equals(to)) {
                    matched.add(r);
                }
            }
            Collections.sort(matched, new Comparator<Report>() {
                public int compare(Report x, Report y) {
                    return y.reportId.compareTo(x.reportId);
                }
            });
            return matched.isEmpty() ? null : matched.get(0);
        }

        private List<Settlement> findSettlements(String merchantCode, LocalDate from, LocalDate to) {
            List<Settlement> found = new ArrayList<Settlement>();
            for (Settlement s : settlements) {
                if (s.merchantCode.equals(merchantCode)
                        && !s.settleDate.isBefore(from)
                        && !s.settleDate.isAfter(to)) {
                    found.add(s);
                }
            }
            return found;
        }

        private List<Detail> findDetails(String merchantCode, List<Settlement> targetSettlements) {
            Map<String, Boolean> settleIds = new HashMap<String, Boolean>();
            for (Settlement s : targetSettlements) {
                settleIds.put(s.settleId, Boolean.TRUE);
            }

            List<Detail> found = new ArrayList<Detail>();
            for (Detail d : details) {
                if (d.merchantCode.equals(merchantCode) && settleIds.containsKey(d.settleId)) {
                    found.add(d);
                }
            }
            return found;
        }

        private void writeReport(Report report) {
            reports.add(report);
        }

        private String nextReportId(YearMonth period) {
            int seq = 1;
            String prefix = "R" + period.format(YM_FORMAT);
            for (Report r : reports) {
                if (r.reportId.startsWith(prefix)) {
                    seq++;
                }
            }
            return prefix + String.format("%05d", seq);
        }

        private void loadMerchants() {
            merchants.put("M10001", new Merchant("M10001", "青葉書店", MER_STATUS_SETTLEABLE, "0001234567"));
            merchants.put("M10002", new Merchant("M10002", "北浜雑貨", MER_STATUS_HOLD, "0002345678"));
            merchants.put("M10003", new Merchant("M10003", "港南食堂", MER_STATUS_CLOSED, "0003456789"));
        }

        private void loadReports() {
            reports.add(new Report("R20260500001", "M10001", REPORT_KBN_MONTHLY,
                    LocalDate.of(2026, 5, 1), LocalDate.of(2026, 5, 31),
                    "/psrptf/202605/M10001.pdf", STATUS_CREATED));
            reports.add(new Report("R20260500002", "M10002", REPORT_KBN_MONTHLY,
                    LocalDate.of(2026, 5, 1), LocalDate.of(2026, 5, 31),
                    "/psrptf/202605/M10002.pdf", "9"));
        }

        private void loadSettlements() {
            settlements.add(new Settlement("S20260501001", "M10001", 125000L, 375L, 124625L, LocalDate.of(2026, 5, 10)));
            settlements.add(new Settlement("S20260502001", "M10001", 83000L, 249L, 82751L, LocalDate.of(2026, 5, 20)));
            settlements.add(new Settlement("S20260503001", "M10002", 42000L, 126L, 41874L, LocalDate.of(2026, 5, 18)));
            settlements.add(new Settlement("S20260401001", "M10001", 99000L, 297L, 98703L, LocalDate.of(2026, 4, 30)));
        }

        private void loadDetails() {
            details.add(new Detail("D000001", "S20260501001", "M10001", "T900001", 70000L, 210L, "C"));
            details.add(new Detail("D000002", "S20260501001", "M10001", "T900002", 55000L, 165L, "C"));
            details.add(new Detail("D000003", "S20260502001", "M10001", "T900003", 90000L, 270L, "C"));
            details.add(new Detail("D000004", "S20260502001", "M10001", "T900004", -7000L, 21L, "R"));
            details.add(new Detail("D000005", "S20260503001", "M10002", "T910001", 45000L, 135L, "C"));
            details.add(new Detail("D000006", "S20260503001", "M10002", "T910002", -3000L, 9L, "R"));
        }
    }

    private static final class Response {
        private final Merchant merchant;
        private final Report report;
        private final Summary summary;
        private final Links links;
        private final boolean recreateRequested;
        private final List<String> notices;

        private Response(Merchant merchant, Report report, Summary summary, Links links,
                         boolean recreateRequested, List<String> notices) {
            this.merchant = merchant;
            this.report = report;
            this.summary = summary;
            this.links = links;
            this.recreateRequested = recreateRequested;
            this.notices = notices;
        }

        private String toOperatorText() {
            StringBuilder b = new StringBuilder();
            b.append("月次精算レポート照会結果").append(System.lineSeparator());
            b.append("加盟店=").append(merchant.merchantCode).append(" ").append(merchant.merchantName).append(System.lineSeparator());
            b.append("加盟店状態=").append(statusName(merchant.status)).append(System.lineSeparator());
            b.append("レポート番号=").append(report.reportId).append(System.lineSeparator());
            b.append("作成状態=").append(report.createStatus).append(System.lineSeparator());
            b.append("出力先=").append(report.outputPath).append(System.lineSeparator());
            b.append("再作成要求=").append(recreateRequested ? "有" : "無").append(System.lineSeparator());
            b.append("精算件数=").append(summary.settlementCount).append(System.lineSeparator());
            b.append("明細件数=").append(summary.detailCount).append(System.lineSeparator());
            b.append("売上額=").append(summary.salesAmount).append(System.lineSeparator());
            b.append("返金額=").append(summary.refundAmount).append(System.lineSeparator());
            b.append("調整額=").append(summary.adjustmentAmount).append(System.lineSeparator());
            b.append("純額=").append(summary.netAmount).append(System.lineSeparator());
            b.append("精算手数料=").append(summary.recordedCharge).append(System.lineSeparator());
            b.append("明細手数料合計=").append(summary.detailCharge).append(System.lineSeparator());
            b.append("手数料差額=").append(summary.chargeDiff).append(System.lineSeparator());
            b.append("振込予定額=").append(summary.payoutAmount).append(System.lineSeparator());
            b.append("精算サマリ=").append(links.summaryLink).append(System.lineSeparator());
            b.append("手数料明細=").append(links.chargeLink).append(System.lineSeparator());
            b.append("調整行=").append(links.adjustmentLink);
            for (String notice : notices) {
                b.append(System.lineSeparator()).append("通知=").append(notice);
            }
            return b.toString();
        }

        private String statusName(String status) {
            if (MER_STATUS_SETTLEABLE.equals(status)) {
                return "01:精算対象";
            }
            if (MER_STATUS_HOLD.equals(status)) {
                return "02:精算保留";
            }
            if (MER_STATUS_CLOSED.equals(status)) {
                return "09:解約";
            }
            return status + ":不明";
        }
    }

    private static final class Report {
        private final String reportId;
        private final String merchantCode;
        private final String reportKbn;
        private final LocalDate periodFrom;
        private final LocalDate periodTo;
        private final String outputPath;
        private final String createStatus;

        private Report(String reportId, String merchantCode, String reportKbn, LocalDate periodFrom,
                       LocalDate periodTo, String outputPath, String createStatus) {
            this.reportId = reportId;
            this.merchantCode = merchantCode;
            this.reportKbn = reportKbn;
            this.periodFrom = periodFrom;
            this.periodTo = periodTo;
            this.outputPath = outputPath;
            this.createStatus = createStatus;
        }
    }

    private static final class Settlement {
        private final String settleId;
        private final String merchantCode;
        private final long netAmount;
        private final long chargeAmount;
        private final long payoutAmount;
        private final LocalDate settleDate;

        private Settlement(String settleId, String merchantCode, long netAmount, long chargeAmount,
                           long payoutAmount, LocalDate settleDate) {
            this.settleId = settleId;
            this.merchantCode = merchantCode;
            this.netAmount = netAmount;
            this.chargeAmount = chargeAmount;
            this.payoutAmount = payoutAmount;
            this.settleDate = settleDate;
        }
    }

    private static final class Detail {
        private final String detailId;
        private final String settleId;
        private final String merchantCode;
        private final String txnId;
        private final long txnAmount;
        private final long chargeAmount;
        private final String lineKbn;

        private Detail(String detailId, String settleId, String merchantCode, String txnId,
                       long txnAmount, long chargeAmount, String lineKbn) {
            this.detailId = detailId;
            this.settleId = settleId;
            this.merchantCode = merchantCode;
            this.txnId = txnId;
            this.txnAmount = txnAmount;
            this.chargeAmount = chargeAmount;
            this.lineKbn = lineKbn;
        }
    }

    private static final class Merchant {
        private final String merchantCode;
        private final String merchantName;
        private final String status;
        private final String bankAccountNo;

        private Merchant(String merchantCode, String merchantName, String status, String bankAccountNo) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.status = status;
            this.bankAccountNo = bankAccountNo;
        }
    }

    private static final class Summary {
        private final int settlementCount;
        private final int detailCount;
        private final long salesAmount;
        private final long refundAmount;
        private final long adjustmentAmount;
        private final long netAmount;
        private final long recordedCharge;
        private final long detailCharge;
        private final long chargeDiff;
        private final long payoutAmount;

        private Summary(int settlementCount, int detailCount, long salesAmount, long refundAmount,
                        long adjustmentAmount, long netAmount, long recordedCharge,
                        long detailCharge, long chargeDiff, long payoutAmount) {
            this.settlementCount = settlementCount;
            this.detailCount = detailCount;
            this.salesAmount = salesAmount;
            this.refundAmount = refundAmount;
            this.adjustmentAmount = adjustmentAmount;
            this.netAmount = netAmount;
            this.recordedCharge = recordedCharge;
            this.detailCharge = detailCharge;
            this.chargeDiff = chargeDiff;
            this.payoutAmount = payoutAmount;
        }
    }

    private static final class Links {
        private final String summaryLink;
        private final String chargeLink;
        private final String adjustmentLink;

        private Links(String summaryLink, String chargeLink, String adjustmentLink) {
            this.summaryLink = summaryLink;
            this.chargeLink = chargeLink;
            this.adjustmentLink = adjustmentLink;
        }
    }
}
