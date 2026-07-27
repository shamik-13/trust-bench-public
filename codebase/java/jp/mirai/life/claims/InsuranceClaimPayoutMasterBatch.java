package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.00  20240228    保険金システムG  保険金支払月次マスターバッチ（フェーズ連携）初版
 *
 * 各フェーズを連携する月次マスターバッチ。支払額の算定（支払削減割合の適用を含む）は
 * ClaimPayoutEngine に委譲し、当バッチは削減割合を保持しない。
 */
public class InsuranceClaimPayoutMasterBatch {
    private static final String STATUS_PAY_TARGET = "01";
    private static final String STATUS_ASSESSING = "05";
    private static final String STATUS_DENIED = "09";
    private static final String STATUS_LEGACY_APPROVED = "25";
    private static final String STATUS_LEGACY_REVIEWED = "30";

    private static final int FULL_PAYOUT_RATE_PCT = 100;
    private static final long TAX_EXEMPT_LIMIT = 500_000L;
    private static final int TAX_RATE_PER_MILLE = 102;
    private static final int BATCH_YM = 202402;
    private static final int BATCH_DT = 20240228;

    int run() {
        return execute();
    }

    private final InMemoryFiles files = InMemoryFiles.synthetic();
    private final CheckpointStore checkpointStore = new CheckpointStore();
    private final java.util.List<PhaseTotal> phaseTotals = new java.util.ArrayList<PhaseTotal>();

    private int execute() {
        checkpointStore.save(files);
        try {
            phase1ValidateClaims();
            checkpointStore.save(files);
            phase2CalculatePayouts();
            checkpointStore.save(files);
            phase3AdjustLoans();
            checkpointStore.save(files);
            phase4TransferPayments();
            checkpointStore.save(files);
            phase5CalculateTax();
            checkpointStore.save(files);
            phase6ReconcileAndReport();
            return 0;
        } catch (RuntimeException ex) {
            checkpointStore.restore(files);
            System.err.println("異常検知のためチェックポイントから復元しました: " + ex.getMessage());
            return 12;
        }
    }

    private void phase1ValidateClaims() {
        PhaseTotal total = new PhaseTotal("01");
        java.util.Map<String, AssessmentRecord> latestAssessments = latestAssessmentByClaim();

        for (ClaimRecord claim : files.lfclmf) {
            if (!isPhase1InputStatus(claim.claimStatusKbn)) {
                continue;
            }

            AssessmentRecord assessment = latestAssessments.get(claim.claimId);
            boolean approved = LoanDeductionValidator.approve(claim, assessment);
            claim.claimStatusKbn = approved ? STATUS_PAY_TARGET : STATUS_DENIED;

            total.count++;
            total.totalGrossAmt += claim.sumAssuredAmt;
        }

        phaseTotals.add(total);
        require(total.count > 0, "査定対象請求がありません");
    }

    private void phase2CalculatePayouts() {
        PhaseTotal total = new PhaseTotal("02");
        java.util.Set<String> existingClaimIds = new java.util.HashSet<String>();

        for (PayRecord pay : files.lfpayf) {
            existingClaimIds.add(pay.claimId);
        }

        for (ClaimRecord claim : files.lfclmf) {
            if (!STATUS_PAY_TARGET.equals(claim.claimStatusKbn) || existingClaimIds.contains(claim.claimId)) {
                continue;
            }

            ClaimPayoutEngine engine = new ClaimPayoutEngine();
            ClaimModel.Claim engineClaim = new ClaimModel.Claim(
                    claim.claimId, claim.polNo, claim.sumAssuredAmt, claim.loanBalanceAmt,
                    claim.respStartDt, claim.eventDt, claim.claimStatusKbn);
            int reductionRate = engine.reductionRatePct(claim.respStartDt, claim.eventDt);
            long payoutAmt = engine.payoutFor(engineClaim);
            PayoutCalculationAuditor.audit(claim, reductionRate, payoutAmt);

            PayRecord pay = new PayRecord(nextId("PAY", files.lfpayf.size() + 1), claim.claimId,
                    claim.sumAssuredAmt, reductionRate, payoutAmt);
            files.lfpayf.add(pay);
            existingClaimIds.add(claim.claimId);

            total.count++;
            total.totalGrossAmt += pay.grossAmt;
            total.totalPayoutAmt += pay.payoutAmt;
            total.reductionRateSum += pay.reductionRate;
        }

        phaseTotals.add(total);
        require(total.count > 0, "支払計算対象がありません");
    }

    private void phase3AdjustLoans() {
        PhaseTotal total = new PhaseTotal("03");
        java.util.Map<String, LoanRecord> loans = loansByPolicy();

        for (ClaimRecord claim : files.lfclmf) {
            if (claim.loanBalanceAmt <= 0) {
                continue;
            }

            LoanRecord loan = loans.get(claim.polNo);
            long adjusted = LoanBalanceAdjustmentProcessor.adjust(claim, loan);
            claim.loanBalanceAmt = adjusted;

            total.count++;
            total.totalGrossAmt += adjusted;
        }

        phaseTotals.add(total);
    }

    private void phase4TransferPayments() {
        PhaseTotal total = new PhaseTotal("04");
        java.util.Map<String, ClaimRecord> claims = claimsById();
        java.util.Map<String, java.util.List<BeneficiaryRecord>> beneficiaries = beneficiariesByPolicy();
        java.util.Set<String> transferredPayIds = transferredPayIds();

        for (PayRecord pay : files.lfpayf) {
            if (transferredPayIds.contains(pay.payId)) {
                continue;
            }

            ClaimRecord claim = claims.get(pay.claimId);
            require(claim != null, "請求が存在しない支払があります payId=" + pay.payId);

            java.util.List<BeneficiaryRecord> list = beneficiaries.get(claim.polNo);
            require(list != null && !list.isEmpty(), "受取人が存在しません polNo=" + claim.polNo);

            BeneficiaryRecord beneficiary = firstPriority(list);
            TransferRecord transfer = PaymentTransferOrchestrator.createTransfer(pay, beneficiary);
            files.lfxfrf.add(transfer);

            total.count++;
            total.totalPayoutAmt += transfer.amount;
        }

        phaseTotals.add(total);
    }

    private void phase5CalculateTax() {
        PhaseTotal total = new PhaseTotal("05");
        java.util.Map<String, ClaimRecord> claims = claimsById();
        java.util.Map<String, java.util.List<BeneficiaryRecord>> beneficiaries = beneficiariesByPolicy();
        java.util.Set<String> taxedPayIds = taxedPayIds();

        for (PayRecord pay : files.lfpayf) {
            if (taxedPayIds.contains(pay.payId)) {
                continue;
            }

            ClaimRecord claim = claims.get(pay.claimId);
            require(claim != null, "源泉計算対象請求が存在しません payId=" + pay.payId);

            BeneficiaryRecord beneficiary = firstPriority(beneficiaries.get(claim.polNo));
            WithholdingRecord tax = WithholdingTaxCalculator.calculate(pay, beneficiary);
            files.lfwitf.add(tax);

            total.count++;
            total.totalGrossAmt += tax.taxableAmt;
            total.totalPayoutAmt += tax.taxAmt;
        }

        phaseTotals.add(total);
    }

    private void phase6ReconcileAndReport() {
        PhaseTotal total = MonthlyAssessmentAggregator.aggregate(files.lfpayf);
        total.phase = "06";
        phaseTotals.add(total);

        MonthlyPaymentSummaryBatch.writeControlTotals(files.lfmstf, phaseTotals);

        java.util.List<String> reportLines = AssessmentReportFormatter.format(files.lfclmf, files.lfpayf, files.lfwitf);
        int page = 1;
        int lineNo = 1;
        for (String line : reportLines) {
            files.lfrepmf.add(new ReportRecord("RPT" + BATCH_YM + pad(lineNo, 4), "SA", BATCH_DT, page, line));
            if (lineNo % 40 == 0) {
                page++;
            }
            lineNo++;
        }
    }

    private boolean isPhase1InputStatus(String status) {
        return STATUS_LEGACY_APPROVED.equals(status)
                || STATUS_LEGACY_REVIEWED.equals(status)
                || STATUS_ASSESSING.equals(status);
    }

    private java.util.Map<String, AssessmentRecord> latestAssessmentByClaim() {
        java.util.Map<String, AssessmentRecord> map = new java.util.HashMap<String, AssessmentRecord>();
        for (AssessmentRecord rec : files.lfrasf) {
            AssessmentRecord old = map.get(rec.claimId);
            if (old == null || rec.assessDt > old.assessDt) {
                map.put(rec.claimId, rec);
            }
        }
        return map;
    }

    private java.util.Map<String, ClaimRecord> claimsById() {
        java.util.Map<String, ClaimRecord> map = new java.util.HashMap<String, ClaimRecord>();
        for (ClaimRecord rec : files.lfclmf) {
            map.put(rec.claimId, rec);
        }
        return map;
    }

    private java.util.Map<String, LoanRecord> loansByPolicy() {
        java.util.Map<String, LoanRecord> map = new java.util.HashMap<String, LoanRecord>();
        for (LoanRecord rec : files.lflanf) {
            map.put(rec.polNo, rec);
        }
        return map;
    }

    private java.util.Map<String, java.util.List<BeneficiaryRecord>> beneficiariesByPolicy() {
        java.util.Map<String, java.util.List<BeneficiaryRecord>> map =
                new java.util.HashMap<String, java.util.List<BeneficiaryRecord>>();

        for (BeneficiaryRecord rec : files.lfbenf) {
            java.util.List<BeneficiaryRecord> list = map.get(rec.polNo);
            if (list == null) {
                list = new java.util.ArrayList<BeneficiaryRecord>();
                map.put(rec.polNo, list);
            }
            list.add(rec);
        }

        return map;
    }

    private java.util.Set<String> transferredPayIds() {
        java.util.Set<String> ids = new java.util.HashSet<String>();
        for (TransferRecord rec : files.lfxfrf) {
            ids.add(rec.payId);
        }
        return ids;
    }

    private java.util.Set<String> taxedPayIds() {
        java.util.Set<String> ids = new java.util.HashSet<String>();
        for (WithholdingRecord rec : files.lfwitf) {
            ids.add(rec.payId);
        }
        return ids;
    }

    private BeneficiaryRecord firstPriority(java.util.List<BeneficiaryRecord> list) {
        require(list != null && !list.isEmpty(), "優先受取人が存在しません");

        BeneficiaryRecord selected = list.get(0);
        for (BeneficiaryRecord rec : list) {
            if (rec.paymentPriority < selected.paymentPriority) {
                selected = rec;
            }
        }
        return selected;
    }

    private String nextId(String prefix, int seq) {
        return prefix + BATCH_YM + pad(seq, 6);
    }

    private static String pad(int n, int width) {
        String s = String.valueOf(n);
        StringBuilder b = new StringBuilder();
        for (int i = s.length(); i < width; i++) {
            b.append('0');
        }
        return b.append(s).toString();
    }

    private static void require(boolean ok, String message) {
        if (!ok) {
            throw new IllegalStateException(message);
        }
    }

    private static final class LoanDeductionValidator {
        private static boolean approve(ClaimRecord claim, AssessmentRecord assessment) {
            if (assessment == null) {
                return false;
            }
            if (!"1".equals(assessment.resultKbn)) {
                return false;
            }
            if (claim.sumAssuredAmt <= 0 || claim.eventDt < claim.respStartDt) {
                return false;
            }
            return "1".equals(assessment.authLevelKbn) || "2".equals(assessment.authLevelKbn);
        }
    }

    private static final class PayoutCalculationAuditor {
        private static void audit(ClaimRecord claim, int reductionRate, long payoutAmt) {
            require(reductionRate >= 0 && reductionRate <= FULL_PAYOUT_RATE_PCT,
                    "削減率が範囲外です claimId=" + claim.claimId);
            require(payoutAmt >= 0 && payoutAmt <= claim.sumAssuredAmt,
                    "支払額が範囲外です claimId=" + claim.claimId);
        }
    }

    private static final class LoanBalanceAdjustmentProcessor {
        private static long adjust(ClaimRecord claim, LoanRecord loan) {
            require(loan != null, "貸付残高明細が存在しません polNo=" + claim.polNo);

            long ledgerBalance = Math.max(0L, loan.loanAmt + loan.interestAmt);
            long confirmed = Math.min(claim.loanBalanceAmt, Math.max(ledgerBalance, loan.totalBalance));
            loan.totalBalance = confirmed;
            loan.lastUpdateDt = BATCH_DT;
            return confirmed;
        }
    }

    private static final class PaymentTransferOrchestrator {
        private static TransferRecord createTransfer(PayRecord pay, BeneficiaryRecord beneficiary) {
            require(pay.payoutAmt > 0, "振込金額がゼロです payId=" + pay.payId);
            return new TransferRecord("XFR" + pay.payId.substring(Math.max(0, pay.payId.length() - 10)),
                    pay.payId, beneficiary.bankCd, beneficiary.branchCd, beneficiary.acctNo,
                    beneficiary.nameKana, pay.payoutAmt, BATCH_DT);
        }
    }

    private static final class WithholdingTaxCalculator {
        private static WithholdingRecord calculate(PayRecord pay, BeneficiaryRecord beneficiary) {
            long taxable = Math.max(0L, pay.payoutAmt - TAX_EXEMPT_LIMIT);
            long tax = taxable * TAX_RATE_PER_MILLE / 1000L;
            String exempt = taxable == 0L ? "1" : "0";
            return new WithholdingRecord("WIT" + pay.payId.substring(Math.max(0, pay.payId.length() - 10)),
                    pay.payId, beneficiary.beneficiaryId, taxable, tax, BATCH_YM / 100, exempt);
        }
    }

    private static final class MonthlyAssessmentAggregator {
        private static PhaseTotal aggregate(java.util.List<PayRecord> pays) {
            PhaseTotal total = new PhaseTotal("06");
            for (PayRecord pay : pays) {
                total.count++;
                total.totalGrossAmt += pay.grossAmt;
                total.totalPayoutAmt += pay.payoutAmt;
                total.reductionRateSum += pay.reductionRate;
            }
            return total;
        }
    }

    private static final class MonthlyPaymentSummaryBatch {
        private static void writeControlTotals(java.util.List<MasterTotalRecord> out,
                                                java.util.List<PhaseTotal> totals) {
            for (PhaseTotal total : totals) {
                long avg = total.count == 0 ? 0L : total.reductionRateSum / total.count;
                out.add(new MasterTotalRecord(String.valueOf(BATCH_YM), total.phase, total.count,
                        total.totalGrossAmt, total.totalPayoutAmt, avg));
            }
        }
    }

    private static final class AssessmentReportFormatter {
        private static java.util.List<String> format(java.util.List<ClaimRecord> claims,
                                                      java.util.List<PayRecord> pays,
                                                      java.util.List<WithholdingRecord> taxes) {
            java.util.Map<String, PayRecord> payByClaim = new java.util.HashMap<String, PayRecord>();
            for (PayRecord pay : pays) {
                payByClaim.put(pay.claimId, pay);
            }

            java.util.Map<String, WithholdingRecord> taxByPay = new java.util.HashMap<String, WithholdingRecord>();
            for (WithholdingRecord tax : taxes) {
                taxByPay.put(tax.payId, tax);
            }

            java.util.List<String> lines = new java.util.ArrayList<String>();
            lines.add("査定書 " + BATCH_YM + " 保険金支払月次マスター");

            for (ClaimRecord claim : claims) {
                PayRecord pay = payByClaim.get(claim.claimId);
                if (pay == null) {
                    continue;
                }

                WithholdingRecord tax = taxByPay.get(pay.payId);
                long taxAmt = tax == null ? 0L : tax.taxAmt;
                lines.add(claim.claimId + " " + claim.polNo + " 支払額=" + pay.payoutAmt + " 源泉税=" + taxAmt);
            }

            return lines;
        }
    }

    private static final class CheckpointStore {
        private java.util.List<ClaimRecord> claimCheckpoint = new java.util.ArrayList<ClaimRecord>();
        private java.util.List<PayRecord> payCheckpoint = new java.util.ArrayList<PayRecord>();

        private void save(InMemoryFiles files) {
            claimCheckpoint = new java.util.ArrayList<ClaimRecord>();
            for (ClaimRecord rec : files.lfclmf) {
                claimCheckpoint.add(rec.copy());
            }

            payCheckpoint = new java.util.ArrayList<PayRecord>();
            for (PayRecord rec : files.lfpayf) {
                payCheckpoint.add(rec.copy());
            }
        }

        private void restore(InMemoryFiles files) {
            files.lfclmf.clear();
            for (ClaimRecord rec : claimCheckpoint) {
                files.lfclmf.add(rec.copy());
            }

            files.lfpayf.clear();
            for (PayRecord rec : payCheckpoint) {
                files.lfpayf.add(rec.copy());
            }
        }
    }

    private static final class InMemoryFiles {
        private final java.util.List<ClaimRecord> lfclmf = new java.util.ArrayList<ClaimRecord>();
        private final java.util.List<PayRecord> lfpayf = new java.util.ArrayList<PayRecord>();
        private final java.util.List<AssessmentRecord> lfrasf = new java.util.ArrayList<AssessmentRecord>();
        private final java.util.List<BeneficiaryRecord> lfbenf = new java.util.ArrayList<BeneficiaryRecord>();
        private final java.util.List<LoanRecord> lflanf = new java.util.ArrayList<LoanRecord>();
        private final java.util.List<TransferRecord> lfxfrf = new java.util.ArrayList<TransferRecord>();
        private final java.util.List<WithholdingRecord> lfwitf = new java.util.ArrayList<WithholdingRecord>();
        private final java.util.List<ReportRecord> lfrepmf = new java.util.ArrayList<ReportRecord>();
        private final java.util.List<MasterTotalRecord> lfmstf = new java.util.ArrayList<MasterTotalRecord>();

        private static InMemoryFiles synthetic() {
            InMemoryFiles f = new InMemoryFiles();
            f.lfclmf.add(new ClaimRecord("CLM000001", "POL100001", 10_000_000L, 240_000L, 20200101, 20240202, STATUS_LEGACY_APPROVED));
            f.lfclmf.add(new ClaimRecord("CLM000002", "POL100002", 8_000_000L, 0L, 20231201, 20240205, STATUS_LEGACY_REVIEWED));
            f.lfclmf.add(new ClaimRecord("CLM000003", "POL100003", 5_000_000L, 150_000L, 20220120, 20240211, STATUS_ASSESSING));

            f.lfrasf.add(new AssessmentRecord("ASM000001", "CLM000001", 20240220, "01", "2", "1", "A0192"));
            f.lfrasf.add(new AssessmentRecord("ASM000002", "CLM000002", 20240222, "01", "1", "1", "A0210"));
            f.lfrasf.add(new AssessmentRecord("ASM000003", "CLM000003", 20240223, "01", "1", "9", "A0104"));

            f.lfbenf.add(new BeneficiaryRecord("POL100001", "BEN000001", "ミライ タロウ", "01", "0005", "103", "1234567", 1));
            f.lfbenf.add(new BeneficiaryRecord("POL100002", "BEN000002", "ミライ ハナコ", "02", "0009", "221", "2345678", 1));
            f.lfbenf.add(new BeneficiaryRecord("POL100003", "BEN000003", "ミライ イチロウ", "01", "0001", "001", "3456789", 1));

            f.lflanf.add(new LoanRecord("POL100001", 200_000L, 40_000L, 240_000L, 20240131));
            f.lflanf.add(new LoanRecord("POL100003", 120_000L, 30_000L, 150_000L, 20240131));
            return f;
        }
    }

    private static final class ClaimRecord {
        private final String claimId;
        private final String polNo;
        private final long sumAssuredAmt;
        private long loanBalanceAmt;
        private final int respStartDt;
        private final int eventDt;
        private String claimStatusKbn;

        private ClaimRecord(String claimId, String polNo, long sumAssuredAmt, long loanBalanceAmt,
                            int respStartDt, int eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }

        private ClaimRecord copy() {
            return new ClaimRecord(claimId, polNo, sumAssuredAmt, loanBalanceAmt,
                    respStartDt, eventDt, claimStatusKbn);
        }
    }

    private static final class PayRecord {
        private final String payId;
        private final String claimId;
        private final long grossAmt;
        private final int reductionRate;
        private final long payoutAmt;

        private PayRecord(String payId, String claimId, long grossAmt, int reductionRate, long payoutAmt) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
        }

        private PayRecord copy() {
            return new PayRecord(payId, claimId, grossAmt, reductionRate, payoutAmt);
        }
    }

    private static final class AssessmentRecord {
        private final String assessId;
        private final String claimId;
        private final int assessDt;
        private final String categoryKbn;
        private final String authLevelKbn;
        private final String resultKbn;
        private final String assessorId;

        private AssessmentRecord(String assessId, String claimId, int assessDt, String categoryKbn,
                                 String authLevelKbn, String resultKbn, String assessorId) {
            this.assessId = assessId;
            this.claimId = claimId;
            this.assessDt = assessDt;
            this.categoryKbn = categoryKbn;
            this.authLevelKbn = authLevelKbn;
            this.resultKbn = resultKbn;
            this.assessorId = assessorId;
        }
    }

    private static final class BeneficiaryRecord {
        private final String polNo;
        private final String beneficiaryId;
        private final String nameKana;
        private final String relationshipKbn;
        private final String bankCd;
        private final String branchCd;
        private final String acctNo;
        private final int paymentPriority;

        private BeneficiaryRecord(String polNo, String beneficiaryId, String nameKana, String relationshipKbn,
                                  String bankCd, String branchCd, String acctNo, int paymentPriority) {
            this.polNo = polNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.paymentPriority = paymentPriority;
        }
    }

    private static final class LoanRecord {
        private final String polNo;
        private final long loanAmt;
        private final long interestAmt;
        private long totalBalance;
        private int lastUpdateDt;

        private LoanRecord(String polNo, long loanAmt, long interestAmt, long totalBalance, int lastUpdateDt) {
            this.polNo = polNo;
            this.loanAmt = loanAmt;
            this.interestAmt = interestAmt;
            this.totalBalance = totalBalance;
            this.lastUpdateDt = lastUpdateDt;
        }
    }

    private static final class TransferRecord {
        private final String transferId;
        private final String payId;
        private final String bankCd;
        private final String branchCd;
        private final String acctNo;
        private final String acctHolderKna;
        private final long amount;
        private final int transferDt;

        private TransferRecord(String transferId, String payId, String bankCd, String branchCd,
                               String acctNo, String acctHolderKna, long amount, int transferDt) {
            this.transferId = transferId;
            this.payId = payId;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.acctHolderKna = acctHolderKna;
            this.amount = amount;
            this.transferDt = transferDt;
        }
    }

    private static final class WithholdingRecord {
        private final String reportId;
        private final String payId;
        private final String beneficiaryId;
        private final long taxableAmt;
        private final long taxAmt;
        private final int taxYear;
        private final String taxExemptFlg;

        private WithholdingRecord(String reportId, String payId, String beneficiaryId,
                                  long taxableAmt, long taxAmt, int taxYear, String taxExemptFlg) {
            this.reportId = reportId;
            this.payId = payId;
            this.beneficiaryId = beneficiaryId;
            this.taxableAmt = taxableAmt;
            this.taxAmt = taxAmt;
            this.taxYear = taxYear;
            this.taxExemptFlg = taxExemptFlg;
        }
    }

    private static final class ReportRecord {
        private final String reportId;
        private final String reportTypeKbn;
        private final int outputDt;
        private final int pageNo;
        private final String lineData;

        private ReportRecord(String reportId, String reportTypeKbn, int outputDt, int pageNo, String lineData) {
            this.reportId = reportId;
            this.reportTypeKbn = reportTypeKbn;
            this.outputDt = outputDt;
            this.pageNo = pageNo;
            this.lineData = lineData;
        }
    }

    private static final class MasterTotalRecord {
        private final String yearMonth;
        private final String categoryKbn;
        private final long count;
        private final long totalGrossAmt;
        private final long totalPayoutAmt;
        private final long avgReductionRate;

        private MasterTotalRecord(String yearMonth, String categoryKbn, long count,
                                  long totalGrossAmt, long totalPayoutAmt, long avgReductionRate) {
            this.yearMonth = yearMonth;
            this.categoryKbn = categoryKbn;
            this.count = count;
            this.totalGrossAmt = totalGrossAmt;
            this.totalPayoutAmt = totalPayoutAmt;
            this.avgReductionRate = avgReductionRate;
        }
    }

    private static final class PhaseTotal {
        private String phase;
        private long count;
        private long totalGrossAmt;
        private long totalPayoutAmt;
        private long reductionRateSum;

        private PhaseTotal(String phase) {
            this.phase = phase;
        }
    }
}
