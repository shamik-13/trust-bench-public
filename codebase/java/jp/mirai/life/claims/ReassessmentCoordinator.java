package jp.mirai.life.claims;

import java.util.ArrayList;
import java.util.List;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.00  20240712    保険金システムG  再査定連携の現行判定前ロジックを作成
 *
 * 再査定対象の請求について、現行支払を再計算して履歴に退避する連携サービス。
 * 支払額の再計算（支払削減割合の適用を含む）は ClaimPayoutEngine に委譲し、
 * 当サービスは削減割合を保持しない（割合は約款/支払規程の決裁値による）。
 */
public class ReassessmentCoordinator {
    private static final String CL_STATUS_PAYABLE = "01";
    private static final String CL_STATUS_REASSESS_REQUEST = "25";
    private static final String PH_STATUS_FROM = "40";
    private static final String PH_STATUS_TO = "35";

    void execute() {
        DataStore store = DataStore.synthetic();
        int processed = 0;
        long totalDelta = 0L;

        List<AssessmentRecord> targets = new ArrayList<AssessmentRecord>(store.assessments);
        for (AssessmentRecord assessment : targets) {
            if (!"再査定".equals(assessment.categoryKbn)) {
                continue;
            }

            ClaimFileRecord claim = store.findClaim(assessment.claimId);
            PayoutFileRecord currentPayout = store.findPayout(assessment.claimId);

            if (claim == null) {
                store.log("請求が見つかりません CLAIM-ID=" + assessment.claimId);
                continue;
            }
            if (currentPayout == null) {
                store.log("現行支払が見つかりません CLAIM-ID=" + assessment.claimId);
                continue;
            }
            if (!CL_STATUS_PAYABLE.equals(claim.claimStatusKbn)) {
                store.log("支払対象外のため再査定を保留 CLAIM-ID=" + claim.claimId + " STATUS=" + claim.claimStatusKbn);
                continue;
            }

            PayoutHistoryRecord archive = new PayoutHistoryRecord(
                    store.nextHistorySeq(),
                    claim.claimId,
                    PH_STATUS_FROM,
                    PH_STATUS_TO,
                    assessment.assessDt,
                    assessment.assessorId);
            store.writeHistory(archive);

            ClaimPayoutEngine engine = new ClaimPayoutEngine();
            ClaimModel.Claim engineClaim = new ClaimModel.Claim(
                    claim.claimId, claim.polNo, claim.sumAssuredAmt, claim.loanBalanceAmt,
                    claim.respStartDt, claim.eventDt, claim.claimStatusKbn);
            int ratePct = engine.reductionRatePct(claim.respStartDt, claim.eventDt);
            long recomputedPayoutAmount = engine.payoutFor(engineClaim);

            PayoutFileRecord newPayout = new PayoutFileRecord(
                    currentPayout.payId,
                    currentPayout.claimId,
                    currentPayout.grossAmt,
                    ratePct,
                    recomputedPayoutAmount);

            PayoutCalculationAuditor auditor = new PayoutCalculationAuditor();
            AuditResult audit = auditor.validate(newPayout, claim);
            if (!audit.ok) {
                store.log("支払監査エラー CLAIM-ID=" + claim.claimId + " 理由=" + audit.message);
                continue;
            }

            claim.claimStatusKbn = CL_STATUS_REASSESS_REQUEST;
            store.replacePayout(newPayout);

            long delta = newPayout.payoutAmt - currentPayout.payoutAmt;
            totalDelta += delta;
            processed++;

            AssessmentRecord reassessmentResult = new AssessmentRecord(
                    store.nextAssessmentId(),
                    claim.claimId,
                    assessment.assessDt,
                    assessment.categoryKbn,
                    String.valueOf(delta),
                    assessment.resultKbn,
                    assessment.assessorId);
            store.writeAssessment(reassessmentResult);

            store.log("再査定完了 CLAIM-ID=" + claim.claimId
                    + " 旧支払額=" + currentPayout.payoutAmt
                    + " 新支払額=" + newPayout.payoutAmt
                    + " 差額=" + delta
                    + " 理由=" + assessment.resultKbn);
        }

        store.log("再査定件数=" + processed + " 差額合計=" + totalDelta);
    }

    private static final class DataStore {
        private final List<AssessmentRecord> assessments = new ArrayList<AssessmentRecord>();
        private final List<PayoutFileRecord> payouts = new ArrayList<PayoutFileRecord>();
        private final List<ClaimFileRecord> claims = new ArrayList<ClaimFileRecord>();
        private final List<PayoutHistoryRecord> histories = new ArrayList<PayoutHistoryRecord>();
        private int historySeq = 9000;
        private int assessmentSeq = 7000;

        private static DataStore synthetic() {
            DataStore store = new DataStore();
            store.assessments.add(new AssessmentRecord("AS000001", "CLM00001", 20240701, "再査定", "2", "事由A", "OP1001"));
            store.assessments.add(new AssessmentRecord("AS000002", "CLM00002", 20240701, "再査定", "1", "事由B", "OP1002"));
            store.assessments.add(new AssessmentRecord("AS000003", "CLM00003", 20240701, "通常", "1", "事由C", "OP1003"));

            store.payouts.add(new PayoutFileRecord("PY000001", "CLM00001", 5000000L, 100, 4200000L));
            store.payouts.add(new PayoutFileRecord("PY000002", "CLM00002", 3000000L, 100, 2600000L));
            store.payouts.add(new PayoutFileRecord("PY000003", "CLM00003", 2000000L, 100, 1700000L));

            store.claims.add(new ClaimFileRecord("CLM00001", "POL000001", 5000000L, 800000L, 20200110, 20240520, CL_STATUS_PAYABLE));
            store.claims.add(new ClaimFileRecord("CLM00002", "POL000002", 3000000L, 400000L, 20240401, 20240610, CL_STATUS_PAYABLE));
            store.claims.add(new ClaimFileRecord("CLM00003", "POL000003", 2000000L, 300000L, 20190401, 20240615, "05"));
            return store;
        }

        private ClaimFileRecord findClaim(String claimId) {
            for (ClaimFileRecord claim : claims) {
                if (claim.claimId.equals(claimId)) {
                    return claim;
                }
            }
            return null;
        }

        private PayoutFileRecord findPayout(String claimId) {
            for (PayoutFileRecord payout : payouts) {
                if (payout.claimId.equals(claimId)) {
                    return payout;
                }
            }
            return null;
        }

        private int nextHistorySeq() {
            return ++historySeq;
        }

        private String nextAssessmentId() {
            return "AS" + (++assessmentSeq);
        }

        private void writeHistory(PayoutHistoryRecord record) {
            histories.add(record);
        }

        private void writeAssessment(AssessmentRecord record) {
            assessments.add(record);
        }

        private void replacePayout(PayoutFileRecord newRecord) {
            for (int i = 0; i < payouts.size(); i++) {
                if (payouts.get(i).payId.equals(newRecord.payId)) {
                    payouts.set(i, newRecord);
                    return;
                }
            }
            payouts.add(newRecord);
        }

        private void log(String text) {
            System.out.println(text);
        }
    }

    private static final class PayoutCalculationAuditor {
        private AuditResult validate(PayoutFileRecord payout, ClaimFileRecord claim) {
            if (!payout.claimId.equals(claim.claimId)) {
                return new AuditResult(false, "請求番号不一致");
            }
            if (payout.grossAmt < 0L || payout.payoutAmt < 0L) {
                return new AuditResult(false, "金額不正");
            }
            if (payout.payoutAmt > payout.grossAmt) {
                return new AuditResult(false, "支払額超過");
            }
            if (payout.reductionRate < 0 || payout.reductionRate > 100) {
                return new AuditResult(false, "削減率不正");
            }
            return new AuditResult(true, "正常");
        }
    }

    private static final class AuditResult {
        private final boolean ok;
        private final String message;

        private AuditResult(boolean ok, String message) {
            this.ok = ok;
            this.message = message;
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

    private static final class PayoutFileRecord {
        private final String payId;
        private final String claimId;
        private final long grossAmt;
        private final int reductionRate;
        private final long payoutAmt;

        private PayoutFileRecord(String payId, String claimId, long grossAmt, int reductionRate, long payoutAmt) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
        }
    }

    private static final class ClaimFileRecord {
        private final String claimId;
        private final String polNo;
        private final long sumAssuredAmt;
        private final long loanBalanceAmt;
        private final int respStartDt;
        private final int eventDt;
        private String claimStatusKbn;

        private ClaimFileRecord(String claimId, String polNo, long sumAssuredAmt, long loanBalanceAmt,
                                int respStartDt, int eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }
    }

    private static final class PayoutHistoryRecord {
        private final int seqNo;
        private final String claimId;
        private final String statusFrom;
        private final String statusTo;
        private final int changeDt;
        private final String operatorId;

        private PayoutHistoryRecord(int seqNo, String claimId, String statusFrom, String statusTo,
                                    int changeDt, String operatorId) {
            this.seqNo = seqNo;
            this.claimId = claimId;
            this.statusFrom = statusFrom;
            this.statusTo = statusTo;
            this.changeDt = changeDt;
            this.operatorId = operatorId;
        }
    }
}
