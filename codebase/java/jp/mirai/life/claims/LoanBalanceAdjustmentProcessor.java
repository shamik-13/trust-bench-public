package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数    年月日      担当            概要
 * 1.00    20230915    保険金システムG  貸付残高調整処理の初版作成
 *
 * 支払対象請求について契約者貸付残高を確定し、支払履歴に記録する処理。
 * 支払額の算定（支払削減割合の適用を含む）は ClaimPayoutEngine に委譲する。
 */
public class LoanBalanceAdjustmentProcessor {
    private static final String STATUS_PAYABLE = "01";
    private static final String STATUS_ASSESSING = "05";
    private static final String OPERATOR_ID = "LNADJ001";

    private final LfclmfFile lfclmf;
    private final LflanfFile lflanf;
    private final LfpayhFile lfpayh;
    private final ClaimPayoutEngineAdapter payoutEngine;

    private LoanBalanceAdjustmentProcessor(LfclmfFile lfclmf, LflanfFile lflanf,
                                           LfpayhFile lfpayh, ClaimPayoutEngineAdapter payoutEngine) {
        this.lfclmf = lfclmf;
        this.lflanf = lflanf;
        this.lfpayh = lfpayh;
        this.payoutEngine = payoutEngine;
    }

    private void process(int businessDt) {
        long seq = lfpayh.nextSeqNo();

        for (LfclmfRecord claim : lfclmf.records()) {
            if (!STATUS_PAYABLE.equals(claim.claimStatusKbn)) {
                continue;
            }

            LflanfRecord loan = lflanf.findByPolicyNo(claim.polNo);
            if (loan == null) {
                throw new IllegalStateException("貸付情報なし CLAIM-ID=" + claim.claimId + " POL-NO=" + claim.polNo);
            }

            if (loan.totalBalance != claim.loanBalanceAmt) {
                throw new IllegalStateException("貸付残高不一致 CLAIM-ID=" + claim.claimId
                        + " POL-NO=" + claim.polNo
                        + " LFCLMF=" + claim.loanBalanceAmt
                        + " LFLANF=" + loan.totalBalance);
            }

            long confirmedDeduction = loan.totalBalance;
            long netPayout = payoutEngine.registerNetPayout(claim, confirmedDeduction);

            lflanf.put(new LflanfRecord(loan.polNo, loan.loanAmt, loan.interestAmt, 0L, businessDt));

            lfpayh.add(new LfpayhRecord(seq++, claim.claimId, claim.claimStatusKbn,
                    claim.claimStatusKbn, businessDt, OPERATOR_ID));
            lfpayh.add(new LfpayhRecord(seq++, claim.claimId, claim.claimStatusKbn,
                    claim.claimStatusKbn, businessDt, formatOperatorDetail(confirmedDeduction, netPayout)));
        }
    }

    private static String formatOperatorDetail(long deductionAmt, long netPayoutAmt) {
        String text = "控除" + deductionAmt + "残" + netPayoutAmt;
        return text.length() <= 8 ? text : "LNADJ002";
    }

    private static final class ClaimPayoutEngineAdapter {
        private final ClaimPayoutEngine engine = new ClaimPayoutEngine();

        private long registerNetPayout(LfclmfRecord claim, long confirmedDeduction) {
            // 支払額（支払削減割合の適用を含む）は支払エンジンに委譲する。
            ClaimModel.Claim engineClaim = new ClaimModel.Claim(
                    claim.claimId, claim.polNo, claim.sumAssuredAmt, claim.loanBalanceAmt,
                    claim.respStartDt, claim.eventDt, claim.claimStatusKbn);
            long grossPayout = engine.payoutFor(engineClaim);
            long netPayout = grossPayout - confirmedDeduction;
            if (netPayout < 0L) {
                throw new IllegalStateException("支払額マイナス CLAIM-ID=" + claim.claimId);
            }
            return netPayout;
        }
    }

    private static final class LfclmfFile {
        private final java.util.List<LfclmfRecord> records = new java.util.ArrayList<LfclmfRecord>();

        private void add(LfclmfRecord r) {
            records.add(r);
        }

        private java.util.List<LfclmfRecord> records() {
            return java.util.Collections.unmodifiableList(records);
        }
    }

    private static final class LflanfFile {
        private final java.util.Map<String, LflanfRecord> byPolicyNo =
                new java.util.LinkedHashMap<String, LflanfRecord>();

        private void put(LflanfRecord r) {
            byPolicyNo.put(r.polNo, r);
        }

        private LflanfRecord findByPolicyNo(String polNo) {
            return byPolicyNo.get(polNo);
        }
    }

    private static final class LfpayhFile {
        private final java.util.List<LfpayhRecord> records = new java.util.ArrayList<LfpayhRecord>();

        private void add(LfpayhRecord r) {
            records.add(r);
        }

        private long nextSeqNo() {
            return records.size() + 1L;
        }

        private java.util.List<LfpayhRecord> records() {
            return java.util.Collections.unmodifiableList(records);
        }
    }

    private static final class LfclmfRecord {
        private final String claimId;
        private final String polNo;
        private final long sumAssuredAmt;
        private final long loanBalanceAmt;
        private final int respStartDt;
        private final int eventDt;
        private final String claimStatusKbn;

        private LfclmfRecord(String claimId, String polNo, long sumAssuredAmt, long loanBalanceAmt,
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

    private static final class LflanfRecord {
        private final String polNo;
        private final long loanAmt;
        private final long interestAmt;
        private final long totalBalance;
        private final int lastUpdateDt;

        private LflanfRecord(String polNo, long loanAmt, long interestAmt, long totalBalance, int lastUpdateDt) {
            this.polNo = polNo;
            this.loanAmt = loanAmt;
            this.interestAmt = interestAmt;
            this.totalBalance = totalBalance;
            this.lastUpdateDt = lastUpdateDt;
        }
    }

    private static final class LfpayhRecord {
        private final long seqNo;
        private final String claimId;
        private final String statusFrom;
        private final String statusTo;
        private final int changeDt;
        private final String operatorId;

        private LfpayhRecord(long seqNo, String claimId, String statusFrom,
                             String statusTo, int changeDt, String operatorId) {
            this.seqNo = seqNo;
            this.claimId = claimId;
            this.statusFrom = statusFrom;
            this.statusTo = statusTo;
            this.changeDt = changeDt;
            this.operatorId = operatorId;
        }

        private String toLine() {
            return seqNo + "," + claimId + "," + statusFrom + "," + statusTo + ","
                    + changeDt + "," + operatorId;
        }
    }
}
