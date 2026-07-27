package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.0   2024-06-18  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class ChargebackResponseExporter {
    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";
    private static final String DECLINE_WINDOW = "WIN";
    private static final String DECLINE_AMOUNT = "AMT";
    private static final String DECLINE_TRANSACTION = "TXN";

    private static final String STATUS_OPEN = "10";
    private static final String STATUS_APPROVED = "40";
    private static final String STATUS_DECLINED = "50";
    private static final String STATUS_REQUERY = "80";
    private static final String STATUS_SENT = "90";

    public static void main(String[] a) {
        ChargebackResponseExporter exporter = new ChargebackResponseExporter();
        ExportResult result = exporter.export(buildPrrspf(), buildPrcbf(), buildSentCases());

        System.out.println("処理件数=" + result.readCount);
        System.out.println("更新件数=" + result.updateCount);
        System.out.println("再照会件数=" + result.requeryCount);
        System.out.println("除外件数=" + result.skipCount);
        System.out.println("警告件数=" + result.warningCount);

        for (PrcbfRecord record : result.updatedRecords.values()) {
            System.out.println(
                    "CASE-ID=" + record.caseId
                            + ", ORIG-TXN-ID=" + record.origTxnId
                            + ", CARD-SCHEME=" + record.cardScheme
                            + ", DISPUTE-AMT=" + record.disputeAmt
                            + ", DISPUTE-DT=" + record.disputeDt
                            + ", STATUS-KBN=" + record.statusKbn);
        }
    }

    private ExportResult export(PrrspfRecord[] responses, PrcbfRecord[] cases, SentCase[] sentCases) {
        java.util.Map<String, PrcbfRecord> caseByTxn = new java.util.LinkedHashMap<String, PrcbfRecord>();
        java.util.Map<String, PrcbfRecord> updated = new java.util.LinkedHashMap<String, PrcbfRecord>();
        java.util.Map<String, SentCase> sentByCase = new java.util.HashMap<String, SentCase>();
        java.util.Set<String> duplicatedTxn = new java.util.HashSet<String>();

        for (PrcbfRecord record : cases) {
            PrcbfRecord previous = caseByTxn.put(record.origTxnId, record);
            if (previous != null) {
                duplicatedTxn.add(record.origTxnId);
            }
            updated.put(record.caseId, record);
        }

        for (SentCase sent : sentCases) {
            sentByCase.put(sent.caseId, sent);
        }

        int readCount = 0;
        int updateCount = 0;
        int requeryCount = 0;
        int skipCount = 0;
        int warningCount = 0;

        for (PrrspfRecord response : responses) {
            readCount++;

            if (!isValidResponse(response)) {
                warningCount++;
                continue;
            }

            if (duplicatedTxn.contains(response.origTxnId)) {
                warningCount++;
                continue;
            }

            PrcbfRecord target = caseByTxn.get(response.origTxnId);
            if (target == null) {
                warningCount++;
                continue;
            }

            if (!STATUS_OPEN.equals(target.statusKbn) && !STATUS_REQUERY.equals(target.statusKbn)) {
                skipCount++;
                continue;
            }

            String decidedStatus = toCaseStatus(response);
            if (decidedStatus == null) {
                warningCount++;
                continue;
            }

            if (DECISION_ACCEPT.equals(response.decisionKbn) && response.eligibleAmt > target.disputeAmt) {
                decidedStatus = STATUS_DECLINED;
                response = new PrrspfRecord(
                        response.reqId,
                        response.origTxnId,
                        DECISION_DECLINE,
                        DECLINE_AMOUNT,
                        response.eligibleAmt);
            }

            SentCase sent = sentByCase.get(target.caseId);
            if (sent != null) {
                if (hasDiff(sent, decidedStatus, response)) {
                    PrcbfRecord requeryRecord = target.withStatus(STATUS_REQUERY);
                    updated.put(requeryRecord.caseId, requeryRecord);
                    requeryCount++;
                } else {
                    skipCount++;
                }
                continue;
            }

            if (!target.statusKbn.equals(decidedStatus)) {
                PrcbfRecord written = target.withStatus(decidedStatus);
                updated.put(written.caseId, written);
                updateCount++;
            }
        }

        return new ExportResult(readCount, updateCount, requeryCount, skipCount, warningCount, updated);
    }

    private static boolean isValidResponse(PrrspfRecord response) {
        if (response.reqId == null || response.reqId.length() == 0) {
            return false;
        }
        if (response.origTxnId == null || response.origTxnId.length() == 0) {
            return false;
        }
        if (DECISION_ACCEPT.equals(response.decisionKbn)) {
            return response.declineReason.length() == 0 && response.eligibleAmt >= 0L;
        }
        if (DECISION_DECLINE.equals(response.decisionKbn)) {
            return isDeclineReason(response.declineReason);
        }
        return false;
    }

    private static boolean isDeclineReason(String declineReason) {
        return DECLINE_WINDOW.equals(declineReason)
                || DECLINE_AMOUNT.equals(declineReason)
                || DECLINE_TRANSACTION.equals(declineReason);
    }

    private static String toCaseStatus(PrrspfRecord response) {
        if (DECISION_ACCEPT.equals(response.decisionKbn)) {
            return STATUS_APPROVED;
        }
        if (DECISION_DECLINE.equals(response.decisionKbn)) {
            return STATUS_DECLINED;
        }
        return null;
    }

    private static boolean hasDiff(SentCase sent, String decidedStatus, PrrspfRecord response) {
        if (!sent.sentStatusKbn.equals(decidedStatus)) {
            return true;
        }
        if (sent.sentEligibleAmt != response.eligibleAmt) {
            return true;
        }
        return !sent.sentDeclineReason.equals(response.declineReason);
    }

    private static PrrspfRecord[] buildPrrspf() {
        return new PrrspfRecord[] {
                new PrrspfRecord("RS202606280001", "TXN202606010001", DECISION_ACCEPT, "", 12800L),
                new PrrspfRecord("RS202606280002", "TXN202606010002", DECISION_DECLINE, DECLINE_WINDOW, 0L),
                new PrrspfRecord("RS202606280003", "TXN202606010003", DECISION_ACCEPT, "", 8800L),
                new PrrspfRecord("RS202606280004", "TXN202606010004", DECISION_DECLINE, DECLINE_TRANSACTION, 0L),
                new PrrspfRecord("RS202606280005", "TXN202606010005", DECISION_ACCEPT, "", 42000L),
                new PrrspfRecord("RS202606280006", "TXN202606010006", DECISION_DECLINE, DECLINE_AMOUNT, 0L),
                new PrrspfRecord("RS202606280007", "TXN202606019999", DECISION_ACCEPT, "", 1900L),
                new PrrspfRecord("RS202606280008", "TXN202606010008", DECISION_ACCEPT, "", 7600L)
        };
    }

    private static PrcbfRecord[] buildPrcbf() {
        return new PrcbfRecord[] {
                new PrcbfRecord("CB202606150001", "TXN202606010001", "VISA", 12800L, "20260615", STATUS_OPEN),
                new PrcbfRecord("CB202606150002", "TXN202606010002", "JCB", 5400L, "20260615", STATUS_OPEN),
                new PrcbfRecord("CB202606150003", "TXN202606010003", "MC", 8800L, "20260616", STATUS_SENT),
                new PrcbfRecord("CB202606150004", "TXN202606010004", "AMEX", 3100L, "20260616", STATUS_OPEN),
                new PrcbfRecord("CB202606150005", "TXN202606010005", "VISA", 38000L, "20260617", STATUS_OPEN),
                new PrcbfRecord("CB202606150006", "TXN202606010006", "JCB", 22000L, "20260618", STATUS_OPEN),
                new PrcbfRecord("CB202606150008", "TXN202606010008", "MC", 7600L, "20260619", STATUS_OPEN)
        };
    }

    private static SentCase[] buildSentCases() {
        return new SentCase[] {
                new SentCase("CB202606150003", STATUS_APPROVED, 8200L, "")
        };
    }

    private static final class PrrspfRecord {
        private final String reqId;
        private final String origTxnId;
        private final String decisionKbn;
        private final String declineReason;
        private final long eligibleAmt;

        private PrrspfRecord(String reqId, String origTxnId, String decisionKbn, String declineReason, long eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class PrcbfRecord {
        private final String caseId;
        private final String origTxnId;
        private final String cardScheme;
        private final long disputeAmt;
        private final String disputeDt;
        private final String statusKbn;

        private PrcbfRecord(String caseId, String origTxnId, String cardScheme, long disputeAmt, String disputeDt, String statusKbn) {
            this.caseId = caseId;
            this.origTxnId = origTxnId;
            this.cardScheme = cardScheme;
            this.disputeAmt = disputeAmt;
            this.disputeDt = disputeDt;
            this.statusKbn = statusKbn;
        }

        private PrcbfRecord withStatus(String nextStatusKbn) {
            return new PrcbfRecord(caseId, origTxnId, cardScheme, disputeAmt, disputeDt, nextStatusKbn);
        }
    }

    private static final class SentCase {
        private final String caseId;
        private final String sentStatusKbn;
        private final long sentEligibleAmt;
        private final String sentDeclineReason;

        private SentCase(String caseId, String sentStatusKbn, long sentEligibleAmt, String sentDeclineReason) {
            this.caseId = caseId;
            this.sentStatusKbn = sentStatusKbn;
            this.sentEligibleAmt = sentEligibleAmt;
            this.sentDeclineReason = sentDeclineReason;
        }
    }

    private static final class ExportResult {
        private final int readCount;
        private final int updateCount;
        private final int requeryCount;
        private final int skipCount;
        private final int warningCount;
        private final java.util.Map<String, PrcbfRecord> updatedRecords;

        private ExportResult(
                int readCount,
                int updateCount,
                int requeryCount,
                int skipCount,
                int warningCount,
                java.util.Map<String, PrcbfRecord> updatedRecords) {
            this.readCount = readCount;
            this.updateCount = updateCount;
            this.requeryCount = requeryCount;
            this.skipCount = skipCount;
            this.warningCount = warningCount;
            this.updatedRecords = updatedRecords;
        }
    }
}
