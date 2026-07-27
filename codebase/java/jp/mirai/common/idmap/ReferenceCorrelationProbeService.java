package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2025-04-08  共通基盤    参照番号相関確認サービス初版
 */
public class ReferenceCorrelationProbeService {
    private static final String STATUS_CONFIRMED = "01";
    private static final String STATUS_CANCELLED = "09";
    private static final String AUDIT_ACTIVE = "01";
    private static final String JOURNAL_ACTIVE = "01";

    private static final String EVENT_OK = "71";
    private static final String EVENT_UNREACHED = "72";
    private static final String EVENT_AUDIT_MISMATCH = "73";
    private static final String EVENT_JOURNAL_MISSING = "74";

    private static final long JOURNAL_START_SEQ = 920000000001L;

    private static final Cmtxnf[] CMTXNF = {
            new Cmtxnf("T202504080001", "BK", 1002003001L, 1250000L, "01"),
            new Cmtxnf("T202504080002", "SC", 2003004002L, 380000L, "01"),
            new Cmtxnf("T202504080003", "CD", 3004005003L, 48200L, "01"),
            new Cmtxnf("T202504080004", "PY", 4005006004L, 1980L, "09"),
            new Cmtxnf("T202504080005", "LF", 5006007005L, 760000L, "01"),
            new Cmtxnf("T202504080006", "CM", 6007008006L, 0L, "01"),
            new Cmtxnf("T202504080007", "BK", 1002003007L, 9100000L, "01"),
            new Cmtxnf("T202504080008", "SC", 2003004008L, 540000L, "01")
    };

    private static final Cajrnf[] INITIAL_CAJRNF = {
            new Cajrnf(810000000001L, "A202504080001", 0L, "21", "01"),
            new Cajrnf(810000000002L, "A202504080002", 0L, "21", "01"),
            new Cajrnf(810000000003L, "A202504080003", 0L, "21", "09"),
            new Cajrnf(810000000004L, "A202504080005", 0L, "21", "01"),
            new Cajrnf(810000000005L, "A202504080006", 0L, "21", "01"),
            new Cajrnf(810000000006L, "A202504080007X", 0L, "21", "01")
    };

    private static Cajrnf[] CAJRNF = INITIAL_CAJRNF;

    private ReferenceCorrelationProbeService() {
    }

    public static void main(String[] a) {
        IdMapGateway idMap = new IdMapGateway();
        Cmaudf[] auditRows = buildCmaudf(idMap);
        ProbeSummary summary = probe(idMap, CMTXNF, auditRows);

        System.out.println("参照番号相関確認サービス");
        System.out.println("確認件数=" + summary.totalCount
                + " 到達可=" + summary.reachedCount
                + " 監査一致=" + summary.auditMatchedCount
                + " ジャーナル有=" + summary.journalFoundCount
                + " 異常=" + summary.exceptionCount);
        System.out.println("CAJRNF追記件数=" + summary.writtenCount);
    }

    private static ProbeSummary probe(IdMapGateway idMap, Cmtxnf[] transactions, Cmaudf[] audits) {
        ProbeSummary summary = new ProbeSummary();
        long nextJournalSeq = JOURNAL_START_SEQ;

        for (int i = 0; i < transactions.length; i++) {
            Cmtxnf txn = transactions[i];
            if (!STATUS_CONFIRMED.equals(txn.txnStatusKbn)) {
                continue;
            }

            summary.totalCount++;
            long issuedGroupRef = idMap.toGroupRef(txn.companyCode, txn.localTxnNo);
            String reverseCompany = idMap.companyOf(issuedGroupRef);
            long reverseLocalNo = idMap.localNoOf(issuedGroupRef);
            boolean reached = txn.companyCode.equals(reverseCompany) && txn.localTxnNo == reverseLocalNo;

            Cmaudf audit = findAudit(audits, issuedGroupRef, txn.companyCode, txn.localTxnNo);
            boolean auditMatched = audit != null && AUDIT_ACTIVE.equals(audit.auditStatusKbn);
            boolean journalFound = auditMatched && existsJournal(audit.auditId, issuedGroupRef);

            String eventType = decideEventType(reached, auditMatched, journalFound);
            String auditId = audit == null ? "AUDIT-NOT-FOUND" : audit.auditId;
            appendJournal(new Cajrnf(nextJournalSeq++, auditId, issuedGroupRef, eventType, JOURNAL_ACTIVE));

            if (reached) {
                summary.reachedCount++;
            }
            if (auditMatched) {
                summary.auditMatchedCount++;
            }
            if (journalFound) {
                summary.journalFoundCount++;
            }
            if (!EVENT_OK.equals(eventType)) {
                summary.exceptionCount++;
            }
            summary.writtenCount++;
        }

        return summary;
    }

    private static Cmaudf[] buildCmaudf(IdMapGateway idMap) {
        return new Cmaudf[]{
                new Cmaudf("A202504080001", idMap.toGroupRef("BK", 1002003001L), "BK", 1002003001L, "01"),
                new Cmaudf("A202504080002", idMap.toGroupRef("SC", 2003004002L), "SC", 2003004002L, "01"),
                new Cmaudf("A202504080003", idMap.toGroupRef("CD", 3004005003L), "CD", 3004005003L, "01"),
                new Cmaudf("A202504080004", idMap.toGroupRef("PY", 4005006004L), "PY", 4005006004L, "01"),
                new Cmaudf("A202504080005", idMap.toGroupRef("LF", 5006007005L), "LF", 5006007005L, "01"),
                new Cmaudf("A202504080006", idMap.toGroupRef("CM", 6007008006L), "CM", 6007008006L, "01"),
                new Cmaudf("A202504080007", idMap.toGroupRef("BK", 1002003007L), "BK", 1002003007L, "09"),
                new Cmaudf("A202504080008", idMap.toGroupRef("SC", 2003004008L), "SC", 2003004999L, "01")
        };
    }

    private static Cmaudf findAudit(Cmaudf[] audits, long groupRefNo, String companyCode, long localTxnNo) {
        for (int i = 0; i < audits.length; i++) {
            Cmaudf audit = audits[i];
            if (audit.groupRefNo == groupRefNo
                    && companyCode.equals(audit.companyCode)
                    && audit.localTxnNo == localTxnNo) {
                return audit;
            }
        }
        return null;
    }

    private static boolean existsJournal(String auditId, long groupRefNo) {
        for (int i = 0; i < CAJRNF.length; i++) {
            Cajrnf journal = CAJRNF[i];
            if (auditId.equals(journal.auditId)
                    && journal.groupRefNo == groupRefNo
                    && JOURNAL_ACTIVE.equals(journal.journalStatusKbn)) {
                return true;
            }
        }
        return false;
    }

    private static String decideEventType(boolean reached, boolean auditMatched, boolean journalFound) {
        if (!reached) {
            return EVENT_UNREACHED;
        }
        if (!auditMatched) {
            return EVENT_AUDIT_MISMATCH;
        }
        if (!journalFound) {
            return EVENT_JOURNAL_MISSING;
        }
        return EVENT_OK;
    }

    private static void appendJournal(Cajrnf entry) {
        Cajrnf[] expanded = new Cajrnf[CAJRNF.length + 1];
        System.arraycopy(CAJRNF, 0, expanded, 0, CAJRNF.length);
        expanded[expanded.length - 1] = entry;
        CAJRNF = expanded;
    }

    private static final class IdMapGateway {
        private final GroupRefService groupRefService = new GroupRefService();
        private final AuditLinkService auditLinkService = new AuditLinkService();

        long toGroupRef(String companyCode, long localTxnNo) {
            try {
                return groupRefService.toGroupRef(companyCode, localTxnNo);
            } catch (RuntimeException e) {
                throw new IllegalStateException("グループ参照番号発行呼出に失敗しました", e);
            }
        }

        String companyOf(long groupRefNo) {
            try {
                return auditLinkService.companyOf(groupRefNo);
            } catch (RuntimeException e) {
                throw new IllegalStateException("会社コード逆引呼出に失敗しました", e);
            }
        }

        long localNoOf(long groupRefNo) {
            try {
                return auditLinkService.localNoOf(groupRefNo);
            } catch (RuntimeException e) {
                throw new IllegalStateException("ローカル取引番号逆引呼出に失敗しました", e);
            }
        }
    }

    private static final class Cmtxnf {
        final String txnId;
        final String companyCode;
        final long localTxnNo;
        final long txnAmt;
        final String txnStatusKbn;

        Cmtxnf(String txnId, String companyCode, long localTxnNo, long txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class Cmaudf {
        final String auditId;
        final long groupRefNo;
        final String companyCode;
        final long localTxnNo;
        final String auditStatusKbn;

        Cmaudf(String auditId, long groupRefNo, String companyCode, long localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    private static final class Cajrnf {
        final long journalSeq;
        final String auditId;
        final long groupRefNo;
        final String eventTypeKbn;
        final String journalStatusKbn;

        Cajrnf(long journalSeq, String auditId, long groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }

    private static final class ProbeSummary {
        int totalCount;
        int reachedCount;
        int auditMatchedCount;
        int journalFoundCount;
        int exceptionCount;
        int writtenCount;
    }
}
