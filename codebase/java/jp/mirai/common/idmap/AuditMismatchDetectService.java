package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2024/11/19  共通基盤G   初版作成
 */
public class AuditMismatchDetectService {
    private static final String IMPORT_BATCH_ID = "IDMAP-AUD-20241119-001";
    private static final String ERROR_STATUS_OPEN = "01";
    private static final String JOURNAL_STATUS_CREATED = "01";

    private static final String EVENT_AUDIT_NOT_CREATED = "A01";
    private static final String EVENT_AUDIT_STATUS_MISMATCH = "A02";
    private static final String EVENT_REVERSE_LOOKUP_FAILED = "A03";

    private static final String ERROR_AUDIT_NOT_CREATED = "EA01";
    private static final String ERROR_AUDIT_STATUS_MISMATCH = "EA02";
    private static final String ERROR_REVERSE_LOOKUP_FAILED = "EA03";

    public static void main(String[] a) {
        AuditMismatchDetectService service = new AuditMismatchDetectService(new DefaultAuditLinkService());
        DetectionResult result = service.detect(
                sampleTransactions(),
                sampleAudits(),
                sampleJournals()
        );

        for (CmerrfRecord error : result.errors) {
            System.out.println(error.toLine());
        }
        for (CajrnfRecord journal : result.appendedJournals) {
            System.out.println(journal.toLine());
        }
    }

    private final AuditLinkService auditLinkService;

    private AuditMismatchDetectService(AuditLinkService auditLinkService) {
        this.auditLinkService = auditLinkService;
    }

    private DetectionResult detect(CmtxnfRecord[] transactions, CmaudfRecord[] audits, CajrnfRecord[] existingJournals) {
        CmaudfRecord[] auditByTxn = new CmaudfRecord[transactions.length];
        CmerrfRecord[] errors = new CmerrfRecord[transactions.length * 3];
        CajrnfRecord[] appendedJournals = new CajrnfRecord[transactions.length * 3];

        int errorCount = 0;
        int journalCount = 0;
        long nextJournalSeq = nextJournalSeq(existingJournals);

        for (int i = 0; i < transactions.length; i++) {
            CmtxnfRecord txn = transactions[i];
            CmaudfRecord audit = findAudit(audits, txn.companyCode, txn.localTxnNo);
            auditByTxn[i] = audit;

            if (audit == null) {
                String errorId = nextErrorId(errorCount + 1);
                errors[errorCount++] = new CmerrfRecord(
                        errorId,
                        IMPORT_BATCH_ID,
                        txn.companyCode,
                        txn.localTxnNo,
                        ERROR_AUDIT_NOT_CREATED,
                        ERROR_STATUS_OPEN
                );
                appendedJournals[journalCount++] = new CajrnfRecord(
                        nextJournalSeq++,
                        "",
                        "",
                        EVENT_AUDIT_NOT_CREATED,
                        JOURNAL_STATUS_CREATED
                );
                continue;
            }

            if (!isAuditStatusExpected(txn.txnStatusKbn, audit.auditStatusKbn)) {
                String errorId = nextErrorId(errorCount + 1);
                errors[errorCount++] = new CmerrfRecord(
                        errorId,
                        IMPORT_BATCH_ID,
                        txn.companyCode,
                        txn.localTxnNo,
                        ERROR_AUDIT_STATUS_MISMATCH,
                        ERROR_STATUS_OPEN
                );
                appendedJournals[journalCount++] = new CajrnfRecord(
                        nextJournalSeq++,
                        audit.auditId,
                        audit.groupRefNo,
                        EVENT_AUDIT_STATUS_MISMATCH,
                        JOURNAL_STATUS_CREATED
                );
            }

            if (!auditLinkService.canReverseLookup(audit.groupRefNo, txn.companyCode, txn.localTxnNo)) {
                String errorId = nextErrorId(errorCount + 1);
                errors[errorCount++] = new CmerrfRecord(
                        errorId,
                        IMPORT_BATCH_ID,
                        txn.companyCode,
                        txn.localTxnNo,
                        ERROR_REVERSE_LOOKUP_FAILED,
                        ERROR_STATUS_OPEN
                );
                appendedJournals[journalCount++] = new CajrnfRecord(
                        nextJournalSeq++,
                        audit.auditId,
                        audit.groupRefNo,
                        EVENT_REVERSE_LOOKUP_FAILED,
                        JOURNAL_STATUS_CREATED
                );
            }
        }

        return new DetectionResult(copyErrors(errors, errorCount), copyJournals(appendedJournals, journalCount), auditByTxn);
    }

    private static CmaudfRecord findAudit(CmaudfRecord[] audits, String companyCode, long localTxnNo) {
        for (CmaudfRecord audit : audits) {
            if (audit.companyCode.equals(companyCode) && audit.localTxnNo == localTxnNo) {
                return audit;
            }
        }
        return null;
    }

    private static boolean isAuditStatusExpected(String txnStatusKbn, String auditStatusKbn) {
        if ("01".equals(txnStatusKbn)) {
            return "01".equals(auditStatusKbn);
        }
        if ("09".equals(txnStatusKbn)) {
            return "09".equals(auditStatusKbn);
        }
        return false;
    }

    private static long nextJournalSeq(CajrnfRecord[] journals) {
        long max = 0L;
        for (CajrnfRecord journal : journals) {
            if (journal.journalSeq > max) {
                max = journal.journalSeq;
            }
        }
        return max + 1L;
    }

    private static String nextErrorId(int serial) {
        return String.format("E%08d", Integer.valueOf(serial));
    }

    private static CmerrfRecord[] copyErrors(CmerrfRecord[] source, int size) {
        CmerrfRecord[] copied = new CmerrfRecord[size];
        System.arraycopy(source, 0, copied, 0, size);
        return copied;
    }

    private static CajrnfRecord[] copyJournals(CajrnfRecord[] source, int size) {
        CajrnfRecord[] copied = new CajrnfRecord[size];
        System.arraycopy(source, 0, copied, 0, size);
        return copied;
    }

    private static CmtxnfRecord[] sampleTransactions() {
        return new CmtxnfRecord[] {
                new CmtxnfRecord("T202411190001", "BK", 100000000101L, 1250000L, "01"),
                new CmtxnfRecord("T202411190002", "SC", 200000000201L, 880000L, "01"),
                new CmtxnfRecord("T202411190003", "CD", 300000000301L, 42000L, "09"),
                new CmtxnfRecord("T202411190004", "PY", 400000000401L, 2600L, "01"),
                new CmtxnfRecord("T202411190005", "LF", 500000000501L, 360000L, "09")
        };
    }

    private static CmaudfRecord[] sampleAudits() {
        // 統合取引参照番号は採番サービスから取得する(本サービスは採番方式を持たない)。
        GroupRefService gr = new GroupRefService();
        return new CmaudfRecord[] {
                new CmaudfRecord("A202411190001", String.valueOf(gr.toGroupRef("BK", 100000000101L)), "BK", 100000000101L, "01"),
                new CmaudfRecord("A202411190002", String.valueOf(gr.toGroupRef("SC", 200000000201L)), "SC", 200000000201L, "09"),
                new CmaudfRecord("A202411190003", String.valueOf(gr.toGroupRef("CD", 300000000301L)), "CD", 300000000301L, "09"),
                new CmaudfRecord("A202411190005", String.valueOf(gr.toGroupRef("LF", 500000000501L)), "LF", 500000000501L, "09")
        };
    }

    private static CajrnfRecord[] sampleJournals() {
        GroupRefService gr = new GroupRefService();
        return new CajrnfRecord[] {
                new CajrnfRecord(900001L, "A202411180001", String.valueOf(gr.toGroupRef("BK", 100000000098L)), "A00", "01"),
                new CajrnfRecord(900002L, "A202411180002", String.valueOf(gr.toGroupRef("SC", 200000000199L)), "A00", "01")
        };
    }

    private interface AuditLinkService {
        boolean canReverseLookup(String groupRefNo, String companyCode, long localTxnNo);
    }

    private static final class DefaultAuditLinkService implements AuditLinkService {
        private final jp.mirai.common.idmap.AuditLinkService delegate = new jp.mirai.common.idmap.AuditLinkService();

        public boolean canReverseLookup(String groupRefNo, String companyCode, long localTxnNo) {
            long numericGroupRefNo;
            try {
                numericGroupRefNo = Long.parseLong(groupRefNo);
            } catch (NumberFormatException e) {
                return false;
            }

            try {
                IdMapModel.AuditEntry probe = new IdMapModel.AuditEntry("", numericGroupRefNo, "", 0L, "U");
                IdMapModel.AuditEntry resolved = delegate.resolve(probe);
                return companyCode.equals(resolved.companyCode()) && localTxnNo == resolved.localTxnNo();
            } catch (RuntimeException e) {
                return false;
            }
        }
    }

    private static final class DetectionResult {
        private final CmerrfRecord[] errors;
        private final CajrnfRecord[] appendedJournals;
        private final CmaudfRecord[] auditByTxn;

        private DetectionResult(CmerrfRecord[] errors, CajrnfRecord[] appendedJournals, CmaudfRecord[] auditByTxn) {
            this.errors = errors;
            this.appendedJournals = appendedJournals;
            this.auditByTxn = auditByTxn;
        }
    }

    private static final class CmtxnfRecord {
        private final String txnId;
        private final String companyCode;
        private final long localTxnNo;
        private final long txnAmt;
        private final String txnStatusKbn;

        private CmtxnfRecord(String txnId, String companyCode, long localTxnNo, long txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class CmaudfRecord {
        private final String auditId;
        private final String groupRefNo;
        private final String companyCode;
        private final long localTxnNo;
        private final String auditStatusKbn;

        private CmaudfRecord(String auditId, String groupRefNo, String companyCode, long localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    private static final class CmerrfRecord {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final long localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        private CmerrfRecord(String errorId, String importBatchId, String companyCode, long localTxnNo, String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }

        private String toLine() {
            return "CMERRF|" + errorId + "|" + importBatchId + "|" + companyCode + "|" + localTxnNo + "|" + errorCode + "|" + errorStatusKbn;
        }
    }

    private static final class CajrnfRecord {
        private final long journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private CajrnfRecord(long journalSeq, String auditId, String groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }

        private String toLine() {
            return "CAJRNF|" + journalSeq + "|" + auditId + "|" + groupRefNo + "|" + eventTypeKbn + "|" + journalStatusKbn;
        }
    }
}
