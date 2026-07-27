package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-07-12  共通基盤  監査相互参照サービス初版
 */
public class AuditCrossReferenceService {
    private static final String STATUS_AUDIT_ACTIVE = "01";
    private static final String STATUS_LINK_ACTIVE = "01";
    private static final String STATUS_JOURNAL_OPEN = "01";

    private static final String EVENT_REFERENCE_OK = "XR";
    private static final String EVENT_REFERENCE_NG = "XF";

    private static final String RESULT_OK = "00";
    private static final String RESULT_AUDIT_STOP = "41";
    private static final String RESULT_JOURNAL_STOP = "42";
    private static final String RESULT_GROUP_REF_MISMATCH = "43";
    private static final String RESULT_LINK_NOT_FOUND = "44";
    private static final String RESULT_LINK_STOP = "45";
    private static final String RESULT_IDMAP_MISMATCH = "46";
    private static final String RESULT_AUDITLINK_ERROR = "47";
    private static final String RESULT_DUPLICATE_AUDIT = "48";

    private static long nextJournalSeq = 90000001L;

    public static void main(String[] a) {
        java.util.List<AuditRecord> auditRecords = loadCmaudf();
        java.util.List<JournalRecord> journalRecords = loadCajrnf();
        java.util.Map<String, IdMapRecord> idMapRecords = loadCxidmf();
        jp.mirai.common.idmap.AuditLinkService auditLinkService = new jp.mirai.common.idmap.AuditLinkService();

        java.util.List<JournalRecord> outputJournal = buildCrossReferenceJournal(
                auditRecords,
                journalRecords,
                idMapRecords,
                auditLinkService);

        for (JournalRecord record : outputJournal) {
            System.out.println(record.toLine());
        }
    }

    private static java.util.List<JournalRecord> buildCrossReferenceJournal(
            java.util.List<AuditRecord> auditRecords,
            java.util.List<JournalRecord> journalRecords,
            java.util.Map<String, IdMapRecord> idMapRecords,
            jp.mirai.common.idmap.AuditLinkService auditLinkService) {
        java.util.Map<String, Integer> auditCount = countByAuditId(auditRecords);
        java.util.Map<String, JournalRecord> latestJournalByAuditId = latestJournalByAuditId(journalRecords);
        java.util.List<JournalRecord> result = new java.util.ArrayList<>();

        for (AuditRecord audit : auditRecords) {
            if (auditCount.get(audit.auditId).intValue() > 1) {
                result.add(writeFailure(audit, RESULT_DUPLICATE_AUDIT));
                continue;
            }
            if (!STATUS_AUDIT_ACTIVE.equals(audit.auditStatusKbn)) {
                result.add(writeFailure(audit, RESULT_AUDIT_STOP));
                continue;
            }

            JournalRecord latestJournal = latestJournalByAuditId.get(audit.auditId);
            if (latestJournal == null || !STATUS_JOURNAL_OPEN.equals(latestJournal.journalStatusKbn)) {
                result.add(writeFailure(audit, RESULT_JOURNAL_STOP));
                continue;
            }
            if (latestJournal.groupRefNo != audit.groupRefNo) {
                result.add(writeFailure(audit, RESULT_GROUP_REF_MISMATCH));
                continue;
            }

            IdMapModel.AuditEntry resolved;
            try {
                IdMapModel.AuditEntry probe = new IdMapModel.AuditEntry(
                        audit.auditId, audit.groupRefNo, "", 0L, "U");
                resolved = auditLinkService.resolve(probe);
            } catch (RuntimeException ex) {
                result.add(writeFailure(audit, RESULT_AUDITLINK_ERROR));
                continue;
            }
            if (!audit.companyCode.equals(resolved.companyCode()) || audit.localTxnNo != resolved.localTxnNo()) {
                result.add(writeFailure(audit, RESULT_GROUP_REF_MISMATCH));
                continue;
            }

            IdMapRecord idMap = idMapRecords.get(idMapKey(audit.companyCode, audit.localTxnNo));
            if (idMap == null) {
                result.add(writeFailure(audit, RESULT_LINK_NOT_FOUND));
                continue;
            }
            if (!STATUS_LINK_ACTIVE.equals(idMap.linkStatusKbn)) {
                result.add(writeFailure(audit, RESULT_LINK_STOP));
                continue;
            }
            if (!audit.companyCode.equals(idMap.companyCode) || audit.localTxnNo != idMap.localTxnNo) {
                result.add(writeFailure(audit, RESULT_IDMAP_MISMATCH));
                continue;
            }

            result.add(writeSuccess(audit));
        }

        return result;
    }

    private static java.util.Map<String, Integer> countByAuditId(java.util.List<AuditRecord> auditRecords) {
        java.util.Map<String, Integer> count = new java.util.HashMap<>();
        for (AuditRecord audit : auditRecords) {
            Integer current = count.get(audit.auditId);
            count.put(audit.auditId, Integer.valueOf(current == null ? 1 : current.intValue() + 1));
        }
        return count;
    }

    private static java.util.Map<String, JournalRecord> latestJournalByAuditId(java.util.List<JournalRecord> journalRecords) {
        java.util.Map<String, JournalRecord> latest = new java.util.HashMap<>();
        for (JournalRecord journal : journalRecords) {
            JournalRecord current = latest.get(journal.auditId);
            if (current == null || journal.journalSeq > current.journalSeq) {
                latest.put(journal.auditId, journal);
            }
        }
        return latest;
    }

    private static JournalRecord writeSuccess(AuditRecord audit) {
        return new JournalRecord(nextJournalSeq++, audit.auditId, audit.groupRefNo, EVENT_REFERENCE_OK, RESULT_OK);
    }

    private static JournalRecord writeFailure(AuditRecord audit, String reasonKbn) {
        return new JournalRecord(nextJournalSeq++, audit.auditId, audit.groupRefNo, EVENT_REFERENCE_NG, reasonKbn);
    }

    private static String idMapKey(String companyCode, long localTxnNo) {
        return companyCode + ":" + localTxnNo;
    }

    private static java.util.List<AuditRecord> loadCmaudf() {
        // 統合取引参照番号は採番サービスから取得する(本サービスは採番方式を持たない)。
        GroupRefService gr = new GroupRefService();
        java.util.List<AuditRecord> records = new java.util.ArrayList<>();
        records.add(new AuditRecord("AUD-20250318-0001", gr.toGroupRef("BK", 123L), "BK", 123L, "01"));
        records.add(new AuditRecord("AUD-20250318-0002", gr.toGroupRef("SC", 456L), "SC", 456L, "01"));
        records.add(new AuditRecord("AUD-20250318-0003", gr.toGroupRef("CD", 789L), "CD", 789L, "09"));
        records.add(new AuditRecord("AUD-20250318-0004", gr.toGroupRef("PY", 110L), "PY", 110L, "01"));
        records.add(new AuditRecord("AUD-20250318-0005", gr.toGroupRef("LF", 220L), "LF", 220L, "01"));
        records.add(new AuditRecord("AUD-20250318-0006", gr.toGroupRef("CM", 330L), "CM", 330L, "01"));
        records.add(new AuditRecord("AUD-20250318-0007", gr.toGroupRef("CM", 331L), "CM", 331L, "01"));
        return records;
    }

    private static java.util.List<JournalRecord> loadCajrnf() {
        GroupRefService gr = new GroupRefService();
        java.util.List<JournalRecord> records = new java.util.ArrayList<>();
        records.add(new JournalRecord(81000001L, "AUD-20250318-0001", gr.toGroupRef("BK", 123L), "AU", "01"));
        records.add(new JournalRecord(81000002L, "AUD-20250318-0002", gr.toGroupRef("SC", 456L), "AU", "01"));
        records.add(new JournalRecord(81000003L, "AUD-20250318-0003", gr.toGroupRef("CD", 789L), "AU", "01"));
        records.add(new JournalRecord(81000004L, "AUD-20250318-0004", gr.toGroupRef("PY", 110L), "AU", "09"));
        records.add(new JournalRecord(81000005L, "AUD-20250318-0005", gr.toGroupRef("LF", 221L), "AU", "01"));
        records.add(new JournalRecord(81000006L, "AUD-20250318-0006", gr.toGroupRef("CM", 330L), "AU", "01"));
        return records;
    }

    private static java.util.Map<String, IdMapRecord> loadCxidmf() {
        java.util.Map<String, IdMapRecord> records = new java.util.HashMap<>();
        putIdMap(records, new IdMapRecord("BK:123", "BK", 123L, "CUST-BK-00000123", "01"));
        putIdMap(records, new IdMapRecord("SC:456", "SC", 456L, "CUST-SC-00000456", "01"));
        putIdMap(records, new IdMapRecord("CD:789", "CD", 789L, "CUST-CD-00000789", "01"));
        putIdMap(records, new IdMapRecord("PY:110", "PY", 110L, "CUST-PY-00000110", "09"));
        putIdMap(records, new IdMapRecord("LF:220", "LF", 220L, "CUST-LF-00000220", "01"));
        putIdMap(records, new IdMapRecord("CM:330", "CM", 330L, "CUST-CM-00000330", "01"));
        return records;
    }

    private static void putIdMap(java.util.Map<String, IdMapRecord> records, IdMapRecord record) {
        records.put(record.idMapKey, record);
    }

    private static final class AuditRecord {
        private final String auditId;
        private final long groupRefNo;
        private final String companyCode;
        private final long localTxnNo;
        private final String auditStatusKbn;

        private AuditRecord(String auditId, long groupRefNo, String companyCode, long localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    private static final class JournalRecord {
        private final long journalSeq;
        private final String auditId;
        private final long groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private JournalRecord(long journalSeq, String auditId, long groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }

        private String toLine() {
            return journalSeq + "," + auditId + "," + groupRefNo + "," + eventTypeKbn + "," + journalStatusKbn;
        }
    }

    private static final class IdMapRecord {
        private final String idMapKey;
        private final String companyCode;
        private final long localTxnNo;
        private final String customerAliasId;
        private final String linkStatusKbn;

        private IdMapRecord(String idMapKey, String companyCode, long localTxnNo, String customerAliasId, String linkStatusKbn) {
            this.idMapKey = idMapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
        }
    }
}
