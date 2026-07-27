package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2025-06-29  共通基盤  初版作成
 */
public class AuditJournalEnrichService {
    private static final String STATUS_NORMAL = "0";
    private static final String STATUS_WARN = "1";
    private static final String STATUS_ERROR = "9";

    public static void main(String[] a) {
        new AuditJournalEnrichService().execute();
    }

    private void execute() {
        java.util.List<CajrnfRecord> auditJournal = loadCajrnf();
        java.util.List<CmaudfRecord> auditDetail = loadCmaudf();
        java.util.List<CmtxnfRecord> transactions = loadCmtxnf();
        java.util.List<CxidmfRecord> idMaps = loadCxidmf();

        java.util.Map<String, CmaudfRecord> auditById = indexAuditDetail(auditDetail);
        java.util.Map<String, CmtxnfRecord> txnByCompanyAndLocalNo = indexTransactions(transactions);
        java.util.Map<String, CxidmfRecord> idMapByCompanyAndLocalNo = indexIdMaps(idMaps);

        java.util.List<CajrnfRecord> enriched = new java.util.ArrayList<>();

        int normalCount = 0;
        int warnCount = 0;
        int errorCount = 0;

        for (CajrnfRecord journal : auditJournal) {
            CmaudfRecord audit = auditById.get(journal.auditId);
            if (audit == null) {
                enriched.add(journal.withStatus(STATUS_ERROR));
                errorCount++;
                log("監査明細なし JOURNAL-SEQ=" + journal.journalSeq + " AUDIT-ID=" + journal.auditId);
                continue;
            }

            CmtxnfRecord txn = txnByCompanyAndLocalNo.get(key(audit.companyCode, audit.localTxnNo));
            CxidmfRecord idMap = idMapByCompanyAndLocalNo.get(key(audit.companyCode, audit.localTxnNo));

            java.util.List<String> defects = new java.util.ArrayList<>();
            validateCompany(audit.companyCode, defects);
            validateGroupRef(journal, audit, defects);
            validateTransaction(audit, txn, defects);
            validateIdMap(idMap, defects);

            String nextStatus;
            if (!defects.isEmpty()) {
                nextStatus = STATUS_ERROR;
                errorCount++;
                log("補足不可 JOURNAL-SEQ=" + journal.journalSeq + " AUDIT-ID=" + journal.auditId + " 理由=" + String.join(",", defects));
            } else if ("09".equals(txn.txnStatusKbn) || !"01".equals(idMap.linkStatusKbn)) {
                nextStatus = STATUS_WARN;
                warnCount++;
                log("補足注意 JOURNAL-SEQ=" + journal.journalSeq + " AUDIT-ID=" + journal.auditId + " 取引状態=" + txn.txnStatusKbn + " 名寄せ状態=" + idMap.linkStatusKbn);
            } else {
                nextStatus = STATUS_NORMAL;
                normalCount++;
            }

            enriched.add(new CajrnfRecord(
                    journal.journalSeq,
                    journal.auditId,
                    audit.groupRefNo,
                    journal.eventTypeKbn,
                    nextStatus));
        }

        writeCajrnf(enriched);
        log("処理件数=" + enriched.size() + " 正常=" + normalCount + " 注意=" + warnCount + " 異常=" + errorCount);
    }

    private java.util.Map<String, CmaudfRecord> indexAuditDetail(java.util.List<CmaudfRecord> records) {
        java.util.Map<String, CmaudfRecord> index = new java.util.LinkedHashMap<>();
        for (CmaudfRecord record : records) {
            if (isBlank(record.auditId)) {
                log("監査明細キー不正 AUDIT-ID未設定");
                continue;
            }
            CmaudfRecord previous = index.put(record.auditId, record);
            if (previous != null) {
                log("監査明細重複 AUDIT-ID=" + record.auditId);
            }
        }
        return index;
    }

    private java.util.Map<String, CmtxnfRecord> indexTransactions(java.util.List<CmtxnfRecord> records) {
        java.util.Map<String, CmtxnfRecord> index = new java.util.LinkedHashMap<>();
        for (CmtxnfRecord record : records) {
            java.util.List<String> defects = new java.util.ArrayList<>();
            validateCompany(record.companyCode, defects);
            if (isBlank(record.localTxnNo)) {
                defects.add("LOCAL-TXN-NO未設定");
            }
            if (record.txnAmt.signum() < 0) {
                defects.add("取引金額不正");
            }
            if (!"01".equals(record.txnStatusKbn) && !"09".equals(record.txnStatusKbn)) {
                defects.add("取引状態不正");
            }
            if (!defects.isEmpty()) {
                log("取引索引除外 TXN-ID=" + record.txnId + " 理由=" + String.join(",", defects));
                continue;
            }
            index.put(key(record.companyCode, record.localTxnNo), record);
        }
        return index;
    }

    private java.util.Map<String, CxidmfRecord> indexIdMaps(java.util.List<CxidmfRecord> records) {
        java.util.Map<String, CxidmfRecord> index = new java.util.LinkedHashMap<>();
        for (CxidmfRecord record : records) {
            if (isBlank(record.companyCode) || isBlank(record.localTxnNo) || isBlank(record.customerAliasId)) {
                log("名寄せ索引除外 IDMAP-KEY=" + record.idmapKey);
                continue;
            }
            index.put(key(record.companyCode, record.localTxnNo), record);
        }
        return index;
    }

    private void validateGroupRef(CajrnfRecord journal, CmaudfRecord audit, java.util.List<String> defects) {
        if (isBlank(audit.groupRefNo)) {
            defects.add("GROUP-REF-NO未設定");
        }
        if (!isBlank(journal.groupRefNo) && !journal.groupRefNo.equals(audit.groupRefNo)) {
            defects.add("GROUP-REF-NO差異");
        }
    }

    private void validateTransaction(CmaudfRecord audit, CmtxnfRecord txn, java.util.List<String> defects) {
        if (isBlank(audit.localTxnNo)) {
            defects.add("LOCAL-TXN-NO未設定");
            return;
        }
        if (txn == null) {
            defects.add("取引なし");
        }
    }

    private void validateIdMap(CxidmfRecord idMap, java.util.List<String> defects) {
        if (idMap == null) {
            defects.add("名寄せなし");
            return;
        }
        if (!"01".equals(idMap.linkStatusKbn) && !"09".equals(idMap.linkStatusKbn)) {
            defects.add("名寄せ状態不正");
        }
    }

    private void validateCompany(String companyCode, java.util.List<String> defects) {
        if (!"BK".equals(companyCode)
                && !"SC".equals(companyCode)
                && !"CD".equals(companyCode)
                && !"PY".equals(companyCode)
                && !"LF".equals(companyCode)
                && !"CM".equals(companyCode)) {
            defects.add("会社コード不正");
        }
    }

    private void writeCajrnf(java.util.List<CajrnfRecord> records) {
        for (CajrnfRecord record : records) {
            log("CAJRNF出力 JOURNAL-SEQ=" + record.journalSeq
                    + " AUDIT-ID=" + record.auditId
                    + " GROUP-REF-NO=" + record.groupRefNo
                    + " EVENT-TYPE-KBN=" + record.eventTypeKbn
                    + " JOURNAL-STATUS-KBN=" + record.journalStatusKbn);
        }
    }

    private java.util.List<CajrnfRecord> loadCajrnf() {
        java.util.List<CajrnfRecord> records = new java.util.ArrayList<>();
        records.add(new CajrnfRecord("000000000001", "AU-BK-0001", "", "10", "0"));
        records.add(new CajrnfRecord("000000000002", "AU-SC-0001", "", "10", "0"));
        records.add(new CajrnfRecord("000000000003", "AU-CD-0001", "GR-CD-20250629-0001", "20", "0"));
        records.add(new CajrnfRecord("000000000004", "AU-PY-0001", "", "10", "0"));
        records.add(new CajrnfRecord("000000000005", "AU-LF-0009", "", "10", "0"));
        return records;
    }

    private java.util.List<CmaudfRecord> loadCmaudf() {
        java.util.List<CmaudfRecord> records = new java.util.ArrayList<>();
        records.add(new CmaudfRecord("AU-BK-0001", "GR-BK-20250629-0001", "BK", "BK-TXN-0001", "01"));
        records.add(new CmaudfRecord("AU-SC-0001", "GR-SC-20250629-0001", "SC", "SC-TXN-0001", "01"));
        records.add(new CmaudfRecord("AU-CD-0001", "GR-CD-20250629-0001", "CD", "CD-TXN-0001", "01"));
        records.add(new CmaudfRecord("AU-PY-0001", "GR-PY-20250629-0001", "PY", "PY-TXN-0001", "01"));
        return records;
    }

    private java.util.List<CmtxnfRecord> loadCmtxnf() {
        java.util.List<CmtxnfRecord> records = new java.util.ArrayList<>();
        records.add(new CmtxnfRecord("TX-BK-0001", "BK", "BK-TXN-0001", new java.math.BigDecimal("1250000"), "01"));
        records.add(new CmtxnfRecord("TX-SC-0001", "SC", "SC-TXN-0001", new java.math.BigDecimal("843000"), "09"));
        records.add(new CmtxnfRecord("TX-CD-0001", "CD", "CD-TXN-0001", new java.math.BigDecimal("24800"), "01"));
        records.add(new CmtxnfRecord("TX-PY-0001", "PY", "PY-TXN-0001", new java.math.BigDecimal("7800"), "01"));
        return records;
    }

    private java.util.List<CxidmfRecord> loadCxidmf() {
        java.util.List<CxidmfRecord> records = new java.util.ArrayList<>();
        records.add(new CxidmfRecord("BK:BK-TXN-0001", "BK", "BK-TXN-0001", "ALIAS-BK-10001", "01"));
        records.add(new CxidmfRecord("SC:SC-TXN-0001", "SC", "SC-TXN-0001", "ALIAS-SC-20001", "01"));
        records.add(new CxidmfRecord("CD:CD-TXN-0001", "CD", "CD-TXN-0001", "ALIAS-CD-30001", "09"));
        return records;
    }

    private String key(String companyCode, String localTxnNo) {
        return companyCode + '\u0001' + localTxnNo;
    }

    private boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private void log(String message) {
        System.out.println(message);
    }

    private static final class CajrnfRecord {
        private final String journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private CajrnfRecord(String journalSeq, String auditId, String groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }

        private CajrnfRecord withStatus(String status) {
            return new CajrnfRecord(journalSeq, auditId, groupRefNo, eventTypeKbn, status);
        }
    }

    private static final class CmaudfRecord {
        private final String auditId;
        private final String groupRefNo;
        private final String companyCode;
        private final String localTxnNo;
        private final String auditStatusKbn;

        private CmaudfRecord(String auditId, String groupRefNo, String companyCode, String localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    private static final class CmtxnfRecord {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final java.math.BigDecimal txnAmt;
        private final String txnStatusKbn;

        private CmtxnfRecord(String txnId, String companyCode, String localTxnNo, java.math.BigDecimal txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class CxidmfRecord {
        private final String idmapKey;
        private final String companyCode;
        private final String localTxnNo;
        private final String customerAliasId;
        private final String linkStatusKbn;

        private CxidmfRecord(String idmapKey, String companyCode, String localTxnNo, String customerAliasId, String linkStatusKbn) {
            this.idmapKey = idmapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
        }
    }
}
