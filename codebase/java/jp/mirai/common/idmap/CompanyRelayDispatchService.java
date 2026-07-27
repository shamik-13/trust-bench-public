package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2025-06-29  共通基盤    会社間連携送信サービス初版作成
 */
public class CompanyRelayDispatchService {
    private static final String STATUS_KAKUTEI = "01";
    private static final String STATUS_TORIKESHI = "09";
    private static final String LINK_STATUS_SEIJO = "01";
    private static final String ERROR_STATUS_MI = "01";
    private static final String JOURNAL_STATUS_SAKUSEI = "01";
    private static final String EVENT_TYPE_RELAY_SEND = "41";
    private static final String IMPORT_BATCH_ID = "BATCH-20250629-001";

    private CompanyRelayDispatchService() {
    }

    public static void main(String[] a) {
        java.util.List<CmtxnfRecord> txnRecords = java.util.Arrays.asList(
                new CmtxnfRecord("TXN-BK-000001", "BK", "LTX-000001", new java.math.BigDecimal("1250000"), "01"),
                new CmtxnfRecord("TXN-SC-000002", "SC", "LTX-000002", new java.math.BigDecimal("830000"), "01"),
                new CmtxnfRecord("TXN-CD-000003", "CD", "LTX-000003", new java.math.BigDecimal("42000"), "09"),
                new CmtxnfRecord("TXN-PY-000004", "PY", "LTX-000004", new java.math.BigDecimal("15800"), "01"),
                new CmtxnfRecord("TXN-LF-000005", "LF", "LTX-000005", new java.math.BigDecimal("9300000"), "01")
        );

        java.util.List<CmaudfRecord> auditRecords = java.util.Arrays.asList(
                new CmaudfRecord("AUD-BK-000001", "GRP-BK-20250629-000001", "BK", "LTX-000001", "01"),
                new CmaudfRecord("AUD-SC-000002", "GRP-SC-20250629-000002", "SC", "LTX-000002", "01"),
                new CmaudfRecord("AUD-CD-000003", "GRP-CD-20250629-000003", "CD", "LTX-000003", "01"),
                new CmaudfRecord("AUD-PY-000004", "GRP-PY-20250629-000004", "PY", "LTX-000004", "09"),
                new CmaudfRecord("AUD-CM-000099", "GRP-CM-20250629-000099", "CM", "LTX-000099", "01")
        );

        java.util.List<CxidmfRecord> idMapRecords = java.util.Arrays.asList(
                new CxidmfRecord("IDM-BK-LTX-000001", "BK", "LTX-000001", "ALIAS-BK-7711", "01", null),
                new CxidmfRecord("IDM-SC-LTX-000002", "SC", "LTX-000002", "ALIAS-SC-8831", "01", null),
                new CxidmfRecord("IDM-CD-LTX-000003", "CD", "LTX-000003", "ALIAS-CD-5290", "01", null),
                new CxidmfRecord("IDM-PY-LTX-000004", "PY", "LTX-000004", "ALIAS-PY-1104", "09", null)
        );

        DispatchResult result = dispatch(txnRecords, auditRecords, idMapRecords);

        System.out.println("送信ペイロード件数=" + result.payloads.size());
        for (RelayPayload payload : result.payloads) {
            System.out.println("送信対象 会社=" + payload.companyCode
                    + " 取引番号=" + payload.localTxnNo
                    + " 取引キー=" + payload.txnId
                    + " 監査キー=" + payload.auditId
                    + " 名寄せキー=" + payload.idmapKey);
        }

        System.out.println("ジャーナル件数=" + result.journals.size());
        for (CajrnfRecord journal : result.journals) {
            System.out.println("ジャーナル 登録順=" + journal.journalSeq
                    + " 監査キー=" + journal.auditId
                    + " 統合参照番号=" + journal.groupRefNo
                    + " 事象区分=" + journal.eventTypeKbn);
        }

        System.out.println("エラー件数=" + result.errors.size());
        for (CmerrfRecord error : result.errors) {
            System.out.println("エラー 会社=" + error.companyCode
                    + " 取引番号=" + error.localTxnNo
                    + " コード=" + error.errorCode
                    + " 識別子=" + error.errorId);
        }
    }

    private static DispatchResult dispatch(
            java.util.List<CmtxnfRecord> txnRecords,
            java.util.List<CmaudfRecord> auditRecords,
            java.util.List<CxidmfRecord> idMapRecords) {
        java.util.Map<String, CmaudfRecord> auditByBusinessKey = new java.util.LinkedHashMap<>();
        for (CmaudfRecord audit : auditRecords) {
            auditByBusinessKey.put(businessKey(audit.companyCode, audit.localTxnNo), audit);
        }

        java.util.Map<String, CxidmfRecord> idMapByBusinessKey = new java.util.LinkedHashMap<>();
        for (CxidmfRecord idMap : idMapRecords) {
            idMapByBusinessKey.put(businessKey(idMap.companyCode, idMap.localTxnNo), idMap);
        }

        java.util.List<RelayPayload> payloads = new java.util.ArrayList<>();
        java.util.List<CajrnfRecord> journals = new java.util.ArrayList<>();
        java.util.List<CmerrfRecord> errors = new java.util.ArrayList<>();

        long journalSeq = 1L;
        int errorSeq = 1;

        for (CmtxnfRecord txn : txnRecords) {
            String key = businessKey(txn.companyCode, txn.localTxnNo);
            CmaudfRecord audit = auditByBusinessKey.get(key);
            CxidmfRecord idMap = idMapByBusinessKey.get(key);

            java.util.List<String> errorCodes = validate(txn, audit, idMap);
            if (!errorCodes.isEmpty()) {
                for (String errorCode : errorCodes) {
                    errors.add(new CmerrfRecord(
                            "ERR-" + IMPORT_BATCH_ID + "-" + String.format("%04d", errorSeq++),
                            IMPORT_BATCH_ID,
                            txn.companyCode,
                            txn.localTxnNo,
                            errorCode,
                            ERROR_STATUS_MI));
                }
                continue;
            }

            payloads.add(new RelayPayload(
                    txn.txnId,
                    audit.auditId,
                    audit.groupRefNo,
                    txn.companyCode,
                    txn.localTxnNo,
                    txn.txnAmt,
                    txn.txnStatusKbn,
                    audit.auditStatusKbn,
                    idMap.idmapKey,
                    idMap.customerAliasId,
                    idMap.linkStatusKbn,
                    idMap.idMapModel));

            journals.add(new CajrnfRecord(
                    journalSeq++,
                    audit.auditId,
                    audit.groupRefNo,
                    EVENT_TYPE_RELAY_SEND,
                    JOURNAL_STATUS_SAKUSEI));
        }

        return new DispatchResult(payloads, journals, errors);
    }

    private static java.util.List<String> validate(CmtxnfRecord txn, CmaudfRecord audit, CxidmfRecord idMap) {
        java.util.List<String> errors = new java.util.ArrayList<>();

        if (!isCompanyCode(txn.companyCode)) {
            errors.add("E-COMPANY");
        }
        if (!STATUS_KAKUTEI.equals(txn.txnStatusKbn) && !STATUS_TORIKESHI.equals(txn.txnStatusKbn)) {
            errors.add("E-TXN-STATUS");
        }
        if (audit == null) {
            errors.add("E-AUDIT-MISS");
        } else if (!STATUS_KAKUTEI.equals(audit.auditStatusKbn)) {
            errors.add("E-AUDIT-STATUS");
        }
        if (idMap == null) {
            errors.add("E-IDMAP-MISS");
        } else if (!LINK_STATUS_SEIJO.equals(idMap.linkStatusKbn)) {
            errors.add("E-IDMAP-STATUS");
        }

        return errors;
    }

    private static boolean isCompanyCode(String companyCode) {
        return "BK".equals(companyCode)
                || "SC".equals(companyCode)
                || "CD".equals(companyCode)
                || "PY".equals(companyCode)
                || "LF".equals(companyCode)
                || "CM".equals(companyCode);
    }

    private static String businessKey(String companyCode, String localTxnNo) {
        return companyCode + ":" + localTxnNo;
    }

    private static final class CmtxnfRecord {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final java.math.BigDecimal txnAmt;
        private final String txnStatusKbn;

        private CmtxnfRecord(String txnId, String companyCode, String localTxnNo,
                java.math.BigDecimal txnAmt, String txnStatusKbn) {
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
        private final String localTxnNo;
        private final String auditStatusKbn;

        private CmaudfRecord(String auditId, String groupRefNo, String companyCode,
                String localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    private static final class CxidmfRecord {
        private final String idmapKey;
        private final String companyCode;
        private final String localTxnNo;
        private final String customerAliasId;
        private final String linkStatusKbn;
        private final IdMapModel idMapModel;

        private CxidmfRecord(String idmapKey, String companyCode, String localTxnNo,
                String customerAliasId, String linkStatusKbn, IdMapModel idMapModel) {
            this.idmapKey = idmapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
            this.idMapModel = idMapModel;
        }
    }

    private static final class CajrnfRecord {
        private final long journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private CajrnfRecord(long journalSeq, String auditId, String groupRefNo,
                String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }

    private static final class CmerrfRecord {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        private CmerrfRecord(String errorId, String importBatchId, String companyCode,
                String localTxnNo, String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }
    }

    private static final class RelayPayload {
        private final String txnId;
        private final String auditId;
        private final String groupRefNo;
        private final String companyCode;
        private final String localTxnNo;
        private final java.math.BigDecimal txnAmt;
        private final String txnStatusKbn;
        private final String auditStatusKbn;
        private final String idmapKey;
        private final String customerAliasId;
        private final String linkStatusKbn;
        private final IdMapModel idMapModel;

        private RelayPayload(String txnId, String auditId, String groupRefNo,
                String companyCode, String localTxnNo, java.math.BigDecimal txnAmt,
                String txnStatusKbn, String auditStatusKbn, String idmapKey,
                String customerAliasId, String linkStatusKbn, IdMapModel idMapModel) {
            this.txnId = txnId;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
            this.auditStatusKbn = auditStatusKbn;
            this.idmapKey = idmapKey;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
            this.idMapModel = idMapModel;
        }
    }

    private static final class DispatchResult {
        private final java.util.List<RelayPayload> payloads;
        private final java.util.List<CajrnfRecord> journals;
        private final java.util.List<CmerrfRecord> errors;

        private DispatchResult(java.util.List<RelayPayload> payloads,
                java.util.List<CajrnfRecord> journals,
                java.util.List<CmerrfRecord> errors) {
            this.payloads = java.util.Collections.unmodifiableList(payloads);
            this.journals = java.util.Collections.unmodifiableList(journals);
            this.errors = java.util.Collections.unmodifiableList(errors);
        }
    }
}
