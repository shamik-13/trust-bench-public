package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/02/15  共通基盤  初版作成
 * 1.01  2024/04/08  共通基盤  監査対象判定を追加
 */
public class ImportErrorRegistrationService {

    private static final String CODE_TYPE_ERROR_STATUS = "ERRSTS";
    private static final String CODE_TYPE_AUDIT_ERROR = "AUDERR";
    private static final String STATUS_UNREGISTERED = "00";
    private static final String STATUS_REGISTERED = "10";
    private static final String STATUS_DUPLICATED = "90";
    private static final String CODE_ACTIVE = "1";
    private static final String JOURNAL_STATUS_NORMAL = "1";
    private static final String EVENT_TYPE_IMPORT_ERROR = "IE";

    public static void main(String[] a) {
        java.time.LocalDate businessDate = java.time.LocalDate.of(2024, 4, 8);

        java.util.List<CmerrfRecord> currentErrors = new java.util.ArrayList<CmerrfRecord>();
        currentErrors.add(new CmerrfRecord("E2404080001", "B20240408001", "001", "T-00000031", "IMP001", STATUS_UNREGISTERED));
        currentErrors.add(new CmerrfRecord("E2404080002", "B20240408001", "001", "T-00000032", "IMP021", STATUS_UNREGISTERED));
        currentErrors.add(new CmerrfRecord("E2404080003", "B20240408001", "002", "T-00000033", "NAM041", STATUS_REGISTERED));

        java.util.List<CmerrfRecord> incomingErrors = new java.util.ArrayList<CmerrfRecord>();
        incomingErrors.add(new CmerrfRecord("E2404080001", "B20240408001", "001", "T-00000031", "IMP001", STATUS_UNREGISTERED));
        incomingErrors.add(new CmerrfRecord("E2404080004", "B20240408001", "001", "T-00000034", "NUM017", STATUS_UNREGISTERED));
        incomingErrors.add(new CmerrfRecord("E2404080005", "B20240408001", "003", "T-00000035", "NAM041", STATUS_UNREGISTERED));
        incomingErrors.add(new CmerrfRecord("E2404080005", "B20240408001", "003", "T-00000035", "NAM041", STATUS_UNREGISTERED));

        java.util.List<CmcodfRecord> codes = new java.util.ArrayList<CmcodfRecord>();
        codes.add(new CmcodfRecord("10", CODE_TYPE_ERROR_STATUS, "登録済", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), CODE_ACTIVE));
        codes.add(new CmcodfRecord("90", CODE_TYPE_ERROR_STATUS, "重複", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), CODE_ACTIVE));
        codes.add(new CmcodfRecord("IMP021", CODE_TYPE_AUDIT_ERROR, "監査対象", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), CODE_ACTIVE));
        codes.add(new CmcodfRecord("NAM041", CODE_TYPE_AUDIT_ERROR, "監査対象", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), CODE_ACTIVE));

        ProcessingResult result = process(currentErrors, incomingErrors, codes, businessDate, 800000L);

        for (CmerrfRecord record : result.writtenErrors) {
            System.out.println("CMERRF出力 ERROR-ID=" + record.errorId
                    + " IMPORT-BATCH-ID=" + record.importBatchId
                    + " COMPANY-CODE=" + record.companyCode
                    + " LOCAL-TXN-NO=" + record.localTxnNo
                    + " ERROR-CODE=" + record.errorCode
                    + " ERROR-STATUS-KBN=" + record.errorStatusKbn);
        }

        for (CajrnfRecord record : result.writtenJournals) {
            System.out.println("CAJRNF出力 JOURNAL-SEQ=" + record.journalSeq
                    + " AUDIT-ID=" + record.auditId
                    + " GROUP-REF-NO=" + record.groupRefNo
                    + " EVENT-TYPE-KBN=" + record.eventTypeKbn
                    + " JOURNAL-STATUS-KBN=" + record.journalStatusKbn);
        }
    }

    private static ProcessingResult process(
            java.util.List<CmerrfRecord> existingErrors,
            java.util.List<CmerrfRecord> incomingErrors,
            java.util.List<CmcodfRecord> codeMaster,
            java.time.LocalDate businessDate,
            long firstJournalSeq) {

        if (businessDate == null) {
            throw new IllegalArgumentException("業務日が未設定です");
        }

        java.util.Map<String, CmerrfRecord> registeredByErrorId = new java.util.LinkedHashMap<String, CmerrfRecord>();
        for (CmerrfRecord record : existingErrors) {
            validateErrorRecord(record);
            registeredByErrorId.put(record.errorId, record);
        }

        java.util.Set<String> validStatusCodes = collectValidCodeKeys(codeMaster, CODE_TYPE_ERROR_STATUS, businessDate);
        java.util.Set<String> auditErrorCodes = collectValidCodeKeys(codeMaster, CODE_TYPE_AUDIT_ERROR, businessDate);

        if (!validStatusCodes.contains(STATUS_REGISTERED)) {
            throw new IllegalStateException("コードマスタに登録済ステータスがありません");
        }
        if (!validStatusCodes.contains(STATUS_DUPLICATED)) {
            throw new IllegalStateException("コードマスタに重複ステータスがありません");
        }

        java.util.List<CmerrfRecord> writtenErrors = new java.util.ArrayList<CmerrfRecord>();
        java.util.List<CajrnfRecord> writtenJournals = new java.util.ArrayList<CajrnfRecord>();
        java.util.Set<String> seenInRequest = new java.util.HashSet<String>();

        long journalSeq = firstJournalSeq;

        for (CmerrfRecord incoming : incomingErrors) {
            validateErrorRecord(incoming);

            String fixedStatus;
            if (registeredByErrorId.containsKey(incoming.errorId) || seenInRequest.contains(incoming.errorId)) {
                fixedStatus = STATUS_DUPLICATED;
            } else {
                fixedStatus = STATUS_REGISTERED;
                registeredByErrorId.put(incoming.errorId, incoming.withStatus(fixedStatus));
            }

            seenInRequest.add(incoming.errorId);
            CmerrfRecord output = incoming.withStatus(fixedStatus);
            writtenErrors.add(output);

            if (STATUS_REGISTERED.equals(fixedStatus) && auditErrorCodes.contains(output.errorCode)) {
                writtenJournals.add(new CajrnfRecord(
                        journalSeq,
                        buildAuditId(output),
                        buildGroupRefNo(output),
                        EVENT_TYPE_IMPORT_ERROR,
                        JOURNAL_STATUS_NORMAL));
                journalSeq++;
            }
        }

        return new ProcessingResult(writtenErrors, writtenJournals);
    }

    private static java.util.Set<String> collectValidCodeKeys(
            java.util.List<CmcodfRecord> codeMaster,
            String codeType,
            java.time.LocalDate businessDate) {

        java.util.Set<String> keys = new java.util.HashSet<String>();
        for (CmcodfRecord code : codeMaster) {
            validateCodeRecord(code);
            if (codeType.equals(code.codeType)
                    && CODE_ACTIVE.equals(code.codeStatusKbn)
                    && !businessDate.isBefore(code.validFrom)
                    && !businessDate.isAfter(code.validTo)) {
                keys.add(code.codeKey);
            }
        }
        return keys;
    }

    private static void validateErrorRecord(CmerrfRecord record) {
        if (record == null) {
            throw new IllegalArgumentException("エラー情報が未設定です");
        }
        requireText(record.errorId, "ERROR-ID");
        requireText(record.importBatchId, "IMPORT-BATCH-ID");
        requireText(record.companyCode, "COMPANY-CODE");
        requireText(record.localTxnNo, "LOCAL-TXN-NO");
        requireText(record.errorCode, "ERROR-CODE");
        requireText(record.errorStatusKbn, "ERROR-STATUS-KBN");

        if (record.errorId.length() > 32) {
            throw new IllegalArgumentException("ERROR-IDが桁数超過です: " + record.errorId);
        }
        if (record.companyCode.length() != 3) {
            throw new IllegalArgumentException("COMPANY-CODEが不正です: " + record.companyCode);
        }
    }

    private static void validateCodeRecord(CmcodfRecord record) {
        if (record == null) {
            throw new IllegalArgumentException("コード情報が未設定です");
        }
        requireText(record.codeKey, "CODE-KEY");
        requireText(record.codeType, "CODE-TYPE");
        requireText(record.codeValue, "CODE-VALUE");
        requireText(record.codeStatusKbn, "CODE-STATUS-KBN");

        if (record.validFrom == null || record.validTo == null) {
            throw new IllegalArgumentException("コード有効期間が未設定です: " + record.codeType + "/" + record.codeKey);
        }
        if (record.validFrom.isAfter(record.validTo)) {
            throw new IllegalArgumentException("コード有効期間が逆転しています: " + record.codeType + "/" + record.codeKey);
        }
    }

    private static void requireText(String value, String itemName) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(itemName + "が未設定です");
        }
    }

    private static String buildAuditId(CmerrfRecord record) {
        return "AUD-" + record.importBatchId + "-" + record.errorId;
    }

    private static String buildGroupRefNo(CmerrfRecord record) {
        return record.companyCode + "-" + record.localTxnNo;
    }

    private static final class ProcessingResult {
        private final java.util.List<CmerrfRecord> writtenErrors;
        private final java.util.List<CajrnfRecord> writtenJournals;

        private ProcessingResult(java.util.List<CmerrfRecord> writtenErrors, java.util.List<CajrnfRecord> writtenJournals) {
            this.writtenErrors = java.util.Collections.unmodifiableList(new java.util.ArrayList<CmerrfRecord>(writtenErrors));
            this.writtenJournals = java.util.Collections.unmodifiableList(new java.util.ArrayList<CajrnfRecord>(writtenJournals));
        }
    }

    private static final class CmerrfRecord {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        private CmerrfRecord(
                String errorId,
                String importBatchId,
                String companyCode,
                String localTxnNo,
                String errorCode,
                String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }

        private CmerrfRecord withStatus(String status) {
            return new CmerrfRecord(errorId, importBatchId, companyCode, localTxnNo, errorCode, status);
        }
    }

    private static final class CmcodfRecord {
        private final String codeKey;
        private final String codeType;
        private final String codeValue;
        private final java.time.LocalDate validFrom;
        private final java.time.LocalDate validTo;
        private final String codeStatusKbn;

        private CmcodfRecord(
                String codeKey,
                String codeType,
                String codeValue,
                java.time.LocalDate validFrom,
                java.time.LocalDate validTo,
                String codeStatusKbn) {
            this.codeKey = codeKey;
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.codeStatusKbn = codeStatusKbn;
        }
    }

    private static final class CajrnfRecord {
        private final long journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private CajrnfRecord(
                long journalSeq,
                String auditId,
                String groupRefNo,
                String eventTypeKbn,
                String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }
}
