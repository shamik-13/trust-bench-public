package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2025-06-29  共通基盤    会社間連携受信サービス初版
 */
public class CompanyRelayReceiveService {
    private static final java.time.LocalDate BUSINESS_DATE = java.time.LocalDate.of(2025, 6, 29);
    private static final java.time.format.DateTimeFormatter DATE_FORMAT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    private static final String CODE_TYPE_COMPANY = "COMPANY-CODE";
    private static final String CODE_TYPE_TXN_STATUS = "TX-TXN-STATUS-KBN";
    private static final String CODE_STATUS_VALID = "1";

    private static final String EVENT_TYPE_RECEIVE = "10";
    private static final String JOURNAL_STATUS_NORMAL = "01";
    private static final String JOURNAL_STATUS_ERROR = "09";
    private static final String ERROR_STATUS_RETURNABLE = "01";

    private static long txnSequence = 900000000000L;
    private static long journalSequence = 700000000000L;
    private static long errorSequence = 800000000000L;

    public static void main(String[] a) {
        java.util.List<CodeRecord> codeMaster = loadCodeMaster();
        java.util.List<RelayReceiveRecord> receiveRecords = loadReceiveRecords();

        java.util.List<TransactionRecord> transactions = new java.util.ArrayList<>();
        java.util.List<JournalRecord> journals = new java.util.ArrayList<>();
        java.util.List<ErrorRecord> errors = new java.util.ArrayList<>();

        java.util.Map<String, CodeRecord> codeIndex = buildCodeIndex(codeMaster);

        int receivedCount = 0;
        int storedCount = 0;
        long storedAmount = 0L;

        for (RelayReceiveRecord receive : receiveRecords) {
            receivedCount++;

            java.util.List<String> validationErrors = validate(receive, codeIndex);
            String auditId = buildAuditId(receive.importBatchId, receivedCount);

            if (validationErrors.isEmpty()) {
                TransactionRecord transaction = new TransactionRecord(
                        nextTxnId(),
                        receive.companyCode,
                        receive.localTxnNo,
                        receive.txnAmount,
                        receive.txnStatusKbn);
                transactions.add(transaction);

                journals.add(new JournalRecord(
                        nextJournalSeq(),
                        auditId,
                        receive.groupRefNo,
                        EVENT_TYPE_RECEIVE,
                        JOURNAL_STATUS_NORMAL));

                storedCount++;
                storedAmount += receive.txnAmount;
            } else {
                for (String errorCode : validationErrors) {
                    errors.add(new ErrorRecord(
                            nextErrorId(),
                            receive.importBatchId,
                            receive.companyCode,
                            receive.localTxnNo,
                            errorCode,
                            ERROR_STATUS_RETURNABLE));
                }

                journals.add(new JournalRecord(
                        nextJournalSeq(),
                        auditId,
                        receive.groupRefNo,
                        EVENT_TYPE_RECEIVE,
                        JOURNAL_STATUS_ERROR));
            }
        }

        printSummary(receivedCount, storedCount, storedAmount, transactions, journals, errors);
    }

    private static java.util.List<CodeRecord> loadCodeMaster() {
        java.util.List<CodeRecord> records = new java.util.ArrayList<>();
        records.add(new CodeRecord("COMPANY-CODE:BK", CODE_TYPE_COMPANY, "BK", "20200101", "20991231", CODE_STATUS_VALID));
        records.add(new CodeRecord("COMPANY-CODE:SC", CODE_TYPE_COMPANY, "SC", "20200101", "20991231", CODE_STATUS_VALID));
        records.add(new CodeRecord("COMPANY-CODE:CD", CODE_TYPE_COMPANY, "CD", "20200101", "20991231", CODE_STATUS_VALID));
        records.add(new CodeRecord("COMPANY-CODE:PY", CODE_TYPE_COMPANY, "PY", "20200101", "20991231", CODE_STATUS_VALID));
        records.add(new CodeRecord("COMPANY-CODE:LF", CODE_TYPE_COMPANY, "LF", "20200101", "20991231", CODE_STATUS_VALID));
        records.add(new CodeRecord("COMPANY-CODE:CM", CODE_TYPE_COMPANY, "CM", "20200101", "20991231", CODE_STATUS_VALID));
        records.add(new CodeRecord("TX-TXN-STATUS-KBN:01", CODE_TYPE_TXN_STATUS, "01", "20200101", "20991231", CODE_STATUS_VALID));
        records.add(new CodeRecord("TX-TXN-STATUS-KBN:09", CODE_TYPE_TXN_STATUS, "09", "20200101", "20991231", CODE_STATUS_VALID));
        return records;
    }

    private static java.util.List<RelayReceiveRecord> loadReceiveRecords() {
        java.util.List<RelayReceiveRecord> records = new java.util.ArrayList<>();
        records.add(new RelayReceiveRecord("IMP20250629001", "BK", "BK-TR-000001", 1250000L, "01", "GRP-20250629-000001"));
        records.add(new RelayReceiveRecord("IMP20250629001", "SC", "SC-TR-000778", 934000L, "01", "GRP-20250629-000002"));
        records.add(new RelayReceiveRecord("IMP20250629001", "CD", "CD-TR-004301", 21800L, "09", "GRP-20250629-000003"));
        records.add(new RelayReceiveRecord("IMP20250629001", "PY", "PY-TR-891120", 5600L, "01", "GRP-20250629-000004"));
        records.add(new RelayReceiveRecord("IMP20250629001", "LF", "LF-TR-000045", 880000L, "01", "GRP-20250629-000005"));
        records.add(new RelayReceiveRecord("IMP20250629001", "XX", "XX-TR-000006", 15000L, "01", "GRP-20250629-000006"));
        records.add(new RelayReceiveRecord("IMP20250629001", "BK", "BK-TR-000007", 480000L, "03", "GRP-20250629-000007"));
        records.add(new RelayReceiveRecord("IMP20250629001", "SC", "SC-TR-000779", -3000L, "01", "GRP-20250629-000008"));
        records.add(new RelayReceiveRecord("IMP20250629001", "CM", "CM-TR-100001", 0L, "09", "GRP-20250629-000009"));
        return records;
    }

    private static java.util.Map<String, CodeRecord> buildCodeIndex(java.util.List<CodeRecord> records) {
        java.util.Map<String, CodeRecord> index = new java.util.HashMap<>();
        for (CodeRecord record : records) {
            index.put(record.codeType + ":" + record.codeValue, record);
        }
        return index;
    }

    private static java.util.List<String> validate(
            RelayReceiveRecord receive,
            java.util.Map<String, CodeRecord> codeIndex) {
        java.util.List<String> errors = new java.util.ArrayList<>();

        if (!isActiveCode(codeIndex.get(CODE_TYPE_COMPANY + ":" + receive.companyCode))) {
            errors.add("E-COMPANY-CODE");
        }
        if (!isActiveCode(codeIndex.get(CODE_TYPE_TXN_STATUS + ":" + receive.txnStatusKbn))) {
            errors.add("E-TXN-STATUS-KBN");
        }
        if (receive.localTxnNo == null || receive.localTxnNo.trim().isEmpty()) {
            errors.add("E-LOCAL-TXN-NO");
        }
        if (receive.txnAmount < 0L) {
            errors.add("E-TXN-AMT");
        }
        if (receive.groupRefNo == null || receive.groupRefNo.trim().isEmpty()) {
            errors.add("E-GROUP-REF-NO");
        }

        return errors;
    }

    private static boolean isActiveCode(CodeRecord code) {
        if (code == null) {
            return false;
        }
        java.time.LocalDate from = java.time.LocalDate.parse(code.validFrom, DATE_FORMAT);
        java.time.LocalDate to = java.time.LocalDate.parse(code.validTo, DATE_FORMAT);
        return CODE_STATUS_VALID.equals(code.codeStatusKbn)
                && !BUSINESS_DATE.isBefore(from)
                && !BUSINESS_DATE.isAfter(to);
    }

    private static String nextTxnId() {
        txnSequence++;
        return "TXN" + txnSequence;
    }

    private static String nextJournalSeq() {
        journalSequence++;
        return "JRN" + journalSequence;
    }

    private static String nextErrorId() {
        errorSequence++;
        return "ERR" + errorSequence;
    }

    private static String buildAuditId(String importBatchId, int lineNo) {
        return importBatchId + "-" + String.format("%05d", lineNo);
    }

    private static void printSummary(
            int receivedCount,
            int storedCount,
            long storedAmount,
            java.util.List<TransactionRecord> transactions,
            java.util.List<JournalRecord> journals,
            java.util.List<ErrorRecord> errors) {
        System.out.println("受信件数=" + receivedCount);
        System.out.println("格納件数=" + storedCount);
        System.out.println("格納金額=" + storedAmount);
        System.out.println("CMTXNF件数=" + transactions.size());
        System.out.println("CAJRNF件数=" + journals.size());
        System.out.println("CMERRF件数=" + errors.size());

        for (TransactionRecord record : transactions) {
            System.out.println("CMTXNF "
                    + record.txnId + ","
                    + record.companyCode + ","
                    + record.localTxnNo + ","
                    + record.txnAmount + ","
                    + record.txnStatusKbn);
        }

        for (JournalRecord record : journals) {
            System.out.println("CAJRNF "
                    + record.journalSeq + ","
                    + record.auditId + ","
                    + record.groupRefNo + ","
                    + record.eventTypeKbn + ","
                    + record.journalStatusKbn);
        }

        for (ErrorRecord record : errors) {
            System.out.println("CMERRF "
                    + record.errorId + ","
                    + record.importBatchId + ","
                    + record.companyCode + ","
                    + record.localTxnNo + ","
                    + record.errorCode + ","
                    + record.errorStatusKbn);
        }
    }

    private static final class CodeRecord {
        private final String codeKey;
        private final String codeType;
        private final String codeValue;
        private final String validFrom;
        private final String validTo;
        private final String codeStatusKbn;

        private CodeRecord(
                String codeKey,
                String codeType,
                String codeValue,
                String validFrom,
                String validTo,
                String codeStatusKbn) {
            this.codeKey = codeKey;
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.codeStatusKbn = codeStatusKbn;
        }
    }

    private static final class RelayReceiveRecord {
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final long txnAmount;
        private final String txnStatusKbn;
        private final String groupRefNo;

        private RelayReceiveRecord(
                String importBatchId,
                String companyCode,
                String localTxnNo,
                long txnAmount,
                String txnStatusKbn,
                String groupRefNo) {
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmount = txnAmount;
            this.txnStatusKbn = txnStatusKbn;
            this.groupRefNo = groupRefNo;
        }
    }

    private static final class TransactionRecord {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final long txnAmount;
        private final String txnStatusKbn;

        private TransactionRecord(
                String txnId,
                String companyCode,
                String localTxnNo,
                long txnAmount,
                String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmount = txnAmount;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class JournalRecord {
        private final String journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private JournalRecord(
                String journalSeq,
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

    private static final class ErrorRecord {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        private ErrorRecord(
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
    }
}
