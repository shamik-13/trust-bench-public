package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025/06/29  共通基盤  初版作成
 */
public class InboundTransactionImportService {
    public static void main(String[] a) {
        java.util.List<ImportBatchRecord> cminbf = new java.util.ArrayList<ImportBatchRecord>();
        cminbf.add(new ImportBatchRecord("IB20250629001", "BKCRM", "2025-06-29T01:00:00+09:00", 4, "01"));
        cminbf.add(new ImportBatchRecord("IB20250629002", "SCTR", "2025-06-29T01:15:00+09:00", 3, "01"));

        java.util.List<CodeRecord> cmcodf = new java.util.ArrayList<CodeRecord>();
        cmcodf.add(new CodeRecord("COMPANY:BK", "COMPANY-CODE", "BK", "2020-01-01", "9999-12-31", "1"));
        cmcodf.add(new CodeRecord("COMPANY:SC", "COMPANY-CODE", "SC", "2020-01-01", "9999-12-31", "1"));
        cmcodf.add(new CodeRecord("COMPANY:CD", "COMPANY-CODE", "CD", "2020-01-01", "9999-12-31", "1"));
        cmcodf.add(new CodeRecord("COMPANY:PY", "COMPANY-CODE", "PY", "2020-01-01", "9999-12-31", "1"));
        cmcodf.add(new CodeRecord("COMPANY:LF", "COMPANY-CODE", "LF", "2020-01-01", "9999-12-31", "1"));
        cmcodf.add(new CodeRecord("COMPANY:CM", "COMPANY-CODE", "CM", "2020-01-01", "9999-12-31", "1"));
        cmcodf.add(new CodeRecord("STATUS:01", "TXN-STATUS-KBN", "01", "2020-01-01", "9999-12-31", "1"));
        cmcodf.add(new CodeRecord("STATUS:09", "TXN-STATUS-KBN", "09", "2020-01-01", "9999-12-31", "1"));

        java.util.Map<String, java.util.List<InboundDetailRecord>> details = new java.util.LinkedHashMap<String, java.util.List<InboundDetailRecord>>();
        details.put("IB20250629001", java.util.Arrays.asList(
                new InboundDetailRecord("IB20250629001", "BK", "bk-20250629-0001", "120000", "01"),
                new InboundDetailRecord("IB20250629001", "BK", "bk-20250629-0002", "3500", "01"),
                new InboundDetailRecord("IB20250629001", "BK", "bk-20250629-0001", "120000", "01"),
                new InboundDetailRecord("IB20250629001", "XX", "xx-20250629-0001", "9000", "01")
        ));
        details.put("IB20250629002", java.util.Arrays.asList(
                new InboundDetailRecord("IB20250629002", "SC", " sc-20250629-0001 ", "7800000", "01"),
                new InboundDetailRecord("IB20250629002", "SC", "sc-20250629-0002", "-15000", "09"),
                new InboundDetailRecord("IB20250629002", "SC", "sc-20250629-0003", "64000", "77")
        ));

        ServiceResult result = importTransactions(cminbf, cmcodf, details, java.time.LocalDate.of(2025, 6, 29));

        System.out.println("取引明細取込サービス 処理結果");
        System.out.println("正常出力件数=" + result.txnRecords.size());
        System.out.println("エラー登録件数=" + result.errorRecords.size());
        System.out.println("正常金額合計=" + result.totalAmount);
        for (TransactionRecord record : result.txnRecords) {
            System.out.println(record.toLine());
        }
        for (ErrorRecord record : result.errorRecords) {
            System.out.println(record.toLine());
        }
    }

    private static ServiceResult importTransactions(
            java.util.List<ImportBatchRecord> batches,
            java.util.List<CodeRecord> codes,
            java.util.Map<String, java.util.List<InboundDetailRecord>> detailsByBatch,
            java.time.LocalDate businessDate) {
        CodeTable codeTable = new CodeTable(codes, businessDate);
        java.util.Set<String> seenLocalTxnNo = new java.util.HashSet<String>();
        java.util.List<TransactionRecord> cmtxnf = new java.util.ArrayList<TransactionRecord>();
        java.util.List<ErrorRecord> cmerrf = new java.util.ArrayList<ErrorRecord>();
        java.math.BigDecimal totalAmount = java.math.BigDecimal.ZERO;
        int txnSeq = 1;
        int errSeq = 1;

        for (ImportBatchRecord batch : batches) {
            if (!"01".equals(batch.importStatusKbn)) {
                cmerrf.add(new ErrorRecord(nextErrorId(errSeq++), batch.importBatchId, "", "", "BATCH-STATUS", "1"));
                continue;
            }

            java.util.List<InboundDetailRecord> details = detailsByBatch.get(batch.importBatchId);
            int actualCount = details == null ? 0 : details.size();
            if (actualCount != batch.recordCount) {
                cmerrf.add(new ErrorRecord(nextErrorId(errSeq++), batch.importBatchId, "", "", "COUNT-MISMATCH", "1"));
            }
            if (details == null) {
                continue;
            }

            for (InboundDetailRecord detail : details) {
                NormalizedDetail normalized = normalize(detail);
                String errorCode = validate(normalized, codeTable, seenLocalTxnNo);
                if (errorCode != null) {
                    cmerrf.add(new ErrorRecord(nextErrorId(errSeq++), normalized.importBatchId,
                            normalized.companyCode, normalized.localTxnNo, errorCode, "1"));
                    continue;
                }

                seenLocalTxnNo.add(normalized.localTxnNo);
                cmtxnf.add(new TransactionRecord(nextTxnId(txnSeq++), normalized.companyCode,
                        normalized.localTxnNo, normalized.txnAmount, normalized.txnStatusKbn));
                totalAmount = totalAmount.add(normalized.txnAmount);
            }
        }

        return new ServiceResult(cmtxnf, cmerrf, totalAmount);
    }

    private static NormalizedDetail normalize(InboundDetailRecord detail) {
        String companyCode = trimUpper(detail.companyCode);
        String localTxnNo = trimUpper(detail.localTxnNo);
        String status = trim(detail.txnStatusKbn);
        java.math.BigDecimal amount = parseAmount(detail.txnAmount);
        return new NormalizedDetail(detail.importBatchId, companyCode, localTxnNo, amount, status);
    }

    private static String validate(NormalizedDetail detail, CodeTable codeTable, java.util.Set<String> seenLocalTxnNo) {
        if (detail.companyCode.length() == 0 || !codeTable.isValid("COMPANY-CODE", detail.companyCode)) {
            return "UNDEFINED-COMPANY";
        }
        if (detail.localTxnNo.length() == 0) {
            return "LOCAL-TXN-NO-BLANK";
        }
        if (seenLocalTxnNo.contains(detail.localTxnNo)) {
            return "DUP-LOCAL-TXN-NO";
        }
        if (detail.txnAmount == null) {
            return "AMOUNT-FORMAT";
        }
        if (detail.txnAmount.scale() > 0 || detail.txnAmount.compareTo(java.math.BigDecimal.ZERO) == 0) {
            return "AMOUNT-RANGE";
        }
        if (!codeTable.isValid("TXN-STATUS-KBN", detail.txnStatusKbn)) {
            return "UNDEFINED-STATUS";
        }
        return null;
    }

    private static java.math.BigDecimal parseAmount(String value) {
        try {
            return new java.math.BigDecimal(trim(value));
        } catch (RuntimeException e) {
            return null;
        }
    }

    private static String nextTxnId(int seq) {
        return String.format("TXN%010d", Integer.valueOf(seq));
    }

    private static String nextErrorId(int seq) {
        return String.format("ERR%010d", Integer.valueOf(seq));
    }

    private static String trimUpper(String value) {
        return trim(value).toUpperCase(java.util.Locale.ROOT);
    }

    private static String trim(String value) {
        return value == null ? "" : value.trim();
    }

    private static final class CodeTable {
        private final java.util.Set<String> validCodes = new java.util.HashSet<String>();

        CodeTable(java.util.List<CodeRecord> codes, java.time.LocalDate businessDate) {
            for (CodeRecord code : codes) {
                java.time.LocalDate from = java.time.LocalDate.parse(code.validFrom);
                java.time.LocalDate to = java.time.LocalDate.parse(code.validTo);
                if ("1".equals(code.codeStatusKbn)
                        && !businessDate.isBefore(from)
                        && !businessDate.isAfter(to)) {
                    validCodes.add(code.codeType + ":" + code.codeValue);
                }
            }
        }

        boolean isValid(String type, String value) {
            return validCodes.contains(type + ":" + value);
        }
    }

    private static final class ImportBatchRecord {
        private final String importBatchId;
        private final String sourceSystemId;
        private final String receivedAt;
        private final int recordCount;
        private final String importStatusKbn;

        ImportBatchRecord(String importBatchId, String sourceSystemId, String receivedAt, int recordCount, String importStatusKbn) {
            this.importBatchId = importBatchId;
            this.sourceSystemId = sourceSystemId;
            this.receivedAt = receivedAt;
            this.recordCount = recordCount;
            this.importStatusKbn = importStatusKbn;
        }
    }

    private static final class CodeRecord {
        private final String codeKey;
        private final String codeType;
        private final String codeValue;
        private final String validFrom;
        private final String validTo;
        private final String codeStatusKbn;

        CodeRecord(String codeKey, String codeType, String codeValue, String validFrom, String validTo, String codeStatusKbn) {
            this.codeKey = codeKey;
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.codeStatusKbn = codeStatusKbn;
        }
    }

    private static final class InboundDetailRecord {
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String txnAmount;
        private final String txnStatusKbn;

        InboundDetailRecord(String importBatchId, String companyCode, String localTxnNo, String txnAmount, String txnStatusKbn) {
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmount = txnAmount;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class NormalizedDetail {
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final java.math.BigDecimal txnAmount;
        private final String txnStatusKbn;

        NormalizedDetail(String importBatchId, String companyCode, String localTxnNo,
                         java.math.BigDecimal txnAmount, String txnStatusKbn) {
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmount = txnAmount;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class TransactionRecord {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final java.math.BigDecimal txnAmt;
        private final String txnStatusKbn;

        TransactionRecord(String txnId, String companyCode, String localTxnNo,
                          java.math.BigDecimal txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }

        String toLine() {
            return "CMTXNF " + txnId + "," + companyCode + "," + localTxnNo + "," + txnAmt.toPlainString() + "," + txnStatusKbn;
        }
    }

    private static final class ErrorRecord {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        ErrorRecord(String errorId, String importBatchId, String companyCode,
                    String localTxnNo, String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }

        String toLine() {
            return "CMERRF " + errorId + "," + importBatchId + "," + companyCode + "," + localTxnNo + "," + errorCode + "," + errorStatusKbn;
        }
    }

    private static final class ServiceResult {
        private final java.util.List<TransactionRecord> txnRecords;
        private final java.util.List<ErrorRecord> errorRecords;
        private final java.math.BigDecimal totalAmount;

        ServiceResult(java.util.List<TransactionRecord> txnRecords,
                      java.util.List<ErrorRecord> errorRecords,
                      java.math.BigDecimal totalAmount) {
            this.txnRecords = txnRecords;
            this.errorRecords = errorRecords;
            this.totalAmount = totalAmount;
        }
    }
}
