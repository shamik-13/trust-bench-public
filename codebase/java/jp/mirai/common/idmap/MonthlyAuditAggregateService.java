package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025-06-29  基盤開発  月次監査集計サービス初版
 */
public class MonthlyAuditAggregateService {
    private static final int COMMIT_UNIT = 3;
    private static final String STATUS_FIXED = "01";
    private static final String STATUS_CANCEL = "09";
    private static final String STATUS_OPEN_ERROR = "01";
    private static final String STATUS_MONTH_CONFIRMED = "01";

    public static void main(String[] a) {
        String summaryMonth = a.length == 0 ? "202506" : a[0];
        if (!summaryMonth.matches("\\d{6}")) {
            throw new IllegalArgumentException("対象月はYYYYMMで指定してください");
        }

        java.util.List<Cmaudf> audits = java.util.Arrays.asList(
                new Cmaudf("AU-0001", "GR-BK-0000000001", "BK", "BK-TX-001", "01"),
                new Cmaudf("AU-0002", "GR-BK-0000000002", "BK", "BK-TX-002", "01"),
                new Cmaudf("AU-0003", "GR-SC-0000000001", "SC", "SC-TX-001", "01"),
                new Cmaudf("AU-0004", "GR-CD-0000000001", "CD", "CD-TX-001", "02"),
                new Cmaudf("AU-0005", "GR-PY-0000000001", "PY", "PY-TX-001", "01"),
                new Cmaudf("AU-0006", "GR-LF-0000000001", "LF", "LF-TX-001", "01"),
                new Cmaudf("AU-0007", "GR-CM-0000000001", "CM", "CM-TX-001", "01")
        );

        java.util.List<Cajrnf> journals = java.util.Arrays.asList(
                new Cajrnf(10001L, "AU-0001", "GR-BK-0000000001", "10", "01"),
                new Cajrnf(10002L, "AU-0002", "GR-BK-0000000002", "10", "01"),
                new Cajrnf(10003L, "AU-0003", "GR-SC-0000000001", "20", "01"),
                new Cajrnf(10004L, "AU-0004", "GR-CD-0000000001", "10", "09"),
                new Cajrnf(10005L, "AU-0005", "GR-PY-0000000009", "10", "01"),
                new Cajrnf(10006L, "AU-9999", "GR-LF-0000000001", "10", "01")
        );

        java.util.List<Cmtxnf> transactions = java.util.Arrays.asList(
                new Cmtxnf("TX-0001", "BK", "BK-TX-001", 120000L, "01"),
                new Cmtxnf("TX-0002", "BK", "BK-TX-002", 45000L, "01"),
                new Cmtxnf("TX-0003", "SC", "SC-TX-001", 880000L, "01"),
                new Cmtxnf("TX-0004", "CD", "CD-TX-001", 31500L, "09"),
                new Cmtxnf("TX-0005", "PY", "PY-TX-001", 1800L, "01"),
                new Cmtxnf("TX-0006", "LF", "LF-TX-001", 240000L, "01"),
                new Cmtxnf("TX-0007", "CM", "CM-TX-001", 0L, "01")
        );

        java.util.List<Cmerrf> errors = java.util.Arrays.asList(
                new Cmerrf("ER-0001", "BT-202506-01", "BK", "BK-TX-002", "E101", "01"),
                new Cmerrf("ER-0002", "BT-202506-01", "CD", "CD-TX-001", "E201", "09"),
                new Cmerrf("ER-0003", "BT-202506-02", "PY", "PY-TX-001", "E301", "01"),
                new Cmerrf("ER-0004", "BT-202506-02", "LF", "LF-TX-404", "E404", "01")
        );

        java.util.Map<Key, Camonf> camonf = new java.util.LinkedHashMap<>();
        camonf.put(new Key(summaryMonth, "BK"), new Camonf(summaryMonth, "BK", 99, 99, 99, STATUS_MONTH_CONFIRMED));

        java.util.Map<String, Cmaudf> auditById = new java.util.HashMap<>();
        java.util.Map<String, Cmaudf> auditByCompanyTxn = new java.util.HashMap<>();
        for (Cmaudf audit : audits) {
            auditById.put(audit.auditId, audit);
            auditByCompanyTxn.put(audit.companyCode + "\u0000" + audit.localTxnNo, audit);
        }

        java.util.Map<String, java.util.List<Cajrnf>> journalByAuditId = new java.util.HashMap<>();
        for (Cajrnf journal : journals) {
            journalByAuditId.computeIfAbsent(journal.auditId, k -> new java.util.ArrayList<>()).add(journal);
        }

        java.util.Map<String, CompanyAggregate> aggregateByCompany = new java.util.LinkedHashMap<>();
        for (String code : java.util.Arrays.asList("BK", "SC", "CD", "PY", "LF", "CM")) {
            aggregateByCompany.put(code, new CompanyAggregate(code));
        }

        int processed = 0;
        for (Cmtxnf transaction : transactions) {
            CompanyAggregate aggregate = requireCompany(aggregateByCompany, transaction.companyCode);
            if (STATUS_FIXED.equals(transaction.txnStatusKbn)) {
                aggregate.txnCount++;
            }

            Cmaudf audit = auditByCompanyTxn.get(transaction.companyCode + "\u0000" + transaction.localTxnNo);
            if (audit != null) {
                aggregate.auditCount++;
                if (!STATUS_FIXED.equals(audit.auditStatusKbn)) {
                    aggregate.mismatchKeys.add(transaction.localTxnNo);
                }
                java.util.List<Cajrnf> linkedJournals = journalByAuditId.getOrDefault(audit.auditId, java.util.Collections.emptyList());
                if (linkedJournals.isEmpty()) {
                    aggregate.mismatchKeys.add(transaction.localTxnNo);
                }
                for (Cajrnf journal : linkedJournals) {
                    if (!audit.groupRefNo.equals(journal.groupRefNo) || !STATUS_FIXED.equals(journal.journalStatusKbn)) {
                        aggregate.mismatchKeys.add(transaction.localTxnNo);
                    }
                }
            } else if (STATUS_FIXED.equals(transaction.txnStatusKbn)) {
                aggregate.mismatchKeys.add(transaction.localTxnNo);
            }

            processed++;
            if (processed % COMMIT_UNIT == 0) {
                System.out.println("分割コミット 件数=" + processed);
            }
        }

        for (Cmaudf audit : audits) {
            if (!aggregateByCompany.containsKey(audit.companyCode)) {
                throw new IllegalStateException("会社コードが不正です " + audit.companyCode);
            }
            boolean transactionExists = false;
            for (Cmtxnf transaction : transactions) {
                if (audit.companyCode.equals(transaction.companyCode) && audit.localTxnNo.equals(transaction.localTxnNo)) {
                    transactionExists = true;
                    break;
                }
            }
            if (!transactionExists) {
                requireCompany(aggregateByCompany, audit.companyCode).mismatchKeys.add(audit.localTxnNo);
            }
        }

        for (Cajrnf journal : journals) {
            Cmaudf audit = auditById.get(journal.auditId);
            if (audit == null) {
                continue;
            }
            if (!audit.groupRefNo.equals(journal.groupRefNo)) {
                requireCompany(aggregateByCompany, audit.companyCode).mismatchKeys.add(audit.localTxnNo);
            }
        }

        for (Cmerrf error : errors) {
            CompanyAggregate aggregate = requireCompany(aggregateByCompany, error.companyCode);
            if (STATUS_OPEN_ERROR.equals(error.errorStatusKbn)) {
                aggregate.openErrorCount++;
                aggregate.mismatchKeys.add(error.localTxnNo);
            }
        }

        for (CompanyAggregate aggregate : aggregateByCompany.values()) {
            Key key = new Key(summaryMonth, aggregate.companyCode);
            camonf.remove(key);
            camonf.put(key, new Camonf(
                    summaryMonth,
                    aggregate.companyCode,
                    aggregate.txnCount,
                    aggregate.auditCount,
                    aggregate.mismatchKeys.size(),
                    STATUS_MONTH_CONFIRMED));
        }

        for (Camonf row : camonf.values()) {
            System.out.println(row.summaryMonth + "," + row.companyCode + "," + row.txnCount + ","
                    + row.auditCount + "," + row.mismatchCount + "," + row.summaryStatusKbn);
        }
    }

    private static CompanyAggregate requireCompany(java.util.Map<String, CompanyAggregate> aggregates, String companyCode) {
        CompanyAggregate aggregate = aggregates.get(companyCode);
        if (aggregate == null) {
            throw new IllegalArgumentException("会社コードが不正です " + companyCode);
        }
        return aggregate;
    }

    private static final class CompanyAggregate {
        private final String companyCode;
        private int txnCount;
        private int auditCount;
        private int openErrorCount;
        private final java.util.Set<String> mismatchKeys = new java.util.LinkedHashSet<>();

        private CompanyAggregate(String companyCode) {
            this.companyCode = companyCode;
        }
    }

    private static final class Key {
        private final String summaryMonth;
        private final String companyCode;

        private Key(String summaryMonth, String companyCode) {
            this.summaryMonth = summaryMonth;
            this.companyCode = companyCode;
        }

        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof Key)) {
                return false;
            }
            Key key = (Key) other;
            return summaryMonth.equals(key.summaryMonth) && companyCode.equals(key.companyCode);
        }

        public int hashCode() {
            return java.util.Objects.hash(summaryMonth, companyCode);
        }
    }

    private static final class Cmaudf {
        private final String auditId;
        private final String groupRefNo;
        private final String companyCode;
        private final String localTxnNo;
        private final String auditStatusKbn;

        private Cmaudf(String auditId, String groupRefNo, String companyCode, String localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    private static final class Cajrnf {
        private final long journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private Cajrnf(long journalSeq, String auditId, String groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }

    private static final class Cmtxnf {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final long txnAmt;
        private final String txnStatusKbn;

        private Cmtxnf(String txnId, String companyCode, String localTxnNo, long txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class Cmerrf {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        private Cmerrf(String errorId, String importBatchId, String companyCode, String localTxnNo, String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }
    }

    private static final class Camonf {
        private final String summaryMonth;
        private final String companyCode;
        private final int txnCount;
        private final int auditCount;
        private final int mismatchCount;
        private final String summaryStatusKbn;

        private Camonf(String summaryMonth, String companyCode, int txnCount, int auditCount, int mismatchCount, String summaryStatusKbn) {
            this.summaryMonth = summaryMonth;
            this.companyCode = companyCode;
            this.txnCount = txnCount;
            this.auditCount = auditCount;
            this.mismatchCount = mismatchCount;
            this.summaryStatusKbn = summaryStatusKbn;
        }
    }
}
