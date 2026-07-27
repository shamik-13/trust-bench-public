package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2025-06-29  共通基盤  初版作成。取引照会結果の監査提出用エクスポート前判定を実装。
 */
public class TransactionInquiryExportService {
    private static final java.time.Clock CLOCK = java.time.Clock.system(java.time.ZoneId.of("Asia/Tokyo"));
    private static final java.util.Set<String> COMPANY_CODES =
            new java.util.LinkedHashSet<String>(java.util.Arrays.asList("BK", "SC", "CD", "PY", "LF", "CM"));
    private static final java.util.Set<String> TXN_STATUS_CODES =
            new java.util.LinkedHashSet<String>(java.util.Arrays.asList("01", "09"));
    private static final java.util.Set<String> AUDIT_STATUS_CODES =
            new java.util.LinkedHashSet<String>(java.util.Arrays.asList("10", "20", "90"));
    private static final java.util.Set<String> JOURNAL_STATUS_CODES =
            new java.util.LinkedHashSet<String>(java.util.Arrays.asList("00", "10", "90"));
    private static final java.util.Set<String> EVENT_TYPE_CODES =
            new java.util.LinkedHashSet<String>(java.util.Arrays.asList("TX", "AU", "RV"));

    public static void main(String[] a) {
        java.time.YearMonth targetMonth = resolveTargetMonth(a);
        ExportResult result = export(targetMonth, syntheticTransactions(), syntheticAudits(), syntheticJournals());

        System.out.println("対象年月=" + targetMonth);
        System.out.println("帳票件数=" + result.reports.size());
        for (Cmrptf report : result.reports) {
            System.out.println(report.toLine());
        }
        System.out.println("エラー登録候補件数=" + result.errors.size());
        for (CmerrfCandidate error : result.errors) {
            System.out.println(error.toLine());
        }
    }

    private static ExportResult export(
            java.time.YearMonth targetMonth,
            java.util.List<Cmtxnf> transactions,
            java.util.List<Cmaudf> audits,
            java.util.List<Cajrnf> journals) {

        java.util.List<CmerrfCandidate> errors = new java.util.ArrayList<CmerrfCandidate>();
        java.util.Map<TxnKey, Cmtxnf> txnByKey = new java.util.LinkedHashMap<TxnKey, Cmtxnf>();

        for (Cmtxnf txn : transactions) {
            java.util.List<String> violations = validateTransaction(txn);
            if (!violations.isEmpty()) {
                errors.add(CmerrfCandidate.of("CMTXNF", txn.localTxnNo, "-", joinViolations(violations)));
                continue;
            }
            TxnKey key = new TxnKey(txn.companyCode, txn.localTxnNo);
            Cmtxnf previous = txnByKey.putIfAbsent(key, txn);
            if (previous != null) {
                errors.add(CmerrfCandidate.of("CMTXNF", txn.localTxnNo, "-", "取引重複"));
            }
        }

        java.util.Map<String, java.util.List<Cajrnf>> journalsByAuditId = new java.util.LinkedHashMap<String, java.util.List<Cajrnf>>();
        for (Cajrnf journal : journals) {
            java.util.List<String> violations = validateJournal(journal);
            if (!violations.isEmpty()) {
                errors.add(CmerrfCandidate.of("CAJRNF", journal.auditId, journal.groupRefNo, joinViolations(violations)));
                continue;
            }
            java.util.List<Cajrnf> bucket = journalsByAuditId.get(journal.auditId);
            if (bucket == null) {
                bucket = new java.util.ArrayList<Cajrnf>();
                journalsByAuditId.put(journal.auditId, bucket);
            }
            bucket.add(journal);
        }

        java.util.Map<String, Aggregation> aggregations = new java.util.TreeMap<String, Aggregation>();
        java.util.Set<String> seenAuditIds = new java.util.LinkedHashSet<String>();

        for (Cmaudf audit : audits) {
            java.util.List<String> violations = validateAudit(audit);
            if (!violations.isEmpty()) {
                errors.add(CmerrfCandidate.of("CMAUDF", audit.auditId, audit.groupRefNo, joinViolations(violations)));
                continue;
            }
            if (!seenAuditIds.add(audit.auditId)) {
                errors.add(CmerrfCandidate.of("CMAUDF", audit.auditId, audit.groupRefNo, "監査重複"));
                continue;
            }

            Cmtxnf txn = txnByKey.get(new TxnKey(audit.companyCode, audit.localTxnNo));
            if (txn == null) {
                errors.add(CmerrfCandidate.of("CMAUDF", audit.auditId, audit.groupRefNo, "取引未確認"));
                continue;
            }
            if (!"01".equals(txn.txnStatusKbn)) {
                errors.add(CmerrfCandidate.of("CMAUDF", audit.auditId, audit.groupRefNo, "取引取消"));
                continue;
            }

            java.util.List<Cajrnf> linkedJournals = journalsByAuditId.get(audit.auditId);
            if (linkedJournals == null || linkedJournals.isEmpty()) {
                errors.add(CmerrfCandidate.of("CMAUDF", audit.auditId, audit.groupRefNo, "監査イベント未確認"));
                continue;
            }

            boolean accepted = false;
            for (Cajrnf journal : linkedJournals) {
                if (audit.groupRefNo.equals(journal.groupRefNo) && "10".equals(journal.journalStatusKbn)) {
                    accepted = true;
                    break;
                }
            }
            if (!accepted) {
                errors.add(CmerrfCandidate.of("CAJRNF", audit.auditId, audit.groupRefNo, "提出対象イベントなし"));
                continue;
            }

            String type = reportType(audit.companyCode, txn.txnAmt);
            Aggregation aggregation = aggregations.get(type);
            if (aggregation == null) {
                aggregation = new Aggregation(type);
                aggregations.put(type, aggregation);
            }
            aggregation.accept(txn);
        }

        java.time.LocalDateTime createdAt = java.time.LocalDateTime.now(CLOCK).withNano(0);
        java.util.List<Cmrptf> reports = new java.util.ArrayList<Cmrptf>();
        int sequence = 1;
        for (Aggregation aggregation : aggregations.values()) {
            String status = aggregation.cancelCount == 0 ? "10" : "20";
            reports.add(new Cmrptf(
                    String.format("RPT-%s-%03d", targetMonth.toString().replace("-", ""), sequence++),
                    aggregation.reportTypeKbn,
                    targetMonth.toString(),
                    status,
                    createdAt));
        }

        return new ExportResult(reports, errors);
    }

    private static java.time.YearMonth resolveTargetMonth(String[] args) {
        if (args != null && args.length > 0) {
            try {
                return java.time.YearMonth.parse(args[0]);
            } catch (java.time.format.DateTimeParseException ex) {
                System.err.println("対象年月の指定が不正です。形式はyyyy-MMです。");
            }
        }
        return java.time.YearMonth.now(CLOCK).minusMonths(1);
    }

    private static java.util.List<String> validateTransaction(Cmtxnf txn) {
        java.util.List<String> violations = new java.util.ArrayList<String>();
        requireText(violations, txn.txnId, "取引ID未設定");
        requireCode(violations, txn.companyCode, COMPANY_CODES, "会社コード不正");
        requireText(violations, txn.localTxnNo, "ローカル取引番号未設定");
        if (txn.txnAmt == null || txn.txnAmt.signum() <= 0) {
            violations.add("取引金額不正");
        }
        requireCode(violations, txn.txnStatusKbn, TXN_STATUS_CODES, "取引状態不正");
        return violations;
    }

    private static java.util.List<String> validateAudit(Cmaudf audit) {
        java.util.List<String> violations = new java.util.ArrayList<String>();
        requireText(violations, audit.auditId, "監査ID未設定");
        requireText(violations, audit.groupRefNo, "統合参照番号未設定");
        requireCode(violations, audit.companyCode, COMPANY_CODES, "会社コード不正");
        requireText(violations, audit.localTxnNo, "ローカル取引番号未設定");
        requireCode(violations, audit.auditStatusKbn, AUDIT_STATUS_CODES, "監査状態不正");
        return violations;
    }

    private static java.util.List<String> validateJournal(Cajrnf journal) {
        java.util.List<String> violations = new java.util.ArrayList<String>();
        if (journal.journalSeq <= 0) {
            violations.add("ジャーナル順序不正");
        }
        requireText(violations, journal.auditId, "監査ID未設定");
        requireText(violations, journal.groupRefNo, "統合参照番号未設定");
        requireCode(violations, journal.eventTypeKbn, EVENT_TYPE_CODES, "イベント種別不正");
        requireCode(violations, journal.journalStatusKbn, JOURNAL_STATUS_CODES, "ジャーナル状態不正");
        return violations;
    }

    private static void requireText(java.util.List<String> violations, String value, String message) {
        if (value == null || value.trim().isEmpty()) {
            violations.add(message);
        }
    }

    private static void requireCode(java.util.List<String> violations, String value, java.util.Set<String> allowed, String message) {
        if (value == null || !allowed.contains(value)) {
            violations.add(message);
        }
    }

    private static String reportType(String companyCode, java.math.BigDecimal amount) {
        if ("BK".equals(companyCode) || "SC".equals(companyCode)) {
            return amount.compareTo(new java.math.BigDecimal("10000000")) >= 0 ? "HVAL" : "FIN";
        }
        if ("CD".equals(companyCode) || "PY".equals(companyCode)) {
            return "PAY";
        }
        if ("LF".equals(companyCode)) {
            return "INS";
        }
        return "COM";
    }

    private static String joinViolations(java.util.List<String> violations) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < violations.size(); i++) {
            if (i > 0) {
                builder.append("、");
            }
            builder.append(violations.get(i));
        }
        return builder.toString();
    }

    private static java.util.List<Cmtxnf> syntheticTransactions() {
        java.util.List<Cmtxnf> rows = new java.util.ArrayList<Cmtxnf>();
        rows.add(new Cmtxnf("TXN-BK-000001", "BK", "BK202506000001", new java.math.BigDecimal("12500000"), "01"));
        rows.add(new Cmtxnf("TXN-SC-000041", "SC", "SC202506000041", new java.math.BigDecimal("8400000"), "01"));
        rows.add(new Cmtxnf("TXN-CD-000112", "CD", "CD202506000112", new java.math.BigDecimal("32800"), "01"));
        rows.add(new Cmtxnf("TXN-PY-000227", "PY", "PY202506000227", new java.math.BigDecimal("4600"), "01"));
        rows.add(new Cmtxnf("TXN-LF-000019", "LF", "LF202506000019", new java.math.BigDecimal("775000"), "09"));
        rows.add(new Cmtxnf("TXN-CM-000003", "CM", "CM202506000003", new java.math.BigDecimal("190000"), "01"));
        return rows;
    }

    private static java.util.List<Cmaudf> syntheticAudits() {
        java.util.List<Cmaudf> rows = new java.util.ArrayList<Cmaudf>();
        rows.add(new Cmaudf("AUD-000001", "GRP-BK-202506-000001", "BK", "BK202506000001", "20"));
        rows.add(new Cmaudf("AUD-000041", "GRP-SC-202506-000041", "SC", "SC202506000041", "20"));
        rows.add(new Cmaudf("AUD-000112", "GRP-CD-202506-000112", "CD", "CD202506000112", "20"));
        rows.add(new Cmaudf("AUD-000227", "GRP-PY-202506-000227", "PY", "PY202506000227", "20"));
        rows.add(new Cmaudf("AUD-000019", "GRP-LF-202506-000019", "LF", "LF202506000019", "20"));
        rows.add(new Cmaudf("AUD-000888", "GRP-BK-202506-000888", "BK", "BK202506000888", "20"));
        rows.add(new Cmaudf("AUD-000889", "GRP-CM-202506-000003", "CM", "CM202506000003", "20"));
        return rows;
    }

    private static java.util.List<Cajrnf> syntheticJournals() {
        java.util.List<Cajrnf> rows = new java.util.ArrayList<Cajrnf>();
        rows.add(new Cajrnf(1L, "AUD-000001", "GRP-BK-202506-000001", "TX", "10"));
        rows.add(new Cajrnf(2L, "AUD-000041", "GRP-SC-202506-000041", "TX", "10"));
        rows.add(new Cajrnf(3L, "AUD-000112", "GRP-CD-202506-000112", "TX", "10"));
        rows.add(new Cajrnf(4L, "AUD-000227", "GRP-PY-202506-000227", "TX", "10"));
        rows.add(new Cajrnf(5L, "AUD-000019", "GRP-LF-202506-000019", "RV", "10"));
        rows.add(new Cajrnf(6L, "AUD-000889", "GRP-CM-202506-000003", "AU", "90"));
        return rows;
    }

    private static final class Cmtxnf {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final java.math.BigDecimal txnAmt;
        private final String txnStatusKbn;

        private Cmtxnf(String txnId, String companyCode, String localTxnNo, java.math.BigDecimal txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
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

    private static final class Cmrptf {
        private final String reportId;
        private final String reportTypeKbn;
        private final String targetMonth;
        private final String outputStatusKbn;
        private final java.time.LocalDateTime createdAt;

        private Cmrptf(String reportId, String reportTypeKbn, String targetMonth, String outputStatusKbn, java.time.LocalDateTime createdAt) {
            this.reportId = reportId;
            this.reportTypeKbn = reportTypeKbn;
            this.targetMonth = targetMonth;
            this.outputStatusKbn = outputStatusKbn;
            this.createdAt = createdAt;
        }

        private String toLine() {
            return reportId + "," + reportTypeKbn + "," + targetMonth + "," + outputStatusKbn + "," + createdAt;
        }
    }

    private static final class CmerrfCandidate {
        private final String sourceFile;
        private final String sourceKey;
        private final String groupRefNo;
        private final String reason;

        private CmerrfCandidate(String sourceFile, String sourceKey, String groupRefNo, String reason) {
            this.sourceFile = sourceFile;
            this.sourceKey = sourceKey;
            this.groupRefNo = groupRefNo;
            this.reason = reason;
        }

        private static CmerrfCandidate of(String sourceFile, String sourceKey, String groupRefNo, String reason) {
            return new CmerrfCandidate(sourceFile, sourceKey, groupRefNo, reason);
        }

        private String toLine() {
            return sourceFile + "," + sourceKey + "," + groupRefNo + "," + reason;
        }
    }

    private static final class TxnKey {
        private final String companyCode;
        private final String localTxnNo;

        private TxnKey(String companyCode, String localTxnNo) {
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
        }

        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof TxnKey)) {
                return false;
            }
            TxnKey that = (TxnKey) other;
            return java.util.Objects.equals(companyCode, that.companyCode)
                    && java.util.Objects.equals(localTxnNo, that.localTxnNo);
        }

        public int hashCode() {
            return java.util.Objects.hash(companyCode, localTxnNo);
        }
    }

    private static final class Aggregation {
        private final String reportTypeKbn;
        private int acceptedCount;
        private int cancelCount;
        private java.math.BigDecimal totalAmount = java.math.BigDecimal.ZERO;

        private Aggregation(String reportTypeKbn) {
            this.reportTypeKbn = reportTypeKbn;
        }

        private void accept(Cmtxnf txn) {
            acceptedCount++;
            if ("09".equals(txn.txnStatusKbn)) {
                cancelCount++;
            }
            totalAmount = totalAmount.add(txn.txnAmt);
        }
    }

    private static final class ExportResult {
        private final java.util.List<Cmrptf> reports;
        private final java.util.List<CmerrfCandidate> errors;

        private ExportResult(java.util.List<Cmrptf> reports, java.util.List<CmerrfCandidate> errors) {
            this.reports = java.util.Collections.unmodifiableList(new java.util.ArrayList<Cmrptf>(reports));
            this.errors = java.util.Collections.unmodifiableList(new java.util.ArrayList<CmerrfCandidate>(errors));
        }
    }
}
