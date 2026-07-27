package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2025-06-29  共通基盤    会社間連携突合サービス初版
 */
public class CompanyRelayReconcileService {
    private static final String 会社_BK = "BK";
    private static final String 会社_SC = "SC";
    private static final String 会社_CD = "CD";
    private static final String 会社_PY = "PY";
    private static final String 会社_LF = "LF";
    private static final String 会社_CM = "CM";

    private static final String 取引状態_確定 = "01";
    private static final String 取引状態_取消 = "09";

    private static final String 監査状態_済 = "01";
    private static final String 監査状態_保留 = "02";
    private static final String 監査状態_取消 = "09";

    private static final String イベント_送信 = "10";
    private static final String イベント_受信 = "20";
    private static final String イベント_監査差異 = "90";

    private static final String 仕訳状態_有効 = "01";
    private static final String 仕訳状態_取消 = "09";

    private static final String 差異_送信済未受信 = "E401";
    private static final String 差異_受信済取引なし = "E402";
    private static final String 差異_受信済未監査 = "E403";
    private static final String 差異_監査会社不一致 = "E404";
    private static final String 差異_監査状態不正 = "E405";
    private static final String 差異_取引状態不正 = "E406";

    private static final String エラー状態_未処理 = "01";
    private static final String 取込バッチ接頭辞 = "CRR";

    public static void main(String[] args) {
        java.util.List<TorihikiMeisai> cmtxnf = new java.util.ArrayList<TorihikiMeisai>();
        cmtxnf.add(new TorihikiMeisai("T202506290001", 会社_BK, "BK-260629-0001", 1250000L, 取引状態_確定));
        cmtxnf.add(new TorihikiMeisai("T202506290002", 会社_SC, "SC-260629-0017", 388000L, 取引状態_確定));
        cmtxnf.add(new TorihikiMeisai("T202506290003", 会社_CD, "CD-260629-0022", 91800L, 取引状態_取消));
        cmtxnf.add(new TorihikiMeisai("T202506290004", 会社_PY, "PY-260629-0810", 6400L, 取引状態_確定));
        cmtxnf.add(new TorihikiMeisai("T202506290005", 会社_LF, "LF-260629-0301", 220000L, "03"));

        java.util.List<KansaLink> cmaudf = new java.util.ArrayList<KansaLink>();
        cmaudf.add(new KansaLink("A202506290001", "GRP-20250629-000001", 会社_BK, "BK-260629-0001", 監査状態_済));
        cmaudf.add(new KansaLink("A202506290002", "GRP-20250629-000002", 会社_SC, "SC-260629-0017", 監査状態_済));
        cmaudf.add(new KansaLink("A202506290003", "GRP-20250629-000003", 会社_CD, "CD-260629-0022", 監査状態_取消));
        cmaudf.add(new KansaLink("A202506290004", "GRP-20250629-000004", 会社_CM, "PY-260629-0810", 監査状態_保留));

        java.util.List<JournalEvent> cajrnf = new java.util.ArrayList<JournalEvent>();
        cajrnf.add(new JournalEvent(100001L, "A202506290001", "GRP-20250629-000001", イベント_送信, 仕訳状態_有効));
        cajrnf.add(new JournalEvent(100002L, "A202506290001", "GRP-20250629-000001", イベント_受信, 仕訳状態_有効));
        cajrnf.add(new JournalEvent(100003L, "A202506290002", "GRP-20250629-000002", イベント_送信, 仕訳状態_有効));
        cajrnf.add(new JournalEvent(100004L, "A202506290003", "GRP-20250629-000003", イベント_受信, 仕訳状態_有効));
        cajrnf.add(new JournalEvent(100005L, "A202506290004", "GRP-20250629-000004", イベント_受信, 仕訳状態_有効));
        cajrnf.add(new JournalEvent(100006L, "A202506290099", "GRP-20250629-000099", イベント_受信, 仕訳状態_有効));

        TotsugoKekka kekka = reconcile(cmtxnf, cmaudf, cajrnf, "20250629A");

        System.out.println("会社間連携突合 件数=" + kekka.errorRecords.size());
        for (ErrorRecord error : kekka.errorRecords) {
            System.out.println(error.errorId + " " + error.companyCode + " " + error.localTxnNo + " " + error.errorCode);
        }
        System.out.println("監査追記 件数=" + kekka.appendJournalRecords.size());
    }

    public static TotsugoKekka reconcile(
            java.util.List<TorihikiMeisai> cmtxnf,
            java.util.List<KansaLink> cmaudf,
            java.util.List<JournalEvent> cajrnf,
            String importBatchId) {
        if (cmtxnf == null || cmaudf == null || cajrnf == null) {
            throw new IllegalArgumentException("入力ファイルが未設定です");
        }
        if (importBatchId == null || importBatchId.trim().isEmpty()) {
            throw new IllegalArgumentException("取込バッチIDが未設定です");
        }

        java.util.Map<String, TorihikiMeisai> txByCompanyLocal = new java.util.LinkedHashMap<String, TorihikiMeisai>();
        for (TorihikiMeisai tx : cmtxnf) {
            validateTxn(tx);
            txByCompanyLocal.put(companyLocalKey(tx.companyCode, tx.localTxnNo), tx);
        }

        java.util.Map<String, KansaLink> auditById = new java.util.LinkedHashMap<String, KansaLink>();
        java.util.Map<String, KansaLink> auditByGroupRef = new java.util.LinkedHashMap<String, KansaLink>();
        for (KansaLink audit : cmaudf) {
            validateAudit(audit);
            auditById.put(audit.auditId, audit);
            auditByGroupRef.put(audit.groupRefNo, audit);
        }

        java.util.Map<String, EventSummary> summaryByGroupRef = new java.util.LinkedHashMap<String, EventSummary>();
        long maxJournalSeq = 0L;
        for (JournalEvent journal : cajrnf) {
            validateJournal(journal);
            maxJournalSeq = Math.max(maxJournalSeq, journal.journalSeq);
            if (!仕訳状態_有効.equals(journal.journalStatusKbn)) {
                continue;
            }
            EventSummary summary = summaryByGroupRef.get(journal.groupRefNo);
            if (summary == null) {
                summary = new EventSummary(journal.groupRefNo, journal.auditId);
                summaryByGroupRef.put(journal.groupRefNo, summary);
            }
            if (イベント_送信.equals(journal.eventTypeKbn)) {
                summary.sentCount++;
            } else if (イベント_受信.equals(journal.eventTypeKbn)) {
                summary.receivedCount++;
            } else if (イベント_監査差異.equals(journal.eventTypeKbn)) {
                summary.auditErrorCount++;
            }
        }

        java.util.List<ErrorRecord> errors = new java.util.ArrayList<ErrorRecord>();
        java.util.List<JournalEvent> appendJournals = new java.util.ArrayList<JournalEvent>();
        java.util.Set<String> emitted = new java.util.LinkedHashSet<String>();
        long[] errorSeq = new long[] {1L};
        long nextJournalSeq = maxJournalSeq + 1L;

        for (EventSummary summary : summaryByGroupRef.values()) {
            KansaLink audit = auditByGroupRef.get(summary.groupRefNo);
            if (summary.sentCount > 0 && summary.receivedCount == 0) {
                nextJournalSeq = emitError(
                        errors, appendJournals, emitted, errorSeq, nextJournalSeq, importBatchId,
                        audit, summary, 差異_送信済未受信);
            }
            if (summary.receivedCount > 0 && audit == null) {
                nextJournalSeq = emitError(
                        errors, appendJournals, emitted, errorSeq, nextJournalSeq, importBatchId,
                        null, summary, 差異_受信済取引なし);
                continue;
            }
            if (summary.receivedCount > 0 && audit != null) {
                TorihikiMeisai tx = txByCompanyLocal.get(companyLocalKey(audit.companyCode, audit.localTxnNo));
                if (tx == null) {
                    nextJournalSeq = emitError(
                            errors, appendJournals, emitted, errorSeq, nextJournalSeq, importBatchId,
                            audit, summary, 差異_受信済取引なし);
                    continue;
                }
                if (!audit.companyCode.equals(tx.companyCode)) {
                    nextJournalSeq = emitError(
                            errors, appendJournals, emitted, errorSeq, nextJournalSeq, importBatchId,
                            audit, summary, 差異_監査会社不一致);
                }
                if (!監査状態_済.equals(audit.auditStatusKbn) && !監査状態_取消.equals(audit.auditStatusKbn)) {
                    nextJournalSeq = emitError(
                            errors, appendJournals, emitted, errorSeq, nextJournalSeq, importBatchId,
                            audit, summary, 差異_受信済未監査);
                }
                if (!取引状態_確定.equals(tx.txnStatusKbn) && !取引状態_取消.equals(tx.txnStatusKbn)) {
                    nextJournalSeq = emitError(
                            errors, appendJournals, emitted, errorSeq, nextJournalSeq, importBatchId,
                            audit, summary, 差異_取引状態不正);
                }
            }
        }

        for (KansaLink audit : cmaudf) {
            if (!summaryByGroupRef.containsKey(audit.groupRefNo) && !監査状態_済.equals(audit.auditStatusKbn)) {
                EventSummary summary = new EventSummary(audit.groupRefNo, audit.auditId);
                nextJournalSeq = emitError(
                        errors, appendJournals, emitted, errorSeq, nextJournalSeq, importBatchId,
                        audit, summary, 差異_受信済未監査);
            }
        }

        return new TotsugoKekka(errors, appendJournals);
    }

    private static long emitError(
            java.util.List<ErrorRecord> errors,
            java.util.List<JournalEvent> appendJournals,
            java.util.Set<String> emitted,
            long[] errorSeq,
            long nextJournalSeq,
            String importBatchId,
            KansaLink audit,
            EventSummary summary,
            String errorCode) {
        String companyCode = audit == null ? 会社_CM : audit.companyCode;
        String localTxnNo = audit == null ? summary.groupRefNo : audit.localTxnNo;
        String key = summary.groupRefNo + "|" + companyCode + "|" + localTxnNo + "|" + errorCode;
        if (!emitted.add(key)) {
            return nextJournalSeq;
        }

        String errorId = 取込バッチ接頭辞 + "-" + importBatchId + "-" + String.format("%05d", errorSeq[0]++);
        errors.add(new ErrorRecord(errorId, importBatchId, companyCode, localTxnNo, errorCode, エラー状態_未処理));
        appendJournals.add(new JournalEvent(
                nextJournalSeq,
                audit == null ? summary.auditId : audit.auditId,
                summary.groupRefNo,
                イベント_監査差異,
                仕訳状態_有効));
        return nextJournalSeq + 1L;
    }

    private static void validateTxn(TorihikiMeisai tx) {
        if (tx == null) {
            throw new IllegalArgumentException("取引明細が未設定です");
        }
        requireText(tx.txnId, "取引ID");
        requireCompany(tx.companyCode);
        requireText(tx.localTxnNo, "会社別取引番号");
        if (tx.txnAmt < 0L) {
            throw new IllegalArgumentException("取引金額が不正です: " + tx.localTxnNo);
        }
        requireText(tx.txnStatusKbn, "取引状態区分");
    }

    private static void validateAudit(KansaLink audit) {
        if (audit == null) {
            throw new IllegalArgumentException("監査リンクが未設定です");
        }
        requireText(audit.auditId, "監査ID");
        requireText(audit.groupRefNo, "グループ参照番号");
        requireCompany(audit.companyCode);
        requireText(audit.localTxnNo, "会社別取引番号");
        requireText(audit.auditStatusKbn, "監査状態区分");
    }

    private static void validateJournal(JournalEvent journal) {
        if (journal == null) {
            throw new IllegalArgumentException("ジャーナルが未設定です");
        }
        if (journal.journalSeq <= 0L) {
            throw new IllegalArgumentException("ジャーナル連番が不正です");
        }
        requireText(journal.auditId, "監査ID");
        requireText(journal.groupRefNo, "グループ参照番号");
        requireText(journal.eventTypeKbn, "イベント種別区分");
        requireText(journal.journalStatusKbn, "ジャーナル状態区分");
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "が未設定です");
        }
    }

    private static void requireCompany(String companyCode) {
        requireText(companyCode, "会社コード");
        if (!会社_BK.equals(companyCode)
                && !会社_SC.equals(companyCode)
                && !会社_CD.equals(companyCode)
                && !会社_PY.equals(companyCode)
                && !会社_LF.equals(companyCode)
                && !会社_CM.equals(companyCode)) {
            throw new IllegalArgumentException("会社コードが不正です: " + companyCode);
        }
    }

    private static String companyLocalKey(String companyCode, String localTxnNo) {
        return companyCode + "\u0001" + localTxnNo;
    }

    public static final class TotsugoKekka {
        public final java.util.List<ErrorRecord> errorRecords;
        public final java.util.List<JournalEvent> appendJournalRecords;

        public TotsugoKekka(java.util.List<ErrorRecord> errorRecords, java.util.List<JournalEvent> appendJournalRecords) {
            this.errorRecords = java.util.Collections.unmodifiableList(new java.util.ArrayList<ErrorRecord>(errorRecords));
            this.appendJournalRecords = java.util.Collections.unmodifiableList(new java.util.ArrayList<JournalEvent>(appendJournalRecords));
        }
    }

    public static final class TorihikiMeisai {
        public final String txnId;
        public final String companyCode;
        public final String localTxnNo;
        public final long txnAmt;
        public final String txnStatusKbn;

        public TorihikiMeisai(String txnId, String companyCode, String localTxnNo, long txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    public static final class KansaLink {
        public final String auditId;
        public final String groupRefNo;
        public final String companyCode;
        public final String localTxnNo;
        public final String auditStatusKbn;

        public KansaLink(String auditId, String groupRefNo, String companyCode, String localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    public static final class JournalEvent {
        public final long journalSeq;
        public final String auditId;
        public final String groupRefNo;
        public final String eventTypeKbn;
        public final String journalStatusKbn;

        public JournalEvent(long journalSeq, String auditId, String groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }

    public static final class ErrorRecord {
        public final String errorId;
        public final String importBatchId;
        public final String companyCode;
        public final String localTxnNo;
        public final String errorCode;
        public final String errorStatusKbn;

        public ErrorRecord(String errorId, String importBatchId, String companyCode, String localTxnNo, String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }
    }

    private static final class EventSummary {
        private final String groupRefNo;
        private final String auditId;
        private int sentCount;
        private int receivedCount;
        private int auditErrorCount;

        private EventSummary(String groupRefNo, String auditId) {
            this.groupRefNo = groupRefNo;
            this.auditId = auditId;
        }
    }
}
