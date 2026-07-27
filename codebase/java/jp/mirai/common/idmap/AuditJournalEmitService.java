package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2025/06/29  共通基盤部  統合監査ジャーナル出力サービス初版
 */
public class AuditJournalEmitService {
    private static final String CODE_TYPE_EVENT = "EVENT-TYPE-KBN";
    private static final String CODE_TYPE_JOURNAL_STATUS = "JOURNAL-STATUS-KBN";
    private static final String CODE_STATUS_VALID = "01";
    private static final String STATUS_EMITTED = "01";

    public static void main(String[] a) {
        java.time.LocalDate businessDate = java.time.LocalDate.of(2025, 6, 29);

        java.util.List<java.util.Map<String, String>> cmaudf = new java.util.ArrayList<>();
        cmaudf.add(row("AUDIT-ID", "AU202506290001", "GROUP-REF-NO", "GR202506290000000001", "COMPANY-CODE", "BK", "LOCAL-TXN-NO", "BK-18004501", "AUDIT-STATUS-KBN", "01"));
        cmaudf.add(row("AUDIT-ID", "AU202506290002", "GROUP-REF-NO", "GR202506290000000002", "COMPANY-CODE", "SC", "LOCAL-TXN-NO", "SC-77001003", "AUDIT-STATUS-KBN", "01"));
        cmaudf.add(row("AUDIT-ID", "AU202506290003", "GROUP-REF-NO", "GR202506290000000003", "COMPANY-CODE", "CD", "LOCAL-TXN-NO", "CD-24009012", "AUDIT-STATUS-KBN", "09"));
        cmaudf.add(row("AUDIT-ID", "AU202506290004", "GROUP-REF-NO", "GR202506290000000004", "COMPANY-CODE", "PY", "LOCAL-TXN-NO", "PY-51000888", "AUDIT-STATUS-KBN", "02"));
        cmaudf.add(row("AUDIT-ID", "AU202506290001", "GROUP-REF-NO", "GR202506290000000001", "COMPANY-CODE", "BK", "LOCAL-TXN-NO", "BK-18004501", "AUDIT-STATUS-KBN", "01"));

        java.util.List<java.util.Map<String, String>> cmcodf = new java.util.ArrayList<>();
        cmcodf.add(code("EVT-001", CODE_TYPE_EVENT, "01", "2025-01-01", "9999-12-31", CODE_STATUS_VALID));
        cmcodf.add(code("EVT-009", CODE_TYPE_EVENT, "09", "2025-01-01", "9999-12-31", CODE_STATUS_VALID));
        cmcodf.add(code("JST-001", CODE_TYPE_JOURNAL_STATUS, "01", "2025-01-01", "9999-12-31", CODE_STATUS_VALID));
        cmcodf.add(code("JST-009", CODE_TYPE_JOURNAL_STATUS, "09", "2025-01-01", "9999-12-31", CODE_STATUS_VALID));

        java.util.List<java.util.Map<String, String>> cajrnf = emit(cmaudf, cmcodf, businessDate, 1L);
        for (java.util.Map<String, String> journal : cajrnf) {
            System.out.println("出力 JOURNAL-SEQ=" + journal.get("JOURNAL-SEQ")
                    + " AUDIT-ID=" + journal.get("AUDIT-ID")
                    + " GROUP-REF-NO=" + journal.get("GROUP-REF-NO")
                    + " EVENT-TYPE-KBN=" + journal.get("EVENT-TYPE-KBN")
                    + " JOURNAL-STATUS-KBN=" + journal.get("JOURNAL-STATUS-KBN"));
        }
    }

    private static java.util.List<java.util.Map<String, String>> emit(
            java.util.List<java.util.Map<String, String>> cmaudf,
            java.util.List<java.util.Map<String, String>> cmcodf,
            java.time.LocalDate businessDate,
            long startSeq) {
        java.util.Set<String> activeEventTypes = activeCodes(cmcodf, CODE_TYPE_EVENT, businessDate);
        java.util.Set<String> activeJournalStatuses = activeCodes(cmcodf, CODE_TYPE_JOURNAL_STATUS, businessDate);
        java.util.Set<String> emittedKeys = new java.util.HashSet<>();
        java.util.List<java.util.Map<String, String>> journals = new java.util.ArrayList<>();
        long seq = startSeq;

        for (java.util.Map<String, String> audit : cmaudf) {
            String auditId = audit.get("AUDIT-ID");
            String groupRefNo = audit.get("GROUP-REF-NO");
            String companyCode = audit.get("COMPANY-CODE");
            String status = audit.get("AUDIT-STATUS-KBN");

            if (!validCompany(companyCode)) {
                System.err.println("警告 会社コード不正 AUDIT-ID=" + value(auditId) + " COMPANY-CODE=" + value(companyCode));
                continue;
            }
            if (blank(auditId) || blank(groupRefNo)) {
                System.err.println("警告 監査キー不備 AUDIT-ID=" + value(auditId) + " GROUP-REF-NO=" + value(groupRefNo));
                continue;
            }
            if (!activeEventTypes.contains(status)) {
                System.err.println("警告 イベント種別未定義 AUDIT-ID=" + auditId + " EVENT-TYPE-KBN=" + value(status));
                continue;
            }
            if (!activeJournalStatuses.contains(STATUS_EMITTED)) {
                System.err.println("警告 監査状態未定義 JOURNAL-STATUS-KBN=" + STATUS_EMITTED);
                continue;
            }

            String duplicateKey = auditId + '\u0000' + status;
            if (!emittedKeys.add(duplicateKey)) {
                System.err.println("通知 重複イベント抑止 AUDIT-ID=" + auditId + " EVENT-TYPE-KBN=" + status);
                continue;
            }

            java.util.Map<String, String> journal = new java.util.LinkedHashMap<>();
            journal.put("JOURNAL-SEQ", String.format("%012d", seq++));
            journal.put("AUDIT-ID", auditId);
            journal.put("GROUP-REF-NO", groupRefNo);
            journal.put("EVENT-TYPE-KBN", status);
            journal.put("JOURNAL-STATUS-KBN", STATUS_EMITTED);
            journals.add(journal);
        }
        return journals;
    }

    private static java.util.Set<String> activeCodes(
            java.util.List<java.util.Map<String, String>> cmcodf,
            String codeType,
            java.time.LocalDate businessDate) {
        java.util.Set<String> values = new java.util.HashSet<>();
        for (java.util.Map<String, String> code : cmcodf) {
            if (!codeType.equals(code.get("CODE-TYPE"))) {
                continue;
            }
            if (!CODE_STATUS_VALID.equals(code.get("CODE-STATUS-KBN"))) {
                continue;
            }
            java.time.LocalDate from = java.time.LocalDate.parse(code.get("VALID-FROM"));
            java.time.LocalDate to = java.time.LocalDate.parse(code.get("VALID-TO"));
            if (!businessDate.isBefore(from) && !businessDate.isAfter(to)) {
                values.add(code.get("CODE-VALUE"));
            }
        }
        return values;
    }

    private static boolean validCompany(String companyCode) {
        return "BK".equals(companyCode)
                || "SC".equals(companyCode)
                || "CD".equals(companyCode)
                || "PY".equals(companyCode)
                || "LF".equals(companyCode)
                || "CM".equals(companyCode);
    }

    private static boolean blank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static String value(String value) {
        return value == null ? "(NULL)" : value;
    }

    private static java.util.Map<String, String> code(
            String key,
            String type,
            String value,
            String from,
            String to,
            String status) {
        return row("CODE-KEY", key, "CODE-TYPE", type, "CODE-VALUE", value, "VALID-FROM", from, "VALID-TO", to, "CODE-STATUS-KBN", status);
    }

    private static java.util.Map<String, String> row(String... values) {
        if (values.length % 2 != 0) {
            throw new IllegalArgumentException("項目数不正");
        }
        java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
        for (int i = 0; i < values.length; i += 2) {
            row.put(values[i], values[i + 1]);
        }
        return row;
    }
}
