package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2025/06/29  共通基盤  初版作成
 */
public class CommonCodeMaintenanceService {
    private static long journalSequence = 900000000000L;

    private CommonCodeMaintenanceService() {
    }

    public static void main(String[] a) {
        CodeStore cmcodf = new CodeStore();
        JournalStore cajrnf = new JournalStore();

        cmcodf.write(new CodeRecord("BK01:TRANSFER:ATM", "TRANSFER", "ATM",
                20240101, 99991231, "1"));
        cmcodf.write(new CodeRecord("BK01:TRANSFER:BRANCH", "TRANSFER", "BRANCH",
                20240101, 99991231, "1"));
        cmcodf.write(new CodeRecord("BK01:FEE:STD", "FEE", "STD",
                20240101, 20251231, "1"));

        MaintenanceRequest[] requests = new MaintenanceRequest[] {
                new MaintenanceRequest("G202506290001", "ADD", "BK01:FEE:REDUCED",
                        "FEE", "REDUCED", 20250701, 99991231, "1"),
                new MaintenanceRequest("G202506290002", "STOP", "BK01:TRANSFER:ATM",
                        "TRANSFER", "ATM", 20250630, 99991231, "9"),
                new MaintenanceRequest("G202506290003", "CORRECT", "BK01:FEE:STD",
                        "FEE", "STD", 20240101, 20250630, "1"),
                new MaintenanceRequest("G202506290004", "ADD", "BK01:TRANSFER:ATM",
                        "TRANSFER", "ATM", 20250701, 99991231, "1")
        };

        MaintenanceBatchResult result = processRequests(cmcodf, cajrnf, requests);
        System.out.println("処理件数=" + result.totalCount
                + " 正常件数=" + result.acceptedCount
                + " 否認件数=" + result.rejectedCount);
        for (JournalRecord journal : cajrnf.rows) {
            System.out.println(journal.journalSeq + " "
                    + journal.auditId + " "
                    + journal.groupRefNo + " "
                    + journal.eventTypeKbn + " "
                    + journal.journalStatusKbn);
        }
    }

    private static MaintenanceBatchResult processRequests(CodeStore cmcodf,
                                                          JournalStore cajrnf,
                                                          MaintenanceRequest[] requests) {
        int accepted = 0;
        int rejected = 0;

        for (int i = 0; i < requests.length; i++) {
            MaintenanceRequest request = requests[i];
            String auditId = issueAuditId(request.groupRefNo, i + 1);
            ValidationResult validation = validateRequest(cmcodf, request);

            if (validation.accepted) {
                applyMaintenance(cmcodf, request);
                cajrnf.write(new JournalRecord(nextJournalSequence(), auditId,
                        request.groupRefNo, request.operationKbn, "1"));
                accepted++;
            } else {
                cajrnf.write(new JournalRecord(nextJournalSequence(), auditId,
                        request.groupRefNo, request.operationKbn, "8"));
                rejected++;
                System.out.println("保守否認 GROUP-REF-NO=" + request.groupRefNo
                        + " 理由=" + validation.reason);
            }
        }

        return new MaintenanceBatchResult(requests.length, accepted, rejected);
    }

    private static ValidationResult validateRequest(CodeStore cmcodf, MaintenanceRequest request) {
        if (!isKnownOperation(request.operationKbn)) {
            return ValidationResult.reject("操作区分不正");
        }
        if (!isStatusKbn(request.statusKbn)) {
            return ValidationResult.reject("状態区分不正");
        }
        if (request.validFrom > request.validTo) {
            return ValidationResult.reject("有効期間逆転");
        }
        if (request.codeKey == null || request.codeType == null || request.codeValue == null) {
            return ValidationResult.reject("コード項目未設定");
        }

        CodeRecord current = cmcodf.findByKey(request.codeKey);

        if ("ADD".equals(request.operationKbn)) {
            if (current != null) {
                return ValidationResult.reject("同一キー既存");
            }
            if (cmcodf.hasOverlap(request.codeType, request.codeValue,
                    request.validFrom, request.validTo, null)) {
                return ValidationResult.reject("期間重複");
            }
            return ValidationResult.accept();
        }

        if (current == null) {
            return ValidationResult.reject("対象キー不存在");
        }

        if (!current.codeType.equals(request.codeType) || !current.codeValue.equals(request.codeValue)) {
            return ValidationResult.reject("対象コード不一致");
        }

        if ("STOP".equals(request.operationKbn)) {
            if ("9".equals(current.statusKbn)) {
                return ValidationResult.reject("停止済");
            }
            if (request.validFrom < current.validFrom || request.validFrom > current.validTo) {
                return ValidationResult.reject("停止日範囲外");
            }
            return ValidationResult.accept();
        }

        if (cmcodf.hasOverlap(request.codeType, request.codeValue,
                request.validFrom, request.validTo, request.codeKey)) {
            return ValidationResult.reject("期間重複");
        }
        return ValidationResult.accept();
    }

    private static void applyMaintenance(CodeStore cmcodf, MaintenanceRequest request) {
        if ("ADD".equals(request.operationKbn)) {
            cmcodf.write(new CodeRecord(request.codeKey, request.codeType, request.codeValue,
                    request.validFrom, request.validTo, request.statusKbn));
            return;
        }

        CodeRecord current = cmcodf.findByKey(request.codeKey);
        if ("STOP".equals(request.operationKbn)) {
            cmcodf.replace(new CodeRecord(current.codeKey, current.codeType, current.codeValue,
                    current.validFrom, request.validFrom, "9"));
            return;
        }

        cmcodf.replace(new CodeRecord(current.codeKey, current.codeType, current.codeValue,
                request.validFrom, request.validTo, request.statusKbn));
    }

    private static boolean isKnownOperation(String operationKbn) {
        return "ADD".equals(operationKbn) || "STOP".equals(operationKbn) || "CORRECT".equals(operationKbn);
    }

    private static boolean isStatusKbn(String statusKbn) {
        return "1".equals(statusKbn) || "9".equals(statusKbn);
    }

    private static String issueAuditId(String groupRefNo, int branchNo) {
        String source = groupRefNo == null ? "NOREF" : groupRefNo;
        return "AUD" + source + String.format("%03d", branchNo);
    }

    private static long nextJournalSequence() {
        journalSequence++;
        return journalSequence;
    }

    private static final class CodeStore {
        private CodeRecord[] rows = new CodeRecord[16];
        private int size;

        private void write(CodeRecord row) {
            ensureCapacity();
            rows[size++] = row;
        }

        private void replace(CodeRecord row) {
            for (int i = 0; i < size; i++) {
                if (rows[i].codeKey.equals(row.codeKey)) {
                    rows[i] = row;
                    return;
                }
            }
            write(row);
        }

        private CodeRecord findByKey(String codeKey) {
            for (int i = 0; i < size; i++) {
                if (rows[i].codeKey.equals(codeKey)) {
                    return rows[i];
                }
            }
            return null;
        }

        private boolean hasOverlap(String codeType, String codeValue, int from, int to, String excludingKey) {
            for (int i = 0; i < size; i++) {
                CodeRecord row = rows[i];
                if (excludingKey != null && excludingKey.equals(row.codeKey)) {
                    continue;
                }
                if (row.codeType.equals(codeType)
                        && row.codeValue.equals(codeValue)
                        && !"9".equals(row.statusKbn)
                        && from <= row.validTo
                        && to >= row.validFrom) {
                    return true;
                }
            }
            return false;
        }

        private void ensureCapacity() {
            if (size < rows.length) {
                return;
            }
            CodeRecord[] expanded = new CodeRecord[rows.length * 2];
            System.arraycopy(rows, 0, expanded, 0, rows.length);
            rows = expanded;
        }
    }

    private static final class JournalStore {
        private JournalRecord[] rows = new JournalRecord[16];
        private int size;

        private void write(JournalRecord row) {
            ensureCapacity();
            rows[size++] = row;
        }

        private void ensureCapacity() {
            if (size < rows.length) {
                return;
            }
            JournalRecord[] expanded = new JournalRecord[rows.length * 2];
            System.arraycopy(rows, 0, expanded, 0, rows.length);
            rows = expanded;
        }
    }

    private static final class CodeRecord {
        private final String codeKey;
        private final String codeType;
        private final String codeValue;
        private final int validFrom;
        private final int validTo;
        private final String statusKbn;

        private CodeRecord(String codeKey, String codeType, String codeValue,
                           int validFrom, int validTo, String statusKbn) {
            this.codeKey = codeKey;
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.statusKbn = statusKbn;
        }
    }

    private static final class JournalRecord {
        private final long journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private JournalRecord(long journalSeq, String auditId, String groupRefNo,
                              String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }

    private static final class MaintenanceRequest {
        private final String groupRefNo;
        private final String operationKbn;
        private final String codeKey;
        private final String codeType;
        private final String codeValue;
        private final int validFrom;
        private final int validTo;
        private final String statusKbn;

        private MaintenanceRequest(String groupRefNo, String operationKbn, String codeKey,
                                   String codeType, String codeValue, int validFrom,
                                   int validTo, String statusKbn) {
            this.groupRefNo = groupRefNo;
            this.operationKbn = operationKbn;
            this.codeKey = codeKey;
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.statusKbn = statusKbn;
        }
    }

    private static final class ValidationResult {
        private final boolean accepted;
        private final String reason;

        private ValidationResult(boolean accepted, String reason) {
            this.accepted = accepted;
            this.reason = reason;
        }

        private static ValidationResult accept() {
            return new ValidationResult(true, "");
        }

        private static ValidationResult reject(String reason) {
            return new ValidationResult(false, reason);
        }
    }

    private static final class MaintenanceBatchResult {
        private final int totalCount;
        private final int acceptedCount;
        private final int rejectedCount;

        private MaintenanceBatchResult(int totalCount, int acceptedCount, int rejectedCount) {
            this.totalCount = totalCount;
            this.acceptedCount = acceptedCount;
            this.rejectedCount = rejectedCount;
        }
    }
}
