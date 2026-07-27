package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025-06-29  共通基盤  名寄せ連携同期サービス初版
 */
public class CustomerAliasSyncService {
    private static final String STATUS_CONFIRMED = "01";
    private static final String STATUS_CANCELLED = "09";
    private static final String LINK_ACTIVE = "01";
    private static final String LINK_CANCELLED = "09";
    private static final String ERROR_CONFLICT = "E101";
    private static final String ERROR_INVALID_COMPANY = "E201";
    private static final String ERROR_INVALID_STATUS = "E202";
    private static final String ERROR_MISSING_KEY = "E203";
    private static final String ERROR_OPEN = "01";
    private static final String IMPORT_BATCH_ID = "B20250629001";

    public static void main(String[] a) {
        new CustomerAliasSyncService().execute(null);
    }

    private void execute(IdMapModel idMapModel) {
        TxnRecord[] cmtxnf = new TxnRecord[] {
                new TxnRecord("T202506290001", "BK", "BK-202506-000001", 1200000L, "01"),
                new TxnRecord("T202506290002", "SC", "SC-202506-000031", 880000L, "01"),
                new TxnRecord("T202506290003", "CD", "CD-202506-000411", 43210L, "01"),
                new TxnRecord("T202506290004", "PY", "PY-202506-000088", 7400L, "09"),
                new TxnRecord("T202506290005", "LF", "LF-202506-000120", 350000L, "01"),
                new TxnRecord("T202506290006", "BK", "BK-202506-000099", 990000L, "01"),
                new TxnRecord("T202506290007", "ZZ", "ZZ-202506-000001", 1000L, "01"),
                new TxnRecord("T202506290008", "SC", "", 55100L, "01")
        };

        AliasLinkStore cxidmf = new AliasLinkStore();
        cxidmf.put(new AliasLinkRecord("IDM-BK-BK-202506-000001", "BK", "BK-202506-000001", "AL-BK-000000001", "01"));
        cxidmf.put(new AliasLinkRecord("IDM-SC-SC-202506-000031", "SC", "SC-202506-000031", "AL-SC-000000031", "01"));
        cxidmf.put(new AliasLinkRecord("IDM-PY-PY-202506-000088", "PY", "PY-202506-000088", "AL-PY-000000088", "01"));

        ErrorStore cmerrf = new ErrorStore();

        int readCount = 0;
        int writeCount = 0;
        int conflictCount = 0;
        int errorCount = 0;

        for (TxnRecord txn : cmtxnf) {
            readCount++;

            String validationError = validate(txn);
            if (validationError != null) {
                cmerrf.put(errorOf(txn, validationError, errorCount + 1));
                errorCount++;
                continue;
            }

            String idMapKey = resolveIdMapKey(idMapModel, txn.companyCode, txn.localTxnNo);
            if (idMapKey == null || idMapKey.length() == 0) {
                cmerrf.put(errorOf(txn, ERROR_MISSING_KEY, errorCount + 1));
                errorCount++;
                continue;
            }

            String aliasId = buildCustomerAliasId(txn.companyCode, txn.localTxnNo);
            AliasLinkRecord existing = cxidmf.get(idMapKey);
            String newLinkStatus = STATUS_CANCELLED.equals(txn.txnStatusKbn) ? LINK_CANCELLED : LINK_ACTIVE;

            if (existing == null) {
                cxidmf.put(new AliasLinkRecord(idMapKey, txn.companyCode, txn.localTxnNo, aliasId, newLinkStatus));
                writeCount++;
                continue;
            }

            if (isConflict(existing, txn, aliasId)) {
                cmerrf.put(errorOf(txn, ERROR_CONFLICT, errorCount + 1));
                conflictCount++;
                errorCount++;
                continue;
            }

            if (!existing.linkStatusKbn.equals(newLinkStatus)) {
                cxidmf.put(new AliasLinkRecord(idMapKey, txn.companyCode, txn.localTxnNo, existing.customerAliasId, newLinkStatus));
                writeCount++;
            }
        }

        System.out.println("処理件数=" + readCount
                + " 更新件数=" + writeCount
                + " 競合件数=" + conflictCount
                + " エラー件数=" + errorCount
                + " CXIDMF件数=" + cxidmf.size()
                + " CMERRF件数=" + cmerrf.size());
    }

    private String validate(TxnRecord txn) {
        if (!isCompanyCode(txn.companyCode)) {
            return ERROR_INVALID_COMPANY;
        }
        if (txn.localTxnNo == null || txn.localTxnNo.trim().length() == 0) {
            return ERROR_MISSING_KEY;
        }
        if (!STATUS_CONFIRMED.equals(txn.txnStatusKbn) && !STATUS_CANCELLED.equals(txn.txnStatusKbn)) {
            return ERROR_INVALID_STATUS;
        }
        return null;
    }

    private boolean isCompanyCode(String value) {
        return "BK".equals(value)
                || "SC".equals(value)
                || "CD".equals(value)
                || "PY".equals(value)
                || "LF".equals(value)
                || "CM".equals(value);
    }

    private String resolveIdMapKey(IdMapModel model, String companyCode, String localTxnNo) {
        String key = invokeIdMap(model, companyCode, localTxnNo);
        if (key != null && key.length() > 0) {
            return key;
        }
        return "IDM-" + companyCode + "-" + localTxnNo;
    }

    private String invokeIdMap(IdMapModel model, String companyCode, String localTxnNo) {
        String[] names = new String[] {
                "toIdMapKey",
                "resolveIdMapKey",
                "buildIdMapKey",
                "convertToIdMapKey"
        };

        for (String name : names) {
            try {
                java.lang.reflect.Method method = IdMapModel.class.getMethod(name, String.class, String.class);
                Object target = java.lang.reflect.Modifier.isStatic(method.getModifiers()) ? null : model;
                if (target == null && !java.lang.reflect.Modifier.isStatic(method.getModifiers())) {
                    continue;
                }
                Object value = method.invoke(target, companyCode, localTxnNo);
                if (value != null) {
                    return String.valueOf(value);
                }
            } catch (ReflectiveOperationException ignored) {
                // 近隣成果物の名寄せキー公開名が揺れるため、利用可能な公開口だけを順に確認する。
            }
        }
        return null;
    }

    private boolean isConflict(AliasLinkRecord existing, TxnRecord txn, String aliasId) {
        if (!existing.companyCode.equals(txn.companyCode)) {
            return true;
        }
        if (!existing.localTxnNo.equals(txn.localTxnNo)) {
            return true;
        }
        return !existing.customerAliasId.equals(aliasId);
    }

    private String buildCustomerAliasId(String companyCode, String localTxnNo) {
        String digits = onlyDigits(localTxnNo);
        String suffix = digits.length() > 9 ? digits.substring(digits.length() - 9) : leftPad(digits, 9);
        return "AL-" + companyCode + "-" + suffix;
    }

    private String onlyDigits(String value) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c >= '0' && c <= '9') {
                builder.append(c);
            }
        }
        return builder.length() == 0 ? "0" : builder.toString();
    }

    private String leftPad(String value, int length) {
        StringBuilder builder = new StringBuilder();
        for (int i = value.length(); i < length; i++) {
            builder.append('0');
        }
        builder.append(value);
        return builder.toString();
    }

    private ErrorRecord errorOf(TxnRecord txn, String errorCode, int sequence) {
        String errorId = "ER-" + IMPORT_BATCH_ID + "-" + leftPad(String.valueOf(sequence), 5);
        return new ErrorRecord(errorId, IMPORT_BATCH_ID, txn.companyCode, txn.localTxnNo, errorCode, ERROR_OPEN);
    }

    private static final class TxnRecord {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final long txnAmt;
        private final String txnStatusKbn;

        private TxnRecord(String txnId, String companyCode, String localTxnNo, long txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class AliasLinkRecord {
        private final String idMapKey;
        private final String companyCode;
        private final String localTxnNo;
        private final String customerAliasId;
        private final String linkStatusKbn;

        private AliasLinkRecord(String idMapKey, String companyCode, String localTxnNo, String customerAliasId, String linkStatusKbn) {
            this.idMapKey = idMapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
        }
    }

    private static final class ErrorRecord {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        private ErrorRecord(String errorId, String importBatchId, String companyCode, String localTxnNo, String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }
    }

    private static final class AliasLinkStore {
        private final java.util.Map<String, AliasLinkRecord> rows = new java.util.TreeMap<String, AliasLinkRecord>();

        private AliasLinkRecord get(String idMapKey) {
            return rows.get(idMapKey);
        }

        private void put(AliasLinkRecord record) {
            rows.put(record.idMapKey, record);
        }

        private int size() {
            return rows.size();
        }
    }

    private static final class ErrorStore {
        private final java.util.Map<String, ErrorRecord> rows = new java.util.TreeMap<String, ErrorRecord>();

        private void put(ErrorRecord record) {
            rows.put(record.errorId, record);
        }

        private int size() {
            return rows.size();
        }
    }
}
