package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2025/04/07  みらいペイ システム部 精算・連携チーム  初版作成。精算ネット追跡監査の判定前成果物。
 */
public class NetTraceAuditService {
    private static final String KBN_SOKUJI = "1";
    private static final String KBN_KURIKOSHI = "2";
    private static final String KBN_TAISHOGAI = "9";

    private static final String STATUS_SHONIN = "00";
    private static final String STATUS_URIAGE = "30";
    private static final String STATUS_TORIKESHI = "20";

    private static final String RESULT_OK = "OK";
    private static final String RESULT_WARN = "WARN";
    private static final String RESULT_NG = "NG";

    public void run() {
        AuditReport report = audit(samplePtsetf(), samplePcdtlf(), samplePtkeyf());

        System.out.println("精算ネット追跡監査サービス");
        System.out.println("監査件数=" + report.totalCount);
        System.out.println("正常=" + report.okCount + " 警告=" + report.warnCount + " 異常=" + report.ngCount);
        System.out.println("合計金額=" + report.totalAmount);
        System.out.println("更新後PTKEYF");
        for (PtkeyfRecord record : report.updatedKeys) {
            System.out.println(record.toCsvLine());
        }
    }

    private AuditReport audit(PtsetfRecord[] settlements, PcdtlfRecord[] details, PtkeyfRecord[] keys) {
        java.util.Map<String, PtsetfRecord> settlementById = new java.util.LinkedHashMap<>();
        for (PtsetfRecord settlement : settlements) {
            settlementById.put(settlement.settleTxnId, settlement);
        }

        java.util.Map<String, java.util.List<PcdtlfRecord>> detailsBySettleId = new java.util.LinkedHashMap<>();
        for (PcdtlfRecord detail : details) {
            detailsBySettleId.computeIfAbsent(detail.settleTxnId, k -> new java.util.ArrayList<>()).add(detail);
        }

        PtkeyfRecord[] updated = new PtkeyfRecord[keys.length];
        int ok = 0;
        int warn = 0;
        int ng = 0;
        long totalAmount = 0L;

        for (int i = 0; i < keys.length; i++) {
            PtkeyfRecord key = keys[i];
            TraceSnapshot trace = toTraceSnapshot(key, settlementById.get(key.settleTxnId), detailsBySettleId.get(key.settleTxnId));
            String result = validateTraceKey(trace);

            if (RESULT_OK.equals(result)) {
                ok++;
            } else if (RESULT_WARN.equals(result)) {
                warn++;
            } else {
                ng++;
            }

            if (trace.settlement != null) {
                totalAmount += trace.settlement.txnAmount;
            }

            updated[i] = new PtkeyfRecord(
                    key.traceKey,
                    key.holdId,
                    key.capId,
                    key.settleTxnId,
                    key.merchantCode,
                    result
            );
        }

        return new AuditReport(keys.length, ok, warn, ng, totalAmount, updated);
    }

    private TraceSnapshot toTraceSnapshot(PtkeyfRecord key, PtsetfRecord settlement, java.util.List<PcdtlfRecord> details) {
        java.util.List<PcdtlfRecord> safeDetails = details == null
                ? java.util.Collections.emptyList()
                : java.util.Collections.unmodifiableList(new java.util.ArrayList<>(details));
        return new TraceSnapshot(key, settlement, safeDetails);
    }

    private String validateTraceKey(TraceSnapshot trace) {
        if (trace.key.traceKey == null || trace.key.traceKey.trim().isEmpty()) {
            return RESULT_NG;
        }
        if (trace.key.settleTxnId == null || trace.key.merchantCode == null) {
            return RESULT_NG;
        }
        if (trace.settlement == null) {
            return RESULT_NG;
        }
        if (!trace.key.merchantCode.equals(trace.settlement.merchantCode)) {
            return RESULT_NG;
        }
        if (!isKnownSettleKbn(trace.settlement.settleKbn)) {
            return RESULT_NG;
        }
        if (trace.details.isEmpty()) {
            return RESULT_WARN;
        }

        long detailTotal = 0L;
        boolean hasOutputDone = false;
        for (PcdtlfRecord detail : trace.details) {
            if (!trace.key.merchantCode.equals(detail.merchantCode)) {
                return RESULT_NG;
            }
            if (!trace.settlement.settleKbn.equals(detail.settleKbn)) {
                return RESULT_NG;
            }
            detailTotal += detail.txnAmount;
            if ("出力済".equals(detail.outputStatus)) {
                hasOutputDone = true;
            }
        }

        if (detailTotal != trace.settlement.txnAmount) {
            return RESULT_NG;
        }
        if (KBN_TAISHOGAI.equals(trace.settlement.settleKbn) && hasOutputDone) {
            return RESULT_NG;
        }
        if (KBN_SOKUJI.equals(trace.settlement.settleKbn) && !hasOutputDone) {
            return RESULT_WARN;
        }
        return RESULT_OK;
    }

    private boolean isKnownSettleKbn(String settleKbn) {
        return KBN_SOKUJI.equals(settleKbn) || KBN_KURIKOSHI.equals(settleKbn) || KBN_TAISHOGAI.equals(settleKbn);
    }

    private static PtsetfRecord[] samplePtsetf() {
        return new PtsetfRecord[] {
                new PtsetfRecord("ST202504070001", "MRC10001", 128000L, KBN_SOKUJI),
                new PtsetfRecord("ST202504070002", "MRC10002", 75400L, KBN_KURIKOSHI),
                new PtsetfRecord("ST202504070003", "MRC10003", 29800L, KBN_TAISHOGAI),
                new PtsetfRecord("ST202504070004", "MRC10004", 64000L, KBN_SOKUJI)
        };
    }

    private static PcdtlfRecord[] samplePcdtlf() {
        return new PcdtlfRecord[] {
                new PcdtlfRecord("DTL000001", "ST202504070001", "MRC10001", 128000L, KBN_SOKUJI, "出力済"),
                new PcdtlfRecord("DTL000002", "ST202504070002", "MRC10002", 50000L, KBN_KURIKOSHI, "保留"),
                new PcdtlfRecord("DTL000003", "ST202504070002", "MRC10002", 25400L, KBN_KURIKOSHI, "保留"),
                new PcdtlfRecord("DTL000004", "ST202504070003", "MRC10003", 29800L, KBN_TAISHOGAI, "未出力"),
                new PcdtlfRecord("DTL000005", "ST202504070004", "MRC10004", 62000L, KBN_SOKUJI, "出力済")
        };
    }

    private static PtkeyfRecord[] samplePtkeyf() {
        return new PtkeyfRecord[] {
                new PtkeyfRecord("TRK-20250407-0001", "HD900001", "CP800001", "ST202504070001", "MRC10001", STATUS_SHONIN),
                new PtkeyfRecord("TRK-20250407-0002", "HD900002", "CP800002", "ST202504070002", "MRC10002", STATUS_URIAGE),
                new PtkeyfRecord("TRK-20250407-0003", "HD900003", "CP800003", "ST202504070003", "MRC10003", STATUS_TORIKESHI),
                new PtkeyfRecord("TRK-20250407-0004", "HD900004", "CP800004", "ST202504070004", "MRC10004", STATUS_SHONIN),
                new PtkeyfRecord("TRK-20250407-0005", "HD900005", "CP800005", "ST202504079999", "MRC10999", STATUS_SHONIN)
        };
    }

    private static final class PtsetfRecord {
        private final String settleTxnId;
        private final String merchantCode;
        private final long txnAmount;
        private final String settleKbn;

        private PtsetfRecord(String settleTxnId, String merchantCode, long txnAmount, String settleKbn) {
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.txnAmount = txnAmount;
            this.settleKbn = settleKbn;
        }
    }

    private static final class PtkeyfRecord {
        private final String traceKey;
        private final String holdId;
        private final String capId;
        private final String settleTxnId;
        private final String merchantCode;
        private final String checkResult;

        private PtkeyfRecord(String traceKey, String holdId, String capId, String settleTxnId, String merchantCode, String checkResult) {
            this.traceKey = traceKey;
            this.holdId = holdId;
            this.capId = capId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.checkResult = checkResult;
        }

        private String toCsvLine() {
            return traceKey + "," + holdId + "," + capId + "," + settleTxnId + "," + merchantCode + "," + checkResult;
        }
    }

    private static final class PcdtlfRecord {
        private final String detailId;
        private final String settleTxnId;
        private final String merchantCode;
        private final long txnAmount;
        private final String settleKbn;
        private final String outputStatus;

        private PcdtlfRecord(String detailId, String settleTxnId, String merchantCode, long txnAmount, String settleKbn, String outputStatus) {
            this.detailId = detailId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.txnAmount = txnAmount;
            this.settleKbn = settleKbn;
            this.outputStatus = outputStatus;
        }
    }

    private static final class TraceSnapshot {
        private final PtkeyfRecord key;
        private final PtsetfRecord settlement;
        private final java.util.List<PcdtlfRecord> details;

        private TraceSnapshot(PtkeyfRecord key, PtsetfRecord settlement, java.util.List<PcdtlfRecord> details) {
            this.key = key;
            this.settlement = settlement;
            this.details = details;
        }
    }

    private static final class AuditReport {
        private final int totalCount;
        private final int okCount;
        private final int warnCount;
        private final int ngCount;
        private final long totalAmount;
        private final PtkeyfRecord[] updatedKeys;

        private AuditReport(int totalCount, int okCount, int warnCount, int ngCount, long totalAmount, PtkeyfRecord[] updatedKeys) {
            this.totalCount = totalCount;
            this.okCount = okCount;
            this.warnCount = warnCount;
            this.ngCount = ngCount;
            this.totalAmount = totalAmount;
            this.updatedKeys = updatedKeys;
        }
    }
}
