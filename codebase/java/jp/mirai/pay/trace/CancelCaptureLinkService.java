package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2026-06-28  基盤移行  取消連携サービスの初版作成
 */
public class CancelCaptureLinkService {
    private static final int CANCEL_WINDOW_DAYS = 45;
    private static final java.time.LocalDate PROCESS_DATE = java.time.LocalDate.of(2026, 6, 28);
    private static final Class<TraceModel> TRACE_MODEL_CLASS = TraceModel.class;

    public static void main(String[] a) {
        java.util.List<PtcapfRecord> captures = ptcapf();
        java.util.Map<String, PtholdfRecord> holdsById = indexHolds(ptholdf());
        java.util.List<CancelRequest> requests = cancelRequests();

        java.util.List<PtcanfRecord> ptcanf = new java.util.ArrayList<>();
        java.util.List<PtkeyfRecord> ptkeyf = new java.util.ArrayList<>();

        int accepted = 0;
        int rejected = 0;

        for (CancelRequest request : requests) {
            PtcapfRecord capture = findCapture(captures, request.capId);
            String holdId = capture == null ? "" : capture.holdId;
            String settleTxnId = capture == null ? "" : capture.settleTxnId;
            String merchantCode = capture == null ? request.merchantCode : capture.merchantCode;

            CheckResult result = validate(request, capture, holdsById);
            ptkeyf.add(new PtkeyfRecord(
                    traceKey(request.cancelId, request.capId),
                    holdId,
                    request.capId,
                    settleTxnId,
                    merchantCode,
                    result.code
            ));

            if (result.accepted) {
                accepted++;
                ptcanf.add(new PtcanfRecord(
                        request.cancelId,
                        capture.capId,
                        capture.holdId,
                        capture.merchantCode,
                        request.cancelAmt,
                        "01"
                ));
            } else {
                rejected++;
            }
        }

        printPtcanf(ptcanf);
        printPtkeyf(ptkeyf);
        System.out.println("処理日=" + PROCESS_DATE + " 取消予定=" + accepted + " 否認=" + rejected
                + " 参照モデル=" + TRACE_MODEL_CLASS.getSimpleName());
    }

    private static CheckResult validate(
            CancelRequest request,
            PtcapfRecord capture,
            java.util.Map<String, PtholdfRecord> holdsById
    ) {
        if (capture == null) {
            return CheckResult.no("E01");
        }

        PtholdfRecord hold = holdsById.get(capture.holdId);
        if (hold == null) {
            return CheckResult.no("E02");
        }

        if (!"30".equals(hold.holdStatus)) {
            return CheckResult.no("E03");
        }

        if (!capture.merchantCode.equals(request.merchantCode)
                || !hold.merchantCode.equals(request.merchantCode)) {
            return CheckResult.no("E04");
        }

        long elapsedDays = java.time.temporal.ChronoUnit.DAYS.between(capture.captureDate, PROCESS_DATE);
        if (elapsedDays < 0 || elapsedDays > CANCEL_WINDOW_DAYS) {
            return CheckResult.no("E05");
        }

        if (request.cancelAmt <= 0 || request.cancelAmt > capture.capAmt) {
            return CheckResult.no("E06");
        }

        return CheckResult.ok();
    }

    private static PtcapfRecord findCapture(java.util.List<PtcapfRecord> captures, String capId) {
        for (PtcapfRecord capture : captures) {
            if (capture.capId.equals(capId)) {
                return capture;
            }
        }
        return null;
    }

    private static java.util.Map<String, PtholdfRecord> indexHolds(java.util.List<PtholdfRecord> holds) {
        java.util.Map<String, PtholdfRecord> byId = new java.util.LinkedHashMap<>();
        for (PtholdfRecord hold : holds) {
            byId.put(hold.holdId, hold);
        }
        return byId;
    }

    private static String traceKey(String cancelId, String capId) {
        return cancelId + "-" + capId;
    }

    private static void printPtcanf(java.util.List<PtcanfRecord> records) {
        System.out.println("PTCANF 出力件数=" + records.size());
        for (PtcanfRecord r : records) {
            System.out.println(r.cancelId + "," + r.capId + "," + r.holdId + ","
                    + r.merchantCode + "," + r.cancelAmt + "," + r.cancelStatus);
        }
    }

    private static void printPtkeyf(java.util.List<PtkeyfRecord> records) {
        System.out.println("PTKEYF 出力件数=" + records.size());
        for (PtkeyfRecord r : records) {
            System.out.println(r.traceKey + "," + r.holdId + "," + r.capId + ","
                    + r.settleTxnId + "," + r.merchantCode + "," + r.checkResult);
        }
    }

    private static java.util.List<PtcapfRecord> ptcapf() {
        java.util.List<PtcapfRecord> records = new java.util.ArrayList<>();
        records.add(new PtcapfRecord("CAP202606010001", "HLD202605310001", "STL202606010001",
                "MRC10001", "1", 12800, java.time.LocalDate.of(2026, 6, 1)));
        records.add(new PtcapfRecord("CAP202606020002", "HLD202606010002", "STL202606020002",
                "MRC10002", "2", 45600, java.time.LocalDate.of(2026, 6, 2)));
        records.add(new PtcapfRecord("CAP202604200003", "HLD202604190003", "STL202604200003",
                "MRC10003", "1", 9000, java.time.LocalDate.of(2026, 4, 20)));
        records.add(new PtcapfRecord("CAP202606100004", "HLD202606090004", "STL202606100004",
                "MRC10004", "9", 3200, java.time.LocalDate.of(2026, 6, 10)));
        return records;
    }

    private static java.util.List<PtholdfRecord> ptholdf() {
        java.util.List<PtholdfRecord> records = new java.util.ArrayList<>();
        records.add(new PtholdfRecord("HLD202605310001", "WLT000001", "MRC10001", 12800, "30"));
        records.add(new PtholdfRecord("HLD202606010002", "WLT000002", "MRC10002", 45600, "30"));
        records.add(new PtholdfRecord("HLD202604190003", "WLT000003", "MRC10003", 9000, "30"));
        records.add(new PtholdfRecord("HLD202606090004", "WLT000004", "MRC10004", 3200, "00"));
        return records;
    }

    private static java.util.List<CancelRequest> cancelRequests() {
        java.util.List<CancelRequest> records = new java.util.ArrayList<>();
        records.add(new CancelRequest("CAN202606280001", "CAP202606010001", "MRC10001", 12800));
        records.add(new CancelRequest("CAN202606280002", "CAP202606020002", "MRC99999", 45600));
        records.add(new CancelRequest("CAN202606280003", "CAP202604200003", "MRC10003", 9000));
        records.add(new CancelRequest("CAN202606280004", "CAP202606100004", "MRC10004", 3200));
        records.add(new CancelRequest("CAN202606280005", "CAP202606020002", "MRC10002", 50000));
        return records;
    }

    private static final class CheckResult {
        final boolean accepted;
        final String code;

        private CheckResult(boolean accepted, String code) {
            this.accepted = accepted;
            this.code = code;
        }

        static CheckResult ok() {
            return new CheckResult(true, "00");
        }

        static CheckResult no(String code) {
            return new CheckResult(false, code);
        }
    }

    private static final class CancelRequest {
        final String cancelId;
        final String capId;
        final String merchantCode;
        final int cancelAmt;

        CancelRequest(String cancelId, String capId, String merchantCode, int cancelAmt) {
            this.cancelId = cancelId;
            this.capId = capId;
            this.merchantCode = merchantCode;
            this.cancelAmt = cancelAmt;
        }
    }

    private static final class PtcapfRecord {
        final String capId;
        final String holdId;
        final String settleTxnId;
        final String merchantCode;
        final String settleKbn;
        final int capAmt;
        final java.time.LocalDate captureDate;

        PtcapfRecord(String capId, String holdId, String settleTxnId, String merchantCode,
                     String settleKbn, int capAmt, java.time.LocalDate captureDate) {
            this.capId = capId;
            this.holdId = holdId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.settleKbn = settleKbn;
            this.capAmt = capAmt;
            this.captureDate = captureDate;
        }
    }

    private static final class PtholdfRecord {
        final String holdId;
        final String walletId;
        final String merchantCode;
        final int holdAmt;
        final String holdStatus;

        PtholdfRecord(String holdId, String walletId, String merchantCode, int holdAmt, String holdStatus) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.holdAmt = holdAmt;
            this.holdStatus = holdStatus;
        }
    }

    private static final class PtcanfRecord {
        final String cancelId;
        final String capId;
        final String holdId;
        final String merchantCode;
        final int cancelAmt;
        final String cancelStatus;

        PtcanfRecord(String cancelId, String capId, String holdId, String merchantCode,
                     int cancelAmt, String cancelStatus) {
            this.cancelId = cancelId;
            this.capId = capId;
            this.holdId = holdId;
            this.merchantCode = merchantCode;
            this.cancelAmt = cancelAmt;
            this.cancelStatus = cancelStatus;
        }
    }

    private static final class PtkeyfRecord {
        final String traceKey;
        final String holdId;
        final String capId;
        final String settleTxnId;
        final String merchantCode;
        final String checkResult;

        PtkeyfRecord(String traceKey, String holdId, String capId, String settleTxnId,
                     String merchantCode, String checkResult) {
            this.traceKey = traceKey;
            this.holdId = holdId;
            this.capId = capId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.checkResult = checkResult;
        }
    }
}
