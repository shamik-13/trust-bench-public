package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数    年月日      担当                              概要
 * 1.00    2024/11/19  みらいペイ システム部 精算・連携チーム  初版作成
 */
public class CaptureReconcileBatchService {
    private static final String HD_STATUS_AUTHORIZED = "00";
    private static final String HD_STATUS_CAPTURED = "30";
    private static final String HD_STATUS_CANCELED = "20";

    private static final String SETTLE_IMMEDIATE = "1";
    private static final String SETTLE_CARRY = "2";
    private static final String SETTLE_EXCLUDED = "9";

    private static final String CHECK_OK = "OK";
    private static final String IMPORT_UNREFLECTED = "00";
    private static final String IMPORT_RETRY = "10";
    private static final String IMPORT_REMAND = "90";

    private static final long ALLOW_DIFF_YEN = 50L;

    public void run() {
        execute();
    }

    private void execute() {
        java.util.List<HoldRecord> ptholdf = java.util.Arrays.asList(
                new HoldRecord("HLD-20241115-0001", "WLT-10001", "MRC-0001", 12000L, HD_STATUS_CAPTURED),
                new HoldRecord("HLD-20241115-0002", "WLT-10002", "MRC-0002", 8500L, HD_STATUS_AUTHORIZED),
                new HoldRecord("HLD-20241115-0003", "WLT-10003", "MRC-0003", 4300L, HD_STATUS_CANCELED),
                new HoldRecord("HLD-20241115-0004", "WLT-10004", "MRC-0004", 19980L, HD_STATUS_CAPTURED),
                new HoldRecord("HLD-20241115-0005", "WLT-10005", "MRC-0005", 3000L, HD_STATUS_CAPTURED),
                new HoldRecord("HLD-20241115-0002", "WLT-10002", "MRC-0002", 8500L, HD_STATUS_CAPTURED)
        );

        java.util.List<CaptureRecord> ptcapf = java.util.Arrays.asList(
                new CaptureRecord("CAP-20241115-0001", "HLD-20241115-0001", "STL-900001", "MRC-0001", SETTLE_IMMEDIATE, 12000L),
                new CaptureRecord("CAP-20241115-0002", "HLD-20241115-0002", "STL-900002", "MRC-0002", SETTLE_IMMEDIATE, 8510L),
                new CaptureRecord("CAP-20241115-0003", "HLD-20241115-0003", "STL-900003", "MRC-0003", SETTLE_IMMEDIATE, 4300L),
                new CaptureRecord("CAP-20241115-0004", "HLD-20241115-0004", "STL-900004", "MRC-0004", SETTLE_IMMEDIATE, 20550L),
                new CaptureRecord("CAP-20241115-0005", "HLD-20241115-0005", "STL-900005", "MRC-0005", SETTLE_EXCLUDED, 3000L),
                new CaptureRecord("CAP-20241115-0006", "HLD-20241115-0099", "STL-900006", "MRC-0099", SETTLE_CARRY, 7100L)
        );

        java.util.Map<String, KeyRecord> ptkeyf = new java.util.LinkedHashMap<String, KeyRecord>();
        putKey(ptkeyf, new KeyRecord("TRC-0001", "HLD-20241115-0001", "CAP-20241115-0001", "STL-900001", "MRC-0001", CHECK_OK));
        putKey(ptkeyf, new KeyRecord("TRC-0002", "HLD-20241115-0002", "CAP-20241115-0002", "STL-900002", "MRC-0002", CHECK_OK));
        putKey(ptkeyf, new KeyRecord("TRC-0003", "HLD-20241115-0003", "CAP-20241115-0003", "STL-900003", "MRC-0003", CHECK_OK));
        putKey(ptkeyf, new KeyRecord("TRC-0004", "HLD-20241115-0004", "CAP-20241115-0004", "STL-900004", "MRC-0004", CHECK_OK));
        putKey(ptkeyf, new KeyRecord("TRC-0005", "HLD-20241115-0005", "CAP-20241115-0005", "STL-900005", "MRC-0005", CHECK_OK));
        putKey(ptkeyf, new KeyRecord("TRC-0006", "HLD-20241115-0099", "CAP-20241115-0006", "STL-900006", "MRC-0099", "NG"));

        java.util.Map<String, String> importStatusByCapId = new java.util.LinkedHashMap<String, String>();
        importStatusByCapId.put("CAP-20241115-0001", IMPORT_UNREFLECTED);
        importStatusByCapId.put("CAP-20241115-0002", IMPORT_UNREFLECTED);
        importStatusByCapId.put("CAP-20241115-0003", IMPORT_UNREFLECTED);
        importStatusByCapId.put("CAP-20241115-0004", IMPORT_UNREFLECTED);
        importStatusByCapId.put("CAP-20241115-0005", IMPORT_UNREFLECTED);
        importStatusByCapId.put("CAP-20241115-0006", IMPORT_UNREFLECTED);

        java.util.Map<String, HoldRecord> latestHoldById = latestHoldById(ptholdf);
        java.util.List<ImportRecord> ptinpf = new java.util.ArrayList<ImportRecord>();
        java.util.Map<String, String> reasonByCapId = new java.util.LinkedHashMap<String, String>();

        String batchId = "IMB-20241115-001";
        for (CaptureRecord cap : ptcapf) {
            if (!IMPORT_UNREFLECTED.equals(importStatusByCapId.get(cap.capId))) {
                continue;
            }

            Decision decision = decide(cap, latestHoldById.get(cap.holdId), ptkeyf.get(traceKey(cap)));
            if (decision.retry) {
                ptinpf.add(new ImportRecord(batchId, cap.capId, cap.holdId, cap.merchantCode, cap.capAmt, IMPORT_RETRY));
                importStatusByCapId.put(cap.capId, IMPORT_RETRY);
            } else {
                importStatusByCapId.put(cap.capId, IMPORT_REMAND);
                reasonByCapId.put(cap.capId, decision.reasonCode);
            }
        }

        printImportFile(ptinpf);
        printRemand(importStatusByCapId, reasonByCapId);
    }

    private static java.util.Map<String, HoldRecord> latestHoldById(java.util.List<HoldRecord> holds) {
        java.util.Map<String, HoldRecord> latest = new java.util.LinkedHashMap<String, HoldRecord>();
        for (HoldRecord hold : holds) {
            latest.put(hold.holdId, hold);
        }
        return latest;
    }

    private static Decision decide(CaptureRecord cap, HoldRecord hold, KeyRecord key) {
        if (SETTLE_EXCLUDED.equals(cap.settleKbn)) {
            return Decision.remand("R01");
        }
        if (SETTLE_CARRY.equals(cap.settleKbn)) {
            return Decision.remand("R02");
        }
        if (hold == null) {
            return Decision.remand("R03");
        }
        if (!HD_STATUS_CAPTURED.equals(hold.holdStatus)) {
            return Decision.remand("R04");
        }
        if (!hold.merchantCode.equals(cap.merchantCode)) {
            return Decision.remand("R05");
        }
        if (key == null || !CHECK_OK.equals(key.checkResult)) {
            return Decision.remand("R06");
        }
        if (!key.holdId.equals(cap.holdId)
                || !key.capId.equals(cap.capId)
                || !key.settleTxnId.equals(cap.settleTxnId)
                || !key.merchantCode.equals(cap.merchantCode)) {
            return Decision.remand("R07");
        }
        if (Math.abs(hold.holdAmt - cap.capAmt) > ALLOW_DIFF_YEN) {
            return Decision.remand("R08");
        }
        return Decision.retry();
    }

    private static void putKey(java.util.Map<String, KeyRecord> ptkeyf, KeyRecord key) {
        ptkeyf.put(traceKey(key.holdId, key.capId, key.settleTxnId, key.merchantCode), key);
    }

    private static String traceKey(CaptureRecord cap) {
        return traceKey(cap.holdId, cap.capId, cap.settleTxnId, cap.merchantCode);
    }

    private static String traceKey(String holdId, String capId, String settleTxnId, String merchantCode) {
        return holdId + "|" + capId + "|" + settleTxnId + "|" + merchantCode;
    }

    private static void printImportFile(java.util.List<ImportRecord> ptinpf) {
        System.out.println("PTINPF生成件数=" + ptinpf.size());
        for (ImportRecord row : ptinpf) {
            System.out.println(row.importBatchId + "," + row.capId + "," + row.holdId + ","
                    + row.merchantCode + "," + row.capAmt + "," + row.importStatus);
        }
    }

    private static void printRemand(java.util.Map<String, String> importStatusByCapId,
                                    java.util.Map<String, String> reasonByCapId) {
        for (java.util.Map.Entry<String, String> entry : reasonByCapId.entrySet()) {
            System.out.println("差戻 CAP-ID=" + entry.getKey()
                    + " IMPORT-STATUS=" + importStatusByCapId.get(entry.getKey())
                    + " 理由コード=" + entry.getValue());
        }
    }

    private static final class HoldRecord {
        private final String holdId;
        private final String walletId;
        private final String merchantCode;
        private final long holdAmt;
        private final String holdStatus;

        private HoldRecord(String holdId, String walletId, String merchantCode, long holdAmt, String holdStatus) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.holdAmt = holdAmt;
            this.holdStatus = holdStatus;
        }
    }

    private static final class CaptureRecord {
        private final String capId;
        private final String holdId;
        private final String settleTxnId;
        private final String merchantCode;
        private final String settleKbn;
        private final long capAmt;

        private CaptureRecord(String capId, String holdId, String settleTxnId,
                              String merchantCode, String settleKbn, long capAmt) {
            this.capId = capId;
            this.holdId = holdId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.settleKbn = settleKbn;
            this.capAmt = capAmt;
        }
    }

    private static final class KeyRecord {
        private final String traceKey;
        private final String holdId;
        private final String capId;
        private final String settleTxnId;
        private final String merchantCode;
        private final String checkResult;

        private KeyRecord(String traceKey, String holdId, String capId,
                          String settleTxnId, String merchantCode, String checkResult) {
            this.traceKey = traceKey;
            this.holdId = holdId;
            this.capId = capId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.checkResult = checkResult;
        }
    }

    private static final class ImportRecord {
        private final String importBatchId;
        private final String capId;
        private final String holdId;
        private final String merchantCode;
        private final long capAmt;
        private final String importStatus;

        private ImportRecord(String importBatchId, String capId, String holdId,
                             String merchantCode, long capAmt, String importStatus) {
            this.importBatchId = importBatchId;
            this.capId = capId;
            this.holdId = holdId;
            this.merchantCode = merchantCode;
            this.capAmt = capAmt;
            this.importStatus = importStatus;
        }
    }

    private static final class Decision {
        private final boolean retry;
        private final String reasonCode;

        private Decision(boolean retry, String reasonCode) {
            this.retry = retry;
            this.reasonCode = reasonCode;
        }

        private static Decision retry() {
            return new Decision(true, "");
        }

        private static Decision remand(String reasonCode) {
            return new Decision(false, reasonCode);
        }
    }
}
