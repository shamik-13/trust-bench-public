package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2025/01/22  みらいペイ システム部 精算・連携チーム  精算明細出力サービス初版
 */
public class SettleDetailWriterService {
    private static final String HOLD_AUTHORIZED = "00";
    private static final String HOLD_CAPTURED = "30";
    private static final String HOLD_CANCELED = "20";

    private static final String SETTLE_IMMEDIATE = "1";
    private static final String SETTLE_CARRY = "2";
    private static final String SETTLE_EXCLUDED = "9";

    public void run() {
        java.util.List<CaptureRecord> ptcapf = java.util.Arrays.asList(
                new CaptureRecord("CAP0001", "HLD0001", "STL-20250122-0001", "MRC10001", SETTLE_IMMEDIATE, 12500L),
                new CaptureRecord("CAP0002", "HLD0002", "STL-20250122-0002", "MRC10002", SETTLE_IMMEDIATE, 9800L),
                new CaptureRecord("CAP0003", "HLD0003", "STL-20250122-0003", "MRC10003", SETTLE_CARRY, 32100L),
                new CaptureRecord("CAP0004", "HLD0004", "STL-20250122-0004", "MRC10004", SETTLE_IMMEDIATE, 7000L),
                new CaptureRecord("CAP0005", "HLD0005", "STL-20250122-0005", "MRC10005", SETTLE_EXCLUDED, 4500L)
        );

        java.util.List<SettleRecord> ptsetf = readPtsetfCsv(
                "STL-20250122-0001,MRC10001,12500,1\n" +
                "STL-20250122-0002,MRC19999,9800,1\n" +
                "STL-20250122-0003,MRC10003,32000,2\n" +
                "STL-20250122-0004,MRC10004,7000,1\n" +
                "STL-20250122-0005,MRC10005,4500,9\n"
        );

        java.util.List<KeyRecord> ptkeyf = java.util.Arrays.asList(
                new KeyRecord("TRC0001", "HLD0001", "CAP0001", "STL-20250122-0001", "MRC10001", "00"),
                new KeyRecord("TRC0002", "HLD0002", "CAP0002", "STL-20250122-0002", "MRC10002", "00"),
                new KeyRecord("TRC0003", "HLD0003", "CAP0003", "STL-20250122-0003", "MRC10003", "10"),
                new KeyRecord("TRC0004", "HLD0004", "CAP0004", "STL-20250122-0004", "MRC10004", "20"),
                new KeyRecord("TRC0005", "HLD0005", "CAP0005", "STL-20250122-0005", "MRC10005", "00")
        );

        java.util.List<DetailRecord> pcdtlf = writeDetails(ptcapf, ptsetf, ptkeyf);

        for (DetailRecord detail : pcdtlf) {
            System.out.println(detail.toLine());
        }
    }

    private java.util.List<DetailRecord> writeDetails(
            java.util.List<CaptureRecord> captures,
            java.util.List<SettleRecord> settlements,
            java.util.List<KeyRecord> keys) {
        java.util.Map<String, CaptureRecord> captureByTxn = new java.util.LinkedHashMap<String, CaptureRecord>();
        for (CaptureRecord capture : captures) {
            if (captureByTxn.put(capture.settleTxnId, capture) != null) {
                throw new IllegalStateException("PTCAPFの精算取引IDが重複しています: " + capture.settleTxnId);
            }
        }

        java.util.Map<String, SettleRecord> settleByTxn = new java.util.LinkedHashMap<String, SettleRecord>();
        for (SettleRecord settle : settlements) {
            if (settleByTxn.put(settle.settleTxnId, settle) != null) {
                throw new IllegalStateException("PTSETFの精算取引IDが重複しています: " + settle.settleTxnId);
            }
        }

        java.util.Map<String, KeyRecord> keyByTxn = new java.util.LinkedHashMap<String, KeyRecord>();
        for (KeyRecord key : keys) {
            keyByTxn.put(key.settleTxnId, key);
        }

        java.util.List<DetailRecord> output = new java.util.ArrayList<DetailRecord>();
        int sequence = 1;
        for (java.util.Map.Entry<String, CaptureRecord> entry : captureByTxn.entrySet()) {
            String settleTxnId = entry.getKey();
            CaptureRecord capture = entry.getValue();
            SettleRecord settle = settleByTxn.get(settleTxnId);
            KeyRecord key = keyByTxn.get(settleTxnId);

            Decision decision = judge(capture, settle, key);
            if (!decision.outputAllowed) {
                System.out.println("明細出力停止: " + settleTxnId + " 理由=" + decision.reason);
                continue;
            }

            DetailRecord detail = new DetailRecord(
                    String.format("DTL%08d", Integer.valueOf(sequence++)),
                    settle.settleTxnId,
                    settle.merchantCode,
                    settle.txnAmt,
                    settle.settleKbn,
                    "出力済"
            );

            if (traceResultAtMostWarning(key.checkResult)) {
                int rc = mipayDetailEmit(detail);
                if (rc == 0) {
                    output.add(detail);
                } else {
                    System.out.println("明細出力停止: " + settleTxnId + " 理由=C側明細送信エラー RC=" + rc);
                }
            } else {
                System.out.println("明細出力停止: " + settleTxnId + " 理由=TraceModel検証結果が警告超過");
            }
        }
        return output;
    }

    private Decision judge(CaptureRecord capture, SettleRecord settle, KeyRecord key) {
        if (settle == null) {
            return new Decision(false, "PTSETF未検出");
        }
        if (key == null) {
            return new Decision(false, "PTKEYF未検出");
        }
        if (!capture.capId.equals(key.capId) || !capture.holdId.equals(key.holdId)) {
            return new Decision(false, "PTKEYFキー不一致");
        }
        if (!isSettleKbn(capture.settleKbn) || !isSettleKbn(settle.settleKbn)) {
            return new Decision(false, "精算区分不正");
        }
        if (SETTLE_EXCLUDED.equals(capture.settleKbn) || SETTLE_EXCLUDED.equals(settle.settleKbn)) {
            return new Decision(false, "精算対象外");
        }
        if (!capture.merchantCode.equals(settle.merchantCode) || !capture.merchantCode.equals(key.merchantCode)) {
            return new Decision(false, "加盟店差異");
        }
        if (capture.capAmt != settle.txnAmt) {
            return new Decision(false, "金額差異");
        }
        if (!capture.settleKbn.equals(settle.settleKbn)) {
            return new Decision(false, "精算区分差異");
        }
        return new Decision(true, "出力可能");
    }

    private static boolean traceResultAtMostWarning(String checkResult) {
        int level = parseTraceLevel(checkResult);
        consultTraceModel(checkResult, level);
        return level <= 10;
    }

    private static void consultTraceModel(String checkResult, int level) {
        try {
            Class<?> traceModel = Class.forName("jp.mirai.pay.trace.TraceModel");
            java.lang.reflect.Method method = traceModel.getMethod("valueOfCheckResult", String.class);
            Object result = method.invoke(null, checkResult);
            if (result == null && level > 10) {
                throw new IllegalStateException("TraceModel検証結果が未定義です: " + checkResult);
            }
        } catch (ClassNotFoundException e) {
            if (level > 10) {
                System.out.println("TraceModel未配置のためコード値で判定します: " + checkResult);
            }
        } catch (NoSuchMethodException e) {
            if (level > 10) {
                System.out.println("TraceModel検証API未検出のためコード値で判定します: " + checkResult);
            }
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("TraceModel検証結果の参照に失敗しました: " + checkResult, e);
        }
    }

    private static int parseTraceLevel(String checkResult) {
        try {
            return Integer.parseInt(checkResult);
        } catch (NumberFormatException e) {
            return 99;
        }
    }

    private static boolean isSettleKbn(String settleKbn) {
        return SETTLE_IMMEDIATE.equals(settleKbn)
                || SETTLE_CARRY.equals(settleKbn)
                || SETTLE_EXCLUDED.equals(settleKbn);
    }

    private static java.util.List<SettleRecord> readPtsetfCsv(String csv) {
        java.util.List<SettleRecord> records = new java.util.ArrayList<SettleRecord>();
        String[] lines = csv.split("\\R");
        for (String line : lines) {
            if (line.trim().isEmpty()) {
                continue;
            }
            String[] columns = line.split(",", -1);
            if (columns.length != 4) {
                throw new IllegalArgumentException("PTSETF CSV項目数不正: " + line);
            }
            records.add(new SettleRecord(
                    columns[0].trim(),
                    columns[1].trim(),
                    Long.parseLong(columns[2].trim()),
                    columns[3].trim()
            ));
        }
        return records;
    }

    private static int mipayDetailEmit(DetailRecord detail) {
        if (detail.txnAmt < 0L) {
            return 8;
        }
        System.out.println("C側明細送信: " + detail.detailId + " " + detail.settleTxnId);
        return 0;
    }

    private static final class CaptureRecord {
        final String capId;
        final String holdId;
        final String settleTxnId;
        final String merchantCode;
        final String settleKbn;
        final long capAmt;

        CaptureRecord(String capId, String holdId, String settleTxnId, String merchantCode, String settleKbn, long capAmt) {
            this.capId = capId;
            this.holdId = holdId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.settleKbn = settleKbn;
            this.capAmt = capAmt;
        }
    }

    private static final class SettleRecord {
        final String settleTxnId;
        final String merchantCode;
        final long txnAmt;
        final String settleKbn;

        SettleRecord(String settleTxnId, String merchantCode, long txnAmt, String settleKbn) {
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.txnAmt = txnAmt;
            this.settleKbn = settleKbn;
        }
    }

    private static final class KeyRecord {
        final String traceKey;
        final String holdId;
        final String capId;
        final String settleTxnId;
        final String merchantCode;
        final String checkResult;

        KeyRecord(String traceKey, String holdId, String capId, String settleTxnId, String merchantCode, String checkResult) {
            this.traceKey = traceKey;
            this.holdId = holdId;
            this.capId = capId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.checkResult = checkResult;
        }
    }

    private static final class DetailRecord {
        final String detailId;
        final String settleTxnId;
        final String merchantCode;
        final long txnAmt;
        final String settleKbn;
        final String outputStatus;

        DetailRecord(String detailId, String settleTxnId, String merchantCode, long txnAmt, String settleKbn, String outputStatus) {
            this.detailId = detailId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.txnAmt = txnAmt;
            this.settleKbn = settleKbn;
            this.outputStatus = outputStatus;
        }

        String toLine() {
            return detailId + "," + settleTxnId + "," + merchantCode + "," + txnAmt + "," + settleKbn + "," + outputStatus;
        }
    }

    private static final class Decision {
        final boolean outputAllowed;
        final String reason;

        Decision(boolean outputAllowed, String reason) {
            this.outputAllowed = outputAllowed;
            this.reason = reason;
        }
    }
}
