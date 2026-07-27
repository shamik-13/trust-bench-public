package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2024/09/12  みらいペイ システム部 精算・連携チーム  初版作成。取込明細(PTINPF)を検証しPTCAPFへ展開。
 */
public class CaptureImportService {
    private static final String HOLD_STATUS_VALID = "00";
    private static final String HOLD_STATUS_CAPTURED = "30";
    private static final String HOLD_STATUS_CANCELLED = "20";

    private static final String SETTLE_IMMEDIATE = "1";
    private static final String SETTLE_DEFERRED = "2";

    private static final java.math.BigDecimal DEFERRED_LIMIT = new java.math.BigDecimal("300000");

    public static void main(String[] a) throws Exception {
        java.nio.file.Path base = a.length == 0 ? java.nio.file.Paths.get(".") : java.nio.file.Paths.get(a[0]);
        java.nio.file.Path holdFile = base.resolve("PTHOLDF.csv");
        java.nio.file.Path importFile = base.resolve("PTINPF.csv");
        java.nio.file.Path capFile = base.resolve("PTCAPF.csv");
        java.nio.file.Path keyFile = base.resolve("PTKEYF.csv");

        java.util.Map<String, HoldRow> holds = readHolds(holdFile);
        java.util.List<ImportRow> imports = readImports(importFile);

        java.util.List<String> capLines = new java.util.ArrayList<>();
        java.util.List<String> keyLines = new java.util.ArrayList<>();
        java.util.Set<String> acceptedCapIds = new java.util.HashSet<>();

        capLines.add("CAP-ID,HOLD-ID,SETTLE-TXN-ID,MERCHANT-CODE,SETTLE-KBN,CAP-AMT");
        keyLines.add("TRACE-KEY,HOLD-ID,CAP-ID,SETTLE-TXN-ID,MERCHANT-CODE,CHECK-RESULT");

        for (ImportRow in : imports) {
            if (!"0".equals(in.importStatus)) {
                writeCheck(keyLines, in, "", "取込対象外");
                continue;
            }

            if (!acceptedCapIds.add(in.capId)) {
                writeCheck(keyLines, in, "", "重複CAP-ID");
                continue;
            }

            HoldRow hold = holds.get(in.holdId);
            if (hold == null) {
                writeCheck(keyLines, in, "", "ホールド未検出");
                continue;
            }

            if (!hold.merchantCode.equals(in.merchantCode)) {
                writeCheck(keyLines, in, "", "加盟店不一致");
                continue;
            }

            if (HOLD_STATUS_CAPTURED.equals(hold.holdStatus)) {
                writeCheck(keyLines, in, "", "売上確定済");
                continue;
            }

            if (HOLD_STATUS_CANCELLED.equals(hold.holdStatus)) {
                writeCheck(keyLines, in, "", "取消済");
                continue;
            }

            if (!HOLD_STATUS_VALID.equals(hold.holdStatus)) {
                writeCheck(keyLines, in, "", "ホールド状態不正");
                continue;
            }

            if (in.capAmt.compareTo(java.math.BigDecimal.ZERO) <= 0) {
                writeCheck(keyLines, in, "", "売上金額不正");
                continue;
            }

            if (in.capAmt.compareTo(hold.holdAmt) > 0) {
                writeCheck(keyLines, in, "", "売上金額超過");
                continue;
            }

            // SETTLE-TXN-ID は連携元(PTINPF)が採番した不変キーをそのまま引き継ぐ（当処理では導出しない）。
            String settleTxnId = in.settleTxnId;
            if (settleTxnId.isEmpty()) {
                writeCheck(keyLines, in, "", "精算取引ID未設定");
                continue;
            }
            String settleKbn = in.capAmt.compareTo(DEFERRED_LIMIT) > 0 ? SETTLE_DEFERRED : SETTLE_IMMEDIATE;

            capLines.add(csv(in.capId, in.holdId, settleTxnId, in.merchantCode, settleKbn, money(in.capAmt)));
            writeCheck(keyLines, in, settleTxnId, "正常");
        }

        java.nio.file.Files.write(capFile, capLines, java.nio.charset.StandardCharsets.UTF_8);
        java.nio.file.Files.write(keyFile, keyLines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static java.util.Map<String, HoldRow> readHolds(java.nio.file.Path file) throws java.io.IOException {
        java.util.Map<String, HoldRow> rows = new java.util.LinkedHashMap<>();
        for (String line : readDataLines(file)) {
            String[] c = splitCsv(line, 5);
            HoldRow row = new HoldRow(c[0], c[1], c[2], amount(c[3]), c[4]);
            rows.put(row.holdId, row);
        }
        return rows;
    }

    private static java.util.List<ImportRow> readImports(java.nio.file.Path file) throws java.io.IOException {
        java.util.List<ImportRow> rows = new java.util.ArrayList<>();
        for (String line : readDataLines(file)) {
            String[] c = splitCsv(line, 7);
            rows.add(new ImportRow(c[0], c[1], c[2], c[3], c[4], amount(c[5]), c[6]));
        }
        return rows;
    }

    private static java.util.List<String> readDataLines(java.nio.file.Path file) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(file, java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<String> data = new java.util.ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            if (line.isEmpty()) {
                continue;
            }
            if (i == 0 && line.indexOf('-') >= 0) {
                continue;
            }
            data.add(line);
        }
        return data;
    }

    private static String[] splitCsv(String line, int size) {
        String[] c = line.split(",", -1);
        if (c.length != size) {
            throw new IllegalArgumentException("項目数不正: " + line);
        }
        for (int i = 0; i < c.length; i++) {
            c[i] = c[i].trim();
        }
        return c;
    }

    private static java.math.BigDecimal amount(String value) {
        return new java.math.BigDecimal(value).setScale(0, java.math.RoundingMode.UNNECESSARY);
    }

    private static String money(java.math.BigDecimal value) {
        return value.setScale(0, java.math.RoundingMode.UNNECESSARY).toPlainString();
    }

    private static void writeCheck(java.util.List<String> lines, ImportRow in, String settleTxnId, String result) {
        String traceKey = in.importBatchId + ":" + in.capId;
        lines.add(csv(traceKey, in.holdId, in.capId, settleTxnId, in.merchantCode, result));
    }

    private static String csv(String... values) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            String v = values[i] == null ? "" : values[i];
            if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0) {
                b.append('"').append(v.replace("\"", "\"\"")).append('"');
            } else {
                b.append(v);
            }
        }
        return b.toString();
    }

    private static final class HoldRow {
        final String holdId;
        final String walletId;
        final String merchantCode;
        final java.math.BigDecimal holdAmt;
        final String holdStatus;

        HoldRow(String holdId, String walletId, String merchantCode, java.math.BigDecimal holdAmt, String holdStatus) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.holdAmt = holdAmt;
            this.holdStatus = holdStatus;
        }
    }

    private static final class ImportRow {
        final String importBatchId;
        final String capId;
        final String holdId;
        final String settleTxnId;
        final String merchantCode;
        final java.math.BigDecimal capAmt;
        final String importStatus;

        ImportRow(String importBatchId, String capId, String holdId, String settleTxnId,
                  String merchantCode, java.math.BigDecimal capAmt, String importStatus) {
            this.importBatchId = importBatchId;
            this.capId = capId;
            this.holdId = holdId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.capAmt = capAmt;
            this.importStatus = importStatus;
        }
    }
}
