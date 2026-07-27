package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-10-15  みらいペイ システム部  初版作成
 */
public class HoldExpiryReportWriter {
    private static final String 入力ホールドファイル = "PYHOLDF.csv";
    private static final String 入力ウォレットファイル = "PYWALF.csv";
    private static final String 出力通知ファイル = "PYNTFF.csv";

    private static final String ウォレット有効 = "01";
    private static final String ウォレット利用停止 = "02";
    private static final String ウォレット解約 = "03";
    private static final String ウォレット制限中 = "09";

    private static final String ホールド承認済 = "00";
    private static final String 通貨円 = "JPY";

    private static final java.time.format.DateTimeFormatter 日付書式 =
            java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd");
    private static final java.time.format.DateTimeFormatter 時刻書式 =
            java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

    public static void main(String[] args) throws Exception {
        java.nio.file.Path ホールドパス = java.nio.file.Paths.get(args.length > 0 ? args[0] : 入力ホールドファイル);
        java.nio.file.Path ウォレットパス = java.nio.file.Paths.get(args.length > 1 ? args[1] : 入力ウォレットファイル);
        java.nio.file.Path 通知パス = java.nio.file.Paths.get(args.length > 2 ? args[2] : 出力通知ファイル);

        java.util.Map<String, WalletRow> ウォレット索引 = 読込ウォレット(ウォレットパス);
        java.util.Map<String, WalletSummary> 集計表 = new java.util.TreeMap<String, WalletSummary>();

        for (HoldRow ホールド : 読込ホールド(ホールドパス)) {
            if (!ホールド承認済.equals(ホールド.holdResult)) {
                continue;
            }
            if (!通貨円.equals(ホールド.currencyCd)) {
                continue;
            }
            if (ホールド.holdExpDt.isAfter(java.time.LocalDate.now())) {
                continue;
            }

            WalletRow ウォレット = ウォレット索引.get(ホールド.walletId);
            WalletSummary 集計 = 集計表.get(ホールド.walletId);
            if (集計 == null) {
                集計 = new WalletSummary(ホールド.walletId, ウォレット);
                集計表.put(ホールド.walletId, 集計);
            }
            集計.add(ホールド);
        }

        java.util.List<NoticeRow> 通知一覧 = new java.util.ArrayList<NoticeRow>();
        int 連番 = 1;
        String 作成時刻 = java.time.LocalDateTime.now().format(時刻書式);
        String 作成日 = java.time.LocalDate.now().format(日付書式);

        for (WalletSummary 集計 : 集計表.values()) {
            String 通知番号 = "NT" + 作成日 + String.format("%06d", 連番++);
            通知一覧.add(通知作成(通知番号, 集計, 作成時刻));
        }

        書込通知(通知パス, 通知一覧);
        System.out.println("ホールド失効レポート出力 件数=" + 通知一覧.size());
    }

    private static java.util.Map<String, WalletRow> 読込ウォレット(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, WalletRow> rows = new java.util.HashMap<String, WalletRow>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }

        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        for (String line : lines) {
            if (line.trim().isEmpty() || line.startsWith("WALLET-ID")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            if (c.size() < 5) {
                throw new IllegalArgumentException("ウォレット入力形式不正: " + line);
            }
            WalletRow row = new WalletRow(c.get(0), c.get(1), c.get(2), c.get(3), c.get(4));
            rows.put(row.walletId, row);
        }
        return rows;
    }

    private static java.util.List<HoldRow> 読込ホールド(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<HoldRow> rows = new java.util.ArrayList<HoldRow>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }

        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
        for (String line : lines) {
            if (line.trim().isEmpty() || line.startsWith("HOLD-ID")) {
                continue;
            }
            java.util.List<String> c = splitCsv(line);
            if (c.size() < 7) {
                throw new IllegalArgumentException("ホールド入力形式不正: " + line);
            }
            rows.add(new HoldRow(
                    c.get(0),
                    c.get(1),
                    new java.math.BigDecimal(c.get(2)),
                    c.get(3),
                    c.get(4),
                    c.get(5),
                    java.time.LocalDate.parse(c.get(6), 日付書式)));
        }
        return rows;
    }

    private static NoticeRow 通知作成(String noticeId, WalletSummary summary, String createTs) {
        if (summary.wallet == null) {
            return new NoticeRow(
                    noticeId,
                    summary.walletId,
                    "監査",
                    "ウォレット未登録のため通知抑止 対象件数=" + summary.count + " 対象金額=" + summary.amount.toPlainString(),
                    "抑止",
                    createTs);
        }

        String status = summary.wallet.walletStatus;
        if (ウォレット利用停止.equals(status) || ウォレット解約.equals(status) || ウォレット制限中.equals(status)) {
            return new NoticeRow(
                    noticeId,
                    summary.walletId,
                    "監査",
                    "ウォレット状態により通知抑止 状態=" + status + " 対象件数=" + summary.count
                            + " 対象金額=" + summary.amount.toPlainString(),
                    "抑止",
                    createTs);
        }

        if (!ウォレット有効.equals(status)) {
            return new NoticeRow(
                    noticeId,
                    summary.walletId,
                    "監査",
                    "ウォレット状態不正のため通知抑止 状態=" + status + " 対象件数=" + summary.count
                            + " 対象金額=" + summary.amount.toPlainString(),
                    "抑止",
                    createTs);
        }

        String text = summary.wallet.userNameKana + " 様 ホールド失効予定があります。件数="
                + summary.count + " 金額=" + summary.amount.toPlainString() + "円 最古失効日="
                + summary.oldestExpDt.format(日付書式);
        return new NoticeRow(noticeId, summary.walletId, "ホールド失効", text, "作成済", createTs);
    }

    private static void 書込通知(java.nio.file.Path path, java.util.List<NoticeRow> rows) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<String>();
        lines.add("NOTICE-ID,WALLET-ID,NOTICE-KBN,NOTICE-TEXT,SEND-STATUS,CREATE-TS");
        for (NoticeRow row : rows) {
            lines.add(joinCsv(row.noticeId, row.walletId, row.noticeKbn, row.noticeText, row.sendStatus, row.createTs));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder cur = new StringBuilder();
        boolean quote = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quote && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cur.append('"');
                    i++;
                } else {
                    quote = !quote;
                }
            } else if (ch == ',' && !quote) {
                out.add(cur.toString().trim());
                cur.setLength(0);
            } else {
                cur.append(ch);
            }
        }
        out.add(cur.toString().trim());
        return out;
    }

    private static String joinCsv(String... values) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            b.append(escapeCsv(values[i]));
        }
        return b.toString();
    }

    private static String escapeCsv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }

    private static final class HoldRow {
        final String holdId;
        final String walletId;
        final java.math.BigDecimal holdAmt;
        final String holdResult;
        final String merchantCode;
        final String currencyCd;
        final java.time.LocalDate holdExpDt;

        HoldRow(String holdId, String walletId, java.math.BigDecimal holdAmt, String holdResult,
                String merchantCode, String currencyCd, java.time.LocalDate holdExpDt) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.holdAmt = holdAmt;
            this.holdResult = holdResult;
            this.merchantCode = merchantCode;
            this.currencyCd = currencyCd;
            this.holdExpDt = holdExpDt;
        }
    }

    private static final class WalletRow {
        final String walletId;
        final String userId;
        final String walletStatus;
        final String walletTier;
        final String userNameKana;

        WalletRow(String walletId, String userId, String walletStatus, String walletTier, String userNameKana) {
            this.walletId = walletId;
            this.userId = userId;
            this.walletStatus = walletStatus;
            this.walletTier = walletTier;
            this.userNameKana = userNameKana;
        }
    }

    private static final class NoticeRow {
        final String noticeId;
        final String walletId;
        final String noticeKbn;
        final String noticeText;
        final String sendStatus;
        final String createTs;

        NoticeRow(String noticeId, String walletId, String noticeKbn, String noticeText, String sendStatus,
                  String createTs) {
            this.noticeId = noticeId;
            this.walletId = walletId;
            this.noticeKbn = noticeKbn;
            this.noticeText = noticeText;
            this.sendStatus = sendStatus;
            this.createTs = createTs;
        }
    }

    private static final class WalletSummary {
        final String walletId;
        final WalletRow wallet;
        int count;
        java.math.BigDecimal amount = java.math.BigDecimal.ZERO;
        java.time.LocalDate oldestExpDt;

        WalletSummary(String walletId, WalletRow wallet) {
            this.walletId = walletId;
            this.wallet = wallet;
        }

        void add(HoldRow row) {
            count++;
            amount = amount.add(row.holdAmt);
            if (oldestExpDt == null || row.holdExpDt.isBefore(oldestExpDt)) {
                oldestExpDt = row.holdExpDt;
            }
        }
    }
}
