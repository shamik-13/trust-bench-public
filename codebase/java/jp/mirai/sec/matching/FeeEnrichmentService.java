package jp.mirai.sec.matching;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * 変更履歴
 * 版数 / 年月日 / 担当 / 概要
 * 1.00 / 2025-01-15 / 市場基盤部 / 初版作成
 */
public class FeeEnrichmentService {
    private static final Charset 入出力文字コード = StandardCharsets.UTF_8;
    private static final DateTimeFormatter 日時形式 = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final int 判定_ACCEPT = 0;
    private static final int 判定_REJECT_MARGIN = 4;
    private static final int 判定_REJECT_NOTIONAL = 8;
    private static final int 判定_REJECT_TICK = 12;

    private FeeEnrichmentService() {
    }

    public static void main(String[] a) throws Exception {
        Path 手数料ファイル = Paths.get(a.length > 0 ? a[0] : "SCFEEF.csv");
        Path 銘柄ファイル = Paths.get(a.length > 1 ? a[1] : "SCINSTF.csv");
        Path 約定ファイル = Paths.get(a.length > 2 ? a[2] : "SCEXEC.csv");
        Path 出力ファイル = Paths.get(a.length > 3 ? a[3] : "SCTCAP.csv");

        Map<String, 手数料条件> 手数料条件表 = 手数料条件を読む(手数料ファイル);
        Map<String, 銘柄> 銘柄表 = 銘柄を読む(銘柄ファイル);
        処理結果 結果 = 約定を捕捉する(約定ファイル, 出力ファイル, 手数料条件表, 銘柄表);

        System.out.println("処理件数=" + 結果.処理件数
                + ",出力件数=" + 結果.出力件数
                + ",手数料合計=" + 結果.手数料合計
                + ",除外件数=" + 結果.除外件数);
    }

    private static Map<String, 手数料条件> 手数料条件を読む(Path file) throws IOException {
        Map<String, 手数料条件> map = new HashMap<String, 手数料条件>();
        try (BufferedReader br = Files.newBufferedReader(file, 入出力文字コード)) {
            String line;
            int 行番号 = 0;
            while ((line = br.readLine()) != null) {
                行番号++;
                if (空行または見出し(line, "BOARD-CODE")) {
                    continue;
                }
                List<String> c = csv分割(line);
                if (c.size() != 3) {
                    throw new IllegalArgumentException("SCFEEF 項目数不正 行=" + 行番号);
                }
                String board = 必須(c.get(0), "BOARD-CODE", 行番号);
                board確認(board, 行番号);
                BigDecimal rate = decimal(c.get(1), "FEE-RATE", 行番号);
                long minFee = long値(c.get(2), "MIN-FEE-AMT", 行番号);
                if (rate.signum() < 0 || minFee < 0) {
                    throw new IllegalArgumentException("SCFEEF 手数料条件不正 行=" + 行番号);
                }
                if (map.put(board, new 手数料条件(board, rate, minFee)) != null) {
                    throw new IllegalArgumentException("SCFEEF BOARD-CODE 重複 行=" + 行番号);
                }
            }
        }
        return map;
    }

    private static Map<String, 銘柄> 銘柄を読む(Path file) throws IOException {
        Map<String, 銘柄> map = new HashMap<String, 銘柄>();
        try (BufferedReader br = Files.newBufferedReader(file, 入出力文字コード)) {
            String line;
            int 行番号 = 0;
            while ((line = br.readLine()) != null) {
                行番号++;
                if (空行または見出し(line, "INSTR-CODE")) {
                    continue;
                }
                List<String> c = csv分割(line);
                if (c.size() != 6) {
                    throw new IllegalArgumentException("SCINSTF 項目数不正 行=" + 行番号);
                }
                String code = 必須(c.get(0), "INSTR-CODE", 行番号);
                String name = 必須(c.get(1), "INSTR-NAME", 行番号);
                int tier = int値(c.get(2), "INSTR-TIER", 行番号);
                long tick = long値(c.get(3), "TICK-AMT", 行番号);
                long lot = long値(c.get(4), "LOT-QTY", 行番号);
                String board = 必須(c.get(5), "BOARD-CODE", 行番号);
                board確認(board, 行番号);
                tier確認(tier, tick, 行番号);
                if (lot <= 0) {
                    throw new IllegalArgumentException("SCINSTF LOT-QTY 不正 行=" + 行番号);
                }
                if (map.put(code, new 銘柄(code, name, tier, tick, lot, board)) != null) {
                    throw new IllegalArgumentException("SCINSTF INSTR-CODE 重複 行=" + 行番号);
                }
            }
        }
        return map;
    }

    private static 処理結果 約定を捕捉する(
            Path input,
            Path output,
            Map<String, 手数料条件> feeMap,
            Map<String, 銘柄> instMap) throws IOException {
        int 処理件数 = 0;
        int 出力件数 = 0;
        int 除外件数 = 0;
        long 手数料合計 = 0L;
        Set<String> execIds = new HashSet<String>();

        try (BufferedReader br = Files.newBufferedReader(input, 入出力文字コード);
             BufferedWriter bw = Files.newBufferedWriter(output, 入出力文字コード)) {
            bw.write("TRADE-ID,EXEC-ID,ORDER-ID,INSTR-CODE,CIF-NO,TRADE-QTY,TRADE-AMT,CAPTURE-TS");
            bw.newLine();

            String line;
            int 行番号 = 0;
            while ((line = br.readLine()) != null) {
                行番号++;
                if (空行または見出し(line, "EXEC-ID")) {
                    continue;
                }
                処理件数++;
                List<String> c = csv分割(line);
                if (c.size() != 7) {
                    throw new IllegalArgumentException("SCEXEC 項目数不正 行=" + 行番号);
                }

                約定 exec = 約定を作る(c, 行番号);
                if (!execIds.add(exec.execId)) {
                    throw new IllegalArgumentException("SCEXEC EXEC-ID 重複 行=" + 行番号);
                }

                銘柄 inst = instMap.get(exec.instrCode);
                if (inst == null) {
                    除外件数++;
                    System.out.println("除外:銘柄未登録 EXEC-ID=" + exec.execId);
                    continue;
                }
                手数料条件 fee = feeMap.get(inst.boardCode);
                if (fee == null) {
                    除外件数++;
                    System.out.println("除外:手数料未登録 EXEC-ID=" + exec.execId);
                    continue;
                }

                int decision = 判定(exec, inst);
                if (decision != 判定_ACCEPT) {
                    除外件数++;
                    System.out.println("除外:判定=" + decision + ",EXEC-ID=" + exec.execId);
                    continue;
                }

                long feeAmt = 手数料計算(exec.fillAmt, fee);
                手数料合計 = Math.addExact(手数料合計, feeAmt);

                String tradeId = "TRD" + zero埋め(出力件数 + 1, 10);
                String cifNo = cif推定(exec.orderId);
                bw.write(csv結合(tradeId, exec.execId, exec.orderId, exec.instrCode, cifNo,
                        String.valueOf(exec.fillQty), String.valueOf(exec.fillAmt), LocalDateTime.now().format(日時形式)));
                bw.newLine();
                出力件数++;
            }
        }

        return new 処理結果(処理件数, 出力件数, 除外件数, 手数料合計);
    }

    private static 約定 約定を作る(List<String> c, int 行番号) {
        String execId = 必須(c.get(0), "EXEC-ID", 行番号);
        String orderId = 必須(c.get(1), "ORDER-ID", 行番号);
        String instrCode = 必須(c.get(2), "INSTR-CODE", 行番号);
        String side = 必須(c.get(3), "SIDE-KBN", 行番号);
        long fillQty = long値(c.get(4), "FILL-QTY", 行番号);
        long fillAmt = long値(c.get(5), "FILL-AMT", 行番号);
        String ts = 必須(c.get(6), "EXEC-TS", 行番号);

        if (!"B".equals(side) && !"S".equals(side)) {
            throw new IllegalArgumentException("SCEXEC SIDE-KBN 不正 行=" + 行番号);
        }
        if (fillQty <= 0 || fillAmt <= 0) {
            throw new IllegalArgumentException("SCEXEC 約定数量金額不正 行=" + 行番号);
        }
        try {
            LocalDateTime.parse(ts, 日時形式);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException("SCEXEC EXEC-TS 不正 行=" + 行番号);
        }
        return new 約定(execId, orderId, instrCode, side, fillQty, fillAmt, ts);
    }

    private static int 判定(約定 exec, 銘柄 inst) {
        if (exec.fillAmt > MIHFT_MAX_NOTIONAL) {
            return 判定_REJECT_NOTIONAL;
        }
        if (exec.fillAmt % inst.tickAmt != 0) {
            return 判定_REJECT_TICK;
        }
        long margin = Math.multiplyExact(exec.fillAmt, marginBp(inst.tier)) / 10000L;
        if ("B".equals(exec.sideKbn) && margin > MIHFT_MAX_NOTIONAL / 2L) {
            return 判定_REJECT_MARGIN;
        }
        return 判定_ACCEPT;
    }

    private static int marginBp(int tier) {
        if (tier == 1) {
            return 1000;
        }
        if (tier == 2) {
            return 2000;
        }
        if (tier == 3) {
            return 4000;
        }
        throw new IllegalArgumentException("INSTR-TIER 不正");
    }

    private static void tier確認(int tier, long tick, int 行番号) {
        if ((tier == 1 && tick == 100L) || (tier == 2 && tick == 500L) || (tier == 3 && tick == 1000L)) {
            return;
        }
        throw new IllegalArgumentException("SCINSTF INSTR-TIER/TICK-AMT 不整合 行=" + 行番号);
    }

    private static void board確認(String board, int 行番号) {
        if ("T1".equals(board) || "ST".equals(board) || "ETF".equals(board)) {
            return;
        }
        throw new IllegalArgumentException("BOARD-CODE 不正 行=" + 行番号);
    }

    private static long 手数料計算(long fillAmt, 手数料条件 fee) {
        long rated = BigDecimal.valueOf(fillAmt).multiply(fee.feeRate).setScale(0, RoundingMode.HALF_UP).longValueExact();
        return Math.max(rated, fee.minFeeAmt);
    }

    private static boolean 空行または見出し(String line, String firstHeader) {
        String t = line.trim();
        return t.isEmpty() || t.toUpperCase(Locale.ROOT).startsWith(firstHeader);
    }

    private static String 必須(String v, String name, int 行番号) {
        String s = v == null ? "" : v.trim();
        if (s.isEmpty()) {
            throw new IllegalArgumentException(name + " 未設定 行=" + 行番号);
        }
        return s;
    }

    private static int int値(String v, String name, int 行番号) {
        try {
            return Integer.parseInt(必須(v, name, 行番号));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " 数値不正 行=" + 行番号);
        }
    }

    private static long long値(String v, String name, int 行番号) {
        try {
            return Long.parseLong(必須(v, name, 行番号));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " 数値不正 行=" + 行番号);
        }
    }

    private static BigDecimal decimal(String v, String name, int 行番号) {
        try {
            return new BigDecimal(必須(v, name, 行番号));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + " 数値不正 行=" + 行番号);
        }
    }

    private static List<String> csv分割(String line) {
        List<String> out = new ArrayList<String>();
        StringBuilder b = new StringBuilder();
        boolean quote = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quote && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    quote = !quote;
                }
            } else if (ch == ',' && !quote) {
                out.add(b.toString());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        out.add(b.toString());
        return out;
    }

    private static String csv結合(String... values) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            b.append(csv逃がし(values[i]));
        }
        return b.toString();
    }

    private static String csv逃がし(String v) {
        if (v.indexOf(',') < 0 && v.indexOf('"') < 0 && v.indexOf('\n') < 0 && v.indexOf('\r') < 0) {
            return v;
        }
        return '"' + v.replace("\"", "\"\"") + '"';
    }

    private static String zero埋め(int value, int width) {
        String s = String.valueOf(value);
        StringBuilder b = new StringBuilder();
        for (int i = s.length(); i < width; i++) {
            b.append('0');
        }
        return b.append(s).toString();
    }

    private static String cif推定(String orderId) {
        int p = orderId.indexOf('-');
        String base = p > 0 ? orderId.substring(0, p) : orderId;
        if (base.length() >= 10) {
            return base.substring(0, 10);
        }
        return "0000000000".substring(base.length()) + base;
    }

    private static final class 手数料条件 {
        final String boardCode;
        final BigDecimal feeRate;
        final long minFeeAmt;

        手数料条件(String boardCode, BigDecimal feeRate, long minFeeAmt) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmt = minFeeAmt;
        }
    }

    private static final class 銘柄 {
        final String instrCode;
        final String instrName;
        final int tier;
        final long tickAmt;
        final long lotQty;
        final String boardCode;

        銘柄(String instrCode, String instrName, int tier, long tickAmt, long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.tier = tier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class 約定 {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final String execTs;

        約定(String execId, String orderId, String instrCode, String sideKbn, long fillQty, long fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class 処理結果 {
        final int 処理件数;
        final int 出力件数;
        final int 除外件数;
        final long 手数料合計;

        処理結果(int 処理件数, int 出力件数, int 除外件数, long 手数料合計) {
            this.処理件数 = 処理件数;
            this.出力件数 = 出力件数;
            this.除外件数 = 除外件数;
            this.手数料合計 = 手数料合計;
        }
    }
}
