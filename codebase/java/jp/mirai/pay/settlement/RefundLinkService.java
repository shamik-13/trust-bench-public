package jp.mirai.pay.settlement;

/**
 * 返金連携サービス。取消（返金）連携データを元売上と突合し、承認可否を判定して
 * 返金取引をPSTXNFに追記、連携状態をPJCANFに反映する。
 *
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/05/13  加盟店精算チーム  返金連携データ突合バッチ新規作成
 * 1.01    2024/11/26  加盟店精算チーム  返金済額の累計判定を追加
 */

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
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class RefundLinkService {
    private static final Charset 入出力文字コード = StandardCharsets.UTF_8;
    private static final DateTimeFormatter 日付形式 = DateTimeFormatter.BASIC_ISO_DATE;
    private static final String 取引区分売上 = "C";
    private static final String 取引区分返金 = "R";
    private static final String 加盟店精算対象 = "01";
    private static final String 連携未処理 = "0";
    private static final String 連携承認 = "1";
    private static final String 連携拒否 = "9";
    private static final BigDecimal 手数料率 = new BigDecimal("0.0030");

    public static void main(String[] a) {
        if (a.length != 4 && a.length != 5) {
            System.err.println("使用方法: java jp.mirai.pay.settlement.RefundLinkService PJCANF PSTXNF PSMERF 出力ディレクトリ [締日]");
            System.exit(2);
        }

        try {
            Path 取消入力 = Paths.get(a[0]);
            Path 取引入力 = Paths.get(a[1]);
            Path 加盟店入力 = Paths.get(a[2]);
            Path 出力ディレクトリ = Paths.get(a[3]);
            LocalDate 締日 = a.length == 5 ? 日付を読む(a[4], "締日") : LocalDate.now().minusDays(1);

            実行結果 結果 = execute(取消入力, 取引入力, 加盟店入力, 出力ディレクトリ, 締日);
            System.out.println("返金連携処理が終了しました。読込=" + 結果.読込件数
                    + " 承認=" + 結果.承認件数
                    + " 拒否=" + 結果.拒否件数
                    + " 返金額=" + 結果.承認返金額
                    + " 手数料控除=" + 結果.手数料控除額);
        } catch (Exception e) {
            System.err.println("返金連携処理で異常終了しました: " + e.getMessage());
            System.exit(1);
        }
    }

    public static 実行結果 execute(Path 取消入力, Path 取引入力, Path 加盟店入力, Path 出力ディレクトリ, LocalDate 締日) throws IOException {
        Files.createDirectories(出力ディレクトリ);

        Map<String, 加盟店> 加盟店索引 = 加盟店を読む(加盟店入力);
        List<取引> 既存取引 = 取引を読む(取引入力);
        Map<String, 取引> 売上索引 = 売上索引を作る(既存取引);
        Map<String, BigDecimal> 返金済額索引 = 返金済額索引を作る(既存取引);
        List<取消連携> 取消一覧 = 取消を読む(取消入力);

        List<取引> 追記返金 = new ArrayList<>();
        List<取消連携> 更新取消 = new ArrayList<>();
        int 承認件数 = 0;
        int 拒否件数 = 0;
        BigDecimal 承認返金額 = BigDecimal.ZERO;
        BigDecimal 手数料控除額 = BigDecimal.ZERO;

        for (取消連携 取消 : 取消一覧) {
            if (!連携未処理.equals(取消.連携状態)) {
                更新取消.add(取消);
                continue;
            }

            判定 判定結果 = 判定する(取消, 売上索引, 返金済額索引, 加盟店索引, 締日);
            if (判定結果.承認) {
                取引 返金取引 = new 取引(取消.取引ID, 取消.加盟店コード, 取引区分返金, 取消.返金額.negate(), 取消.返金日);
                追記返金.add(返金取引);
                返金済額索引.merge(取消.取引ID, 取消.返金額, BigDecimal::add);
                更新取消.add(取消.状態変更(連携承認));
                承認件数++;
                承認返金額 = 承認返金額.add(取消.返金額);
                手数料控除額 = 手数料控除額.add(取消.返金額.multiply(手数料率).setScale(0, RoundingMode.DOWN));
            } else {
                更新取消.add(取消.状態変更(連携拒否));
                拒否件数++;
                System.err.println("返金拒否 CANCEL-ID=" + 取消.取消ID + " 理由=" + 判定結果.理由);
            }
        }

        Path 取引出力 = 出力ディレクトリ.resolve("PSTXNF.csv");
        Path 取消出力 = 出力ディレクトリ.resolve("PJCANF.dat");
        取引を書く(取引出力, 既存取引, 追記返金);
        取消を書く(取消出力, 更新取消);

        return new 実行結果(取消一覧.size(), 承認件数, 拒否件数, 承認返金額, 手数料控除額);
    }

    private static 判定 判定する(取消連携 取消, Map<String, 取引> 売上索引, Map<String, BigDecimal> 返金済額索引,
                           Map<String, 加盟店> 加盟店索引, LocalDate 締日) {
        if (取消.返金額.signum() <= 0) {
            return 判定.拒否("返金額不正");
        }

        取引 元売上 = 売上索引.get(取消.取引ID);
        if (元売上 == null) {
            return 判定.拒否("元売上なし");
        }

        if (!元売上.加盟店コード.equals(取消.加盟店コード)) {
            return 判定.拒否("加盟店不一致");
        }

        加盟店 加盟店 = 加盟店索引.get(取消.加盟店コード);
        if (加盟店 == null) {
            return 判定.拒否("加盟店台帳なし");
        }

        if (!加盟店精算対象.equals(加盟店.状態)) {
            return 判定.拒否("停止加盟店");
        }

        if (!取消.返金日.isAfter(締日)) {
            return 判定.拒否("締済日付");
        }

        BigDecimal 返金済額 = 返金済額索引.getOrDefault(取消.取引ID, BigDecimal.ZERO);
        if (返金済額.add(取消.返金額).compareTo(元売上.金額) > 0) {
            return 判定.拒否("元取引超過");
        }

        return 判定.承認();
    }

    private static Map<String, 加盟店> 加盟店を読む(Path path) throws IOException {
        Map<String, 加盟店> map = new HashMap<>();
        try (BufferedReader br = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (空行または見出し(line, "MERCHANT-CODE")) {
                    continue;
                }
                List<String> c = csv分解(line);
                if (c.size() != 4) {
                    throw new IOException("PSMERF 項目数不正: " + line);
                }
                map.put(c.get(0), new 加盟店(c.get(0), c.get(1), c.get(2), c.get(3)));
            }
        }
        return map;
    }

    private static List<取引> 取引を読む(Path path) throws IOException {
        List<取引> list = new ArrayList<>();
        try (BufferedReader br = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (空行または見出し(line, "TXN-ID")) {
                    continue;
                }
                List<String> c = csv分解(line);
                if (c.size() != 5) {
                    throw new IOException("PSTXNF 項目数不正: " + line);
                }
                list.add(new 取引(c.get(0), c.get(1), c.get(2), 金額を読む(c.get(3), "TXN-AMT"), 日付を読む(c.get(4), "TXN-DT")));
            }
        }
        return list;
    }

    private static List<取消連携> 取消を読む(Path path) throws IOException {
        List<取消連携> list = new ArrayList<>();
        try (BufferedReader br = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (空行または見出し(line, "CANCEL-ID")) {
                    continue;
                }
                List<String> c = csv分解(line);
                if (c.size() != 6) {
                    throw new IOException("PJCANF 項目数不正: " + line);
                }
                list.add(new 取消連携(c.get(0), c.get(1), c.get(2), 金額を読む(c.get(3), "REFUND-AMT"), 日付を読む(c.get(4), "REFUND-DT"), c.get(5)));
            }
        }
        return list;
    }

    private static Map<String, 取引> 売上索引を作る(List<取引> 取引一覧) throws IOException {
        Map<String, 取引> map = new LinkedHashMap<>();
        for (取引 t : 取引一覧) {
            if (取引区分売上.equals(t.取引区分)) {
                取引 重複 = map.putIfAbsent(t.取引ID, t);
                if (重複 != null) {
                    throw new IOException("PSTXNF 元売上重複 TXN-ID=" + t.取引ID);
                }
            }
        }
        return map;
    }

    private static Map<String, BigDecimal> 返金済額索引を作る(List<取引> 取引一覧) {
        Map<String, BigDecimal> map = new HashMap<>();
        for (取引 t : 取引一覧) {
            if (取引区分返金.equals(t.取引区分)) {
                map.merge(t.取引ID, t.金額.abs(), BigDecimal::add);
            }
        }
        return map;
    }

    private static void 取引を書く(Path path, List<取引> 既存取引, List<取引> 追記返金) throws IOException {
        try (BufferedWriter bw = Files.newBufferedWriter(path, 入出力文字コード)) {
            bw.write("TXN-ID,MERCHANT-CODE,TXN-KBN,TXN-AMT,TXN-DT");
            bw.newLine();
            for (取引 t : 既存取引) {
                bw.write(csv結合(t.取引ID, t.加盟店コード, t.取引区分, t.金額.toPlainString(), t.取引日.format(日付形式)));
                bw.newLine();
            }
            for (取引 t : 追記返金) {
                bw.write(csv結合(t.取引ID, t.加盟店コード, t.取引区分, t.金額.toPlainString(), t.取引日.format(日付形式)));
                bw.newLine();
            }
        }
    }

    private static void 取消を書く(Path path, List<取消連携> 取消一覧) throws IOException {
        try (BufferedWriter bw = Files.newBufferedWriter(path, 入出力文字コード)) {
            bw.write("CANCEL-ID,TXN-ID,MERCHANT-CODE,REFUND-AMT,REFUND-DT,LINK-STATUS");
            bw.newLine();
            for (取消連携 c : 取消一覧) {
                bw.write(csv結合(c.取消ID, c.取引ID, c.加盟店コード, c.返金額.toPlainString(), c.返金日.format(日付形式), c.連携状態));
                bw.newLine();
            }
        }
    }

    private static boolean 空行または見出し(String line, String 見出し先頭) {
        String s = line.trim();
        return s.isEmpty() || s.startsWith(見出し先頭 + ",");
    }

    private static BigDecimal 金額を読む(String s, String 名称) throws IOException {
        try {
            return new BigDecimal(s.trim()).setScale(0, RoundingMode.UNNECESSARY);
        } catch (ArithmeticException | NumberFormatException e) {
            throw new IOException(名称 + " 金額不正: " + s, e);
        }
    }

    private static LocalDate 日付を読む(String s, String 名称) throws IOException {
        try {
            return LocalDate.parse(s.trim(), 日付形式);
        } catch (DateTimeException e) {
            throw new IOException(名称 + " 日付不正: " + s, e);
        }
    }

    private static List<String> csv分解(String line) throws IOException {
        List<String> out = new ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (quoted) {
                if (ch == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        cur.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    cur.append(ch);
                }
            } else if (ch == ',') {
                out.add(cur.toString().trim());
                cur.setLength(0);
            } else if (ch == '"' && cur.length() == 0) {
                quoted = true;
            } else {
                cur.append(ch);
            }
        }
        if (quoted) {
            throw new IOException("CSV引用符未終端: " + line);
        }
        out.add(cur.toString().trim());
        return out;
    }

    private static String csv結合(String... values) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                sb.append(',');
            }
            String v = values[i] == null ? "" : values[i];
            if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
                sb.append('"');
                for (int j = 0; j < v.length(); j++) {
                    char ch = v.charAt(j);
                    if (ch == '"') {
                        sb.append("\"\"");
                    } else {
                        sb.append(ch);
                    }
                }
                sb.append('"');
            } else {
                sb.append(v);
            }
        }
        return sb.toString();
    }

    public static final class 実行結果 {
        public final int 読込件数;
        public final int 承認件数;
        public final int 拒否件数;
        public final BigDecimal 承認返金額;
        public final BigDecimal 手数料控除額;

        private 実行結果(int 読込件数, int 承認件数, int 拒否件数, BigDecimal 承認返金額, BigDecimal 手数料控除額) {
            this.読込件数 = 読込件数;
            this.承認件数 = 承認件数;
            this.拒否件数 = 拒否件数;
            this.承認返金額 = 承認返金額;
            this.手数料控除額 = 手数料控除額;
        }
    }

    private static final class 取消連携 {
        private final String 取消ID;
        private final String 取引ID;
        private final String 加盟店コード;
        private final BigDecimal 返金額;
        private final LocalDate 返金日;
        private final String 連携状態;

        private 取消連携(String 取消ID, String 取引ID, String 加盟店コード, BigDecimal 返金額, LocalDate 返金日, String 連携状態) {
            this.取消ID = 取消ID;
            this.取引ID = 取引ID;
            this.加盟店コード = 加盟店コード;
            this.返金額 = 返金額;
            this.返金日 = 返金日;
            this.連携状態 = 連携状態;
        }

        private 取消連携 状態変更(String 新状態) {
            return new 取消連携(取消ID, 取引ID, 加盟店コード, 返金額, 返金日, 新状態);
        }
    }

    private static final class 取引 {
        private final String 取引ID;
        private final String 加盟店コード;
        private final String 取引区分;
        private final BigDecimal 金額;
        private final LocalDate 取引日;

        private 取引(String 取引ID, String 加盟店コード, String 取引区分, BigDecimal 金額, LocalDate 取引日) {
            this.取引ID = 取引ID;
            this.加盟店コード = 加盟店コード;
            this.取引区分 = 取引区分;
            this.金額 = 金額;
            this.取引日 = 取引日;
        }
    }

    private static final class 加盟店 {
        private final String 加盟店コード;
        private final String 加盟店名;
        private final String 状態;
        private final String 銀行口座番号;

        private 加盟店(String 加盟店コード, String 加盟店名, String 状態, String 銀行口座番号) {
            this.加盟店コード = 加盟店コード;
            this.加盟店名 = 加盟店名;
            this.状態 = 状態;
            this.銀行口座番号 = 銀行口座番号;
        }
    }

    private static final class 判定 {
        private final boolean 承認;
        private final String 理由;

        private 判定(boolean 承認, String 理由) {
            this.承認 = 承認;
            this.理由 = 理由;
        }

        private static 判定 承認() {
            return new 判定(true, "");
        }

        private static 判定 拒否(String 理由) {
            return new 判定(false, 理由);
        }
    }
}
