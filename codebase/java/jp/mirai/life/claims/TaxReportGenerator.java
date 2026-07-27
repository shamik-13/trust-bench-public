package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数    年月日      担当            概要
 * 1.00    2024/03/15  保険金システムG  支払調書生成サービス初版
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
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class TaxReportGenerator {
    private static final Charset 入出力文字コード = StandardCharsets.UTF_8;
    private static final DateTimeFormatter 日付形式 = DateTimeFormatter.BASIC_ISO_DATE;
    private static final String 税務署提出用 = "11";
    private static final String 受取人交付用 = "12";

    public int generate(int 税年, Path 支払調書入力, Path 受取人入力, Path 帳票出力) throws IOException {
        Map<String, 受取人> 受取人索引 = 受取人読込(受取人入力);
        Map<String, 集計> 集計索引 = new LinkedHashMap<>();

        for (支払支給 明細 : 支払支給読込(支払調書入力)) {
            if (明細.税年 != 税年) {
                continue;
            }
            if (明細.受取人番号.isEmpty()) {
                throw new IllegalArgumentException("受取人番号未設定 REPORT-ID=" + 明細.帳票番号);
            }
            if (!受取人索引.containsKey(明細.受取人番号)) {
                throw new IllegalArgumentException("受取人未登録 BENEFICIARY-ID=" + 明細.受取人番号);
            }

            BigDecimal 源泉税額 = "9".equals(明細.税免除区分)
                    ? 源泉税再計算(明細.課税額)
                    : 明細.税額;

            集計 集計値 = 集計索引.computeIfAbsent(明細.受取人番号,
                    k -> new 集計(明細.受取人番号, 受取人索引.get(k)));
            集計値.帳票番号 = 最小帳票番号(集計値.帳票番号, 明細.帳票番号);
            集計値.課税額 = 集計値.課税額.add(明細.課税額);
            集計値.税額 = 集計値.税額.add(源泉税額);
            集計値.支払件数++;
        }

        List<帳票行> 出力行 = 帳票編集(税年, 集計索引);
        帳票書込(帳票出力, 出力行);
        return 出力行.size();
    }

    private static Map<String, 受取人> 受取人読込(Path 入力) throws IOException {
        Map<String, 受取人> 索引 = new HashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(入力, 入出力文字コード)) {
            String 行;
            boolean 初行 = true;
            while ((行 = reader.readLine()) != null) {
                if (行.trim().isEmpty()) {
                    continue;
                }
                if (初行 && 行.toUpperCase(Locale.ROOT).contains("BENEFICIARY-ID")) {
                    初行 = false;
                    continue;
                }
                初行 = false;

                List<String> 列 = csv分割(行);
                if (列.size() < 8) {
                    throw new IllegalArgumentException("LFBENF項目不足 行=" + 行);
                }

                受取人 値 = new 受取人(
                        必須(列.get(0), "POL-NO"),
                        必須(列.get(1), "BENEFICIARY-ID"),
                        必須(列.get(2), "NAME-KANA"),
                        必須(列.get(3), "RELATIONSHIP-KBN"),
                        必須(列.get(4), "BANK-CD"),
                        必須(列.get(5), "BRANCH-CD"),
                        必須(列.get(6), "ACCT-NO"),
                        数値(列.get(7), "PAYMENT-PRIORITY")
                );

                受取人 重複 = 索引.putIfAbsent(値.受取人番号, 値);
                if (重複 != null && 値.支払優先順位 < 重複.支払優先順位) {
                    索引.put(値.受取人番号, 値);
                }
            }
        }
        return 索引;
    }

    private static List<支払支給> 支払支給読込(Path 入力) throws IOException {
        List<支払支給> 一覧 = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(入力, 入出力文字コード)) {
            String 行;
            boolean 初行 = true;
            while ((行 = reader.readLine()) != null) {
                if (行.trim().isEmpty()) {
                    continue;
                }
                if (初行 && 行.toUpperCase(Locale.ROOT).contains("REPORT-ID")) {
                    初行 = false;
                    continue;
                }
                初行 = false;

                List<String> 列 = csv分割(行);
                if (列.size() < 7) {
                    throw new IllegalArgumentException("LFWITF項目不足 行=" + 行);
                }

                BigDecimal 課税額 = 金額(列.get(3), "TAXABLE-AMT");
                BigDecimal 税額 = 金額(列.get(4), "TAX-AMT");
                if (課税額.signum() < 0 || 税額.signum() < 0) {
                    throw new IllegalArgumentException("金額負値 REPORT-ID=" + 列.get(0));
                }

                一覧.add(new 支払支給(
                        必須(列.get(0), "REPORT-ID"),
                        必須(列.get(1), "PAY-ID"),
                        必須(列.get(2), "BENEFICIARY-ID"),
                        課税額,
                        税額,
                        数値(列.get(5), "TAX-YEAR"),
                        必須(列.get(6), "TAX-EXEMPT-FLG")
                ));
            }
        }
        return 一覧;
    }

    private static List<帳票行> 帳票編集(int 税年, Map<String, 集計> 集計索引) {
        List<集計> 集計一覧 = new ArrayList<>(集計索引.values());
        集計一覧.sort(Comparator
                .comparing((集計 v) -> v.受取人.受取人番号)
                .thenComparing(v -> v.受取人.証券番号));

        List<帳票行> 出力 = new ArrayList<>();
        String 出力日 = LocalDate.now().format(日付形式);
        int 頁 = 1;

        for (集計 値 : 集計一覧) {
            if (値.支払件数 <= 0) {
                continue;
            }
            String 帳票番号 = 値.帳票番号 == null ? "REP-" + 値.受取人番号 : 値.帳票番号;

            出力.add(new 帳票行(帳票番号, 税務署提出用, 出力日, 頁++,
                    eTaxXml(税年, 値)));
            出力.add(new 帳票行(帳票番号, 受取人交付用, 出力日, 頁++,
                    a4固定帳票(税年, 値)));
        }

        return 出力;
    }

    private static void 帳票書込(Path 出力, List<帳票行> 行一覧) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(出力, 入出力文字コード)) {
            writer.write("REPORT-ID,REPORT-TYPE-KBN,OUTPUT-DT,PAGE-NO,LINE-DATA");
            writer.newLine();
            for (帳票行 行 : 行一覧) {
                writer.write(csv連結(
                        行.帳票番号,
                        行.帳票種別,
                        行.出力日,
                        Integer.toString(行.頁番号),
                        行.行データ
                ));
                writer.newLine();
            }
        }
    }

    private static BigDecimal 源泉税再計算(BigDecimal 課税額) {
        return 課税額.multiply(new BigDecimal("0.1021")).setScale(0, RoundingMode.DOWN);
    }

    private static String eTaxXml(int 税年, 集計 値) {
        return "<PaymentRecord>"
                + "<TaxYear>" + 税年 + "</TaxYear>"
                + "<BeneficiaryId>" + xml退避(値.受取人番号) + "</BeneficiaryId>"
                + "<NameKana>" + xml退避(値.受取人.氏名カナ) + "</NameKana>"
                + "<RelationshipKbn>" + xml退避(値.受取人.続柄区分) + "</RelationshipKbn>"
                + "<TaxableAmount>" + 整数金額(値.課税額) + "</TaxableAmount>"
                + "<WithholdingTax>" + 整数金額(値.税額) + "</WithholdingTax>"
                + "<PaymentCount>" + 値.支払件数 + "</PaymentCount>"
                + "</PaymentRecord>";
    }

    private static String a4固定帳票(int 税年, 集計 値) {
        return 固定("支払調書", 10)
                + 固定(Integer.toString(税年), 6)
                + 固定(値.受取人.受取人番号, 16)
                + 固定(値.受取人.氏名カナ, 24)
                + 固定(値.受取人.証券番号, 14)
                + 固定(整数金額(値.課税額), 14)
                + 固定(整数金額(値.税額), 12)
                + 固定(値.受取人.銀行コード + "-" + 値.受取人.支店コード + "-" + 値.受取人.口座番号, 24);
    }

    private static String 固定(String 値, int 桁) {
        String s = 値 == null ? "" : 値;
        if (s.length() > 桁) {
            return s.substring(0, 桁);
        }
        StringBuilder b = new StringBuilder(s);
        while (b.length() < 桁) {
            b.append(' ');
        }
        return b.toString();
    }

    private static String 整数金額(BigDecimal 金額) {
        return 金額.setScale(0, RoundingMode.DOWN).toPlainString();
    }

    private static String 最小帳票番号(String 現在, String 候補) {
        if (現在 == null || 現在.compareTo(候補) > 0) {
            return 候補;
        }
        return 現在;
    }

    private static String 必須(String 値, String 名称) {
        String s = 値 == null ? "" : 値.trim();
        if (s.isEmpty()) {
            throw new IllegalArgumentException("必須項目未設定 " + 名称);
        }
        return s;
    }

    private static int 数値(String 値, String 名称) {
        try {
            return Integer.parseInt(必須(値, 名称));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値形式不正 " + 名称 + "=" + 値);
        }
    }

    private static BigDecimal 金額(String 値, String 名称) {
        try {
            return new BigDecimal(必須(値, 名称).replace(",", ""));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("金額形式不正 " + 名称 + "=" + 値);
        }
    }

    private static List<String> csv分割(String 行) {
        List<String> 結果 = new ArrayList<>();
        StringBuilder 現在 = new StringBuilder();
        boolean 引用中 = false;

        for (int i = 0; i < 行.length(); i++) {
            char c = 行.charAt(i);
            if (c == '"') {
                if (引用中 && i + 1 < 行.length() && 行.charAt(i + 1) == '"') {
                    現在.append('"');
                    i++;
                } else {
                    引用中 = !引用中;
                }
            } else if (c == ',' && !引用中) {
                結果.add(現在.toString());
                現在.setLength(0);
            } else {
                現在.append(c);
            }
        }

        if (引用中) {
            throw new IllegalArgumentException("CSV引用符不整合 行=" + 行);
        }

        結果.add(現在.toString());
        return 結果;
    }

    private static String csv連結(String... 値一覧) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < 値一覧.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            b.append(csv退避(値一覧[i]));
        }
        return b.toString();
    }

    private static String csv退避(String 値) {
        String s = 値 == null ? "" : 値;
        if (s.indexOf(',') >= 0 || s.indexOf('"') >= 0 || s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0) {
            return "\"" + s.replace("\"", "\"\"") + "\"";
        }
        return s;
    }

    private static String xml退避(String 値) {
        String s = 値 == null ? "" : 値;
        return s.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&apos;");
    }

    private static final class 支払支給 {
        final String 帳票番号;
        final String 支払番号;
        final String 受取人番号;
        final BigDecimal 課税額;
        final BigDecimal 税額;
        final int 税年;
        final String 税免除区分;

        支払支給(String 帳票番号, String 支払番号, String 受取人番号, BigDecimal 課税額,
             BigDecimal 税額, int 税年, String 税免除区分) {
            this.帳票番号 = 帳票番号;
            this.支払番号 = 支払番号;
            this.受取人番号 = 受取人番号;
            this.課税額 = 課税額;
            this.税額 = 税額;
            this.税年 = 税年;
            this.税免除区分 = 税免除区分;
        }
    }

    private static final class 受取人 {
        final String 証券番号;
        final String 受取人番号;
        final String 氏名カナ;
        final String 続柄区分;
        final String 銀行コード;
        final String 支店コード;
        final String 口座番号;
        final int 支払優先順位;

        受取人(String 証券番号, String 受取人番号, String 氏名カナ, String 続柄区分,
            String 銀行コード, String 支店コード, String 口座番号, int 支払優先順位) {
            this.証券番号 = 証券番号;
            this.受取人番号 = 受取人番号;
            this.氏名カナ = 氏名カナ;
            this.続柄区分 = 続柄区分;
            this.銀行コード = 銀行コード;
            this.支店コード = 支店コード;
            this.口座番号 = 口座番号;
            this.支払優先順位 = 支払優先順位;
        }
    }

    private static final class 集計 {
        final String 受取人番号;
        final 受取人 受取人;
        String 帳票番号;
        BigDecimal 課税額 = BigDecimal.ZERO;
        BigDecimal 税額 = BigDecimal.ZERO;
        int 支払件数;

        集計(String 受取人番号, 受取人 受取人) {
            this.受取人番号 = 受取人番号;
            this.受取人 = 受取人;
        }
    }

    private static final class 帳票行 {
        final String 帳票番号;
        final String 帳票種別;
        final String 出力日;
        final int 頁番号;
        final String 行データ;

        帳票行(String 帳票番号, String 帳票種別, String 出力日, int 頁番号, String 行データ) {
            this.帳票番号 = 帳票番号;
            this.帳票種別 = 帳票種別;
            this.出力日 = 出力日;
            this.頁番号 = 頁番号;
            this.行データ = 行データ;
        }
    }
}
