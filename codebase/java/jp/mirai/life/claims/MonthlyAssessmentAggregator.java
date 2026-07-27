package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.0   2024-03-15  保険金システムG  月次査定集計サービス初版
 */
public class MonthlyAssessmentAggregator {
    private static final int 正常終了 = 0;
    private static final int 入力不正 = 8;
    private static final int 査定未検出 = 12;
    private static final int 入出力異常 = 16;

    private static final int 支払対象経過一年以上割合 = 100;

    private static final java.nio.charset.Charset 文字コード = java.nio.charset.StandardCharsets.UTF_8;
    private static final java.math.RoundingMode 丸め方式 = java.math.RoundingMode.HALF_UP;

    @SuppressWarnings("unused")
    private static final String 共有モデル確認 = "jp.mirai.life.claims.ClaimModel";

    private static int 実行(String[] a) {
        if (a == null || a.length != 4) {
            System.err.println("引数不正: 対象年月 LFPAYF LFRASF LFMSTF を指定してください。");
            return 入力不正;
        }

        String 対象年月 = a[0];
        if (!対象年月.matches("\\d{6}")) {
            System.err.println("対象年月不正: YYYYMM形式で指定してください。");
            return 入力不正;
        }

        java.nio.file.Path 支払ファイル = java.nio.file.Paths.get(a[1]);
        java.nio.file.Path 査定ファイル = java.nio.file.Paths.get(a[2]);
        java.nio.file.Path 集計ファイル = java.nio.file.Paths.get(a[3]);

        try {
            java.util.Map<String, 査定レコード> 査定索引 = 査定索引作成(査定ファイル);
            java.util.Map<String, 集計値> 集計 = new java.util.TreeMap<>();

            try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(支払ファイル, 文字コード)) {
                String 行;
                int 行番号 = 0;
                while ((行 = reader.readLine()) != null) {
                    行番号++;
                    if (空行(行) || 見出し行(行, "PAY-ID")) {
                        continue;
                    }

                    支払レコード 支払 = 支払レコード読込(行, 行番号);
                    if (!対象年月.equals(支払.payId.substring(0, 6))) {
                        continue;
                    }

                    査定レコード 査定 = 査定索引.get(支払.claimId);
                    if (査定 == null) {
                        System.err.println("査定未検出: CLAIM-ID=" + 支払.claimId + " PAY-ID=" + 支払.payId);
                        return 査定未検出;
                    }

                    集計.computeIfAbsent(査定.categoryKbn, k -> new 集計値()).加算(支払);
                }
            }

            集計書込(集計ファイル, 対象年月, 集計);
            return 正常終了;
        } catch (IllegalArgumentException e) {
            System.err.println("入力不正: " + e.getMessage());
            return 入力不正;
        } catch (java.io.IOException e) {
            System.err.println("入出力異常: " + e.getMessage());
            return 入出力異常;
        }
    }

    private static java.util.Map<String, 査定レコード> 査定索引作成(java.nio.file.Path 査定ファイル) throws java.io.IOException {
        java.util.Map<String, 査定レコード> 索引 = new java.util.HashMap<>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(査定ファイル, 文字コード)) {
            String 行;
            int 行番号 = 0;
            while ((行 = reader.readLine()) != null) {
                行番号++;
                if (空行(行) || 見出し行(行, "ASSESS-ID")) {
                    continue;
                }

                査定レコード 査定 = 査定レコード読込(行, 行番号);
                査定レコード 既存 = 索引.putIfAbsent(査定.claimId, 査定);
                if (既存 != null) {
                    throw new IllegalArgumentException("LFRASF CLAIM-ID重複 行=" + 行番号 + " CLAIM-ID=" + 査定.claimId);
                }
            }
        }
        return 索引;
    }

    private static void 集計書込(java.nio.file.Path 集計ファイル, String 対象年月, java.util.Map<String, 集計値> 集計) throws java.io.IOException {
        try (java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(集計ファイル, 文字コード)) {
            writer.write("YEAR-MONTH,CATEGORY-KBN,COUNT,TOTAL-GROSS-AMT,TOTAL-PAYOUT-AMT,AVG-REDUCTION-RATE");
            writer.newLine();

            for (java.util.Map.Entry<String, 集計値> entry : 集計.entrySet()) {
                集計値 値 = entry.getValue();
                writer.write(対象年月);
                writer.write(',');
                writer.write(csv(entry.getKey()));
                writer.write(',');
                writer.write(Long.toString(値.count));
                writer.write(',');
                writer.write(金額文字列(値.totalGrossAmt));
                writer.write(',');
                writer.write(金額文字列(値.totalPayoutAmt));
                writer.write(',');
                writer.write(率文字列(値.平均削減率()));
                writer.newLine();
            }
        }
    }

    private static 支払レコード 支払レコード読込(String 行, int 行番号) {
        java.util.List<String> f = csv分割(行);
        if (f.size() != 5) {
            throw new IllegalArgumentException("LFPAYF項目数不正 行=" + 行番号);
        }

        String payId = 必須(f.get(0), "PAY-ID", 行番号);
        if (payId.length() < 6 || !payId.substring(0, 6).matches("\\d{6}")) {
            throw new IllegalArgumentException("PAY-ID年月部不正 行=" + 行番号 + " PAY-ID=" + payId);
        }

        String claimId = 必須(f.get(1), "CLAIM-ID", 行番号);
        java.math.BigDecimal grossAmt = 非負数(f.get(2), "GROSS-AMT", 行番号);
        java.math.BigDecimal reductionRate = 非負数(f.get(3), "REDUCTION-RATE", 行番号);
        java.math.BigDecimal payoutAmt = 非負数(f.get(4), "PAYOUT-AMT", 行番号);

        if (reductionRate.compareTo(java.math.BigDecimal.valueOf(支払対象経過一年以上割合)) > 0) {
            throw new IllegalArgumentException("REDUCTION-RATE範囲不正 行=" + 行番号);
        }

        return new 支払レコード(payId, claimId, grossAmt, reductionRate, payoutAmt);
    }

    private static 査定レコード 査定レコード読込(String 行, int 行番号) {
        java.util.List<String> f = csv分割(行);
        if (f.size() != 7) {
            throw new IllegalArgumentException("LFRASF項目数不正 行=" + 行番号);
        }

        必須(f.get(0), "ASSESS-ID", 行番号);
        String claimId = 必須(f.get(1), "CLAIM-ID", 行番号);
        必須(f.get(2), "ASSESS-DT", 行番号);
        String categoryKbn = 必須(f.get(3), "CATEGORY-KBN", 行番号);
        必須(f.get(4), "AUTH-LEVEL-KBN", 行番号);
        必須(f.get(5), "RESULT-KBN", 行番号);
        必須(f.get(6), "ASSESSOR-ID", 行番号);

        return new 査定レコード(claimId, categoryKbn);
    }

    private static String 必須(String 値, String 名称, int 行番号) {
        String s = 値 == null ? "" : 値.trim();
        if (s.isEmpty()) {
            throw new IllegalArgumentException(名称 + "未設定 行=" + 行番号);
        }
        return s;
    }

    private static java.math.BigDecimal 非負数(String 値, String 名称, int 行番号) {
        try {
            java.math.BigDecimal n = new java.math.BigDecimal(必須(値, 名称, 行番号));
            if (n.compareTo(java.math.BigDecimal.ZERO) < 0) {
                throw new IllegalArgumentException(名称 + "負数不正 行=" + 行番号);
            }
            return n;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(名称 + "数値不正 行=" + 行番号);
        }
    }

    private static boolean 空行(String 行) {
        return 行 == null || 行.trim().isEmpty();
    }

    private static boolean 見出し行(String 行, String 先頭項目名) {
        java.util.List<String> f = csv分割(行);
        return !f.isEmpty() && 先頭項目名.equalsIgnoreCase(f.get(0).trim());
    }

    private static java.util.List<String> csv分割(String 行) {
        java.util.List<String> 結果 = new java.util.ArrayList<>();
        StringBuilder 項目 = new StringBuilder();
        boolean 引用中 = false;

        for (int i = 0; i < 行.length(); i++) {
            char c = 行.charAt(i);
            if (c == '"') {
                if (引用中 && i + 1 < 行.length() && 行.charAt(i + 1) == '"') {
                    項目.append('"');
                    i++;
                } else {
                    引用中 = !引用中;
                }
            } else if (c == ',' && !引用中) {
                結果.add(項目.toString());
                項目.setLength(0);
            } else {
                項目.append(c);
            }
        }

        if (引用中) {
            throw new IllegalArgumentException("CSV引用符不整合");
        }

        結果.add(項目.toString());
        return 結果;
    }

    private static String csv(String 値) {
        if (値.indexOf(',') < 0 && 値.indexOf('"') < 0 && 値.indexOf('\n') < 0 && 値.indexOf('\r') < 0) {
            return 値;
        }
        return '"' + 値.replace("\"", "\"\"") + '"';
    }

    private static String 金額文字列(java.math.BigDecimal 値) {
        return 値.setScale(0, 丸め方式).toPlainString();
    }

    private static String 率文字列(java.math.BigDecimal 値) {
        return 値.setScale(4, 丸め方式).stripTrailingZeros().toPlainString();
    }

    private static final class 支払レコード {
        private final String payId;
        private final String claimId;
        private final java.math.BigDecimal grossAmt;
        private final java.math.BigDecimal reductionRate;
        private final java.math.BigDecimal payoutAmt;

        private 支払レコード(String payId, String claimId, java.math.BigDecimal grossAmt,
                    java.math.BigDecimal reductionRate, java.math.BigDecimal payoutAmt) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
        }
    }

    private static final class 査定レコード {
        private final String claimId;
        private final String categoryKbn;

        private 査定レコード(String claimId, String categoryKbn) {
            this.claimId = claimId;
            this.categoryKbn = categoryKbn;
        }
    }

    private static final class 集計値 {
        private long count;
        private java.math.BigDecimal totalGrossAmt = java.math.BigDecimal.ZERO;
        private java.math.BigDecimal totalPayoutAmt = java.math.BigDecimal.ZERO;
        private java.math.BigDecimal reductionWeight = java.math.BigDecimal.ZERO;

        private void 加算(支払レコード 支払) {
            count++;
            totalGrossAmt = totalGrossAmt.add(支払.grossAmt);
            totalPayoutAmt = totalPayoutAmt.add(支払.payoutAmt);
            reductionWeight = reductionWeight.add(支払.reductionRate.multiply(支払.grossAmt));
        }

        private java.math.BigDecimal 平均削減率() {
            if (totalGrossAmt.compareTo(java.math.BigDecimal.ZERO) == 0) {
                return java.math.BigDecimal.ZERO;
            }
            return reductionWeight.divide(totalGrossAmt, 8, 丸め方式);
        }
    }
}
