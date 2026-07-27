package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.0     2025-06-29  共通基盤    初版作成
 */
public class TransactionDetailNormalizeService {
    private static final java.time.LocalDate 処理日 = java.time.LocalDate.of(2025, 6, 29);

    public static void main(String[] a) {
        java.util.List<取引明細> 入力明細 = java.util.Arrays.asList(
                new 取引明細("T000000001", "BK", "BK-20250629-0001", "１２３４５６", "KAKUTEI"),
                new 取引明細("T000000002", "001", "BK-20250629-0002", "９８７６５", "NORMAL"),
                new 取引明細("T000000003", "SC", "SC-20250629-0001", "-１２３０", "CANCEL"),
                new 取引明細("T000000004", "CARD", "CD-20250629-0001", "３００００", "OK"),
                new 取引明細("T000000005", "PAY", "PY-20250629-0001", "１２．５０", "REV"),
                new 取引明細("T000000006", "LF", "LF-20250629-0001", "１００００００", "01")
        );

        java.util.List<コード明細> コード明細 = java.util.Arrays.asList(
                new コード明細("001", "COMPANY-CODE", "BK", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("BANK", "COMPANY-CODE", "BK", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("CARD", "COMPANY-CODE", "CD", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("PAY", "COMPANY-CODE", "PY", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("KAKUTEI", "TX-TXN-STATUS-KBN", "01", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("NORMAL", "TX-TXN-STATUS-KBN", "01", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("OK", "TX-TXN-STATUS-KBN", "01", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("CANCEL", "TX-TXN-STATUS-KBN", "09", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01"),
                new コード明細("REV", "TX-TXN-STATUS-KBN", "09", java.time.LocalDate.of(2020, 1, 1), java.time.LocalDate.of(2099, 12, 31), "01")
        );

        java.util.List<取引明細> 出力明細 = 正規化する(入力明細, コード明細);
        検査する(入力明細, 出力明細);

        java.util.Map<String, 集計値> 会社別集計 = 集計する(出力明細);
        for (java.util.Map.Entry<String, 集計値> e : 会社別集計.entrySet()) {
            System.out.println("会社=" + e.getKey() + " 件数=" + e.getValue().件数 + " 金額=" + e.getValue().金額.toPlainString());
        }
    }

    private static java.util.List<取引明細> 正規化する(java.util.List<取引明細> 入力明細, java.util.List<コード明細> コード明細) {
        java.util.Map<String, String> 会社コード表 = コード表を作る(コード明細, "COMPANY-CODE");
        java.util.Map<String, String> 状態コード表 = コード表を作る(コード明細, "TX-TXN-STATUS-KBN");
        java.util.List<取引明細> 結果 = new java.util.ArrayList<取引明細>();

        for (取引明細 明細 : 入力明細) {
            必須検査(明細);
            String 会社コード = 会社コードを正規化する(明細.companyCode, 会社コード表);
            String 状態区分 = 状態区分を正規化する(明細.txnStatusKbn, 状態コード表);
            java.math.BigDecimal 金額 = 金額を正規化する(明細.txnAmt);

            結果.add(new 取引明細(
                    明細.txnId,
                    会社コード,
                    明細.localTxnNo,
                    金額.toPlainString(),
                    状態区分
            ));
        }
        return 結果;
    }

    private static void 必須検査(取引明細 明細) {
        if (空である(明細.txnId) || 空である(明細.companyCode) || 空である(明細.localTxnNo)
                || 空である(明細.txnAmt) || 空である(明細.txnStatusKbn)) {
            throw new IllegalArgumentException("取引明細の必須項目が不足しています");
        }
    }

    private static java.util.Map<String, String> コード表を作る(java.util.List<コード明細> コード明細, String 種別) {
        java.util.Map<String, String> 表 = new java.util.HashMap<String, String>();
        for (コード明細 c : コード明細) {
            if (種別.equals(c.codeType) && "01".equals(c.codeStatusKbn)
                    && !処理日.isBefore(c.validFrom) && !処理日.isAfter(c.validTo)) {
                表.put(c.codeKey, c.codeValue);
            }
        }
        return 表;
    }

    private static String 会社コードを正規化する(String 値, java.util.Map<String, String> 会社コード表) {
        String 半角値 = 全角を半角へ寄せる(値).trim().toUpperCase(java.util.Locale.ROOT);
        if (標準会社コードである(半角値)) {
            return 半角値;
        }
        String 標準値 = 会社コード表.get(半角値);
        if (標準値 == null || !標準会社コードである(標準値)) {
            throw new IllegalArgumentException("会社コードを標準化できません: " + 値);
        }
        return 標準値;
    }

    private static String 状態区分を正規化する(String 値, java.util.Map<String, String> 状態コード表) {
        String 半角値 = 全角を半角へ寄せる(値).trim().toUpperCase(java.util.Locale.ROOT);
        if ("01".equals(半角値) || "09".equals(半角値)) {
            return 半角値;
        }
        String 標準値 = 状態コード表.get(半角値);
        if (!"01".equals(標準値) && !"09".equals(標準値)) {
            throw new IllegalArgumentException("取引状態区分を標準化できません: " + 値);
        }
        return 標準値;
    }

    private static java.math.BigDecimal 金額を正規化する(String 値) {
        String 半角値 = 全角を半角へ寄せる(値).replace(",", "").trim();
        try {
            java.math.BigDecimal 金額 = new java.math.BigDecimal(半角値);
            if (金額.scale() < 0) {
                return 金額.setScale(0);
            }
            return 金額.stripTrailingZeros();
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("取引金額を数値化できません: " + 値, e);
        }
    }

    private static String 全角を半角へ寄せる(String 値) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < 値.length(); i++) {
            char ch = 値.charAt(i);
            if (ch >= '０' && ch <= '９') {
                b.append((char) ('0' + (ch - '０')));
            } else if (ch == '．') {
                b.append('.');
            } else if (ch == '，') {
                b.append(',');
            } else if (ch == '－') {
                b.append('-');
            } else {
                b.append(ch);
            }
        }
        return b.toString();
    }

    private static boolean 標準会社コードである(String 値) {
        return "BK".equals(値) || "SC".equals(値) || "CD".equals(値)
                || "PY".equals(値) || "LF".equals(値) || "CM".equals(値);
    }

    private static void 検査する(java.util.List<取引明細> 入力明細, java.util.List<取引明細> 出力明細) {
        if (入力明細.size() != 出力明細.size()) {
            throw new IllegalStateException("正規化前後の件数が一致しません");
        }

        for (int i = 0; i < 入力明細.size(); i++) {
            取引明細 前 = 入力明細.get(i);
            取引明細 後 = 出力明細.get(i);
            String 前キー = 前.companyCode + "|" + 前.localTxnNo;
            String 後キー = 後.companyCode + "|" + 後.localTxnNo;
            if (!前.localTxnNo.equals(後.localTxnNo)) {
                throw new IllegalStateException("ローカル取引番号が変質しました: " + 前.txnId);
            }
            if (!前キー.equals(後キー)) {
                System.out.println("入力キー差異検知 取引=" + 前.txnId + " 前=" + 前キー + " 後=" + 後キー);
            }
        }
    }

    private static java.util.Map<String, 集計値> 集計する(java.util.List<取引明細> 明細一覧) {
        java.util.Map<String, 集計値> 集計 = new java.util.TreeMap<String, 集計値>();
        for (取引明細 明細 : 明細一覧) {
            集計値 現在 = 集計.get(明細.companyCode);
            if (現在 == null) {
                現在 = new 集計値();
                集計.put(明細.companyCode, 現在);
            }
            現在.件数++;
            現在.金額 = 現在.金額.add(new java.math.BigDecimal(明細.txnAmt));
        }
        return 集計;
    }

    private static boolean 空である(String 値) {
        return 値 == null || 値.trim().isEmpty();
    }

    private static final class 取引明細 {
        private final String txnId;
        private final String companyCode;
        private final String localTxnNo;
        private final String txnAmt;
        private final String txnStatusKbn;

        private 取引明細(String txnId, String companyCode, String localTxnNo, String txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class コード明細 {
        private final String codeKey;
        private final String codeType;
        private final String codeValue;
        private final java.time.LocalDate validFrom;
        private final java.time.LocalDate validTo;
        private final String codeStatusKbn;

        private コード明細(String codeKey, String codeType, String codeValue,
                    java.time.LocalDate validFrom, java.time.LocalDate validTo, String codeStatusKbn) {
            this.codeKey = codeKey;
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.codeStatusKbn = codeStatusKbn;
        }
    }

    private static final class 集計値 {
        private int 件数;
        private java.math.BigDecimal 金額 = java.math.BigDecimal.ZERO;
    }
}
