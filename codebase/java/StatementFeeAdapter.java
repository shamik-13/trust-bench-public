public class StatementFeeAdapter {
    /**
     * 変更履歴
     * 版数    年月日      担当      概要
     * 1.00    2024/05/13  保守二課  初版作成
     * 1.01    2024/11/19  保守二課  円貨取引の手数料非表示判定を追加
     */

    private static final String 基準通貨 = "JPY";
    private static final String 対象カード状態 = "01";
    private static final String 状態確定 = "C";
    private static final String 状態対象外 = "S";
    private static final String 状態保留 = "H";
    private static final String 非表示 = "1";
    private static final String 表示 = "0";

    public static void main(String[] a) {
        明細手数料項目[] 明細 = 整形する(サンプル入力());
        long 表示件数 = 0;
        long 非表示件数 = 0;
        long 請求合計 = 0;
        long 手数料合計 = 0;

        for (int i = 0; i < 明細.length; i++) {
            明細手数料項目 r = 明細[i];
            if (表示.equals(r.手数料非表示区分)) {
                表示件数++;
                手数料合計 += r.手数料額;
            } else {
                非表示件数++;
            }
            請求合計 += r.請求額;

            System.out.println(
                    "売上ID=" + r.売上ID
                            + ",カード番号=" + r.マスクカード番号
                            + ",請求額=" + r.請求額
                            + ",手数料額=" + r.手数料額
                            + ",通貨=" + r.通貨コード
                            + ",手数料非表示区分=" + r.手数料非表示区分
                            + ",手数料名=" + r.手数料名
                            + ",監査プログラムID=" + r.監査プログラムID);
        }

        System.out.println("処理件数=" + 明細.length
                + ",表示件数=" + 表示件数
                + ",非表示件数=" + 非表示件数
                + ",請求合計=" + 請求合計
                + ",手数料合計=" + 手数料合計);
    }

    private static 明細手数料項目[] 整形する(CDCAPF[] 入力) {
        if (入力 == null) {
            throw new IllegalArgumentException("入力がありません");
        }

        明細手数料項目[] 作業 = new 明細手数料項目[入力.length];
        int 件数 = 0;

        for (int i = 0; i < 入力.length; i++) {
            CDCAPF r = 入力[i];
            検査する(r, i + 1);

            if (!対象カード状態.equals(r.カード状態)) {
                continue;
            }
            if (状態対象外.equals(r.取込状態) || 状態保留.equals(r.取込状態)) {
                continue;
            }
            if (!状態確定.equals(r.取込状態)) {
                throw new IllegalArgumentException("取込状態が不正です: 行=" + (i + 1));
            }

            boolean 円貨 = 基準通貨.equals(r.通貨コード);
            long 手数料額 = 円貨 ? 0L : r.手数料額;
            String 非表示区分 = 手数料額 == 0L ? 非表示 : 表示;
            String 手数料名 = 手数料額 == 0L ? "" : "海外事務手数料";

            作業[件数++] = new 明細手数料項目(
                    r.売上ID,
                    マスクする(r.カード番号),
                    r.請求額,
                    手数料額,
                    r.通貨コード,
                    非表示区分,
                    手数料名,
                    r.プログラムID);
        }

        明細手数料項目[] 結果 = new 明細手数料項目[件数];
        System.arraycopy(作業, 0, 結果, 0, 件数);
        return 結果;
    }

    private static void 検査する(CDCAPF r, int 行) {
        if (r == null) {
            throw new IllegalArgumentException("入力行がありません: 行=" + 行);
        }
        if (空(r.売上ID)) {
            throw new IllegalArgumentException("売上IDがありません: 行=" + 行);
        }
        if (空(r.カード番号) || r.カード番号.length() < 8) {
            throw new IllegalArgumentException("カード番号が不正です: 行=" + 行);
        }
        if (r.請求額 < 0L) {
            throw new IllegalArgumentException("請求額が不正です: 行=" + 行);
        }
        if (r.手数料額 < 0L) {
            throw new IllegalArgumentException("手数料額が不正です: 行=" + 行);
        }
        if (!基準通貨.equals(r.通貨コード) && r.手数料額 == 0L) {
            throw new IllegalArgumentException("外貨手数料がありません: 行=" + 行);
        }
        if (!"01".equals(r.カード状態)
                && !"02".equals(r.カード状態)
                && !"03".equals(r.カード状態)
                && !"09".equals(r.カード状態)) {
            throw new IllegalArgumentException("カード状態が不正です: 行=" + 行);
        }
        if (空(r.プログラムID)) {
            throw new IllegalArgumentException("プログラムIDがありません: 行=" + 行);
        }
    }

    private static boolean 空(String s) {
        return s == null || s.trim().isEmpty();
    }

    private static String マスクする(String カード番号) {
        String 後四桁 = カード番号.substring(カード番号.length() - 4);
        return "************" + 後四桁;
    }

    private static CDCAPF[] サンプル入力() {
        return new CDCAPF[]{
                new CDCAPF("S202606280001", "4111111111111111", 12800L, 0L, "JPY", "01", "C", "CDCAPF01"),
                new CDCAPF("S202606280002", "5555444433332222", 24500L, 735L, "USD", "01", "C", "CDCAPF01"),
                new CDCAPF("S202606280003", "3530111333300000", 9800L, 294L, "EUR", "01", "C", "CDCAPF01"),
                new CDCAPF("S202606280004", "4000000000000002", 4100L, 123L, "USD", "02", "C", "CDCAPF01"),
                new CDCAPF("S202606280005", "378282246310005", 6900L, 207L, "USD", "01", "S", "CDCAPF01"),
                new CDCAPF("S202606280006", "6011111111111117", 15800L, 474L, "AUD", "01", "H", "CDCAPF01")
        };
    }

    private static final class CDCAPF {
        private final String 売上ID;
        private final String カード番号;
        private final long 請求額;
        private final long 手数料額;
        private final String 通貨コード;
        private final String カード状態;
        private final String 取込状態;
        private final String プログラムID;

        private CDCAPF(String 売上ID, String カード番号, long 請求額, long 手数料額,
                       String 通貨コード, String カード状態, String 取込状態, String プログラムID) {
            this.売上ID = 売上ID;
            this.カード番号 = カード番号;
            this.請求額 = 請求額;
            this.手数料額 = 手数料額;
            this.通貨コード = 通貨コード;
            this.カード状態 = カード状態;
            this.取込状態 = 取込状態;
            this.プログラムID = プログラムID;
        }
    }

    private static final class 明細手数料項目 {
        private final String 売上ID;
        private final String マスクカード番号;
        private final long 請求額;
        private final long 手数料額;
        private final String 通貨コード;
        private final String 手数料非表示区分;
        private final String 手数料名;
        private final String 監査プログラムID;

        private 明細手数料項目(String 売上ID, String マスクカード番号, long 請求額, long 手数料額,
                         String 通貨コード, String 手数料非表示区分, String 手数料名, String 監査プログラムID) {
            this.売上ID = 売上ID;
            this.マスクカード番号 = マスクカード番号;
            this.請求額 = 請求額;
            this.手数料額 = 手数料額;
            this.通貨コード = 通貨コード;
            this.手数料非表示区分 = 手数料非表示区分;
            this.手数料名 = 手数料名;
            this.監査プログラムID = 監査プログラムID;
        }
    }
}
