/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/04/18  開発担当  初版作成。消込例外台帳の検索とページング、監査履歴追加を実装。
 * 1.01  2024/09/02  取引統制  入金額と消込内訳合計の整合チェックを追加。
 */
public class PaymentExceptionService {
    private static final java.time.format.DateTimeFormatter 日付形式 = java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
    private static final int ページサイズ上限 = 50;

    public static void main(String[] a) {
        例外台帳 台帳 = new 例外台帳();
        台帳.例外一覧.add(new 消込例外("EXC-20260628-0001", "PAY-10001", "411111000001", "AMT_DIFF", 1200L, "CDRECN01", java.time.LocalDate.of(2026, 6, 27)));
        台帳.例外一覧.add(new 消込例外("EXC-20260628-0002", "PAY-10002", "411111000002", "NO_APP", 8400L, "CDRECN02", java.time.LocalDate.of(2026, 6, 27)));
        台帳.例外一覧.add(new 消込例外("EXC-20260628-0003", "PAY-10003", "411111000003", "OVER_PAY", 300L, "CDRECN01", java.time.LocalDate.of(2026, 6, 28)));
        台帳.例外一覧.add(new 消込例外("EXC-20260628-0004", "PAY-10004", "411111000004", "NO_PAY", 5000L, "CDAUDT03", java.time.LocalDate.of(2026, 6, 28)));

        台帳.入金一覧.add(new 入金("PAY-10001", "411111000001", 10000L, java.time.LocalDate.of(2026, 6, 27), "20"));
        台帳.入金一覧.add(new 入金("PAY-10002", "411111000002", 8400L, java.time.LocalDate.of(2026, 6, 27), "10"));
        台帳.入金一覧.add(new 入金("PAY-10003", "411111000003", 15300L, java.time.LocalDate.of(2026, 6, 28), "30"));

        台帳.消込一覧.add(new 消込("PAY-10001", "411111000001", 2000L, 800L, 6000L, 1200L, "P", "CDAPP001"));
        台帳.消込一覧.add(new 消込("PAY-10003", "411111000003", 1000L, 300L, 14000L, 0L, "O", "CDAPP001"));

        検索条件 条件 = new 検索条件("AMT_DIFF", "CDRECN01", 100L, 5000L, java.time.LocalDate.of(2026, 6, 1), java.time.LocalDate.of(2026, 6, 30), 1, 20);
        ページ結果 結果 = 検索する(台帳, 条件);
        出力する("検索件数=" + 結果.総件数 + " 表示件数=" + 結果.明細.size());

        更新要求 要求 = new 更新要求("EXC-20260628-0001", "411111000001", "PAY-10001", "調査済。入金明細と消込残額の差額を確認。", false, true, java.time.LocalDate.of(2026, 6, 28));
        更新結果 更新結果 = 更新する(台帳, 要求);
        出力する("更新結果=" + 更新結果.結果コード + " 履歴件数=" + 台帳.履歴一覧.size());
    }

    private static ページ結果 検索する(例外台帳 台帳, 検索条件 条件) {
        検索条件を検証する(条件);
        java.util.List<例外表示> 一致 = new java.util.ArrayList<>();
        for (消込例外 例外 : 台帳.例外一覧) {
            if (!文字列一致(条件.理由コード, 例外.例外コード)) {
                continue;
            }
            if (!文字列一致(条件.検出プログラム, 例外.検出プログラム)) {
                continue;
            }
            if (条件.最小金額 != null && 例外.例外金額 < 条件.最小金額.longValue()) {
                continue;
            }
            if (条件.最大金額 != null && 例外.例外金額 > 条件.最大金額.longValue()) {
                continue;
            }
            if (条件.開始日 != null && 例外.検出日.isBefore(条件.開始日)) {
                continue;
            }
            if (条件.終了日 != null && 例外.検出日.isAfter(条件.終了日)) {
                continue;
            }
            入金 入金 = 入金を探す(台帳, 例外.支払番号, 例外.カード番号);
            消込 消込 = 消込を探す(台帳, 例外.支払番号, 例外.カード番号);
            long 入金額 = 入金 == null ? 0L : 入金.入金額;
            long 消込済額 = 消込 == null ? 0L : 消込.手数料充当額 + 消込.利息充当額 + 消込.元本充当額;
            long 未確認差額 = 入金額 - 消込済額;
            一致.add(new 例外表示(例外.例外番号, 例外.支払番号, 例外.カード番号, 例外.例外コード, 例外.例外金額, 例外.検出プログラム, 例外.検出日, 入金額, 消込済額, 未確認差額, 消込 == null ? "S" : 消込.状態));
        }
        一致.sort(new java.util.Comparator<例外表示>() {
            public int compare(例外表示 左, 例外表示 右) {
                int 日付比較 = 右.検出日.compareTo(左.検出日);
                if (日付比較 != 0) {
                    return 日付比較;
                }
                return 左.例外番号.compareTo(右.例外番号);
            }
        });

        int 開始位置 = (条件.ページ番号 - 1) * 条件.ページサイズ;
        int 終了位置 = Math.min(開始位置 + 条件.ページサイズ, 一致.size());
        java.util.List<例外表示> 明細 = 開始位置 >= 一致.size()
                ? java.util.Collections.<例外表示>emptyList()
                : new java.util.ArrayList<例外表示>(一致.subList(開始位置, 終了位置));
        return new ページ結果(一致.size(), 条件.ページ番号, 条件.ページサイズ, 明細);
    }

    private static 更新結果 更新する(例外台帳 台帳, 更新要求 要求) {
        更新要求を検証する(要求);
        消込例外 対象 = null;
        for (消込例外 例外 : 台帳.例外一覧) {
            if (例外.例外番号.equals(要求.例外番号)) {
                対象 = 例外;
                break;
            }
        }
        if (対象 == null) {
            return new 更新結果("対象なし", "例外番号が台帳に存在しません");
        }
        if (!対象.カード番号.equals(要求.カード番号) || !対象.支払番号.equals(要求.支払番号)) {
            return new 更新結果("不一致", "カード番号または支払番号が一致しません");
        }

        消込 消込 = 消込を探す(台帳, 要求.支払番号, 要求.カード番号);
        long 履歴金額 = 対象.例外金額;
        if (消込 != null) {
            long 内訳合計 = 消込.手数料充当額 + 消込.利息充当額 + 消込.元本充当額 + 消込.残額;
            入金 入金 = 入金を探す(台帳, 要求.支払番号, 要求.カード番号);
            if (入金 != null && 内訳合計 != 入金.入金額) {
                return new 更新結果("内訳不整合", "入金額と消込内訳合計が一致しません");
            }
        }

        int 次順序 = 次履歴順序(台帳, 要求.カード番号, 要求.支払番号);
        台帳.履歴一覧.add(new 履歴(要求.カード番号, 要求.支払番号, 次順序++, "調査メモ", 0L, 要求.処理日, "PaymentExceptionService"));
        if (要求.保留解除) {
            台帳.履歴一覧.add(new 履歴(要求.カード番号, 要求.支払番号, 次順序++, "保留解除", 履歴金額, 要求.処理日, "PaymentExceptionService"));
        }
        if (要求.再処理依頼) {
            台帳.履歴一覧.add(new 履歴(要求.カード番号, 要求.支払番号, 次順序, "再処理依頼", 履歴金額, 要求.処理日, "PaymentExceptionService"));
        }
        return new 更新結果("正常", "監査履歴を追加しました");
    }

    private static void 検索条件を検証する(検索条件 条件) {
        if (条件.ページ番号 < 1) {
            throw new IllegalArgumentException("ページ番号が不正です");
        }
        if (条件.ページサイズ < 1 || 条件.ページサイズ > ページサイズ上限) {
            throw new IllegalArgumentException("ページサイズが不正です");
        }
        if (条件.最小金額 != null && 条件.最小金額.longValue() < 0L) {
            throw new IllegalArgumentException("最小金額が不正です");
        }
        if (条件.最大金額 != null && 条件.最大金額.longValue() < 0L) {
            throw new IllegalArgumentException("最大金額が不正です");
        }
        if (条件.最小金額 != null && 条件.最大金額 != null && 条件.最小金額.longValue() > 条件.最大金額.longValue()) {
            throw new IllegalArgumentException("金額範囲が不正です");
        }
        if (条件.開始日 != null && 条件.終了日 != null && 条件.開始日.isAfter(条件.終了日)) {
            throw new IllegalArgumentException("発生日範囲が不正です");
        }
    }

    private static void 更新要求を検証する(更新要求 要求) {
        if (空(要求.例外番号) || 空(要求.カード番号) || 空(要求.支払番号)) {
            throw new IllegalArgumentException("識別項目が不足しています");
        }
        if (空(要求.調査メモ) || 要求.調査メモ.length() > 120) {
            throw new IllegalArgumentException("調査メモが不正です");
        }
        if (!要求.保留解除 && !要求.再処理依頼) {
            throw new IllegalArgumentException("更新区分が指定されていません");
        }
        if (要求.処理日 == null) {
            throw new IllegalArgumentException("処理日が未設定です");
        }
    }

    private static boolean 文字列一致(String 条件値, String 実値) {
        return 空(条件値) || 条件値.equals(実値);
    }

    private static boolean 空(String 値) {
        return 値 == null || 値.trim().isEmpty();
    }

    private static 入金 入金を探す(例外台帳 台帳, String 支払番号, String カード番号) {
        for (入金 値 : 台帳.入金一覧) {
            if (値.支払番号.equals(支払番号) && 値.カード番号.equals(カード番号)) {
                return 値;
            }
        }
        return null;
    }

    private static 消込 消込を探す(例外台帳 台帳, String 支払番号, String カード番号) {
        for (消込 値 : 台帳.消込一覧) {
            if (値.支払番号.equals(支払番号) && 値.カード番号.equals(カード番号)) {
                return 値;
            }
        }
        return null;
    }

    private static int 次履歴順序(例外台帳 台帳, String カード番号, String 支払番号) {
        int 最大 = 0;
        for (履歴 値 : 台帳.履歴一覧) {
            if (値.カード番号.equals(カード番号) && 値.支払番号.equals(支払番号) && 値.履歴順序 > 最大) {
                最大 = 値.履歴順序;
            }
        }
        return 最大 + 1;
    }

    private static void 出力する(String 文) {
        System.out.println(文);
    }

    private static final class 例外台帳 {
        final java.util.List<消込例外> 例外一覧 = new java.util.ArrayList<消込例外>();
        final java.util.List<入金> 入金一覧 = new java.util.ArrayList<入金>();
        final java.util.List<消込> 消込一覧 = new java.util.ArrayList<消込>();
        final java.util.List<履歴> 履歴一覧 = new java.util.ArrayList<履歴>();
    }

    private static final class 消込例外 {
        final String 例外番号;
        final String 支払番号;
        final String カード番号;
        final String 例外コード;
        final long 例外金額;
        final String 検出プログラム;
        final java.time.LocalDate 検出日;

        消込例外(String 例外番号, String 支払番号, String カード番号, String 例外コード, long 例外金額, String 検出プログラム, java.time.LocalDate 検出日) {
            this.例外番号 = 例外番号;
            this.支払番号 = 支払番号;
            this.カード番号 = カード番号;
            this.例外コード = 例外コード;
            this.例外金額 = 例外金額;
            this.検出プログラム = 検出プログラム;
            this.検出日 = 検出日;
        }
    }

    private static final class 入金 {
        final String 支払番号;
        final String カード番号;
        final long 入金額;
        final java.time.LocalDate 入金日;
        final String 入金方法;

        入金(String 支払番号, String カード番号, long 入金額, java.time.LocalDate 入金日, String 入金方法) {
            if (!"10".equals(入金方法) && !"20".equals(入金方法) && !"30".equals(入金方法)) {
                throw new IllegalArgumentException("入金方法が不正です");
            }
            this.支払番号 = 支払番号;
            this.カード番号 = カード番号;
            this.入金額 = 入金額;
            this.入金日 = 入金日;
            this.入金方法 = 入金方法;
        }
    }

    private static final class 消込 {
        final String 支払番号;
        final String カード番号;
        final long 手数料充当額;
        final long 利息充当額;
        final long 元本充当額;
        final long 残額;
        final String 状態;
        final String プログラム番号;

        消込(String 支払番号, String カード番号, long 手数料充当額, long 利息充当額, long 元本充当額, long 残額, String 状態, String プログラム番号) {
            if (!"F".equals(状態) && !"P".equals(状態) && !"O".equals(状態) && !"S".equals(状態)) {
                throw new IllegalArgumentException("消込状態が不正です");
            }
            this.支払番号 = 支払番号;
            this.カード番号 = カード番号;
            this.手数料充当額 = 手数料充当額;
            this.利息充当額 = 利息充当額;
            this.元本充当額 = 元本充当額;
            this.残額 = 残額;
            this.状態 = 状態;
            this.プログラム番号 = プログラム番号;
        }
    }

    private static final class 履歴 {
        final String カード番号;
        final String 支払番号;
        final int 履歴順序;
        final String 事象種別;
        final long 事象金額;
        final java.time.LocalDate 事象日;
        final String 発生元プログラム;

        履歴(String カード番号, String 支払番号, int 履歴順序, String 事象種別, long 事象金額, java.time.LocalDate 事象日, String 発生元プログラム) {
            this.カード番号 = カード番号;
            this.支払番号 = 支払番号;
            this.履歴順序 = 履歴順序;
            this.事象種別 = 事象種別;
            this.事象金額 = 事象金額;
            this.事象日 = 事象日;
            this.発生元プログラム = 発生元プログラム;
        }
    }

    private static final class 検索条件 {
        final String 理由コード;
        final String 検出プログラム;
        final Long 最小金額;
        final Long 最大金額;
        final java.time.LocalDate 開始日;
        final java.time.LocalDate 終了日;
        final int ページ番号;
        final int ページサイズ;

        検索条件(String 理由コード, String 検出プログラム, Long 最小金額, Long 最大金額, java.time.LocalDate 開始日, java.time.LocalDate 終了日, int ページ番号, int ページサイズ) {
            this.理由コード = 理由コード;
            this.検出プログラム = 検出プログラム;
            this.最小金額 = 最小金額;
            this.最大金額 = 最大金額;
            this.開始日 = 開始日;
            this.終了日 = 終了日;
            this.ページ番号 = ページ番号;
            this.ページサイズ = ページサイズ;
        }
    }

    private static final class ページ結果 {
        final int 総件数;
        final int ページ番号;
        final int ページサイズ;
        final java.util.List<例外表示> 明細;

        ページ結果(int 総件数, int ページ番号, int ページサイズ, java.util.List<例外表示> 明細) {
            this.総件数 = 総件数;
            this.ページ番号 = ページ番号;
            this.ページサイズ = ページサイズ;
            this.明細 = 明細;
        }
    }

    private static final class 例外表示 {
        final String 例外番号;
        final String 支払番号;
        final String カード番号;
        final String 例外コード;
        final long 例外金額;
        final String 検出プログラム;
        final java.time.LocalDate 検出日;
        final long 入金額;
        final long 消込済額;
        final long 未確認差額;
        final String 消込状態;

        例外表示(String 例外番号, String 支払番号, String カード番号, String 例外コード, long 例外金額, String 検出プログラム, java.time.LocalDate 検出日, long 入金額, long 消込済額, long 未確認差額, String 消込状態) {
            this.例外番号 = 例外番号;
            this.支払番号 = 支払番号;
            this.カード番号 = カード番号;
            this.例外コード = 例外コード;
            this.例外金額 = 例外金額;
            this.検出プログラム = 検出プログラム;
            this.検出日 = 検出日;
            this.入金額 = 入金額;
            this.消込済額 = 消込済額;
            this.未確認差額 = 未確認差額;
            this.消込状態 = 消込状態;
        }
    }

    private static final class 更新要求 {
        final String 例外番号;
        final String カード番号;
        final String 支払番号;
        final String 調査メモ;
        final boolean 保留解除;
        final boolean 再処理依頼;
        final java.time.LocalDate 処理日;

        更新要求(String 例外番号, String カード番号, String 支払番号, String 調査メモ, boolean 保留解除, boolean 再処理依頼, java.time.LocalDate 処理日) {
            this.例外番号 = 例外番号;
            this.カード番号 = カード番号;
            this.支払番号 = 支払番号;
            this.調査メモ = 調査メモ;
            this.保留解除 = 保留解除;
            this.再処理依頼 = 再処理依頼;
            this.処理日 = 処理日;
        }
    }

    private static final class 更新結果 {
        final String 結果コード;
        final String 結果メッセージ;

        更新結果(String 結果コード, String 結果メッセージ) {
            this.結果コード = 結果コード;
            this.結果メッセージ = 結果メッセージ;
        }
    }
}
