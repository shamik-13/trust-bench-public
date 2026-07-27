/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2023-01-23  開発担当  初版作成
 */
public class CardStatusSyncService {

    private static final String 状態_有効 = "01";
    private static final String 状態_利用停止 = "02";
    private static final String 状態_解約 = "03";
    private static final String 状態_延滞 = "09";

    private static final String 元システム_状態確認 = "STSCHK";
    private static final String 理由_同期確認 = "SYNC";
    private static final long 同期遅延許容ミリ秒 = 5L * 60L * 1000L;

    public static void main(String[] a) {
        java.util.List<カード基本レコード> カード基本ファイル = new java.util.ArrayList<>();
        カード基本ファイル.add(new カード基本レコード("4900000000000001", "M000001", 状態_有効, 800000L, "ヤマダタロウ"));
        カード基本ファイル.add(new カード基本レコード("4900000000000002", "M000002", 状態_利用停止, 300000L, "サトウハナコ"));
        カード基本ファイル.add(new カード基本レコード("4900000000000003", "M000003", 状態_延滞, 100000L, "スズキイチロウ"));
        カード基本ファイル.add(new カード基本レコード("4900000000000004", "M000004", 状態_解約, 50000L, "タナカミカ"));

        java.util.List<状態履歴レコード> 状態履歴ファイル = new java.util.ArrayList<>();
        long 現在時刻 = System.currentTimeMillis();
        状態履歴ファイル.add(new 状態履歴レコード("4900000000000001", 状態_有効, "CDMST", 現在時刻 - 120000L, "INIT"));
        状態履歴ファイル.add(new 状態履歴レコード("4900000000000002", 状態_有効, "AUTH", 現在時刻 - 900000L, "OLD"));
        状態履歴ファイル.add(new 状態履歴レコード("4900000000000003", 状態_有効, "AUTH", 現在時刻 - 700000L, "OLD"));
        状態履歴ファイル.add(new 状態履歴レコード("4900000000000004", 状態_解約, "CDMST", 現在時刻 - 60000L, "CLOSE"));

        処理結果 結果 = 状態同期を実行する(カード基本ファイル, 状態履歴ファイル, 現在時刻);

        System.out.println("処理件数=" + 結果.処理件数);
        System.out.println("追記件数=" + 結果.追記件数);
        System.out.println("通知件数=" + 結果.通知件数);
        for (通知イベント 通知 : 結果.通知一覧) {
            System.out.println("通知 会員番号=" + 通知.会員番号 + " カード番号=" + 通知.カード番号
                    + " 旧状態=" + 通知.旧状態 + " 新状態=" + 通知.新状態 + " 理由=" + 通知.理由);
        }
    }

    private static 処理結果 状態同期を実行する(
            java.util.List<カード基本レコード> カード基本ファイル,
            java.util.List<状態履歴レコード> 状態履歴ファイル,
            long 処理時刻) {

        java.util.Map<String, 状態履歴レコード> 最新状態索引 = 最新状態索引を作成する(状態履歴ファイル);
        java.util.List<通知イベント> 通知一覧 = new java.util.ArrayList<>();

        int 処理件数 = 0;
        int 追記件数 = 0;

        for (カード基本レコード カード : カード基本ファイル) {
            処理件数++;

            if (!カード基本が処理対象として妥当か(カード)) {
                continue;
            }

            状態履歴レコード 最新状態 = 最新状態索引.get(カード.カード番号);
            boolean 履歴なし = 最新状態 == null;
            boolean 状態差異あり = 履歴なし || !カード.カード状態.equals(最新状態.新状態);
            boolean 遅延あり = 履歴なし || 処理時刻 - 最新状態.状態時刻 > 同期遅延許容ミリ秒;

            if (状態差異あり && 遅延あり) {
                状態履歴レコード 追記 = new 状態履歴レコード(
                        カード.カード番号,
                        カード.カード状態,
                        元システム_状態確認,
                        処理時刻,
                        理由_同期確認);
                状態履歴ファイル.add(追記);
                最新状態索引.put(カード.カード番号, 追記);
                追記件数++;

                if (会員影響通知が必要か(最新状態, カード.カード状態)) {
                    通知一覧.add(new 通知イベント(
                            カード.会員番号,
                            カード.カード番号,
                            履歴なし ? "" : 最新状態.新状態,
                            カード.カード状態,
                            通知理由を作成する(最新状態, カード.カード状態)));
                }
            }
        }

        return new 処理結果(処理件数, 追記件数, 通知一覧.size(), 通知一覧);
    }

    private static java.util.Map<String, 状態履歴レコード> 最新状態索引を作成する(java.util.List<状態履歴レコード> 状態履歴ファイル) {
        java.util.Map<String, 状態履歴レコード> 索引 = new java.util.HashMap<>();
        for (状態履歴レコード 履歴 : 状態履歴ファイル) {
            if (!状態履歴が妥当か(履歴)) {
                continue;
            }
            状態履歴レコード 登録済 = 索引.get(履歴.カード番号);
            if (登録済 == null || 履歴.状態時刻 >= 登録済.状態時刻) {
                索引.put(履歴.カード番号, 履歴);
            }
        }
        return 索引;
    }

    private static boolean カード基本が処理対象として妥当か(カード基本レコード カード) {
        return カード != null
                && 空でない(カード.カード番号)
                && 空でない(カード.会員番号)
                && 空でない(カード.会員名カナ)
                && カード.与信限度額 >= 0L
                && カード状態が妥当か(カード.カード状態);
    }

    private static boolean 状態履歴が妥当か(状態履歴レコード 履歴) {
        return 履歴 != null
                && 空でない(履歴.カード番号)
                && カード状態が妥当か(履歴.新状態)
                && 空でない(履歴.発生元システム)
                && 履歴.状態時刻 > 0L
                && 空でない(履歴.理由コード);
    }

    private static boolean カード状態が妥当か(String 状態) {
        return 状態_有効.equals(状態)
                || 状態_利用停止.equals(状態)
                || 状態_解約.equals(状態)
                || 状態_延滞.equals(状態);
    }

    private static boolean 会員影響通知が必要か(状態履歴レコード 最新状態, String 新状態) {
        String 旧状態 = 最新状態 == null ? 状態_有効 : 最新状態.新状態;
        boolean 停止系へ変更 = 状態_有効.equals(旧状態)
                && (状態_利用停止.equals(新状態) || 状態_延滞.equals(新状態));
        boolean 再開へ変更 = (状態_利用停止.equals(旧状態) || 状態_延滞.equals(旧状態))
                && 状態_有効.equals(新状態);
        return 停止系へ変更 || 再開へ変更;
    }

    private static String 通知理由を作成する(状態履歴レコード 最新状態, String 新状態) {
        String 旧状態 = 最新状態 == null ? 状態_有効 : 最新状態.新状態;
        if (状態_有効.equals(新状態)) {
            return "再開";
        }
        if (状態_有効.equals(旧状態)) {
            return "停止";
        }
        return "状態変更";
    }

    private static boolean 空でない(String 値) {
        return 値 != null && !値.trim().isEmpty();
    }

    private static final class カード基本レコード {
        private final String カード番号;
        private final String 会員番号;
        private final String カード状態;
        private final long 与信限度額;
        private final String 会員名カナ;

        private カード基本レコード(String カード番号, String 会員番号, String カード状態, long 与信限度額, String 会員名カナ) {
            this.カード番号 = カード番号;
            this.会員番号 = 会員番号;
            this.カード状態 = カード状態;
            this.与信限度額 = 与信限度額;
            this.会員名カナ = 会員名カナ;
        }
    }

    private static final class 状態履歴レコード {
        private final String カード番号;
        private final String 新状態;
        private final String 発生元システム;
        private final long 状態時刻;
        private final String 理由コード;

        private 状態履歴レコード(String カード番号, String 新状態, String 発生元システム, long 状態時刻, String 理由コード) {
            this.カード番号 = カード番号;
            this.新状態 = 新状態;
            this.発生元システム = 発生元システム;
            this.状態時刻 = 状態時刻;
            this.理由コード = 理由コード;
        }
    }

    private static final class 通知イベント {
        private final String 会員番号;
        private final String カード番号;
        private final String 旧状態;
        private final String 新状態;
        private final String 理由;

        private 通知イベント(String 会員番号, String カード番号, String 旧状態, String 新状態, String 理由) {
            this.会員番号 = 会員番号;
            this.カード番号 = カード番号;
            this.旧状態 = 旧状態;
            this.新状態 = 新状態;
            this.理由 = 理由;
        }
    }

    private static final class 処理結果 {
        private final int 処理件数;
        private final int 追記件数;
        private final int 通知件数;
        private final java.util.List<通知イベント> 通知一覧;

        private 処理結果(int 処理件数, int 追記件数, int 通知件数, java.util.List<通知イベント> 通知一覧) {
            this.処理件数 = 処理件数;
            this.追記件数 = 追記件数;
            this.通知件数 = 通知件数;
            this.通知一覧 = java.util.Collections.unmodifiableList(new java.util.ArrayList<>(通知一覧));
        }
    }
}
