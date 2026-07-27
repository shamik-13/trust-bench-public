package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2025/06/29  共通基盤部  初版作成
 */
public class AuditReportOutputService {
    private static final String 対象月 = "2025-05";

    private AuditReportOutputService() {
    }

    public static void main(String[] a) {
        new AuditReportOutputService().実行();
    }

    private void 実行() {
        java.util.List<月次集計> 月次集計一覧 = 月次集計読込();
        java.util.List<監査ジャーナル> 監査ジャーナル一覧 = 監査ジャーナル読込();
        java.util.List<監査定義> 監査定義一覧 = 監査定義読込();

        java.util.Map<String, 会社別集計> 会社別 = 集計する(月次集計一覧, 監査定義一覧);
        java.util.List<String> 相互参照失敗一覧 = 相互参照失敗を抽出する(監査ジャーナル一覧, 監査定義一覧);

        java.util.List<帳票レコード> 帳票 = 帳票作成(会社別, 相互参照失敗一覧);
        帳票出力(帳票);
    }

    private java.util.Map<String, 会社別集計> 集計する(java.util.List<月次集計> 月次集計一覧,
            java.util.List<監査定義> 監査定義一覧) {
        java.util.Map<String, 会社別集計> 会社別 = new java.util.LinkedHashMap<String, 会社別集計>();
        for (String 会社コード : java.util.Arrays.asList("BK", "SC", "CD", "PY", "LF", "CM")) {
            会社別.put(会社コード, new 会社別集計(会社コード));
        }

        for (月次集計 集計 : 月次集計一覧) {
            if (!対象月.equals(集計.集計月)) {
                continue;
            }
            会社別集計 会社集計 = 会社別.get(集計.会社コード);
            if (会社集計 == null) {
                continue;
            }
            会社集計.取引件数 += 集計.取引件数;
            会社集計.監査件数 += 集計.監査件数;
            会社集計.不一致件数 += 集計.不一致件数;
            if (!"10".equals(集計.集計状態区分)) {
                会社集計.未確定集計件数++;
            }
        }

        for (監査定義 定義 : 監査定義一覧) {
            会社別集計 会社集計 = 会社別.get(定義.会社コード);
            if (会社集計 == null) {
                continue;
            }
            if ("90".equals(定義.監査状態区分)) {
                会社集計.否認件数++;
            } else if ("20".equals(定義.監査状態区分)) {
                会社集計.保留件数++;
            } else if (!"10".equals(定義.監査状態区分)) {
                会社集計.状態不明件数++;
            }
        }
        return 会社別;
    }

    private java.util.List<String> 相互参照失敗を抽出する(java.util.List<監査ジャーナル> 監査ジャーナル一覧,
            java.util.List<監査定義> 監査定義一覧) {
        java.util.Map<String, 監査定義> 定義索引 = new java.util.HashMap<String, 監査定義>();
        for (監査定義 定義 : 監査定義一覧) {
            定義索引.put(定義.監査Id, 定義);
        }

        java.util.List<String> 失敗一覧 = new java.util.ArrayList<String>();
        for (監査ジャーナル 履歴 : 監査ジャーナル一覧) {
            監査定義 定義 = 定義索引.get(履歴.監査Id);
            if (定義 == null) {
                失敗一覧.add("監査ID未登録:" + 履歴.監査Id + ":SEQ=" + 履歴.ジャーナルSeq);
                continue;
            }
            if (!履歴.グループ参照番号.equals(定義.グループ参照番号)) {
                失敗一覧.add("GROUP-REF-NO不一致:" + 履歴.監査Id + ":SEQ=" + 履歴.ジャーナルSeq);
            }
            if (!"10".equals(履歴.ジャーナル状態区分)) {
                失敗一覧.add("ジャーナル状態未完了:" + 履歴.監査Id + ":SEQ=" + 履歴.ジャーナルSeq);
            }
        }
        return 失敗一覧;
    }

    private java.util.List<帳票レコード> 帳票作成(java.util.Map<String, 会社別集計> 会社別,
            java.util.List<String> 相互参照失敗一覧) {
        java.util.List<帳票レコード> 帳票 = new java.util.ArrayList<帳票レコード>();
        String 作成日時 = java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));

        for (会社別集計 集計 : 会社別.values()) {
            String 状態 = 集計.未確定集計件数 == 0 && 集計.状態不明件数 == 0 ? "10" : "20";
            String 帳票Id = "AUD-" + 対象月.replace("-", "") + "-" + 集計.会社コード;
            帳票.add(new 帳票レコード(帳票Id, "01", 対象月, 状態, 作成日時));
        }

        int 連番 = 1;
        for (String 失敗 : 相互参照失敗一覧) {
            String 帳票Id = "XRF-" + 対象月.replace("-", "") + "-" + String.format("%04d", 連番);
            帳票.add(new 帳票レコード(帳票Id, "02", 対象月, "30", 作成日時));
            System.out.println("相互参照失敗 " + 失敗);
            連番++;
        }
        return 帳票;
    }

    private void 帳票出力(java.util.List<帳票レコード> 帳票) {
        for (帳票レコード レコード : 帳票) {
            System.out.println(レコード.帳票Id + "," + レコード.帳票種別区分 + "," + レコード.対象月 + ","
                    + レコード.出力状態区分 + "," + レコード.作成日時);
        }
    }

    private java.util.List<月次集計> 月次集計読込() {
        return java.util.Arrays.asList(
                new 月次集計("2025-05", "BK", 184520, 184498, 18, "10"),
                new 月次集計("2025-05", "SC", 94210, 94170, 27, "10"),
                new 月次集計("2025-05", "CD", 318440, 318311, 96, "10"),
                new 月次集計("2025-05", "PY", 587920, 587410, 211, "20"),
                new 月次集計("2025-05", "LF", 42110, 42109, 1, "10"),
                new 月次集計("2025-05", "CM", 22140, 22138, 2, "10"));
    }

    private java.util.List<監査ジャーナル> 監査ジャーナル読込() {
        return java.util.Arrays.asList(
                new 監査ジャーナル(10000001L, "A-BK-000001", "GRP-BK-202505-000001", "01", "10"),
                new 監査ジャーナル(10000002L, "A-SC-000042", "GRP-SC-202505-000042", "01", "10"),
                new 監査ジャーナル(10000003L, "A-CD-000109", "GRP-CD-202505-000109", "09", "10"),
                new 監査ジャーナル(10000004L, "A-PY-000288", "GRP-PY-202505-000288", "01", "20"),
                new 監査ジャーナル(10000005L, "A-LF-000017", "GRP-LF-202505-000017", "01", "10"),
                new 監査ジャーナル(10000006L, "A-CM-000003", "GRP-CM-202505-000003", "01", "10"),
                new 監査ジャーナル(10000007L, "A-PY-未登録", "GRP-PY-202505-999999", "01", "10"));
    }

    private java.util.List<監査定義> 監査定義読込() {
        return java.util.Arrays.asList(
                new 監査定義("A-BK-000001", "GRP-BK-202505-000001", "BK", "BK-L-000001", "10"),
                new 監査定義("A-SC-000042", "GRP-SC-202505-000042", "SC", "SC-L-000042", "10"),
                new 監査定義("A-CD-000109", "GRP-CD-202505-000109", "CD", "CD-L-000109", "90"),
                new 監査定義("A-PY-000288", "GRP-PY-202505-000280", "PY", "PY-L-000288", "20"),
                new 監査定義("A-LF-000017", "GRP-LF-202505-000017", "LF", "LF-L-000017", "10"),
                new 監査定義("A-CM-000003", "GRP-CM-202505-000003", "CM", "CM-L-000003", "10"));
    }

    private static final class 月次集計 {
        private final String 集計月;
        private final String 会社コード;
        private final int 取引件数;
        private final int 監査件数;
        private final int 不一致件数;
        private final String 集計状態区分;

        private 月次集計(String 集計月, String 会社コード, int 取引件数, int 監査件数, int 不一致件数, String 集計状態区分) {
            this.集計月 = 集計月;
            this.会社コード = 会社コード;
            this.取引件数 = 取引件数;
            this.監査件数 = 監査件数;
            this.不一致件数 = 不一致件数;
            this.集計状態区分 = 集計状態区分;
        }
    }

    private static final class 監査ジャーナル {
        private final long ジャーナルSeq;
        private final String 監査Id;
        private final String グループ参照番号;
        private final String イベント種別区分;
        private final String ジャーナル状態区分;

        private 監査ジャーナル(long ジャーナルSeq, String 監査Id, String グループ参照番号, String イベント種別区分,
                String ジャーナル状態区分) {
            this.ジャーナルSeq = ジャーナルSeq;
            this.監査Id = 監査Id;
            this.グループ参照番号 = グループ参照番号;
            this.イベント種別区分 = イベント種別区分;
            this.ジャーナル状態区分 = ジャーナル状態区分;
        }
    }

    private static final class 監査定義 {
        private final String 監査Id;
        private final String グループ参照番号;
        private final String 会社コード;
        private final String ローカル取引番号;
        private final String 監査状態区分;

        private 監査定義(String 監査Id, String グループ参照番号, String 会社コード, String ローカル取引番号, String 監査状態区分) {
            this.監査Id = 監査Id;
            this.グループ参照番号 = グループ参照番号;
            this.会社コード = 会社コード;
            this.ローカル取引番号 = ローカル取引番号;
            this.監査状態区分 = 監査状態区分;
        }
    }

    private static final class 会社別集計 {
        private final String 会社コード;
        private int 取引件数;
        private int 監査件数;
        private int 不一致件数;
        private int 否認件数;
        private int 保留件数;
        private int 未確定集計件数;
        private int 状態不明件数;

        private 会社別集計(String 会社コード) {
            this.会社コード = 会社コード;
        }
    }

    private static final class 帳票レコード {
        private final String 帳票Id;
        private final String 帳票種別区分;
        private final String 対象月;
        private final String 出力状態区分;
        private final String 作成日時;

        private 帳票レコード(String 帳票Id, String 帳票種別区分, String 対象月, String 出力状態区分, String 作成日時) {
            this.帳票Id = 帳票Id;
            this.帳票種別区分 = 帳票種別区分;
            this.対象月 = 対象月;
            this.出力状態区分 = 出力状態区分;
            this.作成日時 = 作成日時;
        }
    }
}
