package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2025/06/29  共通基盤    月次監査締めサービスの初版作成
 */
public class MonthlyAuditCloseService {
    private static final String 対象月 = "2025-05";

    private static final String 集計_仮締 = "1";
    private static final String 集計_確定 = "9";
    private static final String 集計_却下 = "8";

    private static final String エラー_未解消 = "0";
    private static final String エラー_解消済 = "9";

    private static final String 事象_監査 = "AUD";
    private static final String 事象_締め = "CLS";
    private static final String 事象_却下 = "REJ";

    private static final String 記録_有効 = "1";

    private MonthlyAuditCloseService() {
    }

    public static void main(String[] a) {
        java.util.List<月次集計> camonf = 作成CAMONF();
        java.util.List<取込エラー> cmerrf = 作成CMERRF();
        java.util.List<監査仕訳> cajrnf = 作成CAJRNF();

        long 次順序 = 次仕訳番号(cajrnf);

        for (月次集計 集計 : camonf) {
            if (!対象月.equals(集計.summaryMonth)) {
                continue;
            }

            判定結果 判定 = 締め判定(集計, cmerrf, cajrnf);

            if (集計_確定.equals(集計.summaryStatusKbn)) {
                cajrnf.add(new 監査仕訳(次順序++, 監査ID(集計), 集計.companyCode,
                        事象_締め, 記録_有効));
                System.out.println("会社=" + 集計.companyCode + " 月=" + 集計.summaryMonth
                        + " 既存確定を保護しました");
                continue;
            }

            if (判定.締め可能) {
                集計.summaryStatusKbn = 集計_確定;
                cajrnf.add(new 監査仕訳(次順序++, 監査ID(集計), 集計.companyCode,
                        事象_締め, 記録_有効));
                System.out.println("会社=" + 集計.companyCode + " 月=" + 集計.summaryMonth
                        + " 確定しました");
            } else {
                集計.summaryStatusKbn = 集計_却下;
                cajrnf.add(new 監査仕訳(次順序++, 監査ID(集計), 集計.companyCode,
                        事象_却下, 記録_有効));
                System.out.println("会社=" + 集計.companyCode + " 月=" + 集計.summaryMonth
                        + " 却下理由=" + 判定.理由);
            }
        }

        System.out.println("CAMONF件数=" + camonf.size() + " CAJRNF件数=" + cajrnf.size());
    }

    private static 判定結果 締め判定(月次集計 集計, java.util.List<取込エラー> cmerrf,
            java.util.List<監査仕訳> cajrnf) {
        int 未解消件数 = 0;
        for (取込エラー エラー : cmerrf) {
            if (集計.companyCode.equals(エラー.companyCode)
                    && !エラー_解消済.equals(エラー.errorStatusKbn)) {
                未解消件数++;
            }
        }
        if (未解消件数 > 0) {
            return new 判定結果(false, "未解消エラー=" + 未解消件数);
        }

        if (集計.mismatchCount > 0) {
            return new 判定結果(false, "不一致件数=" + 集計.mismatchCount);
        }

        if (集計.txnCount != 集計.auditCount) {
            return new 判定結果(false, "取引件数=" + 集計.txnCount + "/監査件数=" + 集計.auditCount);
        }

        int 監査記録件数 = 0;
        for (監査仕訳 仕訳 : cajrnf) {
            if (集計.companyCode.equals(仕訳.groupRefNo)
                    && 事象_監査.equals(仕訳.eventTypeKbn)
                    && 記録_有効.equals(仕訳.journalStatusKbn)) {
                監査記録件数++;
            }
        }
        if (監査記録件数 < 集計.auditCount) {
            return new 判定結果(false, "未監査イベント=" + (集計.auditCount - 監査記録件数));
        }

        if (!集計_仮締.equals(集計.summaryStatusKbn)) {
            return new 判定結果(false, "状態区分=" + 集計.summaryStatusKbn);
        }

        return new 判定結果(true, "締め可能");
    }

    private static long 次仕訳番号(java.util.List<監査仕訳> cajrnf) {
        long 最大 = 0L;
        for (監査仕訳 仕訳 : cajrnf) {
            if (仕訳.journalSeq > 最大) {
                最大 = 仕訳.journalSeq;
            }
        }
        return 最大 + 1L;
    }

    private static String 監査ID(月次集計 集計) {
        return 集計.summaryMonth.replace("-", "") + "-" + 集計.companyCode;
    }

    private static java.util.List<月次集計> 作成CAMONF() {
        java.util.List<月次集計> rows = new java.util.ArrayList<月次集計>();
        rows.add(new 月次集計("2025-05", "1001", 12840, 12840, 0, 集計_仮締));
        rows.add(new 月次集計("2025-05", "1002", 7310, 7308, 2, 集計_仮締));
        rows.add(new 月次集計("2025-05", "1003", 4590, 4590, 0, 集計_仮締));
        rows.add(new 月次集計("2025-05", "1004", 9100, 9100, 0, 集計_確定));
        rows.add(new 月次集計("2025-04", "1001", 12650, 12650, 0, 集計_確定));
        return rows;
    }

    private static java.util.List<取込エラー> 作成CMERRF() {
        java.util.List<取込エラー> rows = new java.util.ArrayList<取込エラー>();
        rows.add(new 取込エラー("E26050001", "B260501", "1002", "L0007731", "A214", エラー_未解消));
        rows.add(new 取込エラー("E26050002", "B260503", "1003", "L0002188", "W090", エラー_解消済));
        rows.add(new 取込エラー("E26040011", "B260401", "1001", "L0000412", "A214", エラー_解消済));
        return rows;
    }

    private static java.util.List<監査仕訳> 作成CAJRNF() {
        java.util.List<監査仕訳> rows = new java.util.ArrayList<監査仕訳>();
        監査仕訳追加(rows, 1L, "202505-1001", "1001", 12840);
        監査仕訳追加(rows, 12841L, "202505-1002", "1002", 7308);
        監査仕訳追加(rows, 20149L, "202505-1003", "1003", 4587);
        監査仕訳追加(rows, 24736L, "202505-1004", "1004", 9100);
        rows.add(new 監査仕訳(33836L, "202504-1001", "1001", 事象_締め, 記録_有効));
        return rows;
    }

    private static void 監査仕訳追加(java.util.List<監査仕訳> rows, long 開始順序,
            String auditId, String companyCode, int 件数) {
        for (int i = 0; i < 件数; i++) {
            rows.add(new 監査仕訳(開始順序 + i, auditId, companyCode, 事象_監査, 記録_有効));
        }
    }

    private static final class 判定結果 {
        private final boolean 締め可能;
        private final String 理由;

        private 判定結果(boolean 締め可能, String 理由) {
            this.締め可能 = 締め可能;
            this.理由 = 理由;
        }
    }

    private static final class 月次集計 {
        private final String summaryMonth;
        private final String companyCode;
        private final int txnCount;
        private final int auditCount;
        private final int mismatchCount;
        private String summaryStatusKbn;

        private 月次集計(String summaryMonth, String companyCode, int txnCount, int auditCount,
                int mismatchCount, String summaryStatusKbn) {
            this.summaryMonth = summaryMonth;
            this.companyCode = companyCode;
            this.txnCount = txnCount;
            this.auditCount = auditCount;
            this.mismatchCount = mismatchCount;
            this.summaryStatusKbn = summaryStatusKbn;
        }
    }

    private static final class 取込エラー {
        private final String errorId;
        private final String importBatchId;
        private final String companyCode;
        private final String localTxnNo;
        private final String errorCode;
        private final String errorStatusKbn;

        private 取込エラー(String errorId, String importBatchId, String companyCode,
                String localTxnNo, String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }
    }

    private static final class 監査仕訳 {
        private final long journalSeq;
        private final String auditId;
        private final String groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private 監査仕訳(long journalSeq, String auditId, String groupRefNo,
                String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }
}
