public class MerchantInquiryService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2023/11/30  開発一課  初版作成
     * 1.01  2024/09/12  保守二課  権限別の口座マスクと例外理由表示を追加
     */

    private static final String 権限_監査 = "AUDIT";
    private static final String 権限_運用 = "OPER";
    private static final String 状態_未処理 = "OPEN";
    private static final String 状態_保留 = "HOLD";

    public static void main(String[] a) {
        String 加盟店コード = a.length > 0 ? a[0] : "M000001";
        String 利用者権限 = a.length > 1 ? a[1] : 権限_運用;

        照会結果 dto = 照会する(
                加盟店コード,
                利用者権限,
                標準加盟店(),
                標準日次集計(),
                標準精算(),
                標準例外());

        System.out.println(dto.toOperatorText());
    }

    private static 照会結果 照会する(
            String 加盟店コード,
            String 利用者権限,
            加盟店[] 加盟店群,
            日次集計[] 日次集計群,
            精算[] 精算群,
            例外明細[] 例外群) {

        検証する(加盟店コード, 利用者権限, 加盟店群, 日次集計群, 精算群, 例外群);

        加盟店 対象加盟店 = null;
        for (加盟店 rec : 加盟店群) {
            if (加盟店コード.equals(rec.merchantCode)) {
                対象加盟店 = rec;
                break;
            }
        }
        if (対象加盟店 == null) {
            throw new IllegalArgumentException("加盟店コードが存在しません: " + 加盟店コード);
        }

        日次集計 最新集計 = null;
        long 売上件数合計 = 0L;
        long 売上金額合計 = 0L;
        long 返品金額合計 = 0L;
        for (日次集計 rec : 日次集計群) {
            if (!加盟店コード.equals(rec.merchantCode)) {
                continue;
            }
            if (最新集計 == null || rec.summaryDt.compareTo(最新集計.summaryDt) > 0) {
                最新集計 = rec;
            }
            売上件数合計 += rec.saleCount;
            売上金額合計 += rec.saleAmt;
            返品金額合計 += rec.returnAmt;
        }

        精算 最新精算 = null;
        long 保留件数 = 0L;
        long 精算総額合計 = 0L;
        long 精算純額合計 = 0L;
        long 調整額合計 = 0L;
        for (精算 rec : 精算群) {
            if (!加盟店コード.equals(rec.merchantCode)) {
                continue;
            }
            if (最新精算 == null || rec.settleDt.compareTo(最新精算.settleDt) > 0) {
                最新精算 = rec;
            }
            if (状態_保留.equals(rec.settleStatus)) {
                保留件数++;
            }
            精算総額合計 += rec.grossAmt;
            精算純額合計 += rec.netAmt;
            調整額合計 += rec.adjAmt;
        }

        int 未処理例外件数 = 0;
        String 代表例外理由 = "";
        for (例外明細 rec : 例外群) {
            if (!状態_未処理.equals(rec.actionStatus)) {
                continue;
            }
            if (rec.saleId != null && rec.saleId.startsWith(加盟店コード + "-")) {
                未処理例外件数++;
                if (代表例外理由.length() == 0) {
                    代表例外理由 = rec.reasonCd;
                }
            }
        }

        boolean 詳細表示可 = 権限_監査.equals(利用者権限);
        String 精算状態表示 = 最新精算 == null ? "精算なし" : 最新精算.settleStatus;
        String 例外理由表示 = 代表例外理由.length() == 0 ? "該当なし" : 代表例外理由;
        if (!詳細表示可) {
            if (状態_保留.equals(精算状態表示)) {
                精算状態表示 = "要確認";
            }
            if (!"該当なし".equals(例外理由表示)) {
                例外理由表示 = "権限外";
            }
        }

        return new 照会結果(
                対象加盟店.merchantCode,
                対象加盟店.merchantNameKana,
                対象加盟店.settleBankCd,
                maskAccount(対象加盟店.settleAccountNo, 詳細表示可),
                対象加盟店.merchantStatus,
                対象加盟店.feePlanCd,
                最新集計 == null ? "" : 最新集計.summaryDt,
                最新集計 == null ? "" : 最新集計.currencyCd,
                売上件数合計,
                売上金額合計,
                返品金額合計,
                最新精算 == null ? "" : 最新精算.settleDt,
                精算総額合計,
                精算純額合計,
                調整額合計,
                精算状態表示,
                保留件数,
                未処理例外件数,
                例外理由表示);
    }

    private static void 検証する(
            String 加盟店コード,
            String 利用者権限,
            加盟店[] 加盟店群,
            日次集計[] 日次集計群,
            精算[] 精算群,
            例外明細[] 例外群) {

        if (加盟店コード == null || !加盟店コード.matches("M[0-9]{6}")) {
            throw new IllegalArgumentException("加盟店コード形式不正");
        }
        if (!権限_監査.equals(利用者権限) && !権限_運用.equals(利用者権限)) {
            throw new IllegalArgumentException("利用者権限不正");
        }
        if (加盟店群 == null || 日次集計群 == null || 精算群 == null || 例外群 == null) {
            throw new IllegalArgumentException("入力ファイル未設定");
        }
    }

    private static String maskAccount(String accountNo, boolean 詳細表示可) {
        if (詳細表示可 || accountNo == null || accountNo.length() <= 3) {
            return accountNo;
        }
        String 下三桁 = accountNo.substring(accountNo.length() - 3);
        return "****" + 下三桁;
    }

    private static 加盟店[] 標準加盟店() {
        return new 加盟店[] {
                new 加盟店("M000001", "トウキヨウシヨウテン", "0005", "1234567", "ACTIVE", "FP01"),
                new 加盟店("M000002", "オオサカデンキ", "0009", "7654321", "ACTIVE", "FP02")
        };
    }

    private static 日次集計[] 標準日次集計() {
        return new 日次集計[] {
                new 日次集計("M000001-20260625-JPY", "20260625", "M000001", "JPY", 18, 452000, 12000),
                new 日次集計("M000001-20260626-JPY", "20260626", "M000001", "JPY", 24, 681000, 0),
                new 日次集計("M000002-20260626-JPY", "20260626", "M000002", "JPY", 7, 98000, 5000)
        };
    }

    private static 精算[] 標準精算() {
        return new 精算[] {
                new 精算("S20260625001", "M000001", "20260627", 440000, 427000, -13000, "DONE"),
                new 精算("S20260626001", "M000001", "20260628", 681000, 0, 0, "HOLD"),
                new 精算("S20260626002", "M000002", "20260628", 93000, 90100, -2900, "DONE")
        };
    }

    private static 例外明細[] 標準例外() {
        return new 例外明細[] {
                new 例外明細("E000001", "M000001-S000045", "411111******1111", "金額相違", "売上照合", "20260628", "OPEN"),
                new 例外明細("E000002", "M000001-S000051", "555555******4444", "承認取消未着", "承認照合", "20260628", "OPEN"),
                new 例外明細("E000003", "M000002-S000010", "356600******0000", "返品過大", "返品照合", "20260627", "CLOSED")
        };
    }

    private static final class 加盟店 {
        final String merchantCode;
        final String merchantNameKana;
        final String settleBankCd;
        final String settleAccountNo;
        final String merchantStatus;
        final String feePlanCd;

        加盟店(String merchantCode, String merchantNameKana, String settleBankCd,
              String settleAccountNo, String merchantStatus, String feePlanCd) {
            this.merchantCode = merchantCode;
            this.merchantNameKana = merchantNameKana;
            this.settleBankCd = settleBankCd;
            this.settleAccountNo = settleAccountNo;
            this.merchantStatus = merchantStatus;
            this.feePlanCd = feePlanCd;
        }
    }

    private static final class 日次集計 {
        final String summaryKey;
        final String summaryDt;
        final String merchantCode;
        final String currencyCd;
        final long saleCount;
        final long saleAmt;
        final long returnAmt;

        日次集計(String summaryKey, String summaryDt, String merchantCode, String currencyCd,
             long saleCount, long saleAmt, long returnAmt) {
            this.summaryKey = summaryKey;
            this.summaryDt = summaryDt;
            this.merchantCode = merchantCode;
            this.currencyCd = currencyCd;
            this.saleCount = saleCount;
            this.saleAmt = saleAmt;
            this.returnAmt = returnAmt;
        }
    }

    private static final class 精算 {
        final String settlementId;
        final String merchantCode;
        final String settleDt;
        final long grossAmt;
        final long netAmt;
        final long adjAmt;
        final String settleStatus;

        精算(String settlementId, String merchantCode, String settleDt,
           long grossAmt, long netAmt, long adjAmt, String settleStatus) {
            this.settlementId = settlementId;
            this.merchantCode = merchantCode;
            this.settleDt = settleDt;
            this.grossAmt = grossAmt;
            this.netAmt = netAmt;
            this.adjAmt = adjAmt;
            this.settleStatus = settleStatus;
        }
    }

    private static final class 例外明細 {
        final String exceptionId;
        final String saleId;
        final String cardNo;
        final String reasonCd;
        final String detectedPgm;
        final String exceptionDt;
        final String actionStatus;

        例外明細(String exceptionId, String saleId, String cardNo, String reasonCd,
             String detectedPgm, String exceptionDt, String actionStatus) {
            this.exceptionId = exceptionId;
            this.saleId = saleId;
            this.cardNo = cardNo;
            this.reasonCd = reasonCd;
            this.detectedPgm = detectedPgm;
            this.exceptionDt = exceptionDt;
            this.actionStatus = actionStatus;
        }
    }

    private static final class 照会結果 {
        final String merchantCode;
        final String merchantNameKana;
        final String settleBankCd;
        final String settleAccountNo;
        final String merchantStatus;
        final String feePlanCd;
        final String latestSummaryDt;
        final String currencyCd;
        final long saleCount;
        final long saleAmt;
        final long returnAmt;
        final String latestSettleDt;
        final long grossAmt;
        final long netAmt;
        final long adjAmt;
        final String settleStatus;
        final long holdCount;
        final int openExceptionCount;
        final String exceptionReason;

        照会結果(String merchantCode, String merchantNameKana, String settleBankCd,
             String settleAccountNo, String merchantStatus, String feePlanCd,
             String latestSummaryDt, String currencyCd, long saleCount, long saleAmt,
             long returnAmt, String latestSettleDt, long grossAmt, long netAmt,
             long adjAmt, String settleStatus, long holdCount, int openExceptionCount,
             String exceptionReason) {
            this.merchantCode = merchantCode;
            this.merchantNameKana = merchantNameKana;
            this.settleBankCd = settleBankCd;
            this.settleAccountNo = settleAccountNo;
            this.merchantStatus = merchantStatus;
            this.feePlanCd = feePlanCd;
            this.latestSummaryDt = latestSummaryDt;
            this.currencyCd = currencyCd;
            this.saleCount = saleCount;
            this.saleAmt = saleAmt;
            this.returnAmt = returnAmt;
            this.latestSettleDt = latestSettleDt;
            this.grossAmt = grossAmt;
            this.netAmt = netAmt;
            this.adjAmt = adjAmt;
            this.settleStatus = settleStatus;
            this.holdCount = holdCount;
            this.openExceptionCount = openExceptionCount;
            this.exceptionReason = exceptionReason;
        }

        String toOperatorText() {
            return "加盟店照会結果"
                    + "\n加盟店コード=" + merchantCode
                    + "\n加盟店名カナ=" + merchantNameKana
                    + "\n精算銀行=" + settleBankCd
                    + "\n精算口座=" + settleAccountNo
                    + "\n加盟店状態=" + merchantStatus
                    + "\n手数料プラン=" + feePlanCd
                    + "\n最新集計日=" + latestSummaryDt
                    + "\n通貨=" + currencyCd
                    + "\n売上件数=" + saleCount
                    + "\n売上金額=" + saleAmt
                    + "\n返品金額=" + returnAmt
                    + "\n最新精算日=" + latestSettleDt
                    + "\n精算総額=" + grossAmt
                    + "\n精算純額=" + netAmt
                    + "\n調整額=" + adjAmt
                    + "\n精算状態=" + settleStatus
                    + "\n精算保留件数=" + holdCount
                    + "\n未処理例外件数=" + openExceptionCount
                    + "\n代表例外理由=" + exceptionReason;
        }
    }
}
