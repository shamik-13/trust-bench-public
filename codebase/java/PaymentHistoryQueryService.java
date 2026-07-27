public class PaymentHistoryQueryService {
    /**
     * 変更履歴
     * 版数    年月日      担当      概要
     * 1.00    2024/02/14  開発担当  入金履歴照会サービス初版作成。CDPAYF/CDAPPF/CDHISTFを突合し利用者区分別にマスク表示する。
     */

    private static final java.time.format.DateTimeFormatter 日付形式 =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    private static final java.util.Set<String> 利用者区分一覧 =
            new java.util.HashSet<String>(java.util.Arrays.asList("顧客", "社内", "監査"));

    public static void main(String[] a) {
        String カード番号 = a.length > 0 ? a[0] : "3540123412340001";
        String 開始日 = a.length > 1 ? a[1] : "20260601";
        String 終了日 = a.length > 2 ? a[2] : "20260630";
        String payId = a.length > 3 ? a[3] : "";
        String 利用者区分 = a.length > 4 ? a[4] : "顧客";

        try {
            java.util.List<履歴Dto> 履歴 = 検索(カード番号, 開始日, 終了日, payId, 利用者区分);
            for (履歴Dto dto : 履歴) {
                System.out.println(dto);
            }
        } catch (IllegalArgumentException e) {
            System.out.println("入力エラー: " + e.getMessage());
        }
    }

    private static java.util.List<履歴Dto> 検索(
            String カード番号,
            String 開始日文字列,
            String 終了日文字列,
            String payId条件,
            String 利用者区分) {

        検証結果 条件 = 要求検証(カード番号, 開始日文字列, 終了日文字列, payId条件, 利用者区分);

        java.util.Map<String, CDPAYF> 入金Map = new java.util.HashMap<String, CDPAYF>();
        for (CDPAYF r : cdpayf読込()) {
            if (対象入金(r, 条件)) {
                入金Map.put(r.payId, r);
            }
        }

        java.util.Map<String, CDAPPF> 消込Map = new java.util.HashMap<String, CDAPPF>();
        for (CDAPPF r : cdappf読込()) {
            if (対象消込(r, 条件)) {
                消込Map.put(r.payId, r);
            }
        }

        java.util.Map<String, CDHISTF> イベントMap = new java.util.HashMap<String, CDHISTF>();
        for (CDHISTF r : cdhistf読込()) {
            if (!対象履歴(r, 条件)) {
                continue;
            }
            String 重複キー = r.cardNo + "|" + r.payId + "|" + r.eventSeq;
            CDHISTF 既存 = イベントMap.get(重複キー);
            if (既存 == null || r.eventDt.compareTo(既存.eventDt) < 0) {
                イベントMap.put(重複キー, r);
            }
        }

        java.util.List<履歴Dto> 結果 = new java.util.ArrayList<履歴Dto>();

        for (CDPAYF pay : 入金Map.values()) {
            CDAPPF app = 消込Map.get(pay.payId);
            int 消込額 = app == null ? 0 : app.appliedFeeAmt + app.appliedIntAmt + app.appliedPrinAmt;
            int 残額 = app == null ? pay.payAmt : app.remainAmt;
            String 状態 = app == null ? "未消込" : 消込状態名(app.appStatus);
            String 理由 = app == null ? "CDAPPF未到着" : "PGM=" + app.programId + ",STATUS=" + app.appStatus;

            結果.add(new 履歴Dto(
                    pay.payDt,
                    pay.payId,
                    マスクカード(pay.cardNo, 条件.利用者区分),
                    "入金受付",
                    pay.payAmt,
                    入金方法名(pay.payMethod),
                    消込額,
                    残額,
                    状態,
                    マスク理由(理由, 条件.利用者区分)));
        }

        for (CDHISTF ev : イベントMap.values()) {
            CDPAYF pay = 入金Map.get(ev.payId);
            CDAPPF app = 消込Map.get(ev.payId);
            String 方法 = pay == null ? "不明" : 入金方法名(pay.payMethod);
            int 消込額 = app == null ? 0 : app.appliedFeeAmt + app.appliedIntAmt + app.appliedPrinAmt;
            int 残額 = app == null ? 0 : app.remainAmt;
            String 状態 = app == null ? "履歴単独" : 消込状態名(app.appStatus);
            String 理由 = "SRC=" + ev.sourceProgram + ",SEQ=" + ev.eventSeq;

            結果.add(new 履歴Dto(
                    ev.eventDt,
                    ev.payId,
                    マスクカード(ev.cardNo, 条件.利用者区分),
                    イベント名(ev.eventType),
                    ev.eventAmt,
                    方法,
                    消込額,
                    残額,
                    状態,
                    マスク理由(理由, 条件.利用者区分)));
        }

        java.util.Collections.sort(結果, new java.util.Comparator<履歴Dto>() {
            public int compare(履歴Dto l, 履歴Dto r) {
                int d = l.eventDt.compareTo(r.eventDt);
                if (d != 0) {
                    return d;
                }
                int p = l.payId.compareTo(r.payId);
                if (p != 0) {
                    return p;
                }
                return l.eventType.compareTo(r.eventType);
            }
        });

        return 結果;
    }

    private static 検証結果 要求検証(
            String カード番号,
            String 開始日文字列,
            String 終了日文字列,
            String payId条件,
            String 利用者区分) {

        if (カード番号 == null || !カード番号.matches("[0-9]{14,16}")) {
            throw new IllegalArgumentException("カード番号は14桁から16桁の数字で指定してください");
        }
        if (!luhn確認(カード番号)) {
            throw new IllegalArgumentException("カード番号の検査桁が不正です");
        }
        java.time.LocalDate 開始日 = 日付変換(開始日文字列, "開始日");
        java.time.LocalDate 終了日 = 日付変換(終了日文字列, "終了日");
        if (終了日.isBefore(開始日)) {
            throw new IllegalArgumentException("終了日は開始日以降で指定してください");
        }
        if (開始日.plusMonths(13).isBefore(終了日)) {
            throw new IllegalArgumentException("検索期間は13か月以内で指定してください");
        }
        String payId = payId条件 == null ? "" : payId条件.trim();
        if (!payId.isEmpty() && !payId.matches("PAY[0-9]{8}")) {
            throw new IllegalArgumentException("PAY-ID形式が不正です");
        }
        if (!利用者区分一覧.contains(利用者区分)) {
            throw new IllegalArgumentException("利用者区分が不正です");
        }
        return new 検証結果(カード番号, 開始日, 終了日, payId, 利用者区分);
    }

    private static java.time.LocalDate 日付変換(String s, String 名称) {
        if (s == null || !s.matches("[0-9]{8}")) {
            throw new IllegalArgumentException(名称 + "はYYYYMMDDで指定してください");
        }
        try {
            return java.time.LocalDate.parse(s, 日付形式);
        } catch (java.time.DateTimeException e) {
            throw new IllegalArgumentException(名称 + "の日付が不正です");
        }
    }

    private static boolean luhn確認(String s) {
        int 合計 = 0;
        boolean 二倍 = false;
        for (int i = s.length() - 1; i >= 0; i--) {
            int n = s.charAt(i) - '0';
            if (二倍) {
                n *= 2;
                if (n > 9) {
                    n -= 9;
                }
            }
            合計 += n;
            二倍 = !二倍;
        }
        return 合計 % 10 == 0;
    }

    private static boolean 対象入金(CDPAYF r, 検証結果 c) {
        return r.cardNo.equals(c.カード番号)
                && 範囲内(r.payDt, c)
                && (c.payId条件.isEmpty() || r.payId.equals(c.payId条件));
    }

    private static boolean 対象消込(CDAPPF r, 検証結果 c) {
        return r.cardNo.equals(c.カード番号)
                && (c.payId条件.isEmpty() || r.payId.equals(c.payId条件));
    }

    private static boolean 対象履歴(CDHISTF r, 検証結果 c) {
        return r.cardNo.equals(c.カード番号)
                && 範囲内(r.eventDt, c)
                && (c.payId条件.isEmpty() || r.payId.equals(c.payId条件));
    }

    private static boolean 範囲内(java.time.LocalDate d, 検証結果 c) {
        return !d.isBefore(c.開始日) && !d.isAfter(c.終了日);
    }

    private static String マスクカード(String cardNo, String 利用者区分) {
        if ("社内".equals(利用者区分) || "監査".equals(利用者区分)) {
            return cardNo;
        }
        return "************" + cardNo.substring(cardNo.length() - 4);
    }

    private static String マスク理由(String 理由, String 利用者区分) {
        return "社内".equals(利用者区分) || "監査".equals(利用者区分) ? 理由 : "非表示";
    }

    private static String 入金方法名(String code) {
        if ("10".equals(code)) {
            return "口座振替";
        }
        if ("20".equals(code)) {
            return "振込";
        }
        if ("30".equals(code)) {
            return "コンビニ";
        }
        return "不明";
    }

    private static String 消込状態名(String code) {
        if ("F".equals(code)) {
            return "完済";
        }
        if ("P".equals(code)) {
            return "一部消込";
        }
        if ("O".equals(code)) {
            return "過入金";
        }
        if ("S".equals(code)) {
            return "対象外";
        }
        return "不明";
    }

    private static String イベント名(String code) {
        if ("ACCEPT".equals(code)) {
            return "入金受付";
        }
        if ("APPLY".equals(code)) {
            return "消込結果";
        }
        if ("CANCEL".equals(code)) {
            return "取消";
        }
        if ("REFUND".equals(code)) {
            return "返金";
        }
        return "その他";
    }

    private static java.util.List<CDPAYF> cdpayf読込() {
        java.util.List<CDPAYF> list = new java.util.ArrayList<CDPAYF>();
        list.add(new CDPAYF("PAY20260601", "3540123412340001", 120000, 日付変換("20260603", "PAY-DT"), "20"));
        list.add(new CDPAYF("PAY20260602", "3540123412340001", 50000, 日付変換("20260610", "PAY-DT"), "30"));
        list.add(new CDPAYF("PAY20260603", "3540123412340001", 180000, 日付変換("20260622", "PAY-DT"), "10"));
        list.add(new CDPAYF("PAY20260604", "4532123412340006", 90000, 日付変換("20260611", "PAY-DT"), "20"));
        return list;
    }

    private static java.util.List<CDAPPF> cdappf読込() {
        java.util.List<CDAPPF> list = new java.util.ArrayList<CDAPPF>();
        list.add(new CDAPPF("PAY20260601", "3540123412340001", 1000, 9000, 110000, 0, "F", "CDAP100"));
        list.add(new CDAPPF("PAY20260602", "3540123412340001", 500, 4500, 25000, 20000, "P", "CDAP100"));
        list.add(new CDAPPF("PAY20260603", "3540123412340001", 0, 0, 170000, 10000, "O", "CDAP210"));
        list.add(new CDAPPF("PAY20260604", "4532123412340006", 0, 0, 0, 90000, "S", "CDAP310"));
        return list;
    }

    private static java.util.List<CDHISTF> cdhistf読込() {
        java.util.List<CDHISTF> list = new java.util.ArrayList<CDHISTF>();
        list.add(new CDHISTF("3540123412340001", "PAY20260601", 1, "ACCEPT", 120000, 日付変換("20260603", "EVENT-DT"), "CDPAY010"));
        list.add(new CDHISTF("3540123412340001", "PAY20260601", 2, "APPLY", 120000, 日付変換("20260604", "EVENT-DT"), "CDAP100"));
        list.add(new CDHISTF("3540123412340001", "PAY20260601", 2, "APPLY", 120000, 日付変換("20260605", "EVENT-DT"), "CDAP100"));
        list.add(new CDHISTF("3540123412340001", "PAY20260602", 1, "ACCEPT", 50000, 日付変換("20260610", "EVENT-DT"), "CDPAY010"));
        list.add(new CDHISTF("3540123412340001", "PAY20260602", 2, "APPLY", 30000, 日付変換("20260611", "EVENT-DT"), "CDAP100"));
        list.add(new CDHISTF("3540123412340001", "PAY20260602", 3, "CANCEL", -5000, 日付変換("20260612", "EVENT-DT"), "CDCN020"));
        list.add(new CDHISTF("3540123412340001", "PAY20260603", 1, "ACCEPT", 180000, 日付変換("20260622", "EVENT-DT"), "CDPAY010"));
        list.add(new CDHISTF("3540123412340001", "PAY20260603", 2, "REFUND", -10000, 日付変換("20260624", "EVENT-DT"), "CDRF030"));
        return list;
    }

    private static final class 検証結果 {
        final String カード番号;
        final java.time.LocalDate 開始日;
        final java.time.LocalDate 終了日;
        final String payId条件;
        final String 利用者区分;

        検証結果(String カード番号, java.time.LocalDate 開始日, java.time.LocalDate 終了日, String payId条件, String 利用者区分) {
            this.カード番号 = カード番号;
            this.開始日 = 開始日;
            this.終了日 = 終了日;
            this.payId条件 = payId条件;
            this.利用者区分 = 利用者区分;
        }
    }

    private static final class CDPAYF {
        final String payId;
        final String cardNo;
        final int payAmt;
        final java.time.LocalDate payDt;
        final String payMethod;

        CDPAYF(String payId, String cardNo, int payAmt, java.time.LocalDate payDt, String payMethod) {
            this.payId = payId;
            this.cardNo = cardNo;
            this.payAmt = payAmt;
            this.payDt = payDt;
            this.payMethod = payMethod;
        }
    }

    private static final class CDAPPF {
        final String payId;
        final String cardNo;
        final int appliedFeeAmt;
        final int appliedIntAmt;
        final int appliedPrinAmt;
        final int remainAmt;
        final String appStatus;
        final String programId;

        CDAPPF(String payId, String cardNo, int appliedFeeAmt, int appliedIntAmt, int appliedPrinAmt, int remainAmt, String appStatus, String programId) {
            this.payId = payId;
            this.cardNo = cardNo;
            this.appliedFeeAmt = appliedFeeAmt;
            this.appliedIntAmt = appliedIntAmt;
            this.appliedPrinAmt = appliedPrinAmt;
            this.remainAmt = remainAmt;
            this.appStatus = appStatus;
            this.programId = programId;
        }
    }

    private static final class CDHISTF {
        final String cardNo;
        final String payId;
        final int eventSeq;
        final String eventType;
        final int eventAmt;
        final java.time.LocalDate eventDt;
        final String sourceProgram;

        CDHISTF(String cardNo, String payId, int eventSeq, String eventType, int eventAmt, java.time.LocalDate eventDt, String sourceProgram) {
            this.cardNo = cardNo;
            this.payId = payId;
            this.eventSeq = eventSeq;
            this.eventType = eventType;
            this.eventAmt = eventAmt;
            this.eventDt = eventDt;
            this.sourceProgram = sourceProgram;
        }
    }

    private static final class 履歴Dto {
        final java.time.LocalDate eventDt;
        final String payId;
        final String cardNo;
        final String eventType;
        final int eventAmt;
        final String payMethod;
        final int appliedAmt;
        final int remainAmt;
        final String appStatus;
        final String reasonCode;

        履歴Dto(
                java.time.LocalDate eventDt,
                String payId,
                String cardNo,
                String eventType,
                int eventAmt,
                String payMethod,
                int appliedAmt,
                int remainAmt,
                String appStatus,
                String reasonCode) {
            this.eventDt = eventDt;
            this.payId = payId;
            this.cardNo = cardNo;
            this.eventType = eventType;
            this.eventAmt = eventAmt;
            this.payMethod = payMethod;
            this.appliedAmt = appliedAmt;
            this.remainAmt = remainAmt;
            this.appStatus = appStatus;
            this.reasonCode = reasonCode;
        }

        public String toString() {
            return eventDt.format(日付形式)
                    + ",PAY-ID=" + payId
                    + ",CARD-NO=" + cardNo
                    + ",種別=" + eventType
                    + ",金額=" + eventAmt
                    + ",方法=" + payMethod
                    + ",消込額=" + appliedAmt
                    + ",残額=" + remainAmt
                    + ",状態=" + appStatus
                    + ",理由=" + reasonCode;
        }
    }
}
