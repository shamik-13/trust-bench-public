package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2024-08-12  みらいペイ システム部    ホールド失効候補抽出バッチ初版
 */
public class HoldExpiryBatchJob {
    private static final String HD_HOLD_RESULT_ACTIVE = "00";
    private static final String HD_HOLD_RESULT_CAPTURED = "30";
    private static final String TXN_STATUS_CAPTURED = "30";
    private static final String NOTICE_KBN_HOLD_EXPIRE = "HLD";
    private static final String SEND_STATUS_UNSENT = "10";
    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] a) {
        java.time.LocalDate businessDate = java.time.LocalDate.of(2026, 6, 28);
        java.time.LocalDateTime createTs = java.time.LocalDateTime.of(2026, 6, 28, 2, 15, 0);

        java.util.List<java.util.Map<String, String>> pyholdf = loadPyholdf();
        java.util.List<java.util.Map<String, String>> pytxnf = loadPytxnf();

        java.util.List<java.util.Map<String, String>> expiredHolds =
                extractExpiredHoldCandidates(pyholdf, pytxnf, businessDate);

        java.util.List<java.util.Map<String, String>> pyntff =
                buildNoticeFile(expiredHolds, createTs);

        printMerchantSummary(expiredHolds, businessDate);
        printPyntff(pyntff);
    }

    private static java.util.List<java.util.Map<String, String>> loadPyholdf() {
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<>();
        rows.add(row("HOLD-ID", "HLD-20260620-0001", "WALLET-ID", "WL-100001", "HOLD-AMT", "12500",
                "HOLD-RESULT", "00", "MERCHANT-CODE", "M10001", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-06-27"));
        rows.add(row("HOLD-ID", "HLD-20260620-0002", "WALLET-ID", "WL-100002", "HOLD-AMT", "7800",
                "HOLD-RESULT", "30", "MERCHANT-CODE", "M10002", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-06-27"));
        rows.add(row("HOLD-ID", "HLD-20260621-0003", "WALLET-ID", "WL-100003", "HOLD-AMT", "4200",
                "HOLD-RESULT", "00", "MERCHANT-CODE", "M10003", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-06-29"));
        rows.add(row("HOLD-ID", "HLD-20260619-0004", "WALLET-ID", "WL-100004", "HOLD-AMT", "31800",
                "HOLD-RESULT", "00", "MERCHANT-CODE", "M10004", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-06-25"));
        rows.add(row("HOLD-ID", "HLD-20260618-0005", "WALLET-ID", "WL-100005", "HOLD-AMT", "1980",
                "HOLD-RESULT", "20", "MERCHANT-CODE", "M10005", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-06-24"));
        rows.add(row("HOLD-ID", "HLD-20260618-0006", "WALLET-ID", "WL-100006", "HOLD-AMT", "15800",
                "HOLD-RESULT", "00", "MERCHANT-CODE", "M10006", "CURRENCY-CD", "USD", "HOLD-EXP-DT", "2026-06-24"));
        rows.add(row("HOLD-ID", "HLD-20260619-0007", "WALLET-ID", "WL-100007", "HOLD-AMT", "6600",
                "HOLD-RESULT", "00", "MERCHANT-CODE", "M10001", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-06-26"));
        rows.add(row("HOLD-ID", "HLD-20260617-0008", "WALLET-ID", "WL-100008", "HOLD-AMT", "9900",
                "HOLD-RESULT", "00", "MERCHANT-CODE", "M10008", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-06-23"));
        return rows;
    }

    private static java.util.List<java.util.Map<String, String>> loadPytxnf() {
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<>();
        rows.add(row("TXN-ID", "TXN-20260620-0001", "REQ-ID", "REQ-0001", "WALLET-ID", "WL-100001",
                "MERCHANT-CODE", "M10001", "REQ-AMT", "12500", "TXN-STATUS", "10", "AUTH-DT", "2026-06-20", "CAPTURE-DT", ""));
        rows.add(row("TXN-ID", "TXN-20260620-0002", "REQ-ID", "REQ-0002", "WALLET-ID", "WL-100002",
                "MERCHANT-CODE", "M10002", "REQ-AMT", "7800", "TXN-STATUS", "30", "AUTH-DT", "2026-06-20", "CAPTURE-DT", "2026-06-21"));
        rows.add(row("TXN-ID", "TXN-20260621-0003", "REQ-ID", "REQ-0003", "WALLET-ID", "WL-100003",
                "MERCHANT-CODE", "M10003", "REQ-AMT", "4200", "TXN-STATUS", "10", "AUTH-DT", "2026-06-21", "CAPTURE-DT", ""));
        rows.add(row("TXN-ID", "TXN-20260619-0004", "REQ-ID", "REQ-0004", "WALLET-ID", "WL-100004",
                "MERCHANT-CODE", "M10004", "REQ-AMT", "31800", "TXN-STATUS", "10", "AUTH-DT", "2026-06-19", "CAPTURE-DT", ""));
        rows.add(row("TXN-ID", "TXN-20260617-0008", "REQ-ID", "REQ-0008", "WALLET-ID", "WL-100008",
                "MERCHANT-CODE", "M10008", "REQ-AMT", "9900", "TXN-STATUS", "30", "AUTH-DT", "2026-06-17", "CAPTURE-DT", "2026-06-18"));
        return rows;
    }

    private static java.util.List<java.util.Map<String, String>> extractExpiredHoldCandidates(
            java.util.List<java.util.Map<String, String>> pyholdf,
            java.util.List<java.util.Map<String, String>> pytxnf,
            java.time.LocalDate businessDate) {
        java.util.List<java.util.Map<String, String>> candidates = new java.util.ArrayList<>();

        for (java.util.Map<String, String> hold : pyholdf) {
            if (!HD_HOLD_RESULT_ACTIVE.equals(hold.get("HOLD-RESULT"))) {
                continue;
            }
            if (!BASE_CURRENCY.equals(hold.get("CURRENCY-CD"))) {
                continue;
            }

            java.time.LocalDate expireDate = java.time.LocalDate.parse(hold.get("HOLD-EXP-DT"));
            if (expireDate.isAfter(businessDate)) {
                continue;
            }

            if (hasCapturedTransaction(hold, pytxnf)) {
                continue;
            }

            candidates.add(hold);
        }

        return candidates;
    }

    private static boolean hasCapturedTransaction(
            java.util.Map<String, String> hold,
            java.util.List<java.util.Map<String, String>> pytxnf) {
        for (java.util.Map<String, String> txn : pytxnf) {
            boolean sameWallet = hold.get("WALLET-ID").equals(txn.get("WALLET-ID"));
            boolean sameMerchant = hold.get("MERCHANT-CODE").equals(txn.get("MERCHANT-CODE"));
            boolean sameAmount = new java.math.BigDecimal(hold.get("HOLD-AMT"))
                    .compareTo(new java.math.BigDecimal(txn.get("REQ-AMT"))) == 0;

            if (sameWallet && sameMerchant && sameAmount && TXN_STATUS_CAPTURED.equals(txn.get("TXN-STATUS"))) {
                return true;
            }
        }
        return false;
    }

    private static java.util.List<java.util.Map<String, String>> buildNoticeFile(
            java.util.List<java.util.Map<String, String>> expiredHolds,
            java.time.LocalDateTime createTs) {
        java.util.List<java.util.Map<String, String>> notices = new java.util.ArrayList<>();
        int seq = 1;

        for (java.util.Map<String, String> hold : expiredHolds) {
            String noticeId = "NTF-" + createTs.toLocalDate().toString().replace("-", "") + "-" + String.format("%06d", seq++);
            String text = "ホールド期限到来 HOLD-ID=" + hold.get("HOLD-ID")
                    + " 加盟店=" + hold.get("MERCHANT-CODE")
                    + " 金額=" + hold.get("HOLD-AMT")
                    + " 通貨=" + hold.get("CURRENCY-CD")
                    + " 失効日=" + hold.get("HOLD-EXP-DT");

            notices.add(row("NOTICE-ID", noticeId,
                    "WALLET-ID", hold.get("WALLET-ID"),
                    "NOTICE-KBN", NOTICE_KBN_HOLD_EXPIRE,
                    "NOTICE-TEXT", text,
                    "SEND-STATUS", SEND_STATUS_UNSENT,
                    "CREATE-TS", createTs.toString()));
        }

        return notices;
    }

    private static void printMerchantSummary(
            java.util.List<java.util.Map<String, String>> expiredHolds,
            java.time.LocalDate businessDate) {
        java.util.Map<String, java.math.BigDecimal> amountByMerchant = new java.util.TreeMap<>();
        java.util.Map<String, Integer> countByMerchant = new java.util.TreeMap<>();

        for (java.util.Map<String, String> hold : expiredHolds) {
            String merchant = hold.get("MERCHANT-CODE");
            java.math.BigDecimal amount = new java.math.BigDecimal(hold.get("HOLD-AMT"));
            amountByMerchant.put(merchant, amountByMerchant.getOrDefault(merchant, java.math.BigDecimal.ZERO).add(amount));
            countByMerchant.put(merchant, countByMerchant.getOrDefault(merchant, 0) + 1);
        }

        System.out.println("処理日=" + businessDate + " ホールド失効候補件数=" + expiredHolds.size());
        for (String merchant : amountByMerchant.keySet()) {
            System.out.println("加盟店=" + merchant
                    + " 件数=" + countByMerchant.get(merchant)
                    + " 合計金額=" + amountByMerchant.get(merchant).toPlainString());
        }
    }

    private static void printPyntff(java.util.List<java.util.Map<String, String>> pyntff) {
        System.out.println("PYNTFF 出力開始");
        for (java.util.Map<String, String> notice : pyntff) {
            System.out.println(notice.get("NOTICE-ID") + ","
                    + notice.get("WALLET-ID") + ","
                    + notice.get("NOTICE-KBN") + ","
                    + notice.get("NOTICE-TEXT") + ","
                    + notice.get("SEND-STATUS") + ","
                    + notice.get("CREATE-TS"));
        }
        System.out.println("PYNTFF 出力終了");
    }

    private static java.util.Map<String, String> row(String... values) {
        if (values.length % 2 != 0) {
            throw new IllegalArgumentException("項目数不正");
        }

        java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
        for (int i = 0; i < values.length; i += 2) {
            row.put(values[i], values[i + 1]);
        }
        return row;
    }
}
