package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-10-21  みらいペイ システム部  初版作成
 */
public class SalesCaptureLinkService {
    private static final String 判定_承認 = "A";
    private static final String 判定_否認 = "D";
    private static final String ホールド_承認済 = "00";
    private static final String ホールド_取消 = "20";
    private static final String ホールド_売上確定済 = "30";
    private static final String 加盟店_有効 = "01";
    private static final String 通貨_基準 = "JPY";
    private static final String 取引_確定候補 = "10";
    private static final String 保留_未確定 = "10";

    public static void main(String[] a) {
        java.time.LocalDate 処理日 = java.time.LocalDate.of(2026, 6, 28);

        java.util.List<java.util.Map<String, String>> pyholdf = new java.util.ArrayList<>();
        pyholdf.add(行("HOLD-ID", "HD202606280001", "WALLET-ID", "WL000001", "HOLD-AMT", "12000", "HOLD-RESULT", "00", "MERCHANT-CODE", "M10001", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-07-05"));
        pyholdf.add(行("HOLD-ID", "HD202606280002", "WALLET-ID", "WL000002", "HOLD-AMT", "8500", "HOLD-RESULT", "00", "MERCHANT-CODE", "M10002", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-07-05"));
        pyholdf.add(行("HOLD-ID", "HD202606280003", "WALLET-ID", "WL000003", "HOLD-AMT", "50000", "HOLD-RESULT", "00", "MERCHANT-CODE", "M20001", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-07-03"));
        pyholdf.add(行("HOLD-ID", "HD202606280004", "WALLET-ID", "WL000004", "HOLD-AMT", "3000", "HOLD-RESULT", "20", "MERCHANT-CODE", "M10001", "CURRENCY-CD", "JPY", "HOLD-EXP-DT", "2026-07-05"));
        pyholdf.add(行("HOLD-ID", "HD202606280005", "WALLET-ID", "WL000005", "HOLD-AMT", "15000", "HOLD-RESULT", "00", "MERCHANT-CODE", "M30001", "CURRENCY-CD", "USD", "HOLD-EXP-DT", "2026-07-05"));

        java.util.List<java.util.Map<String, String>> pyarspf = new java.util.ArrayList<>();
        pyarspf.add(行("REQ-ID", "HD202606280001", "WALLET-ID", "WL000001", "DECISION-KBN", "A", "AVAIL-AMT", "88000", "REQ-AMT", "12000", "DECLINE-REASON", ""));
        pyarspf.add(行("REQ-ID", "HD202606280002", "WALLET-ID", "WL000002", "DECISION-KBN", "A", "AVAIL-AMT", "140000", "REQ-AMT", "9000", "DECLINE-REASON", ""));
        pyarspf.add(行("REQ-ID", "HD202606280003", "WALLET-ID", "WL000003", "DECISION-KBN", "A", "AVAIL-AMT", "200000", "REQ-AMT", "50000", "DECLINE-REASON", ""));
        pyarspf.add(行("REQ-ID", "HD202606280004", "WALLET-ID", "WL000004", "DECISION-KBN", "D", "AVAIL-AMT", "0", "REQ-AMT", "3000", "DECLINE-REASON", "LIM"));
        pyarspf.add(行("REQ-ID", "HD202606280005", "WALLET-ID", "WL000005", "DECISION-KBN", "A", "AVAIL-AMT", "23000", "REQ-AMT", "15000", "DECLINE-REASON", ""));

        java.util.Map<String, java.util.Map<String, String>> pymerf = new java.util.LinkedHashMap<>();
        pymerf.put("M10001", 行("MERCHANT-CODE", "M10001", "MERCHANT-STATUS", "01", "MCC", "6211", "DAILY-LIMIT-AMT", "1000000", "RISK-RANK", "B", "SETTLE-CYCLE-KBN", "D1"));
        pymerf.put("M10002", 行("MERCHANT-CODE", "M10002", "MERCHANT-STATUS", "01", "MCC", "5734", "DAILY-LIMIT-AMT", "300000", "RISK-RANK", "C", "SETTLE-CYCLE-KBN", "D2"));
        pymerf.put("M20001", 行("MERCHANT-CODE", "M20001", "MERCHANT-STATUS", "09", "MCC", "6211", "DAILY-LIMIT-AMT", "500000", "RISK-RANK", "E", "SETTLE-CYCLE-KBN", "D1"));
        pymerf.put("M30001", 行("MERCHANT-CODE", "M30001", "MERCHANT-STATUS", "01", "MCC", "5812", "DAILY-LIMIT-AMT", "200000", "RISK-RANK", "A", "SETTLE-CYCLE-KBN", "D3"));

        java.util.List<java.util.Map<String, String>> pytxnf = new java.util.ArrayList<>();
        java.util.List<java.util.Map<String, String>> pypendf = new java.util.ArrayList<>();
        java.util.Map<String, java.util.Map<String, String>> 応答索引 = new java.util.LinkedHashMap<>();

        for (java.util.Map<String, String> 応答 : pyarspf) {
            応答索引.put(応答.get("REQ-ID"), 応答);
        }

        java.util.Map<String, java.math.BigDecimal> 加盟店別確定額 = new java.util.LinkedHashMap<>();
        int 取引連番 = 1;
        int 保留連番 = 1;

        for (java.util.Map<String, String> ホールド : pyholdf) {
            String holdId = ホールド.get("HOLD-ID");
            java.util.Map<String, String> 応答 = 応答索引.get(holdId);
            java.util.Map<String, String> 加盟店 = pymerf.get(ホールド.get("MERCHANT-CODE"));

            if (応答 == null || 加盟店 == null) {
                pypendf.add(保留行(保留連番++, ホールド, 処理日, "照合不能"));
                continue;
            }

            java.math.BigDecimal holdAmt = 金額(ホールド.get("HOLD-AMT"));
            java.math.BigDecimal reqAmt = 金額(応答.get("REQ-AMT"));
            boolean 承認済み = 判定_承認.equals(応答.get("DECISION-KBN")) && ホールド_承認済.equals(ホールド.get("HOLD-RESULT"));
            boolean 同一利用者 = ホールド.get("WALLET-ID").equals(応答.get("WALLET-ID"));
            boolean 金額一致 = holdAmt.compareTo(reqAmt) == 0;
            boolean 通貨適合 = 通貨_基準.equals(ホールド.get("CURRENCY-CD"));
            boolean 加盟店有効 = 加盟店_有効.equals(加盟店.get("MERCHANT-STATUS"));

            if (承認済み && 同一利用者 && 金額一致 && 通貨適合 && 加盟店有効) {
                String merchantCode = ホールド.get("MERCHANT-CODE");
                java.math.BigDecimal 当日累計 = 加盟店別確定額.getOrDefault(merchantCode, java.math.BigDecimal.ZERO).add(reqAmt);
                if (当日累計.compareTo(金額(加盟店.get("DAILY-LIMIT-AMT"))) > 0) {
                    pypendf.add(保留行(保留連番++, ホールド, 処理日, "日次上限超過"));
                    continue;
                }
                加盟店別確定額.put(merchantCode, 当日累計);
                pytxnf.add(行("TXN-ID", String.format("TX20260628%06d", 取引連番++),
                        "REQ-ID", 応答.get("REQ-ID"),
                        "WALLET-ID", 応答.get("WALLET-ID"),
                        "MERCHANT-CODE", merchantCode,
                        "REQ-AMT", reqAmt.toPlainString(),
                        "TXN-STATUS", 取引_確定候補,
                        "AUTH-DT", "2026-06-28",
                        "CAPTURE-DT", 処理日.toString()));
            } else if (!金額一致 || !通貨適合 || !加盟店有効) {
                pypendf.add(保留行(保留連番++, ホールド, 処理日, 保留理由(金額一致, 通貨適合, 加盟店有効)));
            }
        }

        出力("PYTXNF", pytxnf);
        出力("PYPENDF", pypendf);
    }

    private static java.util.Map<String, String> 行(String... 値) {
        java.util.Map<String, String> 行 = new java.util.LinkedHashMap<>();
        for (int i = 0; i < 値.length; i += 2) {
            行.put(値[i], 値[i + 1]);
        }
        return 行;
    }

    private static java.math.BigDecimal 金額(String 値) {
        return new java.math.BigDecimal(値);
    }

    private static java.util.Map<String, String> 保留行(int 連番, java.util.Map<String, String> ホールド, java.time.LocalDate 処理日, String 理由) {
        return 行("PEND-ID", String.format("PN20260628%06d", 連番),
                "WALLET-ID", ホールド.get("WALLET-ID"),
                "PEND-AMT", ホールド.get("HOLD-AMT"),
                "PEND-STATUS", 保留_未確定,
                "CAPTURE-DT", 処理日.toString(),
                "PEND-REASON", 理由);
    }

    private static String 保留理由(boolean 金額一致, boolean 通貨適合, boolean 加盟店有効) {
        java.util.List<String> 理由 = new java.util.ArrayList<>();
        if (!金額一致) {
            理由.add("金額差異");
        }
        if (!通貨適合) {
            理由.add("通貨不一致");
        }
        if (!加盟店有効) {
            理由.add("加盟店停止");
        }
        return String.join("・", 理由);
    }

    private static void 出力(String 名称, java.util.List<java.util.Map<String, String>> 行群) {
        System.out.println("[" + 名称 + "]");
        for (java.util.Map<String, String> 行 : 行群) {
            System.out.println(行);
        }
    }
}
