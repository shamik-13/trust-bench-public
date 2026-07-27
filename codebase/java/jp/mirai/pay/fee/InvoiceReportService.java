package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2025/01/20  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class InvoiceReportService {
    private static final String 請求確定 = "01";
    private static final String 加盟店有効 = "01";

    public static void main(String[] a) {
        new InvoiceReportService().run();
    }

    private void run() {
        java.time.LocalDate 業務日 = java.time.LocalDate.of(2026, 6, 28);

        java.util.List<java.util.Map<String, String>> pfinvf = pfinvf();
        java.util.List<java.util.Map<String, String>> pfbilf = pfbilf();
        java.util.List<java.util.Map<String, String>> pfmerf = pfmerf();

        java.util.Map<String, java.util.Map<String, String>> 請求索引 = new java.util.HashMap<>();
        for (java.util.Map<String, String> r : pfbilf) {
            請求索引.put(r.get("BILL-ID"), r);
        }

        java.util.Map<String, java.util.Map<String, String>> 加盟店索引 = new java.util.HashMap<>();
        for (java.util.Map<String, String> r : pfmerf) {
            加盟店索引.put(r.get("MERCHANT-CODE"), r);
        }

        java.util.Map<String, 集計> 加盟店別 = new java.util.TreeMap<>();
        for (java.util.Map<String, String> inv : pfinvf) {
            java.util.Map<String, String> bill = 請求索引.get(inv.get("BILL-ID"));
            if (bill == null) {
                continue;
            }
            if (!same(inv, bill, "MERCHANT-CODE")) {
                continue;
            }
            if (!請求確定.equals(bill.get("STATUS"))) {
                continue;
            }

            java.util.Map<String, String> mer = 加盟店索引.get(inv.get("MERCHANT-CODE"));
            if (mer == null || !加盟店有効.equals(mer.get("MER-STATUS"))) {
                continue;
            }

            String 適格番号 = inv.get("QUALIFIED-INVOICE-NO");
            if (適格番号 == null || 適格番号.trim().isEmpty()) {
                continue;
            }

            集計 g = 加盟店別.computeIfAbsent(inv.get("MERCHANT-CODE"), k -> new 集計(k, mer.get("MERCHANT-NAME"), mer.get("MER-CATEGORY")));
            g.請求件数++;
            g.手数料金額 = g.手数料金額.add(金額(bill.get("FEE-TOTAL-AMT")));
            g.税額 = g.税額.add(金額(bill.get("TAX-AMT")));
            g.発行日 = minDate(g.発行日, java.time.LocalDate.parse(inv.get("ISSUE-DT")));
            g.支払期限 = maxDate(g.支払期限, java.time.LocalDate.parse(bill.get("DUE-DT")));
            add税内訳(g.税内訳, inv.get("TAX-BREAKDOWN"));
        }

        java.util.List<java.util.Map<String, String>> prrptf = new java.util.ArrayList<>();
        int seq = 1;
        for (集計 g : 加盟店別.values()) {
            String 出力種別 = "C5".equals(g.業種区分) ? "CSV" : "PDF";
            java.util.Map<String, String> out = new java.util.LinkedHashMap<>();
            out.put("REPORT-ID", String.format("RPT%08d", seq++));
            out.put("REPORT-TYPE", 出力種別);
            out.put("BUSINESS-DT", 業務日.toString());
            out.put("MERCHANT-CODE", g.加盟店コード);
            out.put("OUTPUT-PATH", "/var/opt/pf/invoice/" + 業務日 + "/" + g.加盟店コード + "." + 出力種別.toLowerCase(java.util.Locale.ROOT));
            out.put("STATUS", "作成済");
            prrptf.add(out);

            System.out.println("帳票作成 " + out.get("REPORT-ID") + " " + g.加盟店コード + " "
                    + g.加盟店名 + " 件数=" + g.請求件数 + " 手数料=" + g.手数料金額
                    + " 税額=" + g.税額 + " 発行日=" + g.発行日 + " 税内訳=" + g.税内訳);
        }

        System.out.println("PRRPTF出力件数=" + prrptf.size());
    }

    private static boolean same(java.util.Map<String, String> a, java.util.Map<String, String> b, String key) {
        return java.util.Objects.equals(a.get(key), b.get(key));
    }

    private static java.math.BigDecimal 金額(String v) {
        return new java.math.BigDecimal(v).setScale(0);
    }

    private static java.time.LocalDate minDate(java.time.LocalDate a, java.time.LocalDate b) {
        return a == null || b.isBefore(a) ? b : a;
    }

    private static java.time.LocalDate maxDate(java.time.LocalDate a, java.time.LocalDate b) {
        return a == null || b.isAfter(a) ? b : a;
    }

    private static void add税内訳(java.util.Map<String, java.math.BigDecimal> dest, String src) {
        if (src == null || src.trim().isEmpty()) {
            return;
        }
        String[] parts = src.split(";");
        for (String p : parts) {
            String[] kv = p.split("=");
            if (kv.length == 2) {
                dest.merge(kv[0], 金額(kv[1]), java.math.BigDecimal::add);
            }
        }
    }

    private static java.util.List<java.util.Map<String, String>> pfinvf() {
        java.util.List<java.util.Map<String, String>> l = new java.util.ArrayList<>();
        l.add(row("INVOICE-ID", "INV26060001", "BILL-ID", "BIL26060001", "MERCHANT-CODE", "M0001001", "QUALIFIED-INVOICE-NO", "T3010000000001", "ISSUE-DT", "2026-06-25", "TAX-BREAKDOWN", "10=1200;8=0"));
        l.add(row("INVOICE-ID", "INV26060002", "BILL-ID", "BIL26060002", "MERCHANT-CODE", "M0001001", "QUALIFIED-INVOICE-NO", "T3010000000001", "ISSUE-DT", "2026-06-26", "TAX-BREAKDOWN", "10=820;8=0"));
        l.add(row("INVOICE-ID", "INV26060003", "BILL-ID", "BIL26060003", "MERCHANT-CODE", "M0002001", "QUALIFIED-INVOICE-NO", "T5010000000002", "ISSUE-DT", "2026-06-25", "TAX-BREAKDOWN", "10=640;8=210"));
        l.add(row("INVOICE-ID", "INV26060004", "BILL-ID", "BIL26060004", "MERCHANT-CODE", "M0003001", "QUALIFIED-INVOICE-NO", "T7010000000003", "ISSUE-DT", "2026-06-25", "TAX-BREAKDOWN", "10=310;8=0"));
        l.add(row("INVOICE-ID", "INV26060005", "BILL-ID", "BIL26060005", "MERCHANT-CODE", "M0004001", "QUALIFIED-INVOICE-NO", "", "ISSUE-DT", "2026-06-25", "TAX-BREAKDOWN", "10=900;8=0"));
        l.add(row("INVOICE-ID", "INV26060006", "BILL-ID", "BIL26060006", "MERCHANT-CODE", "M0005001", "QUALIFIED-INVOICE-NO", "T9010000000005", "ISSUE-DT", "2026-06-27", "TAX-BREAKDOWN", "10=1800;8=0"));
        return l;
    }

    private static java.util.List<java.util.Map<String, String>> pfbilf() {
        java.util.List<java.util.Map<String, String>> l = new java.util.ArrayList<>();
        l.add(row("BILL-ID", "BIL26060001", "MERCHANT-CODE", "M0001001", "BILLING-MONTH", "202606", "FEE-TOTAL-AMT", "12000", "TAX-AMT", "1200", "STATUS", "01", "DUE-DT", "2026-07-31"));
        l.add(row("BILL-ID", "BIL26060002", "MERCHANT-CODE", "M0001001", "BILLING-MONTH", "202606", "FEE-TOTAL-AMT", "8200", "TAX-AMT", "820", "STATUS", "01", "DUE-DT", "2026-07-31"));
        l.add(row("BILL-ID", "BIL26060003", "MERCHANT-CODE", "M0002001", "BILLING-MONTH", "202606", "FEE-TOTAL-AMT", "7060", "TAX-AMT", "850", "STATUS", "01", "DUE-DT", "2026-07-25"));
        l.add(row("BILL-ID", "BIL26060004", "MERCHANT-CODE", "M0003001", "BILLING-MONTH", "202606", "FEE-TOTAL-AMT", "3100", "TAX-AMT", "310", "STATUS", "02", "DUE-DT", "2026-07-20"));
        l.add(row("BILL-ID", "BIL26060005", "MERCHANT-CODE", "M0004001", "BILLING-MONTH", "202606", "FEE-TOTAL-AMT", "9000", "TAX-AMT", "900", "STATUS", "01", "DUE-DT", "2026-07-31"));
        l.add(row("BILL-ID", "BIL26060006", "MERCHANT-CODE", "M0005001", "BILLING-MONTH", "202606", "FEE-TOTAL-AMT", "18000", "TAX-AMT", "1800", "STATUS", "01", "DUE-DT", "2026-07-31"));
        return l;
    }

    private static java.util.List<java.util.Map<String, String>> pfmerf() {
        java.util.List<java.util.Map<String, String>> l = new java.util.ArrayList<>();
        l.add(row("MERCHANT-CODE", "M0001001", "MERCHANT-NAME", "未来堂日本橋店", "MER-CATEGORY", "C1", "MER-STATUS", "01"));
        l.add(row("MERCHANT-CODE", "M0002001", "MERCHANT-NAME", "青葉食堂", "MER-CATEGORY", "C2", "MER-STATUS", "01"));
        l.add(row("MERCHANT-CODE", "M0003001", "MERCHANT-NAME", "港区収納課", "MER-CATEGORY", "C3", "MER-STATUS", "01"));
        l.add(row("MERCHANT-CODE", "M0004001", "MERCHANT-NAME", "北斗通販", "MER-CATEGORY", "C4", "MER-STATUS", "01"));
        l.add(row("MERCHANT-CODE", "M0005001", "MERCHANT-NAME", "東都チケット", "MER-CATEGORY", "C5", "MER-STATUS", "02"));
        return l;
    }

    private static java.util.Map<String, String> row(String... kv) {
        java.util.Map<String, String> m = new java.util.LinkedHashMap<>();
        for (int i = 0; i + 1 < kv.length; i += 2) {
            m.put(kv[i], kv[i + 1]);
        }
        return m;
    }

    private static final class 集計 {
        final String 加盟店コード;
        final String 加盟店名;
        final String 業種区分;
        int 請求件数;
        java.math.BigDecimal 手数料金額 = java.math.BigDecimal.ZERO;
        java.math.BigDecimal 税額 = java.math.BigDecimal.ZERO;
        java.time.LocalDate 発行日;
        java.time.LocalDate 支払期限;
        final java.util.Map<String, java.math.BigDecimal> 税内訳 = new java.util.TreeMap<>();

        集計(String 加盟店コード, String 加盟店名, String 業種区分) {
            this.加盟店コード = 加盟店コード;
            this.加盟店名 = 加盟店名;
            this.業種区分 = 業種区分;
        }
    }
}
