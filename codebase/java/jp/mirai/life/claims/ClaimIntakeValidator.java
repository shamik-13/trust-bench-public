package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/04/01  未来生命    初版作成
 */
public class ClaimIntakeValidator {
    private static final String[] LFCLMF = {
            "CLM24040001,POL10000001,10,2024-04-01,2024-04-03",
            "CLM24040002,POL10000002,10,2024-04-10,2024-04-09",
            "CLM24040003,POL10000003,90,2024-03-20,2024-03-25",
            "CLM24040004,POL10000004,20,2024-02-01,2024-02-14"
    };

    private static final java.util.Set<String> 受付可能状態 =
            new java.util.HashSet<String>(java.util.Arrays.asList("10", "20", "30"));

    public static void main(String[] a) {
        String 請求番号 = a != null && a.length > 0 ? a[0] : "CLM24040001";
        java.util.List<String> 指摘 = new java.util.ArrayList<String>();

        java.util.Map<String, String> 請求 = 請求取得(請求番号);
        if (請求 == null) {
            指摘.add("LFCLMF未登録");
        } else {
            状態検証(請求, 指摘);
            日付順序検証(請求, 指摘);
        }

        if (指摘.isEmpty()) {
            System.out.println("請求受付検証結果=正常 請求番号=" + 請求番号);
        } else {
            System.out.println("請求受付検証結果=否認 請求番号=" + 請求番号 + " 指摘=" + String.join("、", 指摘));
        }
    }

    private static java.util.Map<String, String> 請求取得(String 請求番号) {
        for (String 行 : LFCLMF) {
            String[] 項目 = 行.split(",", -1);
            if (項目.length == 5 && 項目[0].equals(請求番号)) {
                java.util.Map<String, String> 請求 = new java.util.HashMap<String, String>();
                請求.put("請求番号", 項目[0]);
                請求.put("証券番号", 項目[1]);
                請求.put("状態", 項目[2]);
                請求.put("責任開始日", 項目[3]);
                請求.put("支払事由発生日", 項目[4]);
                return 請求;
            }
        }
        return null;
    }

    private static void 状態検証(java.util.Map<String, String> 請求, java.util.List<String> 指摘) {
        String 状態 = 請求.get("状態");
        if (!受付可能状態.contains(状態)) {
            指摘.add("受付対象外状態=" + 状態);
        }
    }

    private static void 日付順序検証(java.util.Map<String, String> 請求, java.util.List<String> 指摘) {
        java.time.LocalDate 責任開始日 = java.time.LocalDate.parse(請求.get("責任開始日"));
        java.time.LocalDate 支払事由発生日 = java.time.LocalDate.parse(請求.get("支払事由発生日"));

        if (支払事由発生日.isBefore(責任開始日)) {
            指摘.add("支払事由発生日が責任開始日前");
        }
    }
}
