package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2025/02/24  みらいペイ システム部 加盟店・手数料チーム  精算サマリ照会サービス初版
 */
public class SettlementSummaryQueryService {
    private static final String CHARGEABLE_STATUS = "01";

    public static void main(String[] a) {
        if (a == null || a.length != 2) {
            System.out.println("使用方法: 加盟店コード 対象月(yyyyMM)");
            return;
        }

        String merchantCode = normalize(a[0]);
        String settleMonth = normalize(a[1]);

        if (!isMerchantCode(merchantCode)) {
            System.out.println("入力エラー: 加盟店コードが不正です");
            return;
        }
        if (!isSettleMonth(settleMonth)) {
            System.out.println("入力エラー: 対象月が不正です");
            return;
        }

        Merchant merchant = findMerchant(merchantCode);
        if (merchant == null) {
            System.out.println("照会結果: 加盟店が存在しません");
            System.out.println("加盟店コード: " + merchantCode);
            System.out.println("対象月: " + settleMonth);
            return;
        }

        Summary total = aggregateSummary(merchantCode, settleMonth);

        System.out.println("精算サマリ照会結果");
        System.out.println("加盟店コード: " + merchant.code);
        System.out.println("加盟店名: " + merchant.name);
        System.out.println("業種区分: " + categoryName(merchant.category));
        System.out.println("加盟店状態: " + statusName(merchant.status));
        System.out.println("対象月: " + settleMonth);

        if (!CHARGEABLE_STATUS.equals(merchant.status)) {
            System.out.println("締め状態: 対象外");
            System.out.println("取引件数: 0");
            System.out.println("取引総額: 0");
            System.out.println("手数料総額: 0");
            System.out.println("精算予定額: 0");
            return;
        }

        if (total.count == 0) {
            System.out.println("締め状態: 未締め");
            System.out.println("取引件数: 0");
            System.out.println("取引総額: 0");
            System.out.println("手数料総額: 0");
            System.out.println("精算予定額: 0");
            return;
        }

        System.out.println("締め状態: 締め済");
        System.out.println("取引件数: " + total.txnCount);
        System.out.println("取引総額: " + total.txnTotalAmount);
        System.out.println("手数料総額: " + total.feeTotalAmount);
        System.out.println("精算予定額: " + total.netSettleAmount);
    }

    private static Summary aggregateSummary(String merchantCode, String settleMonth) {
        long txnCount = 0L;
        long txnTotalAmount = 0L;
        long feeTotalAmount = 0L;
        long netSettleAmount = 0L;
        int count = 0;

        for (Summary row : PFSUMF) {
            if (row.merchantCode.equals(merchantCode) && row.settleMonth.equals(settleMonth)) {
                txnCount = Math.addExact(txnCount, row.txnCount);
                txnTotalAmount = Math.addExact(txnTotalAmount, row.txnTotalAmount);
                feeTotalAmount = Math.addExact(feeTotalAmount, row.feeTotalAmount);
                netSettleAmount = Math.addExact(netSettleAmount, row.netSettleAmount);
                count++;
            }
        }

        return new Summary("合算", merchantCode, settleMonth, txnCount, txnTotalAmount, feeTotalAmount, netSettleAmount, count);
    }

    private static Merchant findMerchant(String merchantCode) {
        for (Merchant row : PFMERF) {
            if (row.code.equals(merchantCode)) {
                return row;
            }
        }
        return null;
    }

    private static boolean isMerchantCode(String value) {
        if (value == null || value.length() != 8) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            if (!Character.isDigit(value.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private static boolean isSettleMonth(String value) {
        if (value == null || value.length() != 6) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            if (!Character.isDigit(value.charAt(i))) {
                return false;
            }
        }
        int month = Integer.parseInt(value.substring(4, 6));
        return month >= 1 && month <= 12;
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim();
    }

    private static String categoryName(String category) {
        switch (category) {
            case "C1":
                return "C1:一般物販";
            case "C2":
                return "C2:飲食";
            case "C3":
                return "C3:公共・公金";
            case "C4":
                return "C4:EC・通信販売";
            case "C5":
                return "C5:高リスク業種";
            default:
                return category + ":不明";
        }
    }

    private static String statusName(String status) {
        switch (status) {
            case "01":
                return "01:有効";
            case "02":
                return "02:停止";
            case "09":
                return "09:解約";
            default:
                return status + ":不明";
        }
    }

    private static final Merchant[] PFMERF = {
        new Merchant("10000001", "ミライ銀座商店", "C1", "01"),
        new Merchant("10000002", "日本橋食堂", "C2", "01"),
        new Merchant("10000003", "東京市税収納", "C3", "01"),
        new Merchant("10000004", "みらい通販", "C4", "01"),
        new Merchant("10000005", "北町チケット", "C5", "02"),
        new Merchant("10000006", "浜松町雑貨", "C1", "09")
    };

    private static final Summary[] PFSUMF = {
        new Summary("S20260410000001", "10000001", "202604", 1280L, 86543000L, 2163575L, 84379425L, 1),
        new Summary("S20260510000001", "10000001", "202605", 1326L, 90214000L, 2255350L, 87958650L, 1),
        new Summary("S20260610000001", "10000001", "202606", 1174L, 80622000L, 2015550L, 78606450L, 1),
        new Summary("S20260410000002", "10000002", "202604", 2430L, 54876000L, 1646280L, 53229720L, 1),
        new Summary("S20260510000002", "10000002", "202605", 2518L, 57234000L, 1717020L, 55516980L, 1),
        new Summary("S20260610000003", "10000003", "202606", 388L, 126400000L, 632000L, 125768000L, 1),
        new Summary("S20260510000004", "10000004", "202605", 8420L, 184320000L, 6451200L, 177868800L, 1),
        new Summary("S20260610000004", "10000004", "202606", 8611L, 191870000L, 6715450L, 185154550L, 1)
    };

    private static final class Merchant {
        private final String code;
        private final String name;
        private final String category;
        private final String status;

        private Merchant(String code, String name, String category, String status) {
            this.code = code;
            this.name = name;
            this.category = category;
            this.status = status;
        }
    }

    private static final class Summary {
        private final String summaryId;
        private final String merchantCode;
        private final String settleMonth;
        private final long txnCount;
        private final long txnTotalAmount;
        private final long feeTotalAmount;
        private final long netSettleAmount;
        private final int count;

        private Summary(String summaryId, String merchantCode, String settleMonth, long txnCount,
                        long txnTotalAmount, long feeTotalAmount, long netSettleAmount, int count) {
            this.summaryId = summaryId;
            this.merchantCode = merchantCode;
            this.settleMonth = settleMonth;
            this.txnCount = txnCount;
            this.txnTotalAmount = txnTotalAmount;
            this.feeTotalAmount = feeTotalAmount;
            this.netSettleAmount = netSettleAmount;
            this.count = count;
        }
    }
}
