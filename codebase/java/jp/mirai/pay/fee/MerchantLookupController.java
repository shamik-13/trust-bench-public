package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/08/19  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class MerchantLookupController {
    private static final String STATUS_ACTIVE = "01";
    private static final String STATUS_STOPPED = "02";
    private static final String STATUS_CLOSED = "09";

    private static final java.util.Map<String, String> CATEGORY_NAMES = createCategoryNames();
    private static final java.util.Map<String, String> MERCHANT_STATUS_NAMES = createMerchantStatusNames();

    public static void main(String[] a) {
        MerchantLookupController controller = new MerchantLookupController();
        LookupRequest request = LookupRequest.fromArgs(a);

        try {
            java.util.List<LookupResponse> responses = controller.search(request);
            System.out.println("照会条件: 加盟店コード=" + valueOrAll(request.merchantCode)
                    + ", 名称前方一致=" + valueOrAll(request.namePrefix)
                    + ", 業種区分=" + valueOrAll(request.category));
            System.out.println("照会件数: " + responses.size());

            for (LookupResponse response : responses) {
                System.out.println(response.toDisplayLine());
            }
        } catch (IllegalArgumentException e) {
            System.err.println("入力エラー: " + e.getMessage());
            System.exit(2);
        }
    }

    private java.util.List<LookupResponse> search(LookupRequest request) {
        validateRequest(request);

        java.util.List<MerchantRecord> merchants = loadMerchantFile();
        java.util.List<BillingRecord> bills = loadBillingFile();
        java.util.List<SettlementSummaryRecord> summaries = loadSummaryFile();

        java.util.Map<String, BillingRecord> latestBillByMerchant = latestBillByMerchant(bills);
        java.util.Map<String, SettlementSummaryRecord> latestSummaryByMerchant = latestSummaryByMerchant(summaries);
        java.util.Map<String, Integer> appliedLineCountByMerchant = appliedLineCountByMerchant(bills);

        java.util.List<LookupResponse> result = new java.util.ArrayList<>();
        for (MerchantRecord merchant : merchants) {
            if (!matches(request, merchant)) {
                continue;
            }

            BillingRecord latestBill = latestBillByMerchant.get(merchant.merchantCode);
            SettlementSummaryRecord latestSummary = latestSummaryByMerchant.get(merchant.merchantCode);

            result.add(new LookupResponse(
                    merchant.merchantCode,
                    merchant.merchantName,
                    merchant.category,
                    categoryName(merchant.category),
                    merchant.status,
                    merchantStatusName(merchant.status),
                    latestBill == null ? "" : latestBill.billingMonth,
                    latestBill == null ? "未請求" : latestBill.status,
                    latestBill == null ? "" : latestBill.dueDate,
                    appliedLineCountByMerchant.getOrDefault(merchant.merchantCode, 0),
                    latestSummary == null ? "" : latestSummary.settleMonth,
                    latestSummary == null ? 0 : latestSummary.txnCount,
                    latestSummary == null ? 0L : latestSummary.txnTotalAmount,
                    latestSummary == null ? 0L : latestSummary.netSettleAmount));
        }

        result.sort(new java.util.Comparator<LookupResponse>() {
            @Override
            public int compare(LookupResponse left, LookupResponse right) {
                int byCategory = left.category.compareTo(right.category);
                if (byCategory != 0) {
                    return byCategory;
                }
                return left.merchantCode.compareTo(right.merchantCode);
            }
        });
        return result;
    }

    private static void validateRequest(LookupRequest request) {
        if (request.merchantCode != null && !request.merchantCode.matches("M[0-9]{7}")) {
            throw new IllegalArgumentException("加盟店コードは M + 7桁で指定してください");
        }
        if (request.namePrefix != null && request.namePrefix.trim().isEmpty()) {
            throw new IllegalArgumentException("名称前方一致に空白のみは指定できません");
        }
        if (request.category != null && !CATEGORY_NAMES.containsKey(request.category)) {
            throw new IllegalArgumentException("業種区分は C1, C2, C3, C4, C5 のいずれかを指定してください");
        }
    }

    private static boolean matches(LookupRequest request, MerchantRecord merchant) {
        if (request.merchantCode != null && !merchant.merchantCode.equals(request.merchantCode)) {
            return false;
        }
        if (request.namePrefix != null && !merchant.merchantName.startsWith(request.namePrefix)) {
            return false;
        }
        return request.category == null || merchant.category.equals(request.category);
    }

    private static java.util.Map<String, BillingRecord> latestBillByMerchant(java.util.List<BillingRecord> bills) {
        java.util.Map<String, BillingRecord> result = new java.util.HashMap<>();
        for (BillingRecord bill : bills) {
            BillingRecord current = result.get(bill.merchantCode);
            if (current == null || bill.billingMonth.compareTo(current.billingMonth) > 0
                    || (bill.billingMonth.equals(current.billingMonth) && bill.billId.compareTo(current.billId) > 0)) {
                result.put(bill.merchantCode, bill);
            }
        }
        return result;
    }

    private static java.util.Map<String, SettlementSummaryRecord> latestSummaryByMerchant(
            java.util.List<SettlementSummaryRecord> summaries) {
        java.util.Map<String, SettlementSummaryRecord> result = new java.util.HashMap<>();
        for (SettlementSummaryRecord summary : summaries) {
            SettlementSummaryRecord current = result.get(summary.merchantCode);
            if (current == null || summary.settleMonth.compareTo(current.settleMonth) > 0
                    || (summary.settleMonth.equals(current.settleMonth)
                    && summary.summaryId.compareTo(current.summaryId) > 0)) {
                result.put(summary.merchantCode, summary);
            }
        }
        return result;
    }

    private static java.util.Map<String, Integer> appliedLineCountByMerchant(java.util.List<BillingRecord> bills) {
        java.util.Map<String, Integer> result = new java.util.HashMap<>();
        for (BillingRecord bill : bills) {
            if (!STATUS_ACTIVE.equals(bill.status)) {
                continue;
            }
            Integer count = result.get(bill.merchantCode);
            result.put(bill.merchantCode, count == null ? 1 : count + 1);
        }
        return result;
    }

    private static java.util.List<MerchantRecord> loadMerchantFile() {
        java.util.List<MerchantRecord> rows = new java.util.ArrayList<>();
        rows.add(new MerchantRecord("M1000001", "未来百貨店新宿", "C1", STATUS_ACTIVE));
        rows.add(new MerchantRecord("M1000002", "未来百貨店横浜", "C1", STATUS_ACTIVE));
        rows.add(new MerchantRecord("M1000003", "青葉食堂本店", "C2", STATUS_ACTIVE));
        rows.add(new MerchantRecord("M1000004", "青葉食堂駅前", "C2", STATUS_STOPPED));
        rows.add(new MerchantRecord("M1000005", "東都水道料金センター", "C3", STATUS_ACTIVE));
        rows.add(new MerchantRecord("M1000006", "北浜電力収納窓口", "C3", STATUS_ACTIVE));
        rows.add(new MerchantRecord("M1000007", "さくら通信販売", "C4", STATUS_ACTIVE));
        rows.add(new MerchantRecord("M1000008", "みらいＥＣモール", "C4", STATUS_ACTIVE));
        rows.add(new MerchantRecord("M1000009", "銀座チケット買取", "C5", STATUS_STOPPED));
        rows.add(new MerchantRecord("M1000010", "大手町会員サービス", "C5", STATUS_CLOSED));
        return rows;
    }

    private static java.util.List<BillingRecord> loadBillingFile() {
        java.util.List<BillingRecord> rows = new java.util.ArrayList<>();
        rows.add(new BillingRecord("B202604001", "M1000001", "202604", 184200L, 18420L, "02", "20260531"));
        rows.add(new BillingRecord("B202605001", "M1000001", "202605", 191350L, 19135L, STATUS_ACTIVE, "20260630"));
        rows.add(new BillingRecord("B202605002", "M1000002", "202605", 88400L, 8840L, STATUS_ACTIVE, "20260630"));
        rows.add(new BillingRecord("B202605003", "M1000003", "202605", 44620L, 4462L, STATUS_ACTIVE, "20260630"));
        rows.add(new BillingRecord("B202605004", "M1000004", "202605", 12980L, 1298L, "03", "20260630"));
        rows.add(new BillingRecord("B202605005", "M1000005", "202605", 30120L, 3012L, STATUS_ACTIVE, "20260625"));
        rows.add(new BillingRecord("B202605006", "M1000006", "202605", 25510L, 2551L, STATUS_ACTIVE, "20260625"));
        rows.add(new BillingRecord("B202605007", "M1000007", "202605", 163800L, 16380L, STATUS_ACTIVE, "20260630"));
        rows.add(new BillingRecord("B202604007", "M1000007", "202604", 151920L, 15192L, "02", "20260531"));
        rows.add(new BillingRecord("B202605008", "M1000008", "202605", 244760L, 24476L, STATUS_ACTIVE, "20260630"));
        rows.add(new BillingRecord("B202605009", "M1000009", "202605", 50740L, 5074L, "08", "20260630"));
        return rows;
    }

    private static java.util.List<SettlementSummaryRecord> loadSummaryFile() {
        java.util.List<SettlementSummaryRecord> rows = new java.util.ArrayList<>();
        rows.add(new SettlementSummaryRecord("S202605001", "M1000001", "202605", 12840, 72834500L, 191350L, 72643150L));
        rows.add(new SettlementSummaryRecord("S202605002", "M1000002", "202605", 6410, 35100200L, 88400L, 35011800L));
        rows.add(new SettlementSummaryRecord("S202605003", "M1000003", "202605", 3120, 13460500L, 44620L, 13415880L));
        rows.add(new SettlementSummaryRecord("S202605004", "M1000004", "202605", 740, 3812000L, 12980L, 3799020L));
        rows.add(new SettlementSummaryRecord("S202605005", "M1000005", "202605", 9820, 100420000L, 30120L, 100389880L));
        rows.add(new SettlementSummaryRecord("S202605006", "M1000006", "202605", 7720, 85031000L, 25510L, 85005490L));
        rows.add(new SettlementSummaryRecord("S202605007", "M1000007", "202605", 10480, 45129000L, 163800L, 44965200L));
        rows.add(new SettlementSummaryRecord("S202605008", "M1000008", "202605", 22060, 90341000L, 244760L, 90096240L));
        rows.add(new SettlementSummaryRecord("S202605009", "M1000009", "202605", 1610, 11820000L, 50740L, 11769260L));
        rows.add(new SettlementSummaryRecord("S202604010", "M1000010", "202604", 95, 670000L, 2200L, 667800L));
        return rows;
    }

    private static java.util.Map<String, String> createCategoryNames() {
        java.util.Map<String, String> map = new java.util.LinkedHashMap<>();
        map.put("C1", "一般物販");
        map.put("C2", "飲食");
        map.put("C3", "公共・公金");
        map.put("C4", "EC・通信販売");
        map.put("C5", "高リスク業種");
        return java.util.Collections.unmodifiableMap(map);
    }

    private static java.util.Map<String, String> createMerchantStatusNames() {
        java.util.Map<String, String> map = new java.util.LinkedHashMap<>();
        map.put(STATUS_ACTIVE, "有効");
        map.put(STATUS_STOPPED, "停止");
        map.put(STATUS_CLOSED, "解約");
        return java.util.Collections.unmodifiableMap(map);
    }

    private static String categoryName(String category) {
        String name = CATEGORY_NAMES.get(category);
        return name == null ? "不明" : name;
    }

    private static String merchantStatusName(String status) {
        String name = MERCHANT_STATUS_NAMES.get(status);
        return name == null ? "不明" : name;
    }

    private static String valueOrAll(String value) {
        return value == null ? "全件" : value;
    }

    private static final class LookupRequest {
        private final String merchantCode;
        private final String namePrefix;
        private final String category;

        private LookupRequest(String merchantCode, String namePrefix, String category) {
            this.merchantCode = emptyToNull(merchantCode);
            this.namePrefix = emptyToNull(namePrefix);
            this.category = emptyToNull(category);
        }

        private static LookupRequest fromArgs(String[] args) {
            String merchantCode = null;
            String namePrefix = null;
            String category = null;

            for (String arg : args) {
                if (arg.startsWith("code=")) {
                    merchantCode = arg.substring("code=".length());
                } else if (arg.startsWith("name=")) {
                    namePrefix = arg.substring("name=".length());
                } else if (arg.startsWith("category=")) {
                    category = arg.substring("category=".length());
                } else {
                    throw new IllegalArgumentException("引数は code=, name=, category= の形式で指定してください");
                }
            }
            return new LookupRequest(merchantCode, namePrefix, category);
        }

        private static String emptyToNull(String value) {
            return value == null || value.isEmpty() ? null : value;
        }
    }

    private static final class MerchantRecord {
        private final String merchantCode;
        private final String merchantName;
        private final String category;
        private final String status;

        private MerchantRecord(String merchantCode, String merchantName, String category, String status) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.category = category;
            this.status = status;
        }
    }

    private static final class BillingRecord {
        private final String billId;
        private final String merchantCode;
        private final String billingMonth;
        private final long feeTotalAmount;
        private final long taxAmount;
        private final String status;
        private final String dueDate;

        private BillingRecord(String billId, String merchantCode, String billingMonth, long feeTotalAmount,
                              long taxAmount, String status, String dueDate) {
            if (feeTotalAmount < 0 || taxAmount < 0) {
                throw new IllegalArgumentException("請求金額に負数は指定できません");
            }
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.billingMonth = billingMonth;
            this.feeTotalAmount = feeTotalAmount;
            this.taxAmount = taxAmount;
            this.status = status;
            this.dueDate = dueDate;
        }
    }

    private static final class SettlementSummaryRecord {
        private final String summaryId;
        private final String merchantCode;
        private final String settleMonth;
        private final int txnCount;
        private final long txnTotalAmount;
        private final long feeTotalAmount;
        private final long netSettleAmount;

        private SettlementSummaryRecord(String summaryId, String merchantCode, String settleMonth, int txnCount,
                                        long txnTotalAmount, long feeTotalAmount, long netSettleAmount) {
            if (txnCount < 0 || txnTotalAmount < 0 || feeTotalAmount < 0 || netSettleAmount < 0) {
                throw new IllegalArgumentException("精算サマリに負数は指定できません");
            }
            this.summaryId = summaryId;
            this.merchantCode = merchantCode;
            this.settleMonth = settleMonth;
            this.txnCount = txnCount;
            this.txnTotalAmount = txnTotalAmount;
            this.feeTotalAmount = feeTotalAmount;
            this.netSettleAmount = netSettleAmount;
        }
    }

    private static final class LookupResponse {
        private final String merchantCode;
        private final String merchantName;
        private final String category;
        private final String categoryName;
        private final String merchantStatus;
        private final String merchantStatusName;
        private final String latestBillingMonth;
        private final String billingStatus;
        private final String dueDate;
        private final int appliedDetailCount;
        private final String latestSettleMonth;
        private final int txnCount;
        private final long txnTotalAmount;
        private final long netSettleAmount;

        private LookupResponse(String merchantCode, String merchantName, String category, String categoryName,
                               String merchantStatus, String merchantStatusName, String latestBillingMonth,
                               String billingStatus, String dueDate, int appliedDetailCount,
                               String latestSettleMonth, int txnCount, long txnTotalAmount, long netSettleAmount) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.category = category;
            this.categoryName = categoryName;
            this.merchantStatus = merchantStatus;
            this.merchantStatusName = merchantStatusName;
            this.latestBillingMonth = latestBillingMonth;
            this.billingStatus = billingStatus;
            this.dueDate = dueDate;
            this.appliedDetailCount = appliedDetailCount;
            this.latestSettleMonth = latestSettleMonth;
            this.txnCount = txnCount;
            this.txnTotalAmount = txnTotalAmount;
            this.netSettleAmount = netSettleAmount;
        }

        private String toDisplayLine() {
            return "加盟店コード=" + merchantCode
                    + ", 名称=" + merchantName
                    + ", 業種=" + category + ":" + categoryName
                    + ", 加盟店状態=" + merchantStatus + ":" + merchantStatusName
                    + ", 最新請求月=" + blankToHyphen(latestBillingMonth)
                    + ", 請求ステータス=" + billingStatus
                    + ", 支払期日=" + blankToHyphen(dueDate)
                    + ", 適用済み明細件数=" + appliedDetailCount
                    + ", 直近精算月=" + blankToHyphen(latestSettleMonth)
                    + ", 取引件数=" + txnCount
                    + ", 取引金額=" + txnTotalAmount
                    + ", 差引精算額=" + netSettleAmount;
        }

        private static String blankToHyphen(String value) {
            return value == null || value.isEmpty() ? "-" : value;
        }
    }
}
