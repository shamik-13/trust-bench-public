package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/11/25  みらいペイ システム部 加盟店・手数料チーム  入金消込サービス初版作成
 */
public class PaymentAllocationService {

    private static final String STATUS_UNMATCHED = "00";
    private static final String STATUS_MATCHED = "01";
    private static final String STATUS_SPLIT = "02";
    private static final String STATUS_OVER = "03";
    private static final String STATUS_ERROR = "09";

    private static final String BILL_UNPAID = "00";
    private static final String BILL_PAID = "01";

    private static final String MERCHANT_ACTIVE = "01";

    public static void main(String[] a) {
        java.util.List<java.util.Map<String, String>> pfpayf = new java.util.ArrayList<>();
        java.util.List<java.util.Map<String, String>> pfbilf = new java.util.ArrayList<>();
        java.util.Map<String, java.util.Map<String, String>> pfmerf = new java.util.HashMap<>();

        pfmerf.put("M10001", merchant("M10001", "東京中央ストア", "C1", "01"));
        pfmerf.put("M10002", merchant("M10002", "銀座食堂", "C2", "01"));
        pfmerf.put("M10003", merchant("M10003", "北区水道局", "C3", "01"));
        pfmerf.put("M10004", merchant("M10004", "東都オンライン", "C4", "02"));
        pfmerf.put("M10005", merchant("M10005", "新宿予約センター", "C5", "09"));

        pfbilf.add(bill("B202606001", "M10001", "202606", "110000", "10000", BILL_UNPAID, "2026-06-30"));
        pfbilf.add(bill("B202606002", "M10002", "202606", "88000", "8000", BILL_UNPAID, "2026-06-30"));
        pfbilf.add(bill("B202606003", "M10003", "202606", "45000", "0", BILL_UNPAID, "2026-06-25"));
        pfbilf.add(bill("B202606004", "M10001", "202606", "33000", "3000", BILL_UNPAID, "2026-06-30"));
        pfbilf.add(bill("B202606005", "M10004", "202606", "77000", "7000", BILL_UNPAID, "2026-06-30"));

        pfpayf.add(payment("P202606001", "M10001", "2026-06-28", "110000", "BK260628001", STATUS_UNMATCHED));
        pfpayf.add(payment("P202606002", "M10002", "2026-06-28", "40000", "BK260628002", STATUS_UNMATCHED));
        pfpayf.add(payment("P202606003", "M10002", "2026-06-29", "48000", "BK260628003", STATUS_UNMATCHED));
        pfpayf.add(payment("P202606004", "M10003", "2026-06-28", "50000", "BK260628004", STATUS_UNMATCHED));
        pfpayf.add(payment("P202606005", "M99999", "2026-06-28", "12000", "BK260628005", STATUS_UNMATCHED));
        pfpayf.add(payment("P202606006", "M10004", "2026-06-28", "77000", "BK260628006", STATUS_UNMATCHED));

        java.util.Map<String, java.util.List<java.util.Map<String, String>>> billsByMerchant = new java.util.HashMap<>();
        for (java.util.Map<String, String> bill : pfbilf) {
            billsByMerchant.computeIfAbsent(bill.get("MERCHANT-CODE"), k -> new java.util.ArrayList<>()).add(bill);
        }
        for (java.util.List<java.util.Map<String, String>> bills : billsByMerchant.values()) {
            bills.sort(java.util.Comparator
                    .comparing((java.util.Map<String, String> b) -> b.get("DUE-DT"))
                    .thenComparing(b -> b.get("BILL-ID")));
        }

        java.util.Map<String, Long> receivedByMerchant = new java.util.HashMap<>();
        for (java.util.Map<String, String> payment : pfpayf) {
            if (!STATUS_UNMATCHED.equals(payment.get("MATCH-STATUS"))) {
                continue;
            }

            String merchantCode = payment.get("MERCHANT-CODE");
            java.util.Map<String, String> merchant = pfmerf.get(merchantCode);
            if (merchant == null) {
                payment.put("MATCH-STATUS", STATUS_ERROR + ":R01");
                log("加盟店未登録 入金ID=" + payment.get("PAYMENT-ID") + " 加盟店=" + merchantCode);
                continue;
            }
            if (!MERCHANT_ACTIVE.equals(merchant.get("MER-STATUS"))) {
                payment.put("MATCH-STATUS", STATUS_ERROR + ":R02");
                log("加盟店状態不正 入金ID=" + payment.get("PAYMENT-ID") + " 加盟店=" + merchantCode);
                continue;
            }

            long amount = yen(payment.get("PAYMENT-AMT"));
            if (amount <= 0L) {
                payment.put("MATCH-STATUS", STATUS_ERROR + ":R03");
                log("入金金額不正 入金ID=" + payment.get("PAYMENT-ID"));
                continue;
            }

            java.util.List<java.util.Map<String, String>> merchantBills =
                    billsByMerchant.getOrDefault(merchantCode, java.util.Collections.emptyList());
            java.util.Map<String, String> target = firstUnpaidBill(merchantBills);
            if (target == null) {
                payment.put("MATCH-STATUS", STATUS_ERROR + ":R04");
                log("未払請求なし 入金ID=" + payment.get("PAYMENT-ID") + " 加盟店=" + merchantCode);
                continue;
            }

            long accumulated = receivedByMerchant.getOrDefault(target.get("BILL-ID"), 0L) + amount;
            long billed = yen(target.get("FEE-TOTAL-AMT"));

            if (accumulated == billed) {
                target.put("STATUS", BILL_PAID);
                payment.put("MATCH-STATUS", accumulated == amount ? STATUS_MATCHED : STATUS_SPLIT);
                receivedByMerchant.remove(target.get("BILL-ID"));
                log("消込完了 入金ID=" + payment.get("PAYMENT-ID") + " 請求ID=" + target.get("BILL-ID"));
            } else if (accumulated < billed) {
                receivedByMerchant.put(target.get("BILL-ID"), accumulated);
                payment.put("MATCH-STATUS", STATUS_SPLIT + ":R05");
                log("分割入金保留 入金ID=" + payment.get("PAYMENT-ID") + " 請求ID=" + target.get("BILL-ID"));
            } else {
                target.put("STATUS", BILL_PAID);
                payment.put("MATCH-STATUS", STATUS_OVER + ":R06");
                receivedByMerchant.remove(target.get("BILL-ID"));
                log("過入金 入金ID=" + payment.get("PAYMENT-ID") + " 請求ID=" + target.get("BILL-ID")
                        + " 超過額=" + (accumulated - billed));
            }
        }

        log("処理件数 入金=" + pfpayf.size() + " 請求=" + pfbilf.size());
    }

    private static java.util.Map<String, String> firstUnpaidBill(java.util.List<java.util.Map<String, String>> bills) {
        for (java.util.Map<String, String> bill : bills) {
            if (BILL_UNPAID.equals(bill.get("STATUS"))) {
                return bill;
            }
        }
        return null;
    }

    private static java.util.Map<String, String> payment(
            String paymentId,
            String merchantCode,
            String paymentDt,
            String paymentAmt,
            String bankRefNo,
            String matchStatus) {
        java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
        row.put("PAYMENT-ID", paymentId);
        row.put("MERCHANT-CODE", merchantCode);
        row.put("PAYMENT-DT", paymentDt);
        row.put("PAYMENT-AMT", paymentAmt);
        row.put("BANK-REF-NO", bankRefNo);
        row.put("MATCH-STATUS", matchStatus);
        return row;
    }

    private static java.util.Map<String, String> bill(
            String billId,
            String merchantCode,
            String billingMonth,
            String feeTotalAmt,
            String taxAmt,
            String status,
            String dueDt) {
        java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
        row.put("BILL-ID", billId);
        row.put("MERCHANT-CODE", merchantCode);
        row.put("BILLING-MONTH", billingMonth);
        row.put("FEE-TOTAL-AMT", feeTotalAmt);
        row.put("TAX-AMT", taxAmt);
        row.put("STATUS", status);
        row.put("DUE-DT", dueDt);
        return row;
    }

    private static java.util.Map<String, String> merchant(
            String merchantCode,
            String merchantName,
            String category,
            String status) {
        java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
        row.put("MERCHANT-CODE", merchantCode);
        row.put("MERCHANT-NAME", merchantName);
        row.put("MER-CATEGORY", category);
        row.put("MER-STATUS", status);
        return row;
    }

    private static long yen(String value) {
        return Long.parseLong(value);
    }

    private static void log(String message) {
        System.out.println(message);
    }
}
