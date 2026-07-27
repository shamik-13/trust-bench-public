package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2025/01/09  みらいペイ システム部 精算・連携チーム  初版作成
 */
public class PaymentNoticeService {
    public void run() {
        java.time.LocalDate settleDate = java.time.LocalDate.of(2024, 11, 30);

        java.util.List<java.util.Map<String, Object>> pcsumf = new java.util.ArrayList<>();
        pcsumf.add(row("MERCHANT-CODE", "M000001", "SETTLE-DATE", settleDate, "SETTLE-KBN", "月次", "TXN-COUNT", 128, "TOTAL-AMT", 3829100L, "CARRY-AMT", -12000L));
        pcsumf.add(row("MERCHANT-CODE", "M000001", "SETTLE-DATE", settleDate, "SETTLE-KBN", "月次", "TXN-COUNT", 47, "TOTAL-AMT", 1450200L, "CARRY-AMT", 0L));
        pcsumf.add(row("MERCHANT-CODE", "M000002", "SETTLE-DATE", settleDate, "SETTLE-KBN", "月次", "TXN-COUNT", 84, "TOTAL-AMT", 2198800L, "CARRY-AMT", 0L));
        pcsumf.add(row("MERCHANT-CODE", "M000003", "SETTLE-DATE", settleDate, "SETTLE-KBN", "月次", "TXN-COUNT", 9, "TOTAL-AMT", 177000L, "CARRY-AMT", 2500L));
        pcsumf.add(row("MERCHANT-CODE", "M000004", "SETTLE-DATE", settleDate, "SETTLE-KBN", "月次", "TXN-COUNT", 0, "TOTAL-AMT", 0L, "CARRY-AMT", -800L));

        java.util.List<java.util.Map<String, Object>> pccarf = new java.util.ArrayList<>();
        pccarf.add(row("CARRY-ID", "C202411-0001", "MERCHANT-CODE", "M000001", "SETTLE-KBN", "月次", "CARRY-AMT", -3000L, "CARRY-REASON", "返品相殺", "NEXT-SETTLE-DATE", settleDate));
        pccarf.add(row("CARRY-ID", "C202411-0002", "MERCHANT-CODE", "M000002", "SETTLE-KBN", "月次", "CARRY-AMT", 6100L, "CARRY-REASON", "前回不足", "NEXT-SETTLE-DATE", settleDate));
        pccarf.add(row("CARRY-ID", "C202411-0003", "MERCHANT-CODE", "M000003", "SETTLE-KBN", "月次", "CARRY-AMT", -180000L, "CARRY-REASON", "差押保留", "NEXT-SETTLE-DATE", settleDate));

        java.util.Map<String, java.util.Map<String, Object>> pjmstf = new java.util.HashMap<>();
        pjmstf.put("M000001", row("MERCHANT-CODE", "M000001", "MERCHANT-NAME", "青葉商店", "BANK-CODE", "0001", "ACCOUNT-NO", "1234567", "ACTIVE-FLAG", "1", "RISK-RANK", "A"));
        pjmstf.put("M000002", row("MERCHANT-CODE", "M000002", "MERCHANT-NAME", "日本橋家電", "BANK-CODE", "0138", "ACCOUNT-NO", "7654321", "ACTIVE-FLAG", "1", "RISK-RANK", "B"));
        pjmstf.put("M000003", row("MERCHANT-CODE", "M000003", "MERCHANT-NAME", "東都食品", "BANK-CODE", "0005", "ACCOUNT-NO", "", "ACTIVE-FLAG", "1", "RISK-RANK", "C"));
        pjmstf.put("M000004", row("MERCHANT-CODE", "M000004", "MERCHANT-NAME", "北町雑貨", "BANK-CODE", "9999", "ACCOUNT-NO", "2223334", "ACTIVE-FLAG", "0", "RISK-RANK", "B"));

        java.util.List<java.util.Map<String, Object>> pjntcf = createPaymentNotices(settleDate, pcsumf, pccarf, pjmstf);
        for (java.util.Map<String, Object> notice : pjntcf) {
            System.out.println("通知作成 " + notice.get("NOTICE-ID") + " 加盟店=" + notice.get("MERCHANT-CODE")
                    + " 入金予定額=" + notice.get("PAYMENT-AMT") + " 状態=" + notice.get("NOTICE-STATUS"));
            System.out.println("mipay_payment_notice 通知番号=" + notice.get("NOTICE-ID"));
        }
    }

    private static java.util.List<java.util.Map<String, Object>> createPaymentNotices(
            java.time.LocalDate settleDate,
            java.util.List<java.util.Map<String, Object>> pcsumf,
            java.util.List<java.util.Map<String, Object>> pccarf,
            java.util.Map<String, java.util.Map<String, Object>> pjmstf) {

        java.util.Map<String, Summary> summaries = new java.util.TreeMap<>();
        for (java.util.Map<String, Object> record : pcsumf) {
            if (!settleDate.equals(record.get("SETTLE-DATE"))) {
                continue;
            }
            String merchantCode = text(record.get("MERCHANT-CODE"));
            String settleKbn = text(record.get("SETTLE-KBN"));
            int txnCount = number(record.get("TXN-COUNT")).intValue();
            long totalAmount = number(record.get("TOTAL-AMT")).longValue();
            long carryAmount = number(record.get("CARRY-AMT")).longValue();

            if (merchantCode.isEmpty() || settleKbn.isEmpty() || txnCount < 0 || totalAmount < 0L) {
                System.out.println("精算サマリ不正 加盟店=" + merchantCode + " 区分=" + settleKbn);
                continue;
            }

            String key = merchantCode + "\u0000" + settleKbn;
            Summary summary = summaries.computeIfAbsent(key, k -> new Summary(merchantCode, settleKbn));
            summary.txnCount += txnCount;
            summary.totalAmount += totalAmount;
            summary.summaryCarryAmount += carryAmount;
        }

        for (java.util.Map<String, Object> carry : pccarf) {
            if (!settleDate.equals(carry.get("NEXT-SETTLE-DATE"))) {
                continue;
            }
            String merchantCode = text(carry.get("MERCHANT-CODE"));
            String settleKbn = text(carry.get("SETTLE-KBN"));
            long carryAmount = number(carry.get("CARRY-AMT")).longValue();
            String key = merchantCode + "\u0000" + settleKbn;
            Summary summary = summaries.computeIfAbsent(key, k -> new Summary(merchantCode, settleKbn));
            summary.detailCarryAmount += carryAmount;
        }

        java.util.List<java.util.Map<String, Object>> notices = new java.util.ArrayList<>();
        int sequence = 1;
        for (Summary summary : summaries.values()) {
            java.util.Map<String, Object> merchant = pjmstf.get(summary.merchantCode);
            if (!isValidMerchant(merchant)) {
                System.out.println("通知対象外 加盟店=" + summary.merchantCode + " 理由=銀行情報不正");
                continue;
            }

            long paymentAmount = summary.totalAmount + summary.summaryCarryAmount + summary.detailCarryAmount;
            if (summary.txnCount == 0 && paymentAmount == 0L) {
                System.out.println("通知対象外 加盟店=" + summary.merchantCode + " 理由=入金予定なし");
                continue;
            }
            if (paymentAmount <= 0L) {
                System.out.println("通知保留 加盟店=" + summary.merchantCode + " 理由=繰越超過 金額=" + paymentAmount);
                continue;
            }

            String noticeId = "PN" + settleDate.toString().replace("-", "") + String.format("%05d", sequence++);
            String bankRefNo = buildBankRefNo(merchant, noticeId);
            notices.add(row("NOTICE-ID", noticeId,
                    "MERCHANT-CODE", summary.merchantCode,
                    "SETTLE-DATE", settleDate,
                    "PAYMENT-AMT", paymentAmount,
                    "BANK-REF-NO", bankRefNo,
                    "NOTICE-STATUS", "作成"));
        }
        return notices;
    }

    private static boolean isValidMerchant(java.util.Map<String, Object> merchant) {
        if (merchant == null || !"1".equals(text(merchant.get("ACTIVE-FLAG")))) {
            return false;
        }
        String bankCode = text(merchant.get("BANK-CODE"));
        String accountNo = text(merchant.get("ACCOUNT-NO"));
        return bankCode.matches("\\d{4}") && !"0000".equals(bankCode) && accountNo.matches("\\d{7}");
    }

    private static String buildBankRefNo(java.util.Map<String, Object> merchant, String noticeId) {
        String bankCode = text(merchant.get("BANK-CODE"));
        String accountNo = text(merchant.get("ACCOUNT-NO"));
        String tail = noticeId.substring(noticeId.length() - 5);
        return bankCode + "-" + accountNo.substring(accountNo.length() - 3) + "-" + tail;
    }

    private static java.util.Map<String, Object> row(Object... values) {
        java.util.Map<String, Object> row = new java.util.LinkedHashMap<>();
        for (int i = 0; i < values.length; i += 2) {
            row.put(String.valueOf(values[i]), values[i + 1]);
        }
        return row;
    }

    private static String text(Object value) {
        return value == null ? "" : String.valueOf(value).trim();
    }

    private static Number number(Object value) {
        if (value instanceof Number) {
            return (Number) value;
        }
        if (value == null || String.valueOf(value).trim().isEmpty()) {
            return 0L;
        }
        return Long.valueOf(String.valueOf(value).trim());
    }

    private static final class Summary {
        private final String merchantCode;
        private final String settleKbn;
        private int txnCount;
        private long totalAmount;
        private long summaryCarryAmount;
        private long detailCarryAmount;

        private Summary(String merchantCode, String settleKbn) {
            this.merchantCode = merchantCode;
            this.settleKbn = settleKbn;
        }
    }
}
