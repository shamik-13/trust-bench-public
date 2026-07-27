package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.0   2024-09-24  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundInquiryFacadeService {
    private static final String DECISION_ACCEPTED = "A";
    private static final String DECISION_DECLINED = "D";

    private static final String DECLINE_WINDOW = "WIN";
    private static final String DECLINE_AMOUNT = "AMT";
    private static final String DECLINE_TXN = "TXN";

    public static void main(String[] a) {
        java.util.List<java.util.Map<String, String>> requests = new java.util.ArrayList<>();
        requests.add(row("REQ-ID", "RQ202606280001", "ORIG-TXN-ID", "TX202606010091", "REFUND-AMT", "1200", "REQ-DT", "20260628", "REQ-REASON", "20"));
        requests.add(row("REQ-ID", "RQ202606280002", "ORIG-TXN-ID", "TX202604110044", "REFUND-AMT", "9800", "REQ-DT", "20260628", "REQ-REASON", "10"));
        requests.add(row("REQ-ID", "RQ202606280003", "ORIG-TXN-ID", "TX202606180018", "REFUND-AMT", "30000", "REQ-DT", "20260628", "REQ-REASON", "30"));
        requests.add(row("REQ-ID", "RQ202606280004", "ORIG-TXN-ID", "TX999999999999", "REFUND-AMT", "700", "REQ-DT", "20260628", "REQ-REASON", "20"));

        java.util.List<java.util.Map<String, String>> responses = new java.util.ArrayList<>();
        responses.add(row("REQ-ID", "RQ202606280001", "ORIG-TXN-ID", "TX202606010091", "DECISION-KBN", "A", "DECLINE-REASON", "", "ELIGIBLE-AMT", "1200"));
        responses.add(row("REQ-ID", "RQ202606280002", "ORIG-TXN-ID", "TX202604110044", "DECISION-KBN", "D", "DECLINE-REASON", "WIN", "ELIGIBLE-AMT", "0"));
        responses.add(row("REQ-ID", "RQ202606280003", "ORIG-TXN-ID", "TX202606180018", "DECISION-KBN", "D", "DECLINE-REASON", "AMT", "ELIGIBLE-AMT", "14500"));
        responses.add(row("REQ-ID", "RQ202606280004", "ORIG-TXN-ID", "TX999999999999", "DECISION-KBN", "D", "DECLINE-REASON", "TXN", "ELIGIBLE-AMT", "0"));

        java.util.List<java.util.Map<String, String>> transactions = new java.util.ArrayList<>();
        transactions.add(row("ORIG-TXN-ID", "TX202606010091", "WALLET-ID", "WL00007192", "MERCHANT-CODE", "MRC10042", "ORIG-TXN-AMT", "6800", "ORIG-TXN-DT", "20260601"));
        transactions.add(row("ORIG-TXN-ID", "TX202604110044", "WALLET-ID", "WL00008110", "MERCHANT-CODE", "MRC30008", "ORIG-TXN-AMT", "9800", "ORIG-TXN-DT", "20260411"));
        transactions.add(row("ORIG-TXN-ID", "TX202606180018", "WALLET-ID", "WL00007192", "MERCHANT-CODE", "MRC20991", "ORIG-TXN-AMT", "14500", "ORIG-TXN-DT", "20260618"));

        java.util.List<java.util.Map<String, String>> balances = new java.util.ArrayList<>();
        balances.add(row("WALLET-ID", "WL00007192", "AVAILABLE-BAL", "42100", "PENDING-REFUND-AMT", "1200", "LAST-ADJ-DT", "20260627"));
        balances.add(row("WALLET-ID", "WL00008110", "AVAILABLE-BAL", "6300", "PENDING-REFUND-AMT", "0", "LAST-ADJ-DT", "20260625"));

        java.util.List<java.util.Map<String, String>> frauds = new java.util.ArrayList<>();
        frauds.add(row("FRAUD-ID", "FR202606280011", "REQ-ID", "RQ202606280001", "WALLET-ID", "WL00007192", "SCORE", "12", "RULE-HIT-CD", "N00", "JUDGE-DT", "20260628"));
        frauds.add(row("FRAUD-ID", "FR202606280012", "REQ-ID", "RQ202606280003", "WALLET-ID", "WL00007192", "SCORE", "78", "RULE-HIT-CD", "C31", "JUDGE-DT", "20260628"));

        java.util.List<java.util.Map<String, String>> inquiryRows = buildInquiryRows(requests, responses, transactions, balances, frauds);
        for (java.util.Map<String, String> inquiryRow : inquiryRows) {
            System.out.println(formatInquiryRow(inquiryRow));
        }
    }

    private static java.util.List<java.util.Map<String, String>> buildInquiryRows(
            java.util.List<java.util.Map<String, String>> requests,
            java.util.List<java.util.Map<String, String>> responses,
            java.util.List<java.util.Map<String, String>> transactions,
            java.util.List<java.util.Map<String, String>> balances,
            java.util.List<java.util.Map<String, String>> frauds) {
        java.util.Map<String, java.util.Map<String, String>> responseByRequestId = indexBy(responses, "REQ-ID");
        java.util.Map<String, java.util.Map<String, String>> transactionByOrigTxnId = indexBy(transactions, "ORIG-TXN-ID");
        java.util.Map<String, java.util.Map<String, String>> balanceByWalletId = indexBy(balances, "WALLET-ID");
        java.util.Map<String, java.util.Map<String, String>> fraudByRequestId = indexBy(frauds, "REQ-ID");

        java.util.List<java.util.Map<String, String>> result = new java.util.ArrayList<>();
        for (java.util.Map<String, String> request : requests) {
            java.util.Map<String, String> response = responseByRequestId.get(request.get("REQ-ID"));
            java.util.Map<String, String> transaction = transactionByOrigTxnId.get(request.get("ORIG-TXN-ID"));

            String walletId = value(transaction, "WALLET-ID");
            java.util.Map<String, String> balance = walletId.isEmpty() ? null : balanceByWalletId.get(walletId);
            java.util.Map<String, String> fraud = fraudByRequestId.get(request.get("REQ-ID"));

            java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
            row.put("REQ-ID", value(request, "REQ-ID"));
            row.put("ORIG-TXN-ID", value(request, "ORIG-TXN-ID"));
            row.put("REQ-DT", value(request, "REQ-DT"));
            row.put("REQ-REASON", value(request, "REQ-REASON"));
            row.put("REQ-REASON-NAME", reasonName(value(request, "REQ-REASON")));
            row.put("REFUND-AMT", value(request, "REFUND-AMT"));

            row.put("DECISION-KBN", value(response, "DECISION-KBN"));
            row.put("DECISION-NAME", decisionName(value(response, "DECISION-KBN")));
            row.put("DECLINE-REASON", value(response, "DECLINE-REASON"));
            row.put("DECLINE-REASON-NAME", declineReasonName(value(response, "DECLINE-REASON")));
            row.put("ELIGIBLE-AMT", value(response, "ELIGIBLE-AMT"));

            row.put("WALLET-ID", walletId);
            row.put("MERCHANT-CODE", value(transaction, "MERCHANT-CODE"));
            row.put("ORIG-TXN-AMT", value(transaction, "ORIG-TXN-AMT"));
            row.put("ORIG-TXN-DT", value(transaction, "ORIG-TXN-DT"));

            row.put("AVAILABLE-BAL", value(balance, "AVAILABLE-BAL"));
            row.put("PENDING-REFUND-AMT", value(balance, "PENDING-REFUND-AMT"));
            row.put("LAST-ADJ-DT", value(balance, "LAST-ADJ-DT"));

            row.put("FRAUD-ID", value(fraud, "FRAUD-ID"));
            row.put("SCORE", value(fraud, "SCORE"));
            row.put("RULE-HIT-CD", value(fraud, "RULE-HIT-CD"));
            row.put("JUDGE-DT", value(fraud, "JUDGE-DT"));
            row.put("CHECK-MEMO", checkMemo(request, response, transaction, balance, fraud));
            result.add(row);
        }
        return result;
    }

    private static String checkMemo(
            java.util.Map<String, String> request,
            java.util.Map<String, String> response,
            java.util.Map<String, String> transaction,
            java.util.Map<String, String> balance,
            java.util.Map<String, String> fraud) {
        java.util.List<String> notes = new java.util.ArrayList<>();

        if (response == null) {
            notes.add("判定応答未到着");
        }
        if (transaction == null) {
            notes.add("原取引参照不可");
        }
        if (transaction != null && parseLong(value(request, "REFUND-AMT")) > parseLong(value(transaction, "ORIG-TXN-AMT"))) {
            notes.add("返金額が原取引額超過");
        }
        if (balance == null && transaction != null) {
            notes.add("残高参考値なし");
        }
        if (fraud == null) {
            notes.add("不正検知結果なし");
        } else if (parseLong(value(fraud, "SCORE")) >= 70L) {
            notes.add("不正検知高スコア");
        }

        return notes.isEmpty() ? "確認事項なし" : String.join("、", notes);
    }

    private static java.util.Map<String, java.util.Map<String, String>> indexBy(
            java.util.List<java.util.Map<String, String>> rows, String key) {
        java.util.Map<String, java.util.Map<String, String>> index = new java.util.LinkedHashMap<>();
        for (java.util.Map<String, String> row : rows) {
            String keyValue = row.get(key);
            if (keyValue != null && !keyValue.isEmpty()) {
                index.put(keyValue, row);
            }
        }
        return index;
    }

    private static java.util.Map<String, String> row(String... keyValues) {
        if (keyValues.length % 2 != 0) {
            throw new IllegalArgumentException("keyValues must contain key/value pairs");
        }

        java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
        for (int i = 0; i < keyValues.length; i += 2) {
            row.put(keyValues[i], keyValues[i + 1]);
        }
        return row;
    }

    private static String value(java.util.Map<String, String> row, String key) {
        if (row == null) {
            return "";
        }
        String value = row.get(key);
        return value == null ? "" : value;
    }

    private static long parseLong(String value) {
        if (value == null || value.isEmpty()) {
            return 0L;
        }
        return Long.parseLong(value);
    }

    private static String reasonName(String code) {
        if ("10".equals(code)) {
            return "顧客都合";
        }
        if ("20".equals(code)) {
            return "加盟店都合";
        }
        if ("30".equals(code)) {
            return "チャージバック";
        }
        return "理由不明";
    }

    private static String decisionName(String code) {
        if (DECISION_ACCEPTED.equals(code)) {
            return "受付";
        }
        if (DECISION_DECLINED.equals(code)) {
            return "否認";
        }
        return "判定なし";
    }

    private static String declineReasonName(String code) {
        if (code == null || code.isEmpty()) {
            return "";
        }
        if (DECLINE_WINDOW.equals(code)) {
            return "返金受付期間超過";
        }
        if (DECLINE_AMOUNT.equals(code)) {
            return "返金額が原取引額超過";
        }
        if (DECLINE_TXN.equals(code)) {
            return "原取引なし";
        }
        return "否認理由不明";
    }

    private static String formatInquiryRow(java.util.Map<String, String> row) {
        return "依頼番号=" + row.get("REQ-ID")
                + ", 原取引番号=" + row.get("ORIG-TXN-ID")
                + ", 判定=" + row.get("DECISION-NAME")
                + ", 否認理由=" + row.get("DECLINE-REASON-NAME")
                + ", 返金依頼額=" + row.get("REFUND-AMT")
                + ", 返金可能額=" + row.get("ELIGIBLE-AMT")
                + ", ウォレット=" + row.get("WALLET-ID")
                + ", 加盟店=" + row.get("MERCHANT-CODE")
                + ", 残高=" + row.get("AVAILABLE-BAL")
                + ", 保留返金額=" + row.get("PENDING-REFUND-AMT")
                + ", 不正スコア=" + row.get("SCORE")
                + ", 確認=" + row.get("CHECK-MEMO");
    }
}
