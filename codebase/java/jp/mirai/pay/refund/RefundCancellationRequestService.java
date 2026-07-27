package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-12-10  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundCancellationRequestService {
    private static final String DECISION_ACCEPTED = "A";
    private static final String DECISION_DECLINED = "D";

    public static void main(String[] a) {
        java.time.LocalDate cancelDate = java.time.LocalDate.of(2026, 6, 28);
        String operatorId = "OPR9017";

        java.util.List<java.util.Map<String, String>> prrspf = new java.util.ArrayList<>();
        prrspf.add(row("REQ-ID", "RQ202606240001", "ORIG-TXN-ID", "TXN202606180045", "DECISION-KBN", "A", "DECLINE-REASON", "", "ELIGIBLE-AMT", "12800"));
        prrspf.add(row("REQ-ID", "RQ202606240002", "ORIG-TXN-ID", "TXN202606190018", "DECISION-KBN", "D", "DECLINE-REASON", "AMT", "ELIGIBLE-AMT", "0"));
        prrspf.add(row("REQ-ID", "RQ202606250003", "ORIG-TXN-ID", "TXN202606200077", "DECISION-KBN", "A", "DECLINE-REASON", "", "ELIGIBLE-AMT", "5400"));
        prrspf.add(row("REQ-ID", "RQ202606250004", "ORIG-TXN-ID", "TXN202606210033", "DECISION-KBN", "D", "DECLINE-REASON", "TXN", "ELIGIBLE-AMT", "0"));
        prrspf.add(row("REQ-ID", "RQ202606260005", "ORIG-TXN-ID", "TXN202606220109", "DECISION-KBN", "D", "DECLINE-REASON", "WIN", "ELIGIBLE-AMT", "0"));

        java.util.List<java.util.Map<String, String>> prreqf = new java.util.ArrayList<>();
        prreqf.add(row("REQ-ID", "RQ202606240001", "ORIG-TXN-ID", "TXN202606180045", "REFUND-AMT", "12800", "REQ-DT", "2026-06-24", "REQ-REASON", "20"));
        prreqf.add(row("REQ-ID", "RQ202606240002", "ORIG-TXN-ID", "TXN202606190018", "REFUND-AMT", "19000", "REQ-DT", "2026-06-24", "REQ-REASON", "10"));
        prreqf.add(row("REQ-ID", "RQ202606250003", "ORIG-TXN-ID", "TXN202606200077", "REFUND-AMT", "5400", "REQ-DT", "2026-06-25", "REQ-REASON", "30"));
        prreqf.add(row("REQ-ID", "RQ202606250004", "ORIG-TXN-ID", "TXN202606210033", "REFUND-AMT", "3200", "REQ-DT", "2026-06-25", "REQ-REASON", "10"));
        prreqf.add(row("REQ-ID", "RQ202606260005", "ORIG-TXN-ID", "TXN202606220109", "REFUND-AMT", "8700", "REQ-DT", "2026-06-26", "REQ-REASON", "20"));

        java.util.List<java.util.Map<String, String>> cancelInput = new java.util.ArrayList<>();
        cancelInput.add(row("REQ-ID", "RQ202606240001", "ORIG-TXN-ID", "TXN202606180045", "CANCEL-REASON", "加盟店取消", "CANCEL-AMT", "12800", "NOTIFIED", "N", "LEDGER-POSTED", "N"));
        cancelInput.add(row("REQ-ID", "RQ202606240002", "ORIG-TXN-ID", "TXN202606190018", "CANCEL-REASON", "顧客申告取消", "CANCEL-AMT", "19000", "NOTIFIED", "N", "LEDGER-POSTED", "N"));
        cancelInput.add(row("REQ-ID", "RQ202606250003", "ORIG-TXN-ID", "TXN202606200077", "CANCEL-REASON", "金額訂正", "CANCEL-AMT", "5000", "NOTIFIED", "N", "LEDGER-POSTED", "N"));
        cancelInput.add(row("REQ-ID", "RQ202606260099", "ORIG-TXN-ID", "TXN202606229999", "CANCEL-REASON", "受付誤り", "CANCEL-AMT", "1200", "NOTIFIED", "N", "LEDGER-POSTED", "N"));
        cancelInput.add(row("REQ-ID", "RQ202606250003", "ORIG-TXN-ID", "TXN202606200077", "CANCEL-REASON", "通知後取消", "CANCEL-AMT", "5400", "NOTIFIED", "Y", "LEDGER-POSTED", "N"));

        java.util.List<java.util.Map<String, String>> prcanf = buildCancellationFile(prrspf, prreqf, cancelInput, cancelDate, operatorId);
        for (java.util.Map<String, String> record : prcanf) {
            System.out.println(record);
        }
    }

    private static java.util.List<java.util.Map<String, String>> buildCancellationFile(
            java.util.List<java.util.Map<String, String>> prrspf,
            java.util.List<java.util.Map<String, String>> prreqf,
            java.util.List<java.util.Map<String, String>> cancelInput,
            java.time.LocalDate cancelDate,
            String operatorId) {
        java.util.Map<String, java.util.Map<String, String>> responseByReqId = indexByReqId(prrspf);
        java.util.Map<String, java.util.Map<String, String>> requestByReqId = indexByReqId(prreqf);
        java.util.List<java.util.Map<String, String>> prcanf = new java.util.ArrayList<>();
        int sequence = 1;

        for (java.util.Map<String, String> cancel : cancelInput) {
            String reqId = cancel.get("REQ-ID");
            java.util.Map<String, String> request = requestByReqId.get(reqId);
            java.util.Map<String, String> response = responseByReqId.get(reqId);

            String rejectReason = judgeRejectReason(cancel, request, response);
            if (rejectReason != null) {
                prcanf.add(row(
                        "CANCEL-ID", "",
                        "REQ-ID", reqId,
                        "CANCEL-REASON", rejectReason,
                        "CANCEL-DT", cancelDate.toString(),
                        "OPERATOR-ID", operatorId));
                continue;
            }

            prcanf.add(row(
                    "CANCEL-ID", "CN" + cancelDate.toString().replace("-", "") + String.format("%04d", sequence++),
                    "REQ-ID", reqId,
                    "CANCEL-REASON", cancel.get("CANCEL-REASON"),
                    "CANCEL-DT", cancelDate.toString(),
                    "OPERATOR-ID", operatorId));
        }

        return prcanf;
    }

    private static String judgeRejectReason(
            java.util.Map<String, String> cancel,
            java.util.Map<String, String> request,
            java.util.Map<String, String> response) {
        if (request == null || response == null) {
            return "取消不可:依頼番号なし";
        }
        if (!same(cancel, request, "ORIG-TXN-ID")) {
            return "取消不可:原取引不一致";
        }
        if (DECISION_DECLINED.equals(response.get("DECISION-KBN"))) {
            return "取消不可:否認済-" + response.get("DECLINE-REASON");
        }
        if (!DECISION_ACCEPTED.equals(response.get("DECISION-KBN"))) {
            return "取消不可:判定区分不正";
        }
        if (new java.math.BigDecimal(cancel.get("CANCEL-AMT")).compareTo(new java.math.BigDecimal(request.get("REFUND-AMT"))) != 0) {
            return "取消不可:金額不一致";
        }
        if (new java.math.BigDecimal(cancel.get("CANCEL-AMT")).compareTo(new java.math.BigDecimal(response.get("ELIGIBLE-AMT"))) != 0) {
            return "取消不可:承認額不一致";
        }
        if ("Y".equals(cancel.get("NOTIFIED")) && "Y".equals(cancel.get("LEDGER-POSTED"))) {
            return "取消不可:通知台帳反映済";
        }
        return null;
    }

    private static java.util.Map<String, java.util.Map<String, String>> indexByReqId(java.util.List<java.util.Map<String, String>> records) {
        java.util.Map<String, java.util.Map<String, String>> indexed = new java.util.LinkedHashMap<>();
        for (java.util.Map<String, String> record : records) {
            indexed.put(record.get("REQ-ID"), record);
        }
        return indexed;
    }

    private static boolean same(java.util.Map<String, String> left, java.util.Map<String, String> right, String key) {
        return java.util.Objects.equals(left.get(key), right.get(key));
    }

    private static java.util.Map<String, String> row(String... values) {
        java.util.Map<String, String> row = new java.util.LinkedHashMap<>();
        for (int i = 0; i < values.length; i += 2) {
            row.put(values[i], values[i + 1]);
        }
        return row;
    }
}
