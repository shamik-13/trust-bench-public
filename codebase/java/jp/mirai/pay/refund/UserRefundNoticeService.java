package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-08-05  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class UserRefundNoticeService {
    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";
    private static final String DECLINE_WINDOW = "WIN";
    private static final String DECLINE_AMOUNT = "AMT";
    private static final String DECLINE_TXN = "TXN";

    public static void main(String[] a) {
        java.util.List<ResponseRecord> prrspf = java.util.Arrays.asList(
                new ResponseRecord("REQ000001", "TXN000001", "A", "", 1280L),
                new ResponseRecord("REQ000002", "TXN000002", "D", "WIN", 0L),
                new ResponseRecord("REQ000003", "TXN000003", "D", "AMT", 0L),
                new ResponseRecord("REQ000004", "TXN999999", "D", "TXN", 0L),
                new ResponseRecord("REQ000001", "TXN000001", "A", "", 1280L)
        );

        java.util.List<RequestRecord> prreqf = java.util.Arrays.asList(
                new RequestRecord("REQ000001", "TXN000001", 1280L, "20260627", "20"),
                new RequestRecord("REQ000002", "TXN000002", 500L, "20260627", "10"),
                new RequestRecord("REQ000003", "TXN000003", 9800L, "20260627", "30"),
                new RequestRecord("REQ000004", "TXN999999", 1200L, "20260627", "20")
        );

        java.util.List<TransactionRecord> prtxnf = java.util.Arrays.asList(
                new TransactionRecord("TXN000001", "WLT10001", "MRC001", 1280L, "20260625"),
                new TransactionRecord("TXN000002", "WLT10002", "MRC002", 2600L, "20260501"),
                new TransactionRecord("TXN000003", "WLT10003", "MRC003", 3200L, "20260620")
        );

        java.util.List<MessageRecord> pnmsgf = createNoticeMessages(prrspf, prreqf, prtxnf);
        for (MessageRecord message : pnmsgf) {
            System.out.println(message.toLine());
        }
    }

    private static java.util.List<MessageRecord> createNoticeMessages(
            java.util.List<ResponseRecord> responses,
            java.util.List<RequestRecord> requests,
            java.util.List<TransactionRecord> transactions) {

        java.util.Map<String, RequestRecord> requestByReqId = new java.util.LinkedHashMap<>();
        for (RequestRecord request : requests) {
            requestByReqId.putIfAbsent(request.reqId, request);
        }

        java.util.Map<String, TransactionRecord> transactionByOrigTxnId = new java.util.LinkedHashMap<>();
        for (TransactionRecord transaction : transactions) {
            transactionByOrigTxnId.putIfAbsent(transaction.origTxnId, transaction);
        }

        java.util.Set<String> notifiedReqIds = new java.util.HashSet<>();
        java.util.List<MessageRecord> messages = new java.util.ArrayList<>();
        int messageSeq = 1;

        for (ResponseRecord response : responses) {
            if (response.reqId == null || response.reqId.isEmpty() || notifiedReqIds.contains(response.reqId)) {
                continue;
            }

            RequestRecord request = requestByReqId.get(response.reqId);
            if (request == null || !sameKey(response.origTxnId, request.origTxnId)) {
                continue;
            }

            TransactionRecord transaction = transactionByOrigTxnId.get(response.origTxnId);
            String walletId = transaction == null ? "" : transaction.walletId;
            String body = buildBody(response, request, transaction);
            if (body.isEmpty()) {
                continue;
            }

            messages.add(new MessageRecord(
                    String.format("MSG%08d", messageSeq++),
                    response.reqId,
                    walletId,
                    "01",
                    body,
                    "0"));
            notifiedReqIds.add(response.reqId);
        }

        return messages;
    }

    private static String buildBody(ResponseRecord response, RequestRecord request, TransactionRecord transaction) {
        if (DECISION_ACCEPT.equals(response.decisionKbn)) {
            if (transaction == null) {
                return "";
            }
            long scheduledAmount = Math.min(response.eligibleAmt, request.refundAmt);
            if (scheduledAmount <= 0L || scheduledAmount > transaction.origTxnAmt) {
                return "";
            }
            return "返金依頼 " + response.reqId + " は受付済です。返金予定額は"
                    + scheduledAmount + "円です。";
        }

        if (DECISION_DECLINE.equals(response.decisionKbn)) {
            String reasonText = declineReasonText(response.declineReason);
            if (reasonText.isEmpty()) {
                return "";
            }
            return "返金依頼 " + response.reqId + " は受付できませんでした。理由："
                    + reasonText + "。";
        }

        return "";
    }

    private static String declineReasonText(String reason) {
        if (DECLINE_WINDOW.equals(reason)) {
            return "返金受付期間超過";
        }
        if (DECLINE_AMOUNT.equals(reason)) {
            return "返金額が原取引額超過";
        }
        if (DECLINE_TXN.equals(reason)) {
            return "原取引なし";
        }
        return "";
    }

    private static boolean sameKey(String left, String right) {
        return left != null && left.equals(right);
    }

    private static final class ResponseRecord {
        private final String reqId;
        private final String origTxnId;
        private final String decisionKbn;
        private final String declineReason;
        private final long eligibleAmt;

        private ResponseRecord(String reqId, String origTxnId, String decisionKbn, String declineReason, long eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class RequestRecord {
        private final String reqId;
        private final String origTxnId;
        private final long refundAmt;
        private final String reqDt;
        private final String reqReason;

        private RequestRecord(String reqId, String origTxnId, long refundAmt, String reqDt, String reqReason) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.refundAmt = refundAmt;
            this.reqDt = reqDt;
            this.reqReason = reqReason;
        }
    }

    private static final class TransactionRecord {
        private final String origTxnId;
        private final String walletId;
        private final String merchantCode;
        private final long origTxnAmt;
        private final String origTxnDt;

        private TransactionRecord(String origTxnId, String walletId, String merchantCode, long origTxnAmt, String origTxnDt) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
        }
    }

    private static final class MessageRecord {
        private final String messageId;
        private final String reqId;
        private final String walletId;
        private final String channelKbn;
        private final String messageBody;
        private final String deliveryKbn;

        private MessageRecord(String messageId, String reqId, String walletId, String channelKbn, String messageBody, String deliveryKbn) {
            this.messageId = messageId;
            this.reqId = reqId;
            this.walletId = walletId;
            this.channelKbn = channelKbn;
            this.messageBody = messageBody;
            this.deliveryKbn = deliveryKbn;
        }

        private String toLine() {
            return messageId + "," + reqId + "," + walletId + "," + channelKbn + ","
                    + messageBody + "," + deliveryKbn;
        }
    }
}
