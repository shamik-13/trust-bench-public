package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-08-26  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class MerchantRefundNoticeService {
    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";
    private static final String DECLINE_WINDOW = "WIN";
    private static final String DECLINE_AMOUNT = "AMT";
    private static final String DECLINE_TRANSACTION = "TXN";
    private static final String DEST_MERCHANT = "M";
    private static final String STATUS_PENDING = "0";
    private static final String TEMPLATE_DETAIL = "MRN-DTL-01";
    private static final String TEMPLATE_SUMMARY = "MRN-SUM-01";

    public static void main(String[] a) throws Exception {
        java.nio.file.Path prrspf = a.length > 0 ? java.nio.file.Paths.get(a[0]) : java.nio.file.Paths.get("PRRSPF.csv");
        java.nio.file.Path prtxnf = a.length > 1 ? java.nio.file.Paths.get(a[1]) : java.nio.file.Paths.get("PRTXNF.csv");
        java.nio.file.Path prntf = a.length > 2 ? java.nio.file.Paths.get(a[2]) : java.nio.file.Paths.get("PRNTF.csv");

        java.util.List<ResultRequest> requests = loadRequests(prrspf);
        java.util.Map<String, OriginalTransaction> transactions = loadTransactions(prtxnf);
        java.util.List<NoticeRecord> notices = createNotices(requests, transactions, java.time.LocalDate.now());

        writeNotices(prntf, notices);
        System.out.println("通知作成件数=" + notices.size());
    }

    private static java.util.List<ResultRequest> loadRequests(java.nio.file.Path path) throws java.io.IOException {
        if (!java.nio.file.Files.exists(path)) {
            return sampleRequests();
        }

        java.util.List<ResultRequest> rows = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (line.trim().isEmpty() || line.startsWith("REQ-ID")) {
                continue;
            }
            String[] c = splitCsv(line, 5);
            rows.add(new ResultRequest(c[0], c[1], c[2], c[3], parseAmount(c[4])));
        }
        return rows;
    }

    private static java.util.Map<String, OriginalTransaction> loadTransactions(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<OriginalTransaction> rows;
        if (!java.nio.file.Files.exists(path)) {
            rows = sampleTransactions();
        } else {
            rows = new java.util.ArrayList<>();
            for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
                if (line.trim().isEmpty() || line.startsWith("ORIG-TXN-ID")) {
                    continue;
                }
                String[] c = splitCsv(line, 5);
                rows.add(new OriginalTransaction(c[0], c[1], c[2], parseAmount(c[3]), java.time.LocalDate.parse(c[4])));
            }
        }

        java.util.Map<String, OriginalTransaction> map = new java.util.HashMap<>();
        for (OriginalTransaction row : rows) {
            map.put(row.origTxnId, row);
        }
        return map;
    }

    private static java.util.List<NoticeRecord> createNotices(
            java.util.List<ResultRequest> requests,
            java.util.Map<String, OriginalTransaction> transactions,
            java.time.LocalDate sendDate) {
        java.util.List<NoticeRecord> notices = new java.util.ArrayList<>();
        java.util.Set<String> noticeKeys = new java.util.HashSet<>();

        for (ResultRequest request : requests) {
            validateRequest(request);
            if (!DECISION_DECLINE.equals(request.decisionKbn)) {
                continue;
            }

            OriginalTransaction txn = transactions.get(request.origTxnId);
            if (txn == null) {
                if (!DECLINE_TRANSACTION.equals(request.declineReason)) {
                    throw new IllegalStateException("原取引未検出: 依頼ID=" + request.reqId);
                }
                continue;
            }
            validateTransaction(txn);

            String key = request.reqId + "|" + txn.merchantCode;
            if (!noticeKeys.add(key)) {
                continue;
            }

            String templateId = selectTemplate(request.declineReason);
            String noticeId = buildNoticeId(sendDate, notices.size() + 1);
            notices.add(new NoticeRecord(noticeId, request.reqId, DEST_MERCHANT, templateId, STATUS_PENDING, sendDate));
        }
        return notices;
    }

    private static void validateRequest(ResultRequest request) {
        requireText(request.reqId, "依頼ID");
        requireText(request.origTxnId, "原取引ID");
        if (!DECISION_ACCEPT.equals(request.decisionKbn) && !DECISION_DECLINE.equals(request.decisionKbn)) {
            throw new IllegalArgumentException("判定区分不正: 依頼ID=" + request.reqId);
        }
        if (DECISION_DECLINE.equals(request.decisionKbn) && !isDeclineReason(request.declineReason)) {
            throw new IllegalArgumentException("辞退理由不正: 依頼ID=" + request.reqId);
        }
        if (request.eligibleAmt.signum() < 0) {
            throw new IllegalArgumentException("返金可能額不正: 依頼ID=" + request.reqId);
        }
    }

    private static void validateTransaction(OriginalTransaction txn) {
        requireText(txn.origTxnId, "原取引ID");
        requireText(txn.walletId, "ウォレットID");
        requireText(txn.merchantCode, "加盟店コード");
        if (txn.origTxnAmt.signum() <= 0) {
            throw new IllegalArgumentException("原取引金額不正: 原取引ID=" + txn.origTxnId);
        }
    }

    private static String selectTemplate(String declineReason) {
        if (DECLINE_AMOUNT.equals(declineReason)) {
            return TEMPLATE_DETAIL;
        }
        if (DECLINE_WINDOW.equals(declineReason) || DECLINE_TRANSACTION.equals(declineReason)) {
            return TEMPLATE_SUMMARY;
        }
        throw new IllegalArgumentException("辞退理由不正: " + declineReason);
    }

    private static boolean isDeclineReason(String value) {
        return DECLINE_WINDOW.equals(value) || DECLINE_AMOUNT.equals(value) || DECLINE_TRANSACTION.equals(value);
    }

    private static void writeNotices(java.nio.file.Path path, java.util.List<NoticeRecord> notices) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add("NOTICE-ID,REQ-ID,DEST-KBN,TEMPLATE-ID,SEND-STATUS,SEND-DT");
        for (NoticeRecord n : notices) {
            lines.add(joinCsv(n.noticeId, n.reqId, n.destKbn, n.templateId, n.sendStatus, n.sendDt.toString()));
        }
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static String buildNoticeId(java.time.LocalDate date, int sequence) {
        return "NTF" + date.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + String.format(java.util.Locale.ROOT, "%06d", sequence);
    }

    private static java.math.BigDecimal parseAmount(String value) {
        try {
            return new java.math.BigDecimal(value.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("金額形式不正: " + value, e);
        }
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "未設定");
        }
    }

    private static String[] splitCsv(String line, int expected) {
        String[] values = line.split(",", -1);
        if (values.length != expected) {
            throw new IllegalArgumentException("CSV項目数不正: " + line);
        }
        for (int i = 0; i < values.length; i++) {
            values[i] = values[i].trim();
        }
        return values;
    }

    private static String joinCsv(String... values) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            b.append(values[i]);
        }
        return b.toString();
    }

    private static java.util.List<ResultRequest> sampleRequests() {
        java.util.List<ResultRequest> rows = new java.util.ArrayList<>();
        rows.add(new ResultRequest("REQ202606280001", "TXN202606010001", DECISION_DECLINE, DECLINE_AMOUNT, new java.math.BigDecimal("14800")));
        rows.add(new ResultRequest("REQ202606280002", "TXN202605120081", DECISION_DECLINE, DECLINE_WINDOW, new java.math.BigDecimal("3200")));
        rows.add(new ResultRequest("REQ202606280003", "TXN202606180442", DECISION_ACCEPT, "", new java.math.BigDecimal("950")));
        rows.add(new ResultRequest("REQ202606280004", "TXN202604300019", DECISION_DECLINE, DECLINE_TRANSACTION, java.math.BigDecimal.ZERO));
        return rows;
    }

    private static java.util.List<OriginalTransaction> sampleTransactions() {
        java.util.List<OriginalTransaction> rows = new java.util.ArrayList<>();
        rows.add(new OriginalTransaction("TXN202606010001", "WLT00021991", "MRC10004567", new java.math.BigDecimal("12800"), java.time.LocalDate.of(2026, 6, 1)));
        rows.add(new OriginalTransaction("TXN202605120081", "WLT00018720", "MRC10000812", new java.math.BigDecimal("3200"), java.time.LocalDate.of(2026, 5, 12)));
        rows.add(new OriginalTransaction("TXN202606180442", "WLT00030118", "MRC10009104", new java.math.BigDecimal("950"), java.time.LocalDate.of(2026, 6, 18)));
        return rows;
    }

    private static final class ResultRequest {
        private final String reqId;
        private final String origTxnId;
        private final String decisionKbn;
        private final String declineReason;
        private final java.math.BigDecimal eligibleAmt;

        private ResultRequest(String reqId, String origTxnId, String decisionKbn, String declineReason, java.math.BigDecimal eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class OriginalTransaction {
        private final String origTxnId;
        private final String walletId;
        private final String merchantCode;
        private final java.math.BigDecimal origTxnAmt;
        private final java.time.LocalDate origTxnDt;

        private OriginalTransaction(String origTxnId, String walletId, String merchantCode, java.math.BigDecimal origTxnAmt, java.time.LocalDate origTxnDt) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
        }
    }

    private static final class NoticeRecord {
        private final String noticeId;
        private final String reqId;
        private final String destKbn;
        private final String templateId;
        private final String sendStatus;
        private final java.time.LocalDate sendDt;

        private NoticeRecord(String noticeId, String reqId, String destKbn, String templateId, String sendStatus, java.time.LocalDate sendDt) {
            this.noticeId = noticeId;
            this.reqId = reqId;
            this.destKbn = destKbn;
            this.templateId = templateId;
            this.sendStatus = sendStatus;
            this.sendDt = sendDt;
        }
    }
}
