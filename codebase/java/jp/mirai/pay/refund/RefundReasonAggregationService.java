package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数    年月日      担当                                概要
 * 1.00    2025-02-04  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundReasonAggregationService {

    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";

    private static final String REASON_CUSTOMER = "10";
    private static final String REASON_MERCHANT = "20";
    private static final String REASON_CHARGEBACK = "30";

    private static final String DEFAULT_GROUP = "未分類";

    public static void main(String[] a) throws Exception {
        if (a.length != 4) {
            throw new IllegalArgumentException("起動引数はPRREQF, PRRSPF, PRRSNF, PRRPTF2の順に指定してください。");
        }

        RefundReasonAggregationService service = new RefundReasonAggregationService();
        service.aggregate(
                java.nio.file.Paths.get(a[0]),
                java.nio.file.Paths.get(a[1]),
                java.nio.file.Paths.get(a[2]),
                java.nio.file.Paths.get(a[3]));
    }

    void aggregate(
            java.nio.file.Path prreqf,
            java.nio.file.Path prrspf,
            java.nio.file.Path prrsnf,
            java.nio.file.Path prrptf2) throws java.io.IOException {

        java.util.Map<String, ReasonDefinition> reasonMap = readReasonDefinitions(prrsnf);
        java.util.Map<String, ResponseRecord> responseMap = readResponses(prrspf);
        java.util.Map<AggregateKey, AggregateValue> aggregateMap = new java.util.TreeMap<>();

        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(prreqf, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isSkippable(line)) {
                    continue;
                }

                RequestRecord request = parseRequest(line, lineNo);
                validateRequest(request, lineNo);

                ResponseRecord response = responseMap.get(request.reqId);
                String reasonGroup = resolveReasonGroup(reasonMap, request.reqReason);
                String merchantCode = resolveMerchantCode(request.origTxnId);
                AggregateKey key = new AggregateKey(request.reqDt, merchantCode, reasonGroup);
                AggregateValue value = aggregateMap.computeIfAbsent(key, k -> new AggregateValue());

                if (response == null) {
                    value.pendingCnt++;
                    continue;
                }

                validateResponsePair(request, response, lineNo);

                if (DECISION_ACCEPT.equals(response.decisionKbn)) {
                    value.refundCnt++;
                    value.refundAmt = value.refundAmt.add(effectiveRefundAmount(request, response));
                } else if (DECISION_DECLINE.equals(response.decisionKbn)) {
                    value.declineCnt++;
                } else {
                    value.pendingCnt++;
                }
            }
        }

        writeReport(prrptf2, aggregateMap);
    }

    private java.util.Map<String, ReasonDefinition> readReasonDefinitions(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, ReasonDefinition> map = new java.util.HashMap<>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isSkippable(line)) {
                    continue;
                }

                String[] cols = splitCsv(line, 4, "PRRSNF", lineNo);
                ReasonDefinition definition = new ReasonDefinition(
                        cols[0].trim(),
                        cols[1].trim(),
                        parseDecimal(cols[2], "RISK-WEIGHT", lineNo),
                        cols[3].trim());

                if (definition.reasonCode.isEmpty() || definition.reasonGroup.isEmpty()) {
                    throw new IllegalArgumentException("PRRSNF " + lineNo + "行目の理由定義が不正です。");
                }
                map.put(definition.reasonCode, definition);
            }
        }
        return map;
    }

    private java.util.Map<String, ResponseRecord> readResponses(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, ResponseRecord> map = new java.util.HashMap<>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isSkippable(line)) {
                    continue;
                }

                String[] cols = splitCsv(line, 5, "PRRSPF", lineNo);
                ResponseRecord response = new ResponseRecord(
                        cols[0].trim(),
                        cols[1].trim(),
                        cols[2].trim(),
                        cols[3].trim(),
                        parseMoney(cols[4], "ELIGIBLE-AMT", lineNo));

                validateResponse(response, lineNo);

                ResponseRecord old = map.put(response.reqId, response);
                if (old != null) {
                    throw new IllegalArgumentException("PRRSPF " + lineNo + "行目のREQ-IDが重複しています。");
                }
            }
        }
        return map;
    }

    private RequestRecord parseRequest(String line, int lineNo) {
        String[] cols = splitCsv(line, 5, "PRREQF", lineNo);
        return new RequestRecord(
                cols[0].trim(),
                cols[1].trim(),
                parseMoney(cols[2], "REFUND-AMT", lineNo),
                parseDate(cols[3], "REQ-DT", lineNo),
                cols[4].trim());
    }

    private void validateRequest(RequestRecord request, int lineNo) {
        if (request.reqId.isEmpty() || request.origTxnId.isEmpty()) {
            throw new IllegalArgumentException("PRREQF " + lineNo + "行目の依頼キーが不正です。");
        }
        if (request.refundAmt.signum() < 0) {
            throw new IllegalArgumentException("PRREQF " + lineNo + "行目の返金金額が不正です。");
        }
        if (!REASON_CUSTOMER.equals(request.reqReason)
                && !REASON_MERCHANT.equals(request.reqReason)
                && !REASON_CHARGEBACK.equals(request.reqReason)) {
            throw new IllegalArgumentException("PRREQF " + lineNo + "行目の依頼理由が不正です。");
        }
    }

    private void validateResponse(ResponseRecord response, int lineNo) {
        if (response.reqId.isEmpty() || response.origTxnId.isEmpty()) {
            throw new IllegalArgumentException("PRRSPF " + lineNo + "行目の判定キーが不正です。");
        }
        if (!DECISION_ACCEPT.equals(response.decisionKbn) && !DECISION_DECLINE.equals(response.decisionKbn)) {
            throw new IllegalArgumentException("PRRSPF " + lineNo + "行目の判定区分が不正です。");
        }
        if (DECISION_DECLINE.equals(response.decisionKbn)
                && !("WIN".equals(response.declineReason)
                || "AMT".equals(response.declineReason)
                || "TXN".equals(response.declineReason))) {
            throw new IllegalArgumentException("PRRSPF " + lineNo + "行目の辞退理由が不正です。");
        }
        if (response.eligibleAmt.signum() < 0) {
            throw new IllegalArgumentException("PRRSPF " + lineNo + "行目の対象金額が不正です。");
        }
    }

    private void validateResponsePair(RequestRecord request, ResponseRecord response, int lineNo) {
        if (!request.origTxnId.equals(response.origTxnId)) {
            throw new IllegalArgumentException("PRREQF " + lineNo + "行目の原取引IDとPRRSPFの原取引IDが一致しません。");
        }
    }

    private java.math.BigDecimal effectiveRefundAmount(RequestRecord request, ResponseRecord response) {
        if (response.eligibleAmt.signum() == 0) {
            return request.refundAmt;
        }
        return request.refundAmt.min(response.eligibleAmt);
    }

    private String resolveReasonGroup(java.util.Map<String, ReasonDefinition> reasonMap, String reasonCode) {
        ReasonDefinition definition = reasonMap.get(reasonCode);
        if (definition == null) {
            return DEFAULT_GROUP;
        }
        return definition.reasonGroup;
    }

    private String resolveMerchantCode(String origTxnId) {
        int pos = origTxnId.indexOf('-');
        if (pos > 0) {
            return origTxnId.substring(0, pos);
        }
        if (origTxnId.length() >= 8) {
            return origTxnId.substring(0, 8);
        }
        return origTxnId;
    }

    private void writeReport(java.nio.file.Path path, java.util.Map<AggregateKey, AggregateValue> aggregateMap) throws java.io.IOException {
        try (java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(path, java.nio.charset.StandardCharsets.UTF_8)) {
            int seq = 1;
            for (java.util.Map.Entry<AggregateKey, AggregateValue> entry : aggregateMap.entrySet()) {
                AggregateKey key = entry.getKey();
                AggregateValue value = entry.getValue();
                String reportId = String.format(
                        java.util.Locale.ROOT,
                        "RR%08d%05d",
                        Integer.parseInt(key.reportDt.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)),
                        seq++);

                writer.write(joinCsv(
                        reportId,
                        key.reportDt.toString(),
                        key.merchantCode,
                        String.valueOf(value.refundCnt),
                        value.refundAmt.setScale(0, java.math.RoundingMode.DOWN).toPlainString(),
                        String.valueOf(value.declineCnt)));
                writer.newLine();
            }
        }
    }

    private static boolean isSkippable(String line) {
        String trimmed = line.trim();
        return trimmed.isEmpty() || trimmed.startsWith("#");
    }

    private static String[] splitCsv(String line, int expected, String fileName, int lineNo) {
        String[] cols = line.split(",", -1);
        if (cols.length != expected) {
            throw new IllegalArgumentException(fileName + " " + lineNo + "行目の項目数が不正です。");
        }
        return cols;
    }

    private static java.time.LocalDate parseDate(String value, String itemName, int lineNo) {
        try {
            return java.time.LocalDate.parse(value.trim(), java.time.format.DateTimeFormatter.ISO_LOCAL_DATE);
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException(itemName + " " + lineNo + "行目の日付形式が不正です。", e);
        }
    }

    private static java.math.BigDecimal parseMoney(String value, String itemName, int lineNo) {
        java.math.BigDecimal parsed = parseDecimal(value, itemName, lineNo);
        return parsed.setScale(0, java.math.RoundingMode.UNNECESSARY);
    }

    private static java.math.BigDecimal parseDecimal(String value, String itemName, int lineNo) {
        try {
            return new java.math.BigDecimal(value.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException(itemName + " " + lineNo + "行目の数値形式が不正です。", e);
        }
    }

    private static String joinCsv(String... values) {
        return String.join(",", values);
    }

    private static final class RequestRecord {
        final String reqId;
        final String origTxnId;
        final java.math.BigDecimal refundAmt;
        final java.time.LocalDate reqDt;
        final String reqReason;

        RequestRecord(String reqId, String origTxnId, java.math.BigDecimal refundAmt, java.time.LocalDate reqDt, String reqReason) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.refundAmt = refundAmt;
            this.reqDt = reqDt;
            this.reqReason = reqReason;
        }
    }

    private static final class ResponseRecord {
        final String reqId;
        final String origTxnId;
        final String decisionKbn;
        final String declineReason;
        final java.math.BigDecimal eligibleAmt;

        ResponseRecord(String reqId, String origTxnId, String decisionKbn, String declineReason, java.math.BigDecimal eligibleAmt) {
            this.reqId = reqId;
            this.origTxnId = origTxnId;
            this.decisionKbn = decisionKbn;
            this.declineReason = declineReason;
            this.eligibleAmt = eligibleAmt;
        }
    }

    private static final class ReasonDefinition {
        final String reasonCode;
        final String reasonGroup;
        final java.math.BigDecimal riskWeight;
        final String autoReviewKbn;

        ReasonDefinition(String reasonCode, String reasonGroup, java.math.BigDecimal riskWeight, String autoReviewKbn) {
            this.reasonCode = reasonCode;
            this.reasonGroup = reasonGroup;
            this.riskWeight = riskWeight;
            this.autoReviewKbn = autoReviewKbn;
        }
    }

    private static final class AggregateKey implements Comparable<AggregateKey> {
        final java.time.LocalDate reportDt;
        final String merchantCode;
        final String reasonGroup;

        AggregateKey(java.time.LocalDate reportDt, String merchantCode, String reasonGroup) {
            this.reportDt = reportDt;
            this.merchantCode = merchantCode;
            this.reasonGroup = reasonGroup;
        }

        @Override
        public int compareTo(AggregateKey other) {
            int result = reportDt.compareTo(other.reportDt);
            if (result != 0) {
                return result;
            }
            result = merchantCode.compareTo(other.merchantCode);
            if (result != 0) {
                return result;
            }
            return reasonGroup.compareTo(other.reasonGroup);
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof AggregateKey)) {
                return false;
            }
            AggregateKey other = (AggregateKey) obj;
            return reportDt.equals(other.reportDt)
                    && merchantCode.equals(other.merchantCode)
                    && reasonGroup.equals(other.reasonGroup);
        }

        @Override
        public int hashCode() {
            return java.util.Objects.hash(reportDt, merchantCode, reasonGroup);
        }
    }

    private static final class AggregateValue {
        long refundCnt;
        java.math.BigDecimal refundAmt = java.math.BigDecimal.ZERO;
        long declineCnt;
        long pendingCnt;
    }
}
