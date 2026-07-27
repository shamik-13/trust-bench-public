package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.0   2024-05-13  みらいペイ システム部 返金・チャージバックチーム  返金判定投入サービス初版
 * 1.1   2025-03-02  みらいペイ システム部 返金・チャージバックチーム  判定結果はPRRSPFから読込む方式へ整理
 */
public class RefundDecisionFeedService {
    private static final String PRREQF = "PRREQF.txt";
    private static final String PRRSPF = "PRRSPF.txt";

    private static final String DECISION_ACCEPT = "A";
    private static final String DECISION_DECLINE = "D";

    private static final String DECLINE_WINDOW = "WIN";
    private static final String DECLINE_AMOUNT = "AMT";
    private static final String DECLINE_TXN = "TXN";

    public static void main(String[] a) throws Exception {
        java.nio.file.Path base = java.nio.file.Paths.get(a.length == 0 ? "." : a[0]);
        java.nio.file.Path reqPath = base.resolve(PRREQF);
        java.nio.file.Path rspPath = base.resolve(PRRSPF);

        // RefundEngine が出力した PRRSPF の判定 (RS-DECISION-KBN / RS-DECLINE-REASON) を取込む。
        // 受付可否そのものはエンジン側で確定済みであり、本サービスでは再判定しない。
        java.util.Map<String, ResponseRecord> decisionByReqId = loadDecisions(rspPath);

        int fed = 0;
        int waiting = 0;
        int invalid = 0;

        for (RequestRecord req : loadRequests(reqPath)) {
            ResponseRecord rsp = decisionByReqId.get(req.reqId);

            if (rsp == null) {
                waiting++;
                System.out.println("判定待ち REQ-ID=" + req.reqId);
                continue;
            }
            if (!req.origTxnId.equals(rsp.origTxnId)) {
                invalid++;
                System.err.println("原取引不一致のため投入抑止: " + req.reqId);
                continue;
            }
            if (!isValidDecision(rsp)) {
                invalid++;
                System.err.println("判定区分不正のため投入抑止: " + req.reqId);
                continue;
            }

            fed++;
            System.out.println(rsp.toLine());
        }

        System.out.println("投入件数=" + fed + " 判定待ち=" + waiting + " 不正=" + invalid);
    }

    private static java.util.List<RequestRecord> loadRequests(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<RequestRecord> rows = new java.util.ArrayList<RequestRecord>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String[] f = split(line);
            if (f.length < 5 || isHeader(f[0], "REQ-ID")) {
                continue;
            }
            validateReason(f[4]);
            rows.add(new RequestRecord(f[0], f[1], amount(f[2]), date(f[3]), f[4]));
        }
        return rows;
    }

    private static java.util.Map<String, ResponseRecord> loadDecisions(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, ResponseRecord> rows = new java.util.LinkedHashMap<String, ResponseRecord>();
        if (!java.nio.file.Files.exists(path)) {
            return rows;
        }
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String[] f = split(line);
            if (f.length < 5 || isHeader(f[0], "REQ-ID")) {
                continue;
            }
            rows.put(f[0], new ResponseRecord(f[0], f[1], f[2], f[3], amount(f[4])));
        }
        return rows;
    }

    private static boolean isValidDecision(ResponseRecord rsp) {
        if (DECISION_ACCEPT.equals(rsp.decisionKbn)) {
            return rsp.declineReason.length() == 0;
        }
        if (DECISION_DECLINE.equals(rsp.decisionKbn)) {
            return DECLINE_WINDOW.equals(rsp.declineReason)
                    || DECLINE_AMOUNT.equals(rsp.declineReason)
                    || DECLINE_TXN.equals(rsp.declineReason);
        }
        return false;
    }

    private static String[] split(String line) {
        String[] raw = line.split(",", -1);
        for (int i = 0; i < raw.length; i++) {
            raw[i] = raw[i].trim();
        }
        return raw;
    }

    private static boolean isHeader(String actual, String expected) {
        return expected.equals(actual);
    }

    private static java.math.BigDecimal amount(String s) {
        return new java.math.BigDecimal(s).setScale(0);
    }

    private static java.time.LocalDate date(String s) {
        return java.time.LocalDate.parse(s);
    }

    private static void validateReason(String reason) {
        if (!"10".equals(reason) && !"20".equals(reason) && !"30".equals(reason)) {
            throw new IllegalArgumentException("REQ-REASON不正: " + reason);
        }
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

        String toLine() {
            return reqId + "," + origTxnId + "," + decisionKbn + "," + declineReason + "," + eligibleAmt.toPlainString();
        }
    }
}
