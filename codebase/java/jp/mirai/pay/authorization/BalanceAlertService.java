package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2024-09-30  みらいペイ システム部  初版作成
 */
public class BalanceAlertService {
    private static final String STS_ACTIVE = "01";
    private static final String STS_SUSPENDED = "02";
    private static final String STS_CLOSED = "03";
    private static final String STS_RESTRICTED = "09";

    private static final String DECISION_APPROVED = "A";
    private static final String DECISION_DECLINED = "D";

    private static final String REASON_INSUFFICIENT = "LIM";
    private static final String REASON_STATUS = "STS";
    private static final String REASON_CURRENCY = "CUR";

    private static final String SEND_WAITING = "10";
    private static final long JPY_ZERO = 0L;

    public static void main(String[] a) {
        java.util.List<Row> pywalf = new java.util.ArrayList<Row>();
        pywalf.add(row("WALLET_ID", "W0001001", "USER_ID", "U90001", "WALLET_STATUS", STS_ACTIVE, "WALLET_TIER", "S", "USER_NAME_KANA", "ヤマダタロウ"));
        pywalf.add(row("WALLET_ID", "W0001002", "USER_ID", "U90002", "WALLET_STATUS", STS_ACTIVE, "WALLET_TIER", "G", "USER_NAME_KANA", "スズキハナコ"));
        pywalf.add(row("WALLET_ID", "W0001003", "USER_ID", "U90003", "WALLET_STATUS", STS_RESTRICTED, "WALLET_TIER", "S", "USER_NAME_KANA", "サトウジロウ"));
        pywalf.add(row("WALLET_ID", "W0001004", "USER_ID", "U90004", "WALLET_STATUS", STS_SUSPENDED, "WALLET_TIER", "B", "USER_NAME_KANA", "タナカミカ"));
        pywalf.add(row("WALLET_ID", "W0001005", "USER_ID", "U90005", "WALLET_STATUS", STS_CLOSED, "WALLET_TIER", "G", "USER_NAME_KANA", "イトウケン"));

        java.util.List<Row> pybalf = new java.util.ArrayList<Row>();
        pybalf.add(row("WALLET_ID", "W0001001", "LEDGER_BAL_AMT", 4200L, "LAST_TOPUP_AMT", 10000L, "BAL_AS_OF_DT", "20260628"));
        pybalf.add(row("WALLET_ID", "W0001002", "LEDGER_BAL_AMT", 18500L, "LAST_TOPUP_AMT", 30000L, "BAL_AS_OF_DT", "20260628"));
        pybalf.add(row("WALLET_ID", "W0001003", "LEDGER_BAL_AMT", 980L, "LAST_TOPUP_AMT", 5000L, "BAL_AS_OF_DT", "20260628"));
        pybalf.add(row("WALLET_ID", "W0001004", "LEDGER_BAL_AMT", 7600L, "LAST_TOPUP_AMT", 10000L, "BAL_AS_OF_DT", "20260628"));
        pybalf.add(row("WALLET_ID", "W0001005", "LEDGER_BAL_AMT", 24000L, "LAST_TOPUP_AMT", 20000L, "BAL_AS_OF_DT", "20260627"));

        java.util.List<Row> pylmtf = new java.util.ArrayList<Row>();
        pylmtf.add(row("TIER_CODE", "B", "PER_TXN_LIMIT_AMT", 30000L, "DAILY_LIMIT_AMT", 100000L, "MONTHLY_LIMIT_AMT", 500000L, "ALERT_THRESHOLD_AMT", 3000L));
        pylmtf.add(row("TIER_CODE", "S", "PER_TXN_LIMIT_AMT", 100000L, "DAILY_LIMIT_AMT", 300000L, "MONTHLY_LIMIT_AMT", 1500000L, "ALERT_THRESHOLD_AMT", 5000L));
        pylmtf.add(row("TIER_CODE", "G", "PER_TXN_LIMIT_AMT", 300000L, "DAILY_LIMIT_AMT", 1000000L, "MONTHLY_LIMIT_AMT", 5000000L, "ALERT_THRESHOLD_AMT", 20000L));

        java.util.List<Row> pyarspf = new java.util.ArrayList<Row>();
        pyarspf.add(row("REQ_ID", "R202606280001", "WALLET_ID", "W0001001", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 4200L, "REQ_AMT", 7600L, "DECLINE_REASON", REASON_INSUFFICIENT));
        pyarspf.add(row("REQ_ID", "R202606280002", "WALLET_ID", "W0001001", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 4200L, "REQ_AMT", 6500L, "DECLINE_REASON", REASON_INSUFFICIENT));
        pyarspf.add(row("REQ_ID", "R202606280003", "WALLET_ID", "W0001001", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 4200L, "REQ_AMT", 5000L, "DECLINE_REASON", REASON_INSUFFICIENT));
        pyarspf.add(row("REQ_ID", "R202606280004", "WALLET_ID", "W0001002", "DECISION_KBN", DECISION_APPROVED, "AVAIL_AMT", 18500L, "REQ_AMT", 12000L, "DECLINE_REASON", ""));
        pyarspf.add(row("REQ_ID", "R202606280005", "WALLET_ID", "W0001003", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 980L, "REQ_AMT", 1000L, "DECLINE_REASON", REASON_STATUS));
        pyarspf.add(row("REQ_ID", "R202606280006", "WALLET_ID", "W0001003", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 980L, "REQ_AMT", 1200L, "DECLINE_REASON", REASON_STATUS));
        pyarspf.add(row("REQ_ID", "R202606280007", "WALLET_ID", "W0001003", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 980L, "REQ_AMT", 900L, "DECLINE_REASON", REASON_STATUS));
        pyarspf.add(row("REQ_ID", "R202606280008", "WALLET_ID", "W0001004", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 7600L, "REQ_AMT", 1000L, "DECLINE_REASON", REASON_STATUS));
        pyarspf.add(row("REQ_ID", "R202606280009", "WALLET_ID", "W0001005", "DECISION_KBN", DECISION_DECLINED, "AVAIL_AMT", 24000L, "REQ_AMT", 1500L, "DECLINE_REASON", REASON_CURRENCY));

        java.util.Map<String, Row> walletById = indexBy(pywalf, "WALLET_ID");
        java.util.Map<String, Row> balanceByWallet = indexBy(pybalf, "WALLET_ID");
        java.util.Map<String, Row> limitByTier = indexBy(pylmtf, "TIER_CODE");
        java.util.Map<String, java.util.List<Row>> authByWallet = groupBy(pyarspf, "WALLET_ID");

        java.util.List<Row> pyntff = new java.util.ArrayList<Row>();
        java.time.format.DateTimeFormatter formatter = java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        String createTs = java.time.LocalDateTime.now().format(formatter);
        int seq = 1;

        for (Row wallet : pywalf) {
            String walletId = wallet.s("WALLET_ID");
            String status = wallet.s("WALLET_STATUS");
            Row balance = balanceByWallet.get(walletId);
            Row limit = limitByTier.get(wallet.s("WALLET_TIER"));
            java.util.List<Row> responses = authByWallet.get(walletId);

            if (balance == null || limit == null) {
                continue;
            }

            long ledger = Math.max(JPY_ZERO, balance.n("LEDGER_BAL_AMT"));
            long threshold = Math.max(JPY_ZERO, limit.n("ALERT_THRESHOLD_AMT"));

            if (STS_ACTIVE.equals(status) && ledger <= threshold) {
                String text = "残高低下: ウォレット=" + walletId
                        + " 残高=" + ledger + "JPY"
                        + " 閾値=" + threshold + "JPY"
                        + " 基準日=" + balance.s("BAL_AS_OF_DT");
                pyntff.add(notice(seq++, walletId, "BAL", text, createTs));
            }

            ConsecutiveDecline decline = countTailDeclines(responses);
            if (decline.count >= 3) {
                String text = "連続否決: ウォレット=" + walletId
                        + " 回数=" + decline.count
                        + " 理由=" + decline.reason
                        + " 直近要求=" + decline.lastReqAmt + "JPY"
                        + " 利用可能=" + decline.lastAvailAmt + "JPY";
                pyntff.add(notice(seq++, walletId, "DEN", text, createTs));
            }
        }

        System.out.println("NOTICE_ID,WALLET_ID,NOTICE_KBN,NOTICE_TEXT,SEND_STATUS,CREATE_TS");
        for (Row notice : pyntff) {
            System.out.println(csv(notice.s("NOTICE_ID")) + ","
                    + csv(notice.s("WALLET_ID")) + ","
                    + csv(notice.s("NOTICE_KBN")) + ","
                    + csv(notice.s("NOTICE_TEXT")) + ","
                    + csv(notice.s("SEND_STATUS")) + ","
                    + csv(notice.s("CREATE_TS")));
        }
    }

    private static Row notice(int seq, String walletId, String noticeKbn, String text, String createTs) {
        return row("NOTICE_ID", String.format("N%010d", seq),
                "WALLET_ID", walletId,
                "NOTICE_KBN", noticeKbn,
                "NOTICE_TEXT", text,
                "SEND_STATUS", SEND_WAITING,
                "CREATE_TS", createTs);
    }

    private static ConsecutiveDecline countTailDeclines(java.util.List<Row> responses) {
        if (responses == null || responses.isEmpty()) {
            return new ConsecutiveDecline(0, "", 0L, 0L);
        }

        int count = 0;
        String reason = "";
        long lastReqAmt = 0L;
        long lastAvailAmt = 0L;

        for (int i = responses.size() - 1; i >= 0; i--) {
            Row response = responses.get(i);
            if (!DECISION_DECLINED.equals(response.s("DECISION_KBN"))) {
                break;
            }
            if (count == 0) {
                reason = response.s("DECLINE_REASON");
                lastReqAmt = response.n("REQ_AMT");
                lastAvailAmt = response.n("AVAIL_AMT");
            }
            count++;
        }

        return new ConsecutiveDecline(count, reason, lastReqAmt, lastAvailAmt);
    }

    private static java.util.Map<String, Row> indexBy(java.util.List<Row> rows, String key) {
        java.util.Map<String, Row> index = new java.util.LinkedHashMap<String, Row>();
        for (Row row : rows) {
            index.put(row.s(key), row);
        }
        return index;
    }

    private static java.util.Map<String, java.util.List<Row>> groupBy(java.util.List<Row> rows, String key) {
        java.util.Map<String, java.util.List<Row>> groups = new java.util.LinkedHashMap<String, java.util.List<Row>>();
        for (Row row : rows) {
            String groupKey = row.s(key);
            java.util.List<Row> group = groups.get(groupKey);
            if (group == null) {
                group = new java.util.ArrayList<Row>();
                groups.put(groupKey, group);
            }
            group.add(row);
        }
        return groups;
    }

    private static Row row(Object... values) {
        Row row = new Row();
        for (int i = 0; i < values.length; i += 2) {
            row.put(String.valueOf(values[i]), values[i + 1]);
        }
        return row;
    }

    private static String csv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }

    private static final class ConsecutiveDecline {
        private final int count;
        private final String reason;
        private final long lastReqAmt;
        private final long lastAvailAmt;

        private ConsecutiveDecline(int count, String reason, long lastReqAmt, long lastAvailAmt) {
            this.count = count;
            this.reason = reason;
            this.lastReqAmt = lastReqAmt;
            this.lastAvailAmt = lastAvailAmt;
        }
    }

    private static final class Row {
        private final java.util.Map<String, Object> values = new java.util.LinkedHashMap<String, Object>();

        private void put(String key, Object value) {
            values.put(key, value);
        }

        private String s(String key) {
            Object value = values.get(key);
            return value == null ? "" : String.valueOf(value);
        }

        private long n(String key) {
            Object value = values.get(key);
            if (value instanceof Number) {
                return ((Number) value).longValue();
            }
            if (value == null || String.valueOf(value).trim().isEmpty()) {
                return 0L;
            }
            return Long.parseLong(String.valueOf(value));
        }
    }
}
