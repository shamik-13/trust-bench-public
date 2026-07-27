/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2021-07-15  中川 美和 (E-283)  初版作成
 */

package jp.mirai.sec.orderbook;

public class RiskLimitService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final int DECISION_ACCEPT = 0;
    private static final int DECISION_REJECT_MARGIN = 4;
    private static final int DECISION_REJECT_NOTIONAL = 8;
    private static final int DECISION_REJECT_TICK = 12;

    private RiskLimitService() {
    }

    public static void main(String[] a) {
        java.util.Map<String, CustomerRow> customers = readCustomers();
        java.util.Map<RiskKey, RiskRow> risks = readRisks();
        java.util.List<OrderRow> orders = readOrders();
        java.util.List<AuditRow> audits = new java.util.ArrayList<>();

        int accepted = 0;
        int rejected = 0;

        for (OrderRow order : orders) {
            RiskKey key = new RiskKey(order.cifNo, order.instrTier);
            CustomerRow customer = customers.get(order.cifNo);
            RiskRow risk = risks.get(key);

            Decision decision = judge(order, customer, risk);

            if (decision.code == DECISION_ACCEPT) {
                accepted++;
                if (customer != null) {
                    customer.acctUsedAmt += decision.requiredMargin;
                    customer.groupUsedAmt += decision.requiredMargin;
                }
            } else {
                rejected++;
                audits.add(new AuditRow(
                        "AU" + String.format("%08d", audits.size() + 1),
                        order.orderId,
                        eventCode(decision.code),
                        order.cifNo,
                        order.instrCode,
                        java.time.OffsetDateTime.now(java.time.ZoneOffset.ofHours(9)).toString(),
                        decision.detailCd));
            }

            if (risk != null && decision.code == DECISION_REJECT_NOTIONAL && order.ordQty > risk.maxQty * 3L) {
                risk.killSwKbn = "1";
                audits.add(new AuditRow(
                        "AU" + String.format("%08d", audits.size() + 1),
                        order.orderId,
                        "KILL",
                        order.cifNo,
                        order.instrCode,
                        java.time.OffsetDateTime.now(java.time.ZoneOffset.ofHours(9)).toString(),
                        "過大注文検知"));
            }
        }

        writeRisks(risks.values());
        writeAudits(audits);
        System.out.println("判定件数=" + orders.size() + " 承認=" + accepted + " 否認=" + rejected + " 監査=" + audits.size());
    }

    private static Decision judge(OrderRow order, CustomerRow customer, RiskRow risk) {
        if (customer == null) {
            return new Decision(DECISION_REJECT_MARGIN, 0L, "顧客未登録");
        }
        if (risk == null) {
            return new Decision(DECISION_REJECT_NOTIONAL, 0L, "リスク枠未登録");
        }
        if ("1".equals(risk.killSwKbn)) {
            return new Decision(DECISION_REJECT_NOTIONAL, 0L, "停止中");
        }

        long notional = multiplyMinor(order.ordQty, effectivePrice(order));
        int tick = tickSize(order.instrTier);
        if ("L".equals(order.ordType) && order.priceAmt % tick != 0L) {
            return new Decision(DECISION_REJECT_TICK, 0L, "呼値不正");
        }
        if (order.ordQty > risk.maxQty) {
            return new Decision(DECISION_REJECT_NOTIONAL, 0L, "数量超過");
        }
        if (notional > risk.maxNotionalAmt || notional > MIHFT_MAX_NOTIONAL) {
            return new Decision(DECISION_REJECT_NOTIONAL, 0L, "想定元本超過");
        }

        long requiredMargin = notional * marginRateBp(order.instrTier) / 10000L;
        if (customer.groupUsedAmt + requiredMargin > customer.groupLimit) {
            return new Decision(DECISION_REJECT_MARGIN, requiredMargin, "グループ枠超過");
        }
        return new Decision(DECISION_ACCEPT, requiredMargin, "承認");
    }

    private static long effectivePrice(OrderRow order) {
        if ("M".equals(order.ordType)) {
            return order.priceAmt == 0L ? referencePrice(order.instrTier) : order.priceAmt;
        }
        return order.priceAmt;
    }

    private static long multiplyMinor(long qty, long price) {
        try {
            return Math.multiplyExact(qty, price);
        } catch (ArithmeticException e) {
            return Long.MAX_VALUE;
        }
    }

    private static int marginRateBp(int tier) {
        switch (tier) {
            case 1:
                return 1000;
            case 2:
                return 2000;
            case 3:
                return 4000;
            default:
                return 10000;
        }
    }

    private static int tickSize(int tier) {
        switch (tier) {
            case 1:
                return 100;
            case 2:
                return 500;
            case 3:
                return 1000;
            default:
                return 1000;
        }
    }

    private static long referencePrice(int tier) {
        switch (tier) {
            case 1:
                return 185000L;
            case 2:
                return 73200L;
            case 3:
                return 24800L;
            default:
                return 10000L;
        }
    }

    private static String eventCode(int decisionCode) {
        switch (decisionCode) {
            case DECISION_REJECT_MARGIN:
                return "MRGN";
            case DECISION_REJECT_NOTIONAL:
                return "NTNL";
            case DECISION_REJECT_TICK:
                return "TICK";
            default:
                return "UNKN";
        }
    }

    private static java.util.Map<String, CustomerRow> readCustomers() {
        String[] lines = {
                "CIF-NO,GROUP-LIMIT,GROUP-USED-AMT,ACCT-USED-AMT",
                "C100001,90000000,12000000,7800000",
                "C100002,250000000,205000000,88000000",
                "C100003,60000000,18000000,16000000",
                "C100004,180000000,172000000,65000000"
        };
        java.util.Map<String, CustomerRow> rows = new java.util.LinkedHashMap<>();
        for (int i = 1; i < lines.length; i++) {
            String[] c = lines[i].split(",", -1);
            rows.put(c[0], new CustomerRow(c[0], Long.parseLong(c[1]), Long.parseLong(c[2]), Long.parseLong(c[3])));
        }
        return rows;
    }

    private static java.util.Map<RiskKey, RiskRow> readRisks() {
        String[] lines = {
                "CIF-NO,INSTR-TIER,MAX-NOTIONAL-AMT,MAX-QTY,KILL-SW-KBN",
                "C100001,1,120000000,1200,0",
                "C100001,2,80000000,900,0",
                "C100002,1,300000000,2500,0",
                "C100002,3,40000000,400,0",
                "C100003,2,55000000,500,1",
                "C100004,3,70000000,600,0"
        };
        java.util.Map<RiskKey, RiskRow> rows = new java.util.LinkedHashMap<>();
        for (int i = 1; i < lines.length; i++) {
            String[] c = lines[i].split(",", -1);
            RiskRow row = new RiskRow(c[0], Integer.parseInt(c[1]), Long.parseLong(c[2]), Long.parseLong(c[3]), c[4]);
            rows.put(new RiskKey(row.cifNo, row.instrTier), row);
        }
        return rows;
    }

    private static java.util.List<OrderRow> readOrders() {
        String[] lines = {
                "ORDER-ID,CIF-NO,INSTR-CODE,SIDE-KBN,ORD-TYPE,TIF-CODE,ORD-QTY,PRICE-AMT,INSTR-TIER",
                "OD202501150001,C100001,7203,B,L,DAY,500,310000,1",
                "OD202501150002,C100001,9984,S,L,IOC,300,821500,1",
                "OD202501150003,C100002,4565,B,M,IOC,1800,0,1",
                "OD202501150004,C100002,4419,B,L,FOK,450,25100,3",
                "OD202501150005,C100003,6326,S,L,DAY,200,73500,2",
                "OD202501150006,C100004,IPOX,B,L,FOK,1900,48000,3",
                "OD202501150007,C100001,1306,B,L,DAY,100,297750,1",
                "OD202501150008,C100004,4890,B,L,IOC,580,99000,3"
        };
        java.util.List<OrderRow> rows = new java.util.ArrayList<>();
        for (int i = 1; i < lines.length; i++) {
            String[] c = lines[i].split(",", -1);
            rows.add(new OrderRow(
                    c[0],
                    c[1],
                    c[2],
                    c[3],
                    c[4],
                    c[5],
                    Long.parseLong(c[6]),
                    Long.parseLong(c[7]),
                    Integer.parseInt(c[8])));
        }
        return rows;
    }

    private static void writeRisks(java.util.Collection<RiskRow> rows) {
        System.out.println("SCRISK2 出力開始");
        for (RiskRow row : rows) {
            System.out.println(row.cifNo + "," + row.instrTier + "," + row.maxNotionalAmt + "," + row.maxQty + "," + row.killSwKbn);
        }
    }

    private static void writeAudits(java.util.List<AuditRow> rows) {
        System.out.println("SCAUDF 出力開始");
        for (AuditRow row : rows) {
            System.out.println(row.auditId + "," + row.orderId + "," + row.eventKbn + "," + row.cifNo + ","
                    + row.instrCode + "," + row.eventTs + "," + row.detailCd);
        }
    }

    private static final class CustomerRow {
        private final String cifNo;
        private final long groupLimit;
        private long groupUsedAmt;
        private long acctUsedAmt;

        private CustomerRow(String cifNo, long groupLimit, long groupUsedAmt, long acctUsedAmt) {
            this.cifNo = cifNo;
            this.groupLimit = groupLimit;
            this.groupUsedAmt = groupUsedAmt;
            this.acctUsedAmt = acctUsedAmt;
        }
    }

    private static final class RiskRow {
        private final String cifNo;
        private final int instrTier;
        private final long maxNotionalAmt;
        private final long maxQty;
        private String killSwKbn;

        private RiskRow(String cifNo, int instrTier, long maxNotionalAmt, long maxQty, String killSwKbn) {
            this.cifNo = cifNo;
            this.instrTier = instrTier;
            this.maxNotionalAmt = maxNotionalAmt;
            this.maxQty = maxQty;
            this.killSwKbn = killSwKbn;
        }
    }

    private static final class OrderRow {
        private final String orderId;
        private final String cifNo;
        private final String instrCode;
        private final String sideKbn;
        private final String ordType;
        private final String tifCode;
        private final long ordQty;
        private final long priceAmt;
        private final int instrTier;

        private OrderRow(String orderId, String cifNo, String instrCode, String sideKbn, String ordType,
                String tifCode, long ordQty, long priceAmt, int instrTier) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.ordType = ordType;
            this.tifCode = tifCode;
            this.ordQty = ordQty;
            this.priceAmt = priceAmt;
            this.instrTier = instrTier;
        }
    }

    private static final class AuditRow {
        private final String auditId;
        private final String orderId;
        private final String eventKbn;
        private final String cifNo;
        private final String instrCode;
        private final String eventTs;
        private final String detailCd;

        private AuditRow(String auditId, String orderId, String eventKbn, String cifNo, String instrCode,
                String eventTs, String detailCd) {
            this.auditId = auditId;
            this.orderId = orderId;
            this.eventKbn = eventKbn;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.eventTs = eventTs;
            this.detailCd = detailCd;
        }
    }

    private static final class RiskKey {
        private final String cifNo;
        private final int instrTier;

        private RiskKey(String cifNo, int instrTier) {
            this.cifNo = cifNo;
            this.instrTier = instrTier;
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof RiskKey)) {
                return false;
            }
            RiskKey other = (RiskKey) o;
            return instrTier == other.instrTier && java.util.Objects.equals(cifNo, other.cifNo);
        }

        @Override
        public int hashCode() {
            return java.util.Objects.hash(cifNo, instrTier);
        }
    }

    private static final class Decision {
        private final int code;
        private final long requiredMargin;
        private final String detailCd;

        private Decision(int code, long requiredMargin, String detailCd) {
            this.code = code;
            this.requiredMargin = requiredMargin;
            this.detailCd = detailCd;
        }
    }
}
