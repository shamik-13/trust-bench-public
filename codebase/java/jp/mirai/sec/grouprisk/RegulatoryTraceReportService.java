package jp.mirai.sec.grouprisk;

public class RegulatoryTraceReportService {

    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2024/07/09  小林 直樹 (E-252)     規制向け注文追跡報告サービス初版作成
     */
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final String ACTOR_ID = "REGTRACE";
    private static final String ACTION_KBN = "REPORT";
    private static final String RESULT_OK = "0";
    private static final String RESULT_WARN = "4";
    private static final String RESULT_NG = "8";

    public static void main(String[] a) {
        RegulatoryTraceReportService service = new RegulatoryTraceReportService();
        ReportResult result = service.reconstructAndReport(
                sampleOrders(),
                sampleExecutions(),
                sampleDecisions(),
                sampleInstruments()
        );
        System.out.println(result.toAuditRecord());
        for (OrderTrace trace : result.traces) {
            System.out.println(trace.toReportLine());
        }
    }

    public ReportResult reconstructAndReport(
            java.util.List<OrderRecord> orders,
            java.util.List<ExecutionRecord> executions,
            java.util.List<DecisionRecord> decisions,
            java.util.List<InstrumentRecord> instruments) {

        if (orders == null || executions == null || decisions == null || instruments == null) {
            throw new IllegalArgumentException("入力データが未設定です");
        }

        java.util.Map<String, InstrumentRecord> instrumentByCode = new java.util.LinkedHashMap<>();
        for (InstrumentRecord instrument : instruments) {
            validateInstrument(instrument);
            instrumentByCode.put(instrument.instrCode, instrument);
        }

        java.util.Map<String, java.util.List<ExecutionRecord>> executionsByOrderId = new java.util.LinkedHashMap<>();
        for (ExecutionRecord execution : executions) {
            validateExecution(execution);
            executionsByOrderId.computeIfAbsent(execution.orderId, k -> new java.util.ArrayList<>()).add(execution);
        }
        for (java.util.List<ExecutionRecord> rows : executionsByOrderId.values()) {
            rows.sort(java.util.Comparator.comparing(x -> x.execTs));
        }

        java.util.Map<String, java.util.List<DecisionRecord>> decisionsByOrderId = new java.util.LinkedHashMap<>();
        for (DecisionRecord decision : decisions) {
            validateDecision(decision);
            decisionsByOrderId.computeIfAbsent(decision.orderId, k -> new java.util.ArrayList<>()).add(decision);
        }
        for (java.util.List<DecisionRecord> rows : decisionsByOrderId.values()) {
            rows.sort(java.util.Comparator.comparing(x -> x.decisionTs));
        }

        java.util.List<OrderTrace> traces = new java.util.ArrayList<>();
        int warningCount = 0;
        int rejectCount = 0;

        for (OrderRecord order : orders) {
            validateOrder(order);
            InstrumentRecord instrument = instrumentByCode.get(order.instrCode);
            if (instrument == null) {
                throw new IllegalArgumentException("銘柄マスタ未登録です: " + order.instrCode);
            }

            int normalizedTier = instrument.instrTier;
            long tickAmount = tickAmountForTier(normalizedTier, instrument.tickAmount);
            boolean tickValid = isTickValid(order, tickAmount);
            long orderNotional = computeOrderNotional(order, instrument);
            int modelDecision = decide(orderNotional, tickValid);
            long marginAmount = computeMargin(orderNotional, normalizedTier);

            java.util.List<ExecutionRecord> orderExecutions =
                    executionsByOrderId.getOrDefault(order.orderId, java.util.Collections.emptyList());
            java.util.List<DecisionRecord> orderDecisions =
                    decisionsByOrderId.getOrDefault(order.orderId, java.util.Collections.emptyList());

            long filledQty = 0L;
            long filledAmt = 0L;
            for (ExecutionRecord execution : orderExecutions) {
                if (!order.instrCode.equals(execution.instrCode) || !order.sideKbn.equals(execution.sideKbn)) {
                    warningCount++;
                    continue;
                }
                filledQty = Math.addExact(filledQty, execution.fillQty);
                filledAmt = Math.addExact(filledAmt, execution.fillAmt);
            }

            String status = determineStatus(order, modelDecision, filledQty, orderDecisions);
            if (modelDecision != 0) {
                rejectCount++;
            }
            if (!tickValid || filledQty > order.ordQty) {
                warningCount++;
            }

            traces.add(new OrderTrace(
                    order.orderId,
                    order.cifNo,
                    order.instrCode,
                    instrument.instrName,
                    order.sideKbn,
                    order.ordType,
                    order.tifCode,
                    order.ordQty,
                    order.priceAmt,
                    normalizedTier,
                    tickAmount,
                    orderNotional,
                    marginAmount,
                    modelDecision,
                    status,
                    filledQty,
                    filledAmt,
                    orderDecisions,
                    orderExecutions
            ));
        }

        traces.sort(java.util.Comparator.comparing(x -> x.orderId));
        String auditId = "AUD" + java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
                .format(java.time.LocalDateTime.now());
        String resultCode = rejectCount > 0 ? RESULT_NG : warningCount > 0 ? RESULT_WARN : RESULT_OK;
        return new ReportResult(auditId, ACTOR_ID, ACTION_KBN, "REGTRACE-" + traces.size(), resultCode,
                java.time.LocalDateTime.now(), traces);
    }

    private static int decide(long orderNotional, boolean tickValid) {
        if (!tickValid) {
            return 12;
        }
        if (orderNotional > MIHFT_MAX_NOTIONAL) {
            return 8;
        }
        return 0;
    }

    private static String determineStatus(OrderRecord order, int modelDecision, long filledQty,
                                          java.util.List<DecisionRecord> decisions) {
        if (modelDecision != 0) {
            return "差止";
        }
        for (DecisionRecord decision : decisions) {
            if ("CANCEL".equals(decision.actionCode)) {
                return "取消";
            }
            if (decision.reasonCode == 4 || decision.reasonCode == 8 || decision.reasonCode == 12) {
                return "差止";
            }
        }
        if (filledQty == 0L) {
            return "受付";
        }
        if (filledQty < order.ordQty) {
            return "一部約定";
        }
        return "全部約定";
    }

    private static long computeOrderNotional(OrderRecord order, InstrumentRecord instrument) {
        long unitPrice = "M".equals(order.ordType) ? instrument.tickAmount * 100L : order.priceAmt;
        return Math.multiplyExact(order.ordQty, unitPrice);
    }

    private static long computeMargin(long notional, int tier) {
        return notional * marginRateBp(tier) / 10000L;
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
                throw new IllegalArgumentException("銘柄ティアが不正です: " + tier);
        }
    }

    private static long tickAmountForTier(int tier, long masterTick) {
        long canonicalTick;
        switch (tier) {
            case 1:
                canonicalTick = 100L;
                break;
            case 2:
                canonicalTick = 500L;
                break;
            case 3:
                canonicalTick = 1000L;
                break;
            default:
                throw new IllegalArgumentException("銘柄ティアが不正です: " + tier);
        }
        return masterTick > 0L ? masterTick : canonicalTick;
    }

    private static boolean isTickValid(OrderRecord order, long tickAmount) {
        return "M".equals(order.ordType) || (order.priceAmt > 0L && order.priceAmt % tickAmount == 0L);
    }

    private static void validateOrder(OrderRecord row) {
        requireText(row.orderId, "ORDER-ID");
        requireText(row.cifNo, "CIF-NO");
        requireText(row.instrCode, "INSTR-CODE");
        requireAny(row.sideKbn, "SIDE-KBN", "B", "S");
        requireAny(row.ordType, "ORD-TYPE", "L", "M");
        requireAny(row.tifCode, "TIF-CODE", "DAY", "IOC", "FOK");
        requirePositive(row.ordQty, "ORD-QTY");
        if ("L".equals(row.ordType)) {
            requirePositive(row.priceAmt, "PRICE-AMT");
        }
        marginRateBp(row.instrTier);
    }

    private static void validateExecution(ExecutionRecord row) {
        requireText(row.execId, "EXEC-ID");
        requireText(row.orderId, "ORDER-ID");
        requireText(row.instrCode, "INSTR-CODE");
        requireAny(row.sideKbn, "SIDE-KBN", "B", "S");
        requirePositive(row.fillQty, "FILL-QTY");
        requirePositive(row.fillAmt, "FILL-AMT");
        if (row.execTs == null) {
            throw new IllegalArgumentException("EXEC-TSが未設定です");
        }
    }

    private static void validateDecision(DecisionRecord row) {
        requireText(row.decisionId, "DECISION-ID");
        requireText(row.orderId, "ORDER-ID");
        requireText(row.instrCode, "INSTR-CODE");
        requireAny(row.actionCode, "ACTION-CODE", "ACCEPT", "REJECT", "CANCEL");
        if (row.reasonCode != 0 && row.reasonCode != 4 && row.reasonCode != 8 && row.reasonCode != 12) {
            throw new IllegalArgumentException("REASON-CODEが不正です: " + row.reasonCode);
        }
        if (row.decisionTs == null) {
            throw new IllegalArgumentException("DECISION-TSが未設定です");
        }
    }

    private static void validateInstrument(InstrumentRecord row) {
        requireText(row.instrCode, "INSTR-CODE");
        requireText(row.instrName, "INSTR-NAME");
        marginRateBp(row.instrTier);
        requirePositive(row.tickAmount, "TICK-AMT");
        requirePositive(row.lotQty, "LOT-QTY");
        requireAny(row.boardCode, "BOARD-CODE", "T1", "ST", "ETF");
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "が未設定です");
        }
    }

    private static void requirePositive(long value, String name) {
        if (value <= 0L) {
            throw new IllegalArgumentException(name + "が不正です: " + value);
        }
    }

    private static void requireAny(String value, String name, String... allowed) {
        requireText(value, name);
        for (String code : allowed) {
            if (code.equals(value)) {
                return;
            }
        }
        throw new IllegalArgumentException(name + "が不正です: " + value);
    }

    private static java.util.List<OrderRecord> sampleOrders() {
        java.util.List<OrderRecord> rows = new java.util.ArrayList<>();
        rows.add(new OrderRecord("OD202501150001", "CIF00012001", "7203", "B", "L", "DAY", 1000L, 320000L, 1));
        rows.add(new OrderRecord("OD202501150002", "CIF00012002", "9984", "S", "L", "IOC", 500L, 895000L, 1));
        rows.add(new OrderRecord("OD202501150003", "CIF00012003", "4478", "B", "L", "FOK", 2000L, 123400L, 3));
        rows.add(new OrderRecord("OD202501150004", "CIF00012004", "1306", "B", "M", "DAY", 300L, 0L, 2));
        return rows;
    }

    private static java.util.List<ExecutionRecord> sampleExecutions() {
        java.util.List<ExecutionRecord> rows = new java.util.ArrayList<>();
        rows.add(new ExecutionRecord("EX202501150001", "OD202501150001", "7203", "B", 600L, 192000000L,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 0, 3)));
        rows.add(new ExecutionRecord("EX202501150002", "OD202501150001", "7203", "B", 400L, 128000000L,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 0, 4)));
        rows.add(new ExecutionRecord("EX202501150003", "OD202501150002", "9984", "S", 200L, 179000000L,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 1, 11)));
        return rows;
    }

    private static java.util.List<DecisionRecord> sampleDecisions() {
        java.util.List<DecisionRecord> rows = new java.util.ArrayList<>();
        rows.add(new DecisionRecord("DC202501150001", "OD202501150001", "7203", "ACCEPT", 0,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 0, 1)));
        rows.add(new DecisionRecord("DC202501150002", "OD202501150002", "9984", "ACCEPT", 0,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 1, 9)));
        rows.add(new DecisionRecord("DC202501150003", "OD202501150003", "4478", "REJECT", 12,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 2, 2)));
        rows.add(new DecisionRecord("DC202501150004", "OD202501150004", "1306", "ACCEPT", 0,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 3, 4)));
        rows.add(new DecisionRecord("DC202501150005", "OD202501150004", "1306", "CANCEL", 0,
                java.time.LocalDateTime.of(2026, 6, 27, 9, 3, 20)));
        return rows;
    }

    private static java.util.List<InstrumentRecord> sampleInstruments() {
        java.util.List<InstrumentRecord> rows = new java.util.ArrayList<>();
        rows.add(new InstrumentRecord("7203", "トヨタ自動車", 1, 100L, 100L, "T1"));
        rows.add(new InstrumentRecord("9984", "ソフトバンクグループ", 1, 100L, 100L, "T1"));
        rows.add(new InstrumentRecord("4478", "フリー", 3, 1000L, 100L, "ST"));
        rows.add(new InstrumentRecord("1306", "ＴＯＰＩＸ連動型上場投信", 2, 500L, 10L, "ETF"));
        return rows;
    }

    public static final class OrderRecord {
        public final String orderId;
        public final String cifNo;
        public final String instrCode;
        public final String sideKbn;
        public final String ordType;
        public final String tifCode;
        public final long ordQty;
        public final long priceAmt;
        public final int instrTier;

        public OrderRecord(String orderId, String cifNo, String instrCode, String sideKbn, String ordType,
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

    public static final class ExecutionRecord {
        public final String execId;
        public final String orderId;
        public final String instrCode;
        public final String sideKbn;
        public final long fillQty;
        public final long fillAmt;
        public final java.time.LocalDateTime execTs;

        public ExecutionRecord(String execId, String orderId, String instrCode, String sideKbn,
                               long fillQty, long fillAmt, java.time.LocalDateTime execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    public static final class DecisionRecord {
        public final String decisionId;
        public final String orderId;
        public final String instrCode;
        public final String actionCode;
        public final int reasonCode;
        public final java.time.LocalDateTime decisionTs;

        public DecisionRecord(String decisionId, String orderId, String instrCode, String actionCode,
                              int reasonCode, java.time.LocalDateTime decisionTs) {
            this.decisionId = decisionId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.actionCode = actionCode;
            this.reasonCode = reasonCode;
            this.decisionTs = decisionTs;
        }
    }

    public static final class InstrumentRecord {
        public final String instrCode;
        public final String instrName;
        public final int instrTier;
        public final long tickAmount;
        public final long lotQty;
        public final String boardCode;

        public InstrumentRecord(String instrCode, String instrName, int instrTier, long tickAmount,
                                long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.tickAmount = tickAmount;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    public static final class ReportResult {
        public final String auditId;
        public final String actorId;
        public final String actionKbn;
        public final String objectId;
        public final String resultCode;
        public final java.time.LocalDateTime auditTs;
        public final java.util.List<OrderTrace> traces;

        public ReportResult(String auditId, String actorId, String actionKbn, String objectId,
                            String resultCode, java.time.LocalDateTime auditTs,
                            java.util.List<OrderTrace> traces) {
            this.auditId = auditId;
            this.actorId = actorId;
            this.actionKbn = actionKbn;
            this.objectId = objectId;
            this.resultCode = resultCode;
            this.auditTs = auditTs;
            this.traces = java.util.Collections.unmodifiableList(new java.util.ArrayList<>(traces));
        }

        public String toAuditRecord() {
            return auditId + "," + actorId + "," + actionKbn + "," + objectId + "," + resultCode + ","
                    + auditTs.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME);
        }
    }

    public static final class OrderTrace {
        public final String orderId;
        public final String cifNo;
        public final String instrCode;
        public final String instrName;
        public final String sideKbn;
        public final String ordType;
        public final String tifCode;
        public final long ordQty;
        public final long priceAmt;
        public final int instrTier;
        public final long tickAmount;
        public final long notionalAmount;
        public final long marginAmount;
        public final int decisionCode;
        public final String traceStatus;
        public final long filledQty;
        public final long filledAmount;
        public final java.util.List<DecisionRecord> decisions;
        public final java.util.List<ExecutionRecord> executions;

        public OrderTrace(String orderId, String cifNo, String instrCode, String instrName, String sideKbn,
                          String ordType, String tifCode, long ordQty, long priceAmt, int instrTier,
                          long tickAmount, long notionalAmount, long marginAmount, int decisionCode,
                          String traceStatus, long filledQty, long filledAmount,
                          java.util.List<DecisionRecord> decisions,
                          java.util.List<ExecutionRecord> executions) {
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.sideKbn = sideKbn;
            this.ordType = ordType;
            this.tifCode = tifCode;
            this.ordQty = ordQty;
            this.priceAmt = priceAmt;
            this.instrTier = instrTier;
            this.tickAmount = tickAmount;
            this.notionalAmount = notionalAmount;
            this.marginAmount = marginAmount;
            this.decisionCode = decisionCode;
            this.traceStatus = traceStatus;
            this.filledQty = filledQty;
            this.filledAmount = filledAmount;
            this.decisions = java.util.Collections.unmodifiableList(new java.util.ArrayList<>(decisions));
            this.executions = java.util.Collections.unmodifiableList(new java.util.ArrayList<>(executions));
        }

        public String toReportLine() {
            return orderId + "," + cifNo + "," + instrCode + "," + instrName + "," + sideKbn + ","
                    + ordType + "," + tifCode + "," + ordQty + "," + priceAmt + "," + instrTier + ","
                    + tickAmount + "," + notionalAmount + "," + marginAmount + "," + decisionCode + ","
                    + traceStatus + "," + filledQty + "," + filledAmount;
        }
    }
}
