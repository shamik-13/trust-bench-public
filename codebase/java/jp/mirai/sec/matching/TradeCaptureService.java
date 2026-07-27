/***************************************************************
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2023/04/18  中川 美和 (E-283)      初版作成
 ***************************************************************/

package jp.mirai.sec.matching;

public class TradeCaptureService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final String SIDE_BUY = "B";
    private static final String SIDE_SELL = "S";

    private static final String ORD_LIMIT = "L";
    private static final String ORD_MARKET = "M";

    private static final String TIF_DAY = "DAY";
    private static final String TIF_IOC = "IOC";
    private static final String TIF_FOK = "FOK";

    private static final String BOARD_T1 = "T1";
    private static final String BOARD_ST = "ST";
    private static final String BOARD_ETF = "ETF";

    private static final int DECISION_ACCEPT = 0;
    private static final int DECISION_REJECT_MARGIN = 4;
    private static final int DECISION_REJECT_NOTIONAL = 8;
    private static final int DECISION_REJECT_TICK = 12;

    private TradeCaptureService() {
    }

    public static void main(String[] a) {
        java.util.Map<String, Sctcap> sctcap = loadSctcap();
        java.util.Map<String, Scexec> scexec = indexExec(loadScexec());
        java.util.Map<String, Scordf> scordf = indexOrder(loadScordf());

        FeeEnrichmentService feeService = new FeeEnrichmentService();
        java.util.List<AuditView> auditViews = reconcile(sctcap, scexec, scordf, feeService);

        for (AuditView view : auditViews) {
            System.out.println(view.toOperatorLine());
        }

        System.out.println("捕捉件数=" + sctcap.size() + " 監査件数=" + auditViews.size());
    }

    private static java.util.List<AuditView> reconcile(
            java.util.Map<String, Sctcap> captures,
            java.util.Map<String, Scexec> executions,
            java.util.Map<String, Scordf> orders,
            FeeEnrichmentService feeService) {
        java.util.List<AuditView> auditViews = new java.util.ArrayList<>();

        for (java.util.Map.Entry<String, Sctcap> entry : captures.entrySet()) {
            Sctcap cap = entry.getValue();
            Scexec exec = executions.get(cap.execId);
            if (exec == null) {
                auditViews.add(AuditView.unmatched(cap, "約定なし"));
                continue;
            }

            Scordf order = orders.get(exec.orderId);
            if (order == null) {
                auditViews.add(AuditView.unmatched(cap, "注文なし"));
                continue;
            }

            String orderId = prefer(cap.orderId, exec.orderId);
            String instrCode = prefer(cap.instrCode, exec.instrCode);
            String cifNo = prefer(cap.cifNo, order.cifNo);

            long qty = cap.tradeQty == 0L ? exec.fillQty : cap.tradeQty;
            long amt = cap.tradeAmt == 0L ? exec.fillAmt : cap.tradeAmt;

            Sctcap updated = new Sctcap(
                    cap.tradeId,
                    cap.execId,
                    orderId,
                    instrCode,
                    cifNo,
                    qty,
                    amt,
                    cap.captureTs);
            captures.put(entry.getKey(), updated);

            int decision = validate(updated, exec, order);
            BoardInfo board = feeService.attachBoard(order.instrCode, order.instrTier);
            auditViews.add(new AuditView(updated, exec, order, board, decision, "照合済"));
        }

        return auditViews;
    }

    private static int validate(Sctcap cap, Scexec exec, Scordf order) {
        if (!cap.orderId.equals(exec.orderId) || !cap.instrCode.equals(exec.instrCode)) {
            return DECISION_REJECT_NOTIONAL;
        }
        if (!exec.orderId.equals(order.orderId) || !exec.instrCode.equals(order.instrCode)) {
            return DECISION_REJECT_NOTIONAL;
        }
        if (cap.tradeQty != exec.fillQty || cap.tradeAmt != exec.fillAmt) {
            return DECISION_REJECT_NOTIONAL;
        }
        if (cap.tradeAmt > MIHFT_MAX_NOTIONAL) {
            return DECISION_REJECT_NOTIONAL;
        }

        TierRule tier = TierRule.of(order.instrTier);
        if (tier == null) {
            return DECISION_REJECT_MARGIN;
        }

        long requiredMargin = cap.tradeAmt * tier.rateBp / 10000L;
        if (requiredMargin <= 0L && SIDE_BUY.equals(order.sideKbn)) {
            return DECISION_REJECT_MARGIN;
        }

        if (ORD_LIMIT.equals(order.ordType) && order.priceAmt % tier.tick != 0L) {
            return DECISION_REJECT_TICK;
        }

        if (!validSide(order.sideKbn) || !validOrderType(order.ordType) || !validTif(order.tifCode)) {
            return DECISION_REJECT_NOTIONAL;
        }

        return DECISION_ACCEPT;
    }

    private static boolean validSide(String sideKbn) {
        return SIDE_BUY.equals(sideKbn) || SIDE_SELL.equals(sideKbn);
    }

    private static boolean validOrderType(String ordType) {
        return ORD_LIMIT.equals(ordType) || ORD_MARKET.equals(ordType);
    }

    private static boolean validTif(String tifCode) {
        return TIF_DAY.equals(tifCode) || TIF_IOC.equals(tifCode) || TIF_FOK.equals(tifCode);
    }

    private static String prefer(String primary, String fallback) {
        if (primary == null || primary.trim().isEmpty()) {
            return fallback;
        }
        return primary;
    }

    private static java.util.Map<String, Sctcap> loadSctcap() {
        java.util.Map<String, Sctcap> table = new java.util.LinkedHashMap<>();
        table.put("T202501150001", new Sctcap("T202501150001", "E900001", "", "", "", 0L, 0L, "2025-01-15T09:00:02.010+09:00"));
        table.put("T202501150002", new Sctcap("T202501150002", "E900002", "O700002", "", "CIF1022", 0L, 0L, "2025-01-15T09:00:02.080+09:00"));
        table.put("T202501150003", new Sctcap("T202501150003", "E900003", "O700003", "7203", "", 1200L, 3588000L, "2025-01-15T09:00:03.130+09:00"));
        table.put("T202501150004", new Sctcap("T202501150004", "E900004", "", "", "", 500000L, 501000000L, "2025-01-15T09:00:03.510+09:00"));
        table.put("T202501150005", new Sctcap("T202501150005", "E900005", "O700005", "9984", "CIF8871", 700L, 6842500L, "2025-01-15T09:00:04.004+09:00"));
        return table;
    }

    private static java.util.List<Scexec> loadScexec() {
        java.util.List<Scexec> rows = new java.util.ArrayList<>();
        rows.add(new Scexec("E900001", "O700001", "8306", SIDE_BUY, 10000L, 15720000L, "2025-01-15T09:00:01.991+09:00"));
        rows.add(new Scexec("E900002", "O700002", "1321", SIDE_SELL, 3000L, 12855000L, "2025-01-15T09:00:02.041+09:00"));
        rows.add(new Scexec("E900003", "O700003", "7203", SIDE_BUY, 1200L, 3588000L, "2025-01-15T09:00:03.071+09:00"));
        rows.add(new Scexec("E900004", "O700004", "4565", SIDE_BUY, 500000L, 501000000L, "2025-01-15T09:00:03.441+09:00"));
        rows.add(new Scexec("E900005", "O700005", "9984", SIDE_BUY, 700L, 6842500L, "2025-01-15T09:00:03.982+09:00"));
        return rows;
    }

    private static java.util.List<Scordf> loadScordf() {
        java.util.List<Scordf> rows = new java.util.ArrayList<>();
        rows.add(new Scordf("O700001", "CIF1001", "8306", SIDE_BUY, ORD_LIMIT, TIF_DAY, 10000L, 1572L, 1));
        rows.add(new Scordf("O700002", "CIF1022", "1321", SIDE_SELL, ORD_MARKET, TIF_IOC, 3000L, 0L, 1));
        rows.add(new Scordf("O700003", "CIF2044", "7203", SIDE_BUY, ORD_LIMIT, TIF_DAY, 1200L, 2990L, 2));
        rows.add(new Scordf("O700004", "CIF7780", "4565", SIDE_BUY, ORD_LIMIT, TIF_FOK, 500000L, 1002L, 3));
        rows.add(new Scordf("O700005", "CIF8871", "9984", SIDE_BUY, ORD_LIMIT, TIF_DAY, 700L, 9775L, 2));
        return rows;
    }

    private static java.util.Map<String, Scexec> indexExec(java.util.List<Scexec> rows) {
        java.util.Map<String, Scexec> index = new java.util.LinkedHashMap<>();
        for (Scexec row : rows) {
            index.put(row.execId, row);
        }
        return index;
    }

    private static java.util.Map<String, Scordf> indexOrder(java.util.List<Scordf> rows) {
        java.util.Map<String, Scordf> index = new java.util.LinkedHashMap<>();
        for (Scordf row : rows) {
            index.put(row.orderId, row);
        }
        return index;
    }

    private static final class Sctcap {
        final String tradeId;
        final String execId;
        final String orderId;
        final String instrCode;
        final String cifNo;
        final long tradeQty;
        final long tradeAmt;
        final String captureTs;

        Sctcap(String tradeId, String execId, String orderId, String instrCode, String cifNo,
               long tradeQty, long tradeAmt, String captureTs) {
            this.tradeId = tradeId;
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.cifNo = cifNo;
            this.tradeQty = tradeQty;
            this.tradeAmt = tradeAmt;
            this.captureTs = captureTs;
        }
    }

    private static final class Scexec {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final long fillAmt;
        final String execTs;

        Scexec(String execId, String orderId, String instrCode, String sideKbn,
               long fillQty, long fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class Scordf {
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final String ordType;
        final String tifCode;
        final long ordQty;
        final long priceAmt;
        final int instrTier;

        Scordf(String orderId, String cifNo, String instrCode, String sideKbn,
               String ordType, String tifCode, long ordQty, long priceAmt, int instrTier) {
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

    private static final class TierRule {
        final int tier;
        final int rateBp;
        final long tick;

        TierRule(int tier, int rateBp, long tick) {
            this.tier = tier;
            this.rateBp = rateBp;
            this.tick = tick;
        }

        static TierRule of(int tier) {
            if (tier == 1) {
                return new TierRule(1, 1000, 100L);
            }
            if (tier == 2) {
                return new TierRule(2, 2000, 500L);
            }
            if (tier == 3) {
                return new TierRule(3, 4000, 1000L);
            }
            return null;
        }
    }

    private static final class BoardInfo {
        final String boardCode;
        final int auditFeeBp;

        BoardInfo(String boardCode, int auditFeeBp) {
            this.boardCode = boardCode;
            this.auditFeeBp = auditFeeBp;
        }
    }

    private static final class FeeEnrichmentService {
        BoardInfo attachBoard(String instrCode, int instrTier) {
            if (instrCode != null && instrCode.startsWith("13")) {
                return new BoardInfo(BOARD_ETF, 1);
            }
            if (instrTier == 3) {
                return new BoardInfo(BOARD_ST, 3);
            }
            return new BoardInfo(BOARD_T1, 2);
        }
    }

    private static final class AuditView {
        final Sctcap capture;
        final Scexec execution;
        final Scordf order;
        final BoardInfo board;
        final int decisionCode;
        final String status;

        AuditView(Sctcap capture, Scexec execution, Scordf order,
                  BoardInfo board, int decisionCode, String status) {
            this.capture = capture;
            this.execution = execution;
            this.order = order;
            this.board = board;
            this.decisionCode = decisionCode;
            this.status = status;
        }

        static AuditView unmatched(Sctcap capture, String status) {
            return new AuditView(capture, null, null, new BoardInfo(BOARD_T1, 0), DECISION_REJECT_NOTIONAL, status);
        }

        String toOperatorLine() {
            String side = order == null ? "-" : order.sideKbn;
            String tif = order == null ? "-" : order.tifCode;
            String boardCode = board == null ? "-" : board.boardCode;
            return "取引=" + capture.tradeId
                    + " 約定=" + capture.execId
                    + " 注文=" + capture.orderId
                    + " 顧客=" + capture.cifNo
                    + " 銘柄=" + capture.instrCode
                    + " 売買=" + side
                    + " 数量=" + capture.tradeQty
                    + " 金額=" + capture.tradeAmt
                    + " 有効=" + tif
                    + " 市場=" + boardCode
                    + " 判定=" + decisionCode
                    + " 状態=" + status;
        }
    }
}
