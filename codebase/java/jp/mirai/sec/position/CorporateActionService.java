/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2022-02-22  渡辺 隆 (E-260)  初版作成
 */

package jp.mirai.sec.position;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class CorporateActionService {
    private static final BigDecimal ZERO = BigDecimal.ZERO;
    private static final BigDecimal MIHFT_MAX_NOTIONAL = new BigDecimal("500000000");

    public static void main(String[] a) {
        new CorporateActionService().runBenchmark();
    }

    private void runBenchmark() {
        List<Sccact> actions = seedActions();
        List<Sclot> lots = seedLots();
        List<Scposf> positions = seedPositions();
        List<Schldf> holdings = seedHoldings();
        List<Scpnlf> pnl = new ArrayList<Scpnlf>();
        Set<String> applied = new HashSet<String>();

        CorporateActionService service = new CorporateActionService();
        ApplyResult result = service.apply(LocalDate.of(2026, 6, 27), actions, lots, positions, holdings, pnl, applied);

        System.out.println("処理件数=" + result.processedActions);
        System.out.println("スキップ件数=" + result.skippedActions);
        System.out.println("ロット件数=" + lots.size());
        System.out.println("ポジション件数=" + positions.size());
        System.out.println("保有件数=" + holdings.size());
        System.out.println("損益件数=" + pnl.size());
    }

    private ApplyResult apply(
            LocalDate sessionDate,
            List<Sccact> actions,
            List<Sclot> lots,
            List<Scposf> positions,
            List<Schldf> holdings,
            List<Scpnlf> pnl,
            Set<String> appliedActionIds) {
        Objects.requireNonNull(sessionDate, "SESS-DT");
        Objects.requireNonNull(actions, "SCCACT");
        Objects.requireNonNull(lots, "SCLOT");
        Objects.requireNonNull(positions, "SCPOSF");
        Objects.requireNonNull(holdings, "SCHLDF");
        Objects.requireNonNull(pnl, "SCPNLF");
        Objects.requireNonNull(appliedActionIds, "ACTION-ID");

        actions.sort(Comparator.comparing((Sccact x) -> x.exDate).thenComparing(x -> x.actionId));

        int processed = 0;
        int skipped = 0;
        for (Sccact action : actions) {
            validateAction(action);
            if (action.exDate.isAfter(sessionDate) || appliedActionIds.contains(action.actionId)) {
                skipped++;
                continue;
            }

            if ("S".equals(action.actionKbn) || "C".equals(action.actionKbn)) {
                applyQuantityAction(sessionDate, action, lots, holdings, pnl);
                rebuildPositions(action.instrCode, lots, positions, pnl);
            } else if ("H".equals(action.actionKbn)) {
                applyCashAction(sessionDate, action, lots, pnl);
            } else {
                throw new IllegalArgumentException("ACTION-KBN不正:" + action.actionKbn);
            }

            appliedActionIds.add(action.actionId);
            processed++;
        }
        return new ApplyResult(processed, skipped);
    }

    private void applyQuantityAction(
            LocalDate sessionDate,
            Sccact action,
            List<Sclot> lots,
            List<Schldf> holdings,
            List<Scpnlf> pnl) {
        BigDecimal ratioNum = BigDecimal.valueOf(action.ratioNum);
        BigDecimal ratioDen = BigDecimal.valueOf(action.ratioDen);

        Map<Key, BigDecimal> cashByKey = new HashMap<Key, BigDecimal>();

        for (Sclot lot : lots) {
            if (!lot.instrCode.equals(action.instrCode)) {
                continue;
            }
            BigDecimal exactQty = BigDecimal.valueOf(lot.openQty).multiply(ratioNum).divide(ratioDen, 12, RoundingMode.HALF_UP);
            long newQty = exactQty.setScale(0, RoundingMode.DOWN).longValueExact();
            BigDecimal fractionalQty = exactQty.subtract(BigDecimal.valueOf(newQty));
            BigDecimal oldUnitAmt = unitAmount(lot.openAmt, lot.openQty);
            BigDecimal fractionalCash = fractionalQty.multiply(oldUnitAmt).setScale(0, RoundingMode.HALF_UP);

            lot.openQty = newQty;
            lot.openAmt = newQty == 0
                    ? ZERO
                    : lot.openAmt.subtract(fractionalCash).max(ZERO).setScale(0, RoundingMode.HALF_UP);

            if (fractionalCash.signum() != 0) {
                Key key = new Key(lot.cifNo, lot.instrCode);
                cashByKey.put(key, cashByKey.getOrDefault(key, ZERO).add(fractionalCash));
            }
        }

        for (Schldf holding : holdings) {
            if (!holding.instrCode.equals(action.instrCode)) {
                continue;
            }
            holding.settledQty = adjustQty(holding.settledQty, action.ratioNum, action.ratioDen);
            holding.tradeQty = adjustQty(holding.tradeQty, action.ratioNum, action.ratioDen);
            holding.restrictedQty = adjustQty(holding.restrictedQty, action.ratioNum, action.ratioDen);
        }

        writePnl(sessionDate, cashByKey, pnl);
    }

    private void applyCashAction(LocalDate sessionDate, Sccact action, List<Sclot> lots, List<Scpnlf> pnl) {
        Map<Key, BigDecimal> cashByKey = new HashMap<Key, BigDecimal>();
        for (Sclot lot : lots) {
            if (!lot.instrCode.equals(action.instrCode) || lot.openQty == 0) {
                continue;
            }
            BigDecimal cash = BigDecimal.valueOf(lot.openQty).multiply(action.cashAmt).setScale(0, RoundingMode.HALF_UP);
            Key key = new Key(lot.cifNo, lot.instrCode);
            cashByKey.put(key, cashByKey.getOrDefault(key, ZERO).add(cash));
            lot.openAmt = lot.openAmt.subtract(cash).max(ZERO).setScale(0, RoundingMode.HALF_UP);
        }
        writePnl(sessionDate, cashByKey, pnl);
    }

    private void rebuildPositions(String instrCode, List<Sclot> lots, List<Scposf> positions, List<Scpnlf> pnl) {
        Map<Key, PositionWork> work = new HashMap<Key, PositionWork>();
        for (Sclot lot : lots) {
            if (!lot.instrCode.equals(instrCode)) {
                continue;
            }
            Key key = new Key(lot.cifNo, lot.instrCode);
            PositionWork w = work.get(key);
            if (w == null) {
                w = new PositionWork();
                work.put(key, w);
            }
            w.qty += lot.openQty;
            w.amount = w.amount.add(lot.openAmt);
        }

        positions.removeIf(p -> p.instrCode.equals(instrCode));
        for (Map.Entry<Key, PositionWork> e : work.entrySet()) {
            PositionWork w = e.getValue();
            BigDecimal avg = w.qty == 0 ? ZERO : w.amount.divide(BigDecimal.valueOf(w.qty), 4, RoundingMode.HALF_UP);
            BigDecimal realized = realizedOf(e.getKey(), pnl);
            positions.add(new Scposf(e.getKey().cifNo, e.getKey().instrCode, w.qty, avg, realized));
        }
        positions.sort(Comparator.comparing((Scposf p) -> p.cifNo).thenComparing(p -> p.instrCode));
    }

    private void writePnl(LocalDate sessionDate, Map<Key, BigDecimal> cashByKey, List<Scpnlf> pnl) {
        LocalDateTime calcTs = LocalDateTime.now();
        for (Map.Entry<Key, BigDecimal> e : cashByKey.entrySet()) {
            BigDecimal amount = e.getValue().setScale(0, RoundingMode.HALF_UP);
            if (amount.signum() == 0) {
                continue;
            }
            pnl.add(new Scpnlf(e.getKey().cifNo, e.getKey().instrCode, sessionDate, amount, ZERO, ZERO, calcTs));
        }
    }

    private BigDecimal realizedOf(Key key, List<Scpnlf> pnl) {
        BigDecimal amount = ZERO;
        for (Scpnlf row : pnl) {
            if (row.cifNo.equals(key.cifNo) && row.instrCode.equals(key.instrCode)) {
                amount = amount.add(row.rlzdAmt);
            }
        }
        return amount.setScale(0, RoundingMode.HALF_UP);
    }

    private void validateAction(Sccact action) {
        if (action.actionId == null || action.actionId.trim().isEmpty()) {
            throw new IllegalArgumentException("ACTION-ID不正");
        }
        if (action.instrCode == null || action.instrCode.trim().isEmpty()) {
            throw new IllegalArgumentException("INSTR-CODE不正");
        }
        if (action.exDate == null) {
            throw new IllegalArgumentException("EX-DT不正");
        }
        if (("S".equals(action.actionKbn) || "C".equals(action.actionKbn))
                && (action.ratioNum <= 0 || action.ratioDen <= 0)) {
            throw new IllegalArgumentException("RATIO不正:" + action.actionId);
        }
        if ("H".equals(action.actionKbn) && action.cashAmt.signum() < 0) {
            throw new IllegalArgumentException("CASH-AMT不正:" + action.actionId);
        }
        if (!"S".equals(action.actionKbn) && !"C".equals(action.actionKbn) && !"H".equals(action.actionKbn)) {
            throw new IllegalArgumentException("ACTION-KBN不正:" + action.actionKbn);
        }
    }

    private long adjustQty(long qty, long ratioNum, long ratioDen) {
        return BigDecimal.valueOf(qty)
                .multiply(BigDecimal.valueOf(ratioNum))
                .divide(BigDecimal.valueOf(ratioDen), 0, RoundingMode.DOWN)
                .longValueExact();
    }

    private BigDecimal unitAmount(BigDecimal amount, long qty) {
        if (qty == 0) {
            return ZERO;
        }
        return amount.divide(BigDecimal.valueOf(qty), 8, RoundingMode.HALF_UP);
    }

    private List<Sccact> seedActions() {
        List<Sccact> rows = new ArrayList<Sccact>();
        rows.add(new Sccact("ACT-20250115-7203-SPL", "7203", LocalDate.of(2026, 6, 25), "S", 3, 1, ZERO));
        rows.add(new Sccact("ACT-20250115-8306-CNV", "8306", LocalDate.of(2026, 6, 26), "C", 1, 5, ZERO));
        rows.add(new Sccact("ACT-20250115-6758-CSH", "6758", LocalDate.of(2026, 6, 27), "H", 1, 1, new BigDecimal("35")));
        rows.add(new Sccact("ACT-20250115-9984-SPL", "9984", LocalDate.of(2026, 6, 28), "S", 2, 1, ZERO));
        return rows;
    }

    private List<Sclot> seedLots() {
        List<Sclot> rows = new ArrayList<Sclot>();
        rows.add(new Sclot("L202501150001", "CIF000001", "7203", 101, new BigDecimal("298455"), LocalDateTime.of(2026, 6, 24, 9, 2, 3), "E202501150001"));
        rows.add(new Sclot("L202501150002", "CIF000001", "8306", 43, new BigDecimal("67395"), LocalDateTime.of(2026, 6, 24, 9, 15, 41), "E202501150122"));
        rows.add(new Sclot("L202501150001", "CIF000002", "8306", 112, new BigDecimal("176064"), LocalDateTime.of(2026, 6, 25, 10, 4, 18), "E202501150045"));
        rows.add(new Sclot("L202501150002", "CIF000003", "6758", 77, new BigDecimal("1016400"), LocalDateTime.of(2026, 6, 25, 12, 44, 9), "E202501150331"));
        rows.add(new Sclot("L202501150001", "CIF000004", "7203", 5, new BigDecimal("14775"), LocalDateTime.of(2026, 6, 26, 14, 51, 32), "E202501150098"));
        return rows;
    }

    private List<Scposf> seedPositions() {
        List<Scposf> rows = new ArrayList<Scposf>();
        rows.add(new Scposf("CIF000001", "7203", 101, new BigDecimal("2955.0000"), ZERO));
        rows.add(new Scposf("CIF000001", "8306", 43, new BigDecimal("1567.3256"), ZERO));
        rows.add(new Scposf("CIF000002", "8306", 112, new BigDecimal("1572.0000"), ZERO));
        rows.add(new Scposf("CIF000003", "6758", 77, new BigDecimal("13200.0000"), ZERO));
        rows.add(new Scposf("CIF000004", "7203", 5, new BigDecimal("2955.0000"), ZERO));
        return rows;
    }

    private List<Schldf> seedHoldings() {
        List<Schldf> rows = new ArrayList<Schldf>();
        rows.add(new Schldf("CIF000001", "7203", LocalDate.of(2026, 6, 26), 100, 1, 0));
        rows.add(new Schldf("CIF000001", "8306", LocalDate.of(2026, 6, 26), 40, 3, 0));
        rows.add(new Schldf("CIF000002", "8306", LocalDate.of(2026, 6, 26), 100, 12, 0));
        rows.add(new Schldf("CIF000003", "6758", LocalDate.of(2026, 6, 26), 77, 0, 0));
        rows.add(new Schldf("CIF000004", "7203", LocalDate.of(2026, 6, 26), 0, 5, 0));
        return rows;
    }

    private static final class Sccact {
        final String actionId;
        final String instrCode;
        final LocalDate exDate;
        final String actionKbn;
        final long ratioNum;
        final long ratioDen;
        final BigDecimal cashAmt;

        Sccact(String actionId, String instrCode, LocalDate exDate, String actionKbn, long ratioNum, long ratioDen, BigDecimal cashAmt) {
            this.actionId = actionId;
            this.instrCode = instrCode;
            this.exDate = exDate;
            this.actionKbn = actionKbn;
            this.ratioNum = ratioNum;
            this.ratioDen = ratioDen;
            this.cashAmt = cashAmt;
        }
    }

    private static final class Sclot {
        final String lotId;
        final String cifNo;
        final String instrCode;
        long openQty;
        BigDecimal openAmt;
        final LocalDateTime acqTs;
        final String srcExecId;

        Sclot(String lotId, String cifNo, String instrCode, long openQty, BigDecimal openAmt, LocalDateTime acqTs, String srcExecId) {
            this.lotId = lotId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.openQty = openQty;
            this.openAmt = openAmt;
            this.acqTs = acqTs;
            this.srcExecId = srcExecId;
        }
    }

    private static final class Scposf {
        final String cifNo;
        final String instrCode;
        final long netQty;
        final BigDecimal avgAmt;
        final BigDecimal rlzdAmt;

        Scposf(String cifNo, String instrCode, long netQty, BigDecimal avgAmt, BigDecimal rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    private static final class Schldf {
        final String cifNo;
        final String instrCode;
        final LocalDate asofDate;
        long settledQty;
        long tradeQty;
        long restrictedQty;

        Schldf(String cifNo, String instrCode, LocalDate asofDate, long settledQty, long tradeQty, long restrictedQty) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.asofDate = asofDate;
            this.settledQty = settledQty;
            this.tradeQty = tradeQty;
            this.restrictedQty = restrictedQty;
        }
    }

    private static final class Scpnlf {
        final String cifNo;
        final String instrCode;
        final LocalDate sessionDate;
        final BigDecimal rlzdAmt;
        final BigDecimal unrlzdAmt;
        final BigDecimal feeAmt;
        final LocalDateTime calcTs;

        Scpnlf(String cifNo, String instrCode, LocalDate sessionDate, BigDecimal rlzdAmt, BigDecimal unrlzdAmt, BigDecimal feeAmt, LocalDateTime calcTs) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sessionDate = sessionDate;
            this.rlzdAmt = rlzdAmt;
            this.unrlzdAmt = unrlzdAmt;
            this.feeAmt = feeAmt;
            this.calcTs = calcTs;
        }
    }

    private static final class ApplyResult {
        final int processedActions;
        final int skippedActions;

        ApplyResult(int processedActions, int skippedActions) {
            this.processedActions = processedActions;
            this.skippedActions = skippedActions;
        }
    }

    private static final class PositionWork {
        long qty;
        BigDecimal amount = ZERO;
    }

    private static final class Key {
        final String cifNo;
        final String instrCode;

        Key(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        public boolean equals(Object o) {
            if (!(o instanceof Key)) {
                return false;
            }
            Key other = (Key) o;
            return cifNo.equals(other.cifNo) && instrCode.equals(other.instrCode);
        }

        public int hashCode() {
            return Objects.hash(cifNo, instrCode);
        }

        public String toString() {
            return String.format(Locale.ROOT, "%s:%s", cifNo, instrCode);
        }
    }
}
