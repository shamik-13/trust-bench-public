package jp.mirai.sec.position;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2021-07-15  村上 健司 (E-301)  平均単価バッチ検証サービス初版
 */
public class AverageCostBatchService {
    private static final BigDecimal MIHFT_MAX_NOTIONAL = new BigDecimal("500000000");
    private static final int TIER1_RATE_BP = 1000;
    private static final int TIER2_RATE_BP = 2000;
    private static final int TIER3_RATE_BP = 4000;
    private static final BigDecimal TIER1_TICK = new BigDecimal("100");
    private static final BigDecimal TIER2_TICK = new BigDecimal("500");
    private static final BigDecimal TIER3_TICK = new BigDecimal("1000");
    private static final DateTimeFormatter TS_FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");

    public static void main(String[] a) throws Exception {
        if (a.length != 4) {
            System.err.println("使用法: java AverageCostBatchService SCEXEC.csv SCPOSF.csv SCPNLF.csv SESS-DT");
            System.exit(12);
        }

        Path execPath = Path.of(a[0]);
        Path posPath = Path.of(a[1]);
        Path pnlPath = Path.of(a[2]);
        LocalDate sessDt = LocalDate.parse(a[3]);

        Map<Key, PositionCalc> positions = readPositions(posPath);
        List<ExecutionRecord> executions = readExecutions(execPath);
        executions.sort(Comparator.comparing(e -> e.execTs));

        for (ExecutionRecord exec : executions) {
            Key key = new Key(exec.cifNo, exec.instrCode);
            PositionCalc calc = positions.computeIfAbsent(key, k -> new PositionCalc(k.cifNo, k.instrCode));
            calc.apply(exec);
        }

        writeCorrections(pnlPath, positions, sessDt);
    }

    private static Map<Key, PositionCalc> readPositions(Path path) throws IOException {
        Map<Key, PositionCalc> map = new HashMap<>();
        try (BufferedReader br = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (first && line.startsWith("CIF-NO,")) {
                    first = false;
                    continue;
                }
                first = false;
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> c = splitCsv(line);
                requireColumns(c, 5, "SCPOSF");
                PositionCalc p = new PositionCalc(c.get(0), c.get(1));
                p.netQty = new BigDecimal(c.get(2));
                p.avgAmt = money(c.get(3));
                p.rlsdAmt = money(c.get(4));
                p.orgNetQty = p.netQty;
                p.orgAvgAmt = p.avgAmt;
                p.orgRlsdAmt = p.rlsdAmt;
                map.put(new Key(p.cifNo, p.instrCode), p);
            }
        }
        return map;
    }

    private static List<ExecutionRecord> readExecutions(Path path) throws IOException {
        List<ExecutionRecord> list = new ArrayList<>();
        try (BufferedReader br = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (first && line.startsWith("EXEC-ID,")) {
                    first = false;
                    continue;
                }
                first = false;
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> c = splitCsv(line);
                requireColumns(c, 7, "SCEXEC");
                String side = c.get(3);
                if (!"B".equals(side) && !"S".equals(side)) {
                    throw new IllegalArgumentException("SIDE-KBN不正: " + side);
                }
                BigDecimal qty = new BigDecimal(c.get(4));
                BigDecimal amt = money(c.get(5));
                if (qty.signum() <= 0 || amt.signum() < 0) {
                    throw new IllegalArgumentException("約定数量または約定金額不正: " + c.get(0));
                }
                if (amt.compareTo(MIHFT_MAX_NOTIONAL) > 0) {
                    throw new IllegalArgumentException("想定元本上限超過: " + c.get(0));
                }
                list.add(new ExecutionRecord(c.get(0), c.get(1), cifFromOrder(c.get(1)), c.get(2), side, qty, amt,
                        LocalDateTime.parse(c.get(6), TS_FMT)));
            }
        }
        return list;
    }

    private static void writeCorrections(Path path, Map<Key, PositionCalc> positions, LocalDate sessDt) throws IOException {
        List<PositionCalc> diffs = new ArrayList<>();
        for (PositionCalc p : positions.values()) {
            if (p.hasDifference()) {
                diffs.add(p);
            }
        }
        diffs.sort(Comparator.comparing((PositionCalc p) -> p.cifNo).thenComparing(p -> p.instrCode));

        try (BufferedWriter bw = Files.newBufferedWriter(path, StandardCharsets.UTF_8)) {
            bw.write("CIF-NO,INSTR-CODE,SESS-DT,RLZD-AMT,UNRLZD-AMT,FEE-AMT,CALC-TS");
            bw.newLine();
            String calcTs = LocalDateTime.now().withNano(0).format(TS_FMT);
            for (PositionCalc p : diffs) {
                BigDecimal rlzdCorrection = p.rlsdAmt.subtract(p.orgRlsdAmt).setScale(0, RoundingMode.HALF_UP);
                BigDecimal avgCorrection = p.avgAmt.subtract(p.orgAvgAmt).multiply(p.netQty.abs()).setScale(0, RoundingMode.HALF_UP);
                bw.write(csv(p.cifNo));
                bw.write(',');
                bw.write(csv(p.instrCode));
                bw.write(',');
                bw.write(sessDt.toString());
                bw.write(',');
                bw.write(plain(rlzdCorrection));
                bw.write(',');
                bw.write(plain(avgCorrection));
                bw.write(',');
                bw.write("0");
                bw.write(',');
                bw.write(calcTs);
                bw.newLine();
            }
        }
    }

    private static String cifFromOrder(String orderId) {
        int p = orderId.indexOf('-');
        if (p > 0) {
            return orderId.substring(0, p);
        }
        p = orderId.indexOf('_');
        if (p > 0) {
            return orderId.substring(0, p);
        }
        throw new IllegalArgumentException("ORDER-IDからCIF-NOを判定不可: " + orderId);
    }

    private static void requireColumns(List<String> c, int n, String name) {
        if (c.size() != n) {
            throw new IllegalArgumentException(name + "項目数不正: " + c.size());
        }
    }

    private static BigDecimal money(String s) {
        return new BigDecimal(s).setScale(0, RoundingMode.HALF_UP);
    }

    private static BigDecimal unitFor(String instrCode) {
        int tier = tierFor(instrCode);
        if (tier == 1) {
            return TIER1_TICK;
        }
        if (tier == 2) {
            return TIER2_TICK;
        }
        return TIER3_TICK;
    }

    private static int tierFor(String instrCode) {
        int h = Math.abs(instrCode.hashCode());
        int bucket = h % 10;
        if (bucket < 5) {
            return 1;
        }
        if (bucket < 8) {
            return 2;
        }
        return 3;
    }

    private static int marginRateBpFor(String instrCode) {
        int tier = tierFor(instrCode);
        if (tier == 1) {
            return TIER1_RATE_BP;
        }
        if (tier == 2) {
            return TIER2_RATE_BP;
        }
        return TIER3_RATE_BP;
    }

    private static List<String> splitCsv(String line) {
        List<String> out = new ArrayList<>();
        StringBuilder b = new StringBuilder();
        boolean q = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (q && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    b.append('"');
                    i++;
                } else {
                    q = !q;
                }
            } else if (ch == ',' && !q) {
                out.add(b.toString());
                b.setLength(0);
            } else {
                b.append(ch);
            }
        }
        out.add(b.toString());
        return out;
    }

    private static String csv(String s) {
        if (s.indexOf(',') < 0 && s.indexOf('"') < 0 && s.indexOf('\n') < 0 && s.indexOf('\r') < 0) {
            return s;
        }
        return '"' + s.replace("\"", "\"\"") + '"';
    }

    private static String plain(BigDecimal n) {
        return n.stripTrailingZeros().toPlainString();
    }

    private static final class PositionCalc {
        final String cifNo;
        final String instrCode;
        BigDecimal netQty = BigDecimal.ZERO;
        BigDecimal avgAmt = BigDecimal.ZERO;
        BigDecimal rlsdAmt = BigDecimal.ZERO;
        BigDecimal orgNetQty = BigDecimal.ZERO;
        BigDecimal orgAvgAmt = BigDecimal.ZERO;
        BigDecimal orgRlsdAmt = BigDecimal.ZERO;

        PositionCalc(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        /*
         * 平均取得単価 (AVG-AMT) の算定は mihft_pos 本体に従う。当バッチは SCPOSF が
         * 保持する AVG-AMT を所与の単価基準として検証に用いるだけで、約定列からの
         * 再算定は行わない。反対売買分について実現損益 (RLZD-AMT) の増分と NET-QTY の
         * 更新のみを行い、建玉がゼロになった銘柄は AVG-AMT を明示的にゼロへ戻す。
         */
        void apply(ExecutionRecord e) {
            BigDecimal execPrice = e.fillAmt.divide(e.fillQty, 10, RoundingMode.HALF_UP);
            if ("B".equals(e.sideKbn)) {
                buy(e.fillQty, execPrice);
            } else {
                sell(e.fillQty, execPrice);
            }
            BigDecimal margin = e.fillAmt.multiply(new BigDecimal(marginRateBpFor(instrCode)))
                    .divide(new BigDecimal("10000"), 0, RoundingMode.HALF_UP);
            if (margin.signum() < 0) {
                throw new IllegalStateException("委託保証金計算不正: " + e.execId);
            }
        }

        private void buy(BigDecimal qty, BigDecimal price) {
            // 追加建て: 数量のみ反映し、AVG-AMT は本体算定値を保持する。
            if (netQty.signum() >= 0) {
                netQty = netQty.add(qty);
                return;
            }

            // 反対売買: 本体確定の単価基準 (avgAmt) を所与に実現損益を増分する。
            BigDecimal closeQty = qty.min(netQty.abs());
            rlsdAmt = rlsdAmt.add(avgAmt.subtract(price).multiply(closeQty)).setScale(0, RoundingMode.HALF_UP);
            netQty = netQty.add(qty);
            if (netQty.signum() == 0) {
                avgAmt = BigDecimal.ZERO;
            }
        }

        private void sell(BigDecimal qty, BigDecimal price) {
            // 追加建て(売り): 数量のみ反映し、AVG-AMT は本体算定値を保持する。
            if (netQty.signum() <= 0) {
                netQty = netQty.subtract(qty);
                return;
            }

            // 反対売買: 本体確定の単価基準 (avgAmt) を所与に実現損益を増分する。
            BigDecimal closeQty = qty.min(netQty);
            rlsdAmt = rlsdAmt.add(price.subtract(avgAmt).multiply(closeQty)).setScale(0, RoundingMode.HALF_UP);
            netQty = netQty.subtract(qty);
            if (netQty.signum() == 0) {
                avgAmt = BigDecimal.ZERO;
            }
        }

        boolean hasDifference() {
            BigDecimal unit = unitFor(instrCode);
            BigDecimal avgDiff = avgAmt.subtract(orgAvgAmt).abs();
            BigDecimal rlsdDiff = rlsdAmt.subtract(orgRlsdAmt).abs();
            return avgDiff.compareTo(unit) > 0 || rlsdDiff.compareTo(unit) > 0 || netQty.compareTo(orgNetQty) != 0;
        }
    }

    private static final class ExecutionRecord {
        final String execId;
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String sideKbn;
        final BigDecimal fillQty;
        final BigDecimal fillAmt;
        final LocalDateTime execTs;

        ExecutionRecord(String execId, String orderId, String cifNo, String instrCode, String sideKbn,
                BigDecimal fillQty, BigDecimal fillAmt, LocalDateTime execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class Key {
        final String cifNo;
        final String instrCode;

        Key(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Key)) {
                return false;
            }
            Key k = (Key) o;
            return Objects.equals(cifNo, k.cifNo) && Objects.equals(instrCode, k.instrCode);
        }

        @Override
        public int hashCode() {
            return Objects.hash(cifNo, instrCode);
        }
    }
}
