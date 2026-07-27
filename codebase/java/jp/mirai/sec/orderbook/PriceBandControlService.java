/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2020-09-02  村上 健司 (E-301)  初版作成
 */

package jp.mirai.sec.orderbook;

public class PriceBandControlService {
    private static final java.math.BigDecimal BP_DENOMINATOR = new java.math.BigDecimal("10000");
    private static final java.math.BigDecimal MIHFT_MAX_NOTIONAL = new java.math.BigDecimal("500000000");

    public static void main(String[] a) {
        try {
            Config c = Config.from(a);
            java.util.Map<String, Instrument> instruments = readInstruments(c.instrumentFile);
            java.util.Map<String, MarketData> market = readMarketData(c.marketFile);
            java.util.Map<String, BandRow> oldBands = readBands(c.bandFile);

            Result r = calculate(instruments, market, oldBands, c.sourceKbn, c.now);
            writeBands(c.bandFile, r.bands);
            appendAudit(c.auditFile, r.audits);

            System.out.println("価格帯更新件数=" + r.updatedCount + " 監査件数=" + r.audits.size());
        } catch (Exception e) {
            System.err.println("価格帯管理異常: " + e.getMessage());
            System.exit(12);
        }
    }

    private static Result calculate(java.util.Map<String, Instrument> instruments,
                                    java.util.Map<String, MarketData> market,
                                    java.util.Map<String, BandRow> oldBands,
                                    String sourceKbn,
                                    java.time.LocalDateTime now) {
        java.util.Map<String, BandRow> nextBands = new java.util.TreeMap<>(oldBands);
        java.util.List<AuditRow> audits = new java.util.ArrayList<>();
        int updated = 0;

        for (Instrument inst : instruments.values()) {
            MarketData md = market.get(inst.instrCode);
            if (md == null) {
                audits.add(AuditRow.of(inst.instrCode, now, "価格未到着"));
                continue;
            }

            TierRule rule = TierRule.of(inst.instrTier);
            java.math.BigDecimal last = md.lastAmt;
            if (last.signum() <= 0) {
                audits.add(AuditRow.of(inst.instrCode, now, "終値不正"));
                continue;
            }

            java.math.BigDecimal notional = last.multiply(new java.math.BigDecimal(inst.lotQty));
            if (notional.compareTo(MIHFT_MAX_NOTIONAL) > 0) {
                audits.add(AuditRow.of(inst.instrCode, now, "想定元本超過"));
                continue;
            }

            java.math.BigDecimal rawWidth = last
                    .multiply(new java.math.BigDecimal(rule.rateBp))
                    .divide(BP_DENOMINATOR, 0, java.math.RoundingMode.HALF_UP);
            java.math.BigDecimal lower = last.subtract(rawWidth);
            if (lower.signum() < 0) {
                lower = java.math.BigDecimal.ZERO;
            }
            java.math.BigDecimal upper = last.add(rawWidth);

            java.math.BigDecimal tick = rule.tickAmt;
            if (inst.tickAmt.signum() > 0) {
                tick = inst.tickAmt;
            }

            java.math.BigDecimal alignedLower = alignDown(lower, tick);
            java.math.BigDecimal alignedUpper = alignUp(upper, tick);

            if (!isAligned(alignedLower, tick) || !isAligned(alignedUpper, tick)) {
                audits.add(AuditRow.of(inst.instrCode, now, "呼値不整合"));
                continue;
            }
            if (alignedUpper.compareTo(alignedLower) <= 0) {
                audits.add(AuditRow.of(inst.instrCode, now, "価格帯逆転"));
                continue;
            }

            BandRow old = nextBands.get(inst.instrCode);
            BandRow band = new BandRow(inst.instrCode, alignedLower, alignedUpper, now, sourceKbn);
            if (!band.sameAmounts(old)) {
                nextBands.put(inst.instrCode, band);
                audits.add(AuditRow.of(inst.instrCode, now, "価格帯更新"));
                updated++;
            }
        }

        return new Result(nextBands, audits, updated);
    }

    private static java.math.BigDecimal alignDown(java.math.BigDecimal v, java.math.BigDecimal tick) {
        if (tick.signum() <= 0) {
            throw new IllegalArgumentException("呼値が不正");
        }
        java.math.BigDecimal units = v.divide(tick, 0, java.math.RoundingMode.FLOOR);
        return units.multiply(tick).setScale(0, java.math.RoundingMode.UNNECESSARY);
    }

    private static java.math.BigDecimal alignUp(java.math.BigDecimal v, java.math.BigDecimal tick) {
        if (tick.signum() <= 0) {
            throw new IllegalArgumentException("呼値が不正");
        }
        java.math.BigDecimal units = v.divide(tick, 0, java.math.RoundingMode.CEILING);
        return units.multiply(tick).setScale(0, java.math.RoundingMode.UNNECESSARY);
    }

    private static boolean isAligned(java.math.BigDecimal v, java.math.BigDecimal tick) {
        return v.remainder(tick).compareTo(java.math.BigDecimal.ZERO) == 0;
    }

    private static java.util.Map<String, Instrument> readInstruments(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, Instrument> m = new java.util.TreeMap<>();
        if (!java.nio.file.Files.exists(p)) {
            throw new java.io.FileNotFoundException("銘柄ファイルなし: " + p);
        }
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(p, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            int no = 0;
            while ((line = br.readLine()) != null) {
                no++;
                if (skip(line)) {
                    continue;
                }
                String[] f = csv(line);
                if (f.length < 6) {
                    throw new IllegalArgumentException("銘柄項目不足 行=" + no);
                }
                Instrument inst = new Instrument(f[0], f[1], parseInt(f[2], no), money(f[3], no),
                        parseLong(f[4], no), f[5]);
                if (!"T1".equals(inst.boardCode) && !"ST".equals(inst.boardCode) && !"ETF".equals(inst.boardCode)) {
                    throw new IllegalArgumentException("市場区分不正 行=" + no);
                }
                TierRule.of(inst.instrTier);
                m.put(inst.instrCode, inst);
            }
        }
        return m;
    }

    private static java.util.Map<String, MarketData> readMarketData(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, MarketData> m = new java.util.HashMap<>();
        if (!java.nio.file.Files.exists(p)) {
            throw new java.io.FileNotFoundException("市況ファイルなし: " + p);
        }
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(p, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            int no = 0;
            while ((line = br.readLine()) != null) {
                no++;
                if (skip(line)) {
                    continue;
                }
                String[] f = csv(line);
                if (f.length < 6) {
                    throw new IllegalArgumentException("市況項目不足 行=" + no);
                }
                MarketData md = new MarketData(f[0], money(f[1], no), money(f[2], no),
                        money(f[3], no), parseLong(f[4], no), parseTime(f[5], no));
                MarketData prev = m.get(md.instrCode);
                if (prev == null || md.tickTs.compareTo(prev.tickTs) >= 0) {
                    m.put(md.instrCode, md);
                }
            }
        }
        return m;
    }

    private static java.util.Map<String, BandRow> readBands(java.nio.file.Path p) throws java.io.IOException {
        java.util.Map<String, BandRow> m = new java.util.TreeMap<>();
        if (!java.nio.file.Files.exists(p)) {
            return m;
        }
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(p, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            int no = 0;
            while ((line = br.readLine()) != null) {
                no++;
                if (skip(line)) {
                    continue;
                }
                String[] f = csv(line);
                if (f.length < 5) {
                    throw new IllegalArgumentException("価格帯項目不足 行=" + no);
                }
                BandRow band = new BandRow(f[0], money(f[1], no), money(f[2], no), parseTime(f[3], no), f[4]);
                m.put(band.instrCode, band);
            }
        }
        return m;
    }

    private static void writeBands(java.nio.file.Path p, java.util.Map<String, BandRow> bands) throws java.io.IOException {
        try (java.io.BufferedWriter bw = java.nio.file.Files.newBufferedWriter(p, java.nio.charset.StandardCharsets.UTF_8)) {
            bw.write("INSTR-CODE,LOWER-AMT,UPPER-AMT,BAND-TS,SOURCE-KBN");
            bw.newLine();
            for (BandRow b : bands.values()) {
                bw.write(join(b.instrCode, b.lowerAmt.toPlainString(), b.upperAmt.toPlainString(),
                        b.bandTs.toString(), b.sourceKbn));
                bw.newLine();
            }
        }
    }

    private static void appendAudit(java.nio.file.Path p, java.util.List<AuditRow> audits) throws java.io.IOException {
        boolean exists = java.nio.file.Files.exists(p);
        try (java.io.BufferedWriter bw = java.nio.file.Files.newBufferedWriter(p, java.nio.charset.StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)) {
            if (!exists) {
                bw.write("AUDIT-ID,ORDER-ID,EVENT-KBN,CIF-NO,INSTR-CODE,EVENT-TS,DETAIL-CD");
                bw.newLine();
            }
            for (AuditRow a : audits) {
                bw.write(join(a.auditId, a.orderId, a.eventKbn, a.cifNo, a.instrCode, a.eventTs.toString(), a.detailCd));
                bw.newLine();
            }
        }
    }

    private static boolean skip(String line) {
        String t = line.trim();
        return t.isEmpty() || t.startsWith("#") || t.startsWith("INSTR-CODE");
    }

    private static String[] csv(String line) {
        return line.split("\\s*,\\s*", -1);
    }

    private static String join(String... v) {
        return String.join(",", v);
    }

    private static java.math.BigDecimal money(String s, int no) {
        try {
            return new java.math.BigDecimal(s.trim()).setScale(0, java.math.RoundingMode.UNNECESSARY);
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("金額不正 行=" + no);
        }
    }

    private static int parseInt(String s, int no) {
        try {
            return Integer.parseInt(s.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("整数不正 行=" + no);
        }
    }

    private static long parseLong(String s, int no) {
        try {
            return Long.parseLong(s.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("数量不正 行=" + no);
        }
    }

    private static java.time.LocalDateTime parseTime(String s, int no) {
        try {
            return java.time.LocalDateTime.parse(s.trim());
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("日時不正 行=" + no);
        }
    }

    private static final class Config {
        final java.nio.file.Path instrumentFile;
        final java.nio.file.Path marketFile;
        final java.nio.file.Path bandFile;
        final java.nio.file.Path auditFile;
        final String sourceKbn;
        final java.time.LocalDateTime now;

        private Config(java.nio.file.Path instrumentFile, java.nio.file.Path marketFile,
                       java.nio.file.Path bandFile, java.nio.file.Path auditFile,
                       String sourceKbn, java.time.LocalDateTime now) {
            this.instrumentFile = instrumentFile;
            this.marketFile = marketFile;
            this.bandFile = bandFile;
            this.auditFile = auditFile;
            this.sourceKbn = sourceKbn;
            this.now = now;
        }

        static Config from(String[] a) {
            java.nio.file.Path base = a.length > 0 ? java.nio.file.Paths.get(a[0]) : java.nio.file.Paths.get(".");
            String source = a.length > 1 ? a[1] : "AUTO";
            java.time.LocalDateTime now = a.length > 2 ? java.time.LocalDateTime.parse(a[2]) : java.time.LocalDateTime.now();
            return new Config(base.resolve("SCINSTF.csv"), base.resolve("SCMKTD.csv"),
                    base.resolve("SCBAND.csv"), base.resolve("SCAUDF.csv"), source, now);
        }
    }

    private static final class Instrument {
        final String instrCode;
        final String instrName;
        final int instrTier;
        final java.math.BigDecimal tickAmt;
        final long lotQty;
        final String boardCode;

        Instrument(String instrCode, String instrName, int instrTier,
                   java.math.BigDecimal tickAmt, long lotQty, String boardCode) {
            this.instrCode = instrCode;
            this.instrName = instrName;
            this.instrTier = instrTier;
            this.tickAmt = tickAmt;
            this.lotQty = lotQty;
            this.boardCode = boardCode;
        }
    }

    private static final class MarketData {
        final String instrCode;
        final java.math.BigDecimal bidAmt;
        final java.math.BigDecimal askAmt;
        final java.math.BigDecimal lastAmt;
        final long volQty;
        final java.time.LocalDateTime tickTs;

        MarketData(String instrCode, java.math.BigDecimal bidAmt, java.math.BigDecimal askAmt,
                   java.math.BigDecimal lastAmt, long volQty, java.time.LocalDateTime tickTs) {
            this.instrCode = instrCode;
            this.bidAmt = bidAmt;
            this.askAmt = askAmt;
            this.lastAmt = lastAmt;
            this.volQty = volQty;
            this.tickTs = tickTs;
        }
    }

    private static final class BandRow {
        final String instrCode;
        final java.math.BigDecimal lowerAmt;
        final java.math.BigDecimal upperAmt;
        final java.time.LocalDateTime bandTs;
        final String sourceKbn;

        BandRow(String instrCode, java.math.BigDecimal lowerAmt, java.math.BigDecimal upperAmt,
                java.time.LocalDateTime bandTs, String sourceKbn) {
            this.instrCode = instrCode;
            this.lowerAmt = lowerAmt;
            this.upperAmt = upperAmt;
            this.bandTs = bandTs;
            this.sourceKbn = sourceKbn;
        }

        boolean sameAmounts(BandRow other) {
            return other != null
                    && lowerAmt.compareTo(other.lowerAmt) == 0
                    && upperAmt.compareTo(other.upperAmt) == 0
                    && sourceKbn.equals(other.sourceKbn);
        }
    }

    private static final class AuditRow {
        final String auditId;
        final String orderId;
        final String eventKbn;
        final String cifNo;
        final String instrCode;
        final java.time.LocalDateTime eventTs;
        final String detailCd;

        private AuditRow(String auditId, String orderId, String eventKbn, String cifNo,
                         String instrCode, java.time.LocalDateTime eventTs, String detailCd) {
            this.auditId = auditId;
            this.orderId = orderId;
            this.eventKbn = eventKbn;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.eventTs = eventTs;
            this.detailCd = detailCd;
        }

        static AuditRow of(String instrCode, java.time.LocalDateTime ts, String detailCd) {
            String id = "PB" + ts.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS"))
                    + Math.abs(java.util.Objects.hash(instrCode, detailCd));
            return new AuditRow(id, "", "BAND", "", instrCode, ts, detailCd);
        }
    }

    private static final class TierRule {
        final int tier;
        final int rateBp;
        final java.math.BigDecimal tickAmt;

        private TierRule(int tier, int rateBp, int tickAmt) {
            this.tier = tier;
            this.rateBp = rateBp;
            this.tickAmt = new java.math.BigDecimal(tickAmt);
        }

        static TierRule of(int tier) {
            if (tier == 1) {
                return new TierRule(1, 1000, 100);
            }
            if (tier == 2) {
                return new TierRule(2, 2000, 500);
            }
            if (tier == 3) {
                return new TierRule(3, 4000, 1000);
            }
            throw new IllegalArgumentException("銘柄階層不正: " + tier);
        }
    }

    private static final class Result {
        final java.util.Map<String, BandRow> bands;
        final java.util.List<AuditRow> audits;
        final int updatedCount;

        Result(java.util.Map<String, BandRow> bands, java.util.List<AuditRow> audits, int updatedCount) {
            this.bands = bands;
            this.audits = audits;
            this.updatedCount = updatedCount;
        }
    }
}
