/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2019/10/22  三宅 拓也 (E-241)      初版作成。ポジション報告サービスの事前判定版。
 */

package jp.mirai.sec.position;

public class PositionReportService {
    private static final long MIHFT_MAX_NOTIONAL = 500000000L;
    private static final int TIER1_MARGIN_BP = 1000;
    private static final int TIER2_MARGIN_BP = 2000;
    private static final int TIER3_MARGIN_BP = 4000;
    private static final int TIER1_TICK = 100;
    private static final int TIER2_TICK = 500;
    private static final int TIER3_TICK = 1000;

    public static void main(String[] a) throws Exception {
        java.time.LocalDate sessDt = a.length > 0 ? java.time.LocalDate.parse(a[0]) : java.time.LocalDate.of(2026, 6, 26);

        java.util.List<Scposf> scposf = a.length > 1 ? readScposf(a[1]) : sampleScposf();
        java.util.List<Scm2mf> scm2mf = a.length > 2 ? readScm2mf(a[2]) : sampleScm2mf(sessDt);
        java.util.List<Scpnlf> scpnlf = a.length > 3 ? readScpnlf(a[3]) : sampleScpnlf(sessDt);
        java.util.List<Schldf> schldf = a.length > 4 ? readSchldf(a[4]) : sampleSchldf(sessDt);
        java.util.List<Scexpf> scexpf = a.length > 5 ? readScexpf(a[5]) : sampleScexpf(sessDt);

        java.util.List<ReportLine> report = build(sessDt, scposf, scm2mf, scpnlf, schldf, scexpf);
        print(report);
    }

    private static java.util.List<ReportLine> build(
            java.time.LocalDate sessDt,
            java.util.List<Scposf> positions,
            java.util.List<Scm2mf> marks,
            java.util.List<Scpnlf> pnls,
            java.util.List<Schldf> holds,
            java.util.List<Scexpf> exposures) {
        java.util.Map<String, Scm2mf> markMap = new java.util.HashMap<String, Scm2mf>();
        java.util.Map<String, Scpnlf> pnlMap = new java.util.HashMap<String, Scpnlf>();
        java.util.Map<String, Schldf> holdMap = new java.util.HashMap<String, Schldf>();
        java.util.Map<String, Scexpf> expMap = new java.util.HashMap<String, Scexpf>();

        for (Scm2mf r : marks) {
            if (sessDt.equals(r.sessDt) && !isInternal(r.instrCode) && r.markAmt != 0L) {
                markMap.put(key(r.cifNo, r.instrCode), r);
            }
        }
        for (Scpnlf r : pnls) {
            if (sessDt.equals(r.sessDt) && !isStale(sessDt, r.calcTs)) {
                pnlMap.put(key(r.cifNo, r.instrCode), r);
            }
        }
        for (Schldf r : holds) {
            if (sessDt.equals(r.asofDt)) {
                holdMap.put(key(r.cifNo, r.instrCode), r);
            }
        }
        for (Scexpf r : exposures) {
            if (sessDt.equals(r.sessDt)) {
                expMap.put(r.cifNo, r);
            }
        }

        java.util.List<ReportLine> out = new java.util.ArrayList<ReportLine>();
        for (Scposf p : positions) {
            if (isInternal(p.instrCode)) {
                continue;
            }

            String key = key(p.cifNo, p.instrCode);
            Scm2mf m = markMap.get(key);
            Scpnlf n = pnlMap.get(key);
            Schldf h = holdMap.get(key);
            Scexpf e = expMap.get(p.cifNo);

            if (m == null || n == null || h == null || e == null) {
                continue;
            }

            long reportQty = p.netQty;
            if (reportQty != m.netQty) {
                reportQty = m.netQty;
            }

            long settledQty = h.settledQty;
            long tradeQty = h.tradeQty;
            long restrictedQty = h.restrictedQty;
            long availableQty = settledQty + tradeQty - restrictedQty;

            int tier = instrTier(p.instrCode);
            int marginBp = marginBp(tier);
            int tick = tickSize(tier);
            int decision = decisionCode(m.markNotionalAmt, m.markAmt, tick);
            long marginReqAmt = absMulDiv(m.markNotionalAmt, marginBp, 10000);

            long realizedAmt = p.rlzdAmt + n.rlzdAmt - n.feeAmt;
            long unrealizedAmt = n.unrlzdAmt;
            long valuationAmt = m.markNotionalAmt;
            long exposureAmt = e.netExposureAmt;

            out.add(new ReportLine(
                    p.cifNo,
                    p.instrCode,
                    sessDt,
                    reportQty,
                    p.avgAmt,
                    m.markAmt,
                    valuationAmt,
                    realizedAmt,
                    unrealizedAmt,
                    availableQty,
                    settledQty,
                    tradeQty,
                    restrictedQty,
                    e.grossLongAmt,
                    e.grossShortAmt,
                    exposureAmt,
                    e.limitUtilPct,
                    marginReqAmt,
                    decision));
        }

        java.util.Collections.sort(out, new java.util.Comparator<ReportLine>() {
            public int compare(ReportLine x, ReportLine y) {
                int c = x.cifNo.compareTo(y.cifNo);
                if (c != 0) {
                    return c;
                }
                return x.instrCode.compareTo(y.instrCode);
            }
        });
        return out;
    }

    private static void print(java.util.List<ReportLine> report) {
        System.out.println("CIF-NO,INSTR-CODE,SESS-DT,NET-QTY,AVG-AMT,MARK-AMT,MARK-NOTIONAL-AMT,RLZD-AMT,UNRLZD-AMT,AVAILABLE-QTY,SETTLED-QTY,TRADE-QTY,RESTRICTED-QTY,GROSS-LONG-AMT,GROSS-SHORT-AMT,NET-EXPOSURE-AMT,LIMIT-UTIL-PCT,MARGIN-REQ-AMT,DECISION-CD");
        for (ReportLine r : report) {
            System.out.println(
                    r.cifNo + "," +
                    r.instrCode + "," +
                    r.sessDt + "," +
                    r.netQty + "," +
                    r.avgAmt + "," +
                    r.markAmt + "," +
                    r.markNotionalAmt + "," +
                    r.rlzdAmt + "," +
                    r.unrlzdAmt + "," +
                    r.availableQty + "," +
                    r.settledQty + "," +
                    r.tradeQty + "," +
                    r.restrictedQty + "," +
                    r.grossLongAmt + "," +
                    r.grossShortAmt + "," +
                    r.netExposureAmt + "," +
                    r.limitUtilPct + "," +
                    r.marginReqAmt + "," +
                    r.decisionCd);
        }
    }

    private static int decisionCode(long notionalAmt, long markAmt, int tick) {
        if (Math.abs(notionalAmt) > MIHFT_MAX_NOTIONAL) {
            return 8;
        }
        if (markAmt % tick != 0L) {
            return 12;
        }
        long margin = absMulDiv(notionalAmt, TIER3_MARGIN_BP, 10000);
        if (margin > MIHFT_MAX_NOTIONAL / 2L) {
            return 4;
        }
        return 0;
    }

    private static boolean isInternal(String instrCode) {
        return instrCode.startsWith("INT") || instrCode.startsWith("9");
    }

    private static boolean isStale(java.time.LocalDate sessDt, java.time.LocalDateTime calcTs) {
        return calcTs.toLocalDate().isBefore(sessDt);
    }

    private static int instrTier(String instrCode) {
        char c = instrCode.length() == 0 ? '0' : instrCode.charAt(0);
        if (c == '1' || c == '2' || c == '3') {
            return 1;
        }
        if (c == '4' || c == '5' || c == '6') {
            return 2;
        }
        return 3;
    }

    private static int marginBp(int tier) {
        if (tier == 1) {
            return TIER1_MARGIN_BP;
        }
        if (tier == 2) {
            return TIER2_MARGIN_BP;
        }
        return TIER3_MARGIN_BP;
    }

    private static int tickSize(int tier) {
        if (tier == 1) {
            return TIER1_TICK;
        }
        if (tier == 2) {
            return TIER2_TICK;
        }
        return TIER3_TICK;
    }

    private static long absMulDiv(long value, int mul, int div) {
        return Math.abs(value) * (long) mul / (long) div;
    }

    private static String key(String cifNo, String instrCode) {
        return cifNo + "\u0001" + instrCode;
    }

    private static java.util.List<String[]> readCsv(String path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<String[]> rows = new java.util.ArrayList<String[]>();
        for (String line : lines) {
            if (line.trim().isEmpty() || line.startsWith("CIF-NO")) {
                continue;
            }
            rows.add(line.split(",", -1));
        }
        return rows;
    }

    private static java.util.List<Scposf> readScposf(String path) throws java.io.IOException {
        java.util.List<Scposf> out = new java.util.ArrayList<Scposf>();
        for (String[] r : readCsv(path)) {
            out.add(new Scposf(r[0], r[1], Long.parseLong(r[2]), Long.parseLong(r[3]), Long.parseLong(r[4])));
        }
        return out;
    }

    private static java.util.List<Scm2mf> readScm2mf(String path) throws java.io.IOException {
        java.util.List<Scm2mf> out = new java.util.ArrayList<Scm2mf>();
        for (String[] r : readCsv(path)) {
            out.add(new Scm2mf(r[0], r[1], java.time.LocalDate.parse(r[2]), Long.parseLong(r[3]), Long.parseLong(r[4]), Long.parseLong(r[5]), Long.parseLong(r[6])));
        }
        return out;
    }

    private static java.util.List<Scpnlf> readScpnlf(String path) throws java.io.IOException {
        java.util.List<Scpnlf> out = new java.util.ArrayList<Scpnlf>();
        for (String[] r : readCsv(path)) {
            out.add(new Scpnlf(r[0], r[1], java.time.LocalDate.parse(r[2]), Long.parseLong(r[3]), Long.parseLong(r[4]), Long.parseLong(r[5]), java.time.LocalDateTime.parse(r[6])));
        }
        return out;
    }

    private static java.util.List<Schldf> readSchldf(String path) throws java.io.IOException {
        java.util.List<Schldf> out = new java.util.ArrayList<Schldf>();
        for (String[] r : readCsv(path)) {
            out.add(new Schldf(r[0], r[1], java.time.LocalDate.parse(r[2]), Long.parseLong(r[3]), Long.parseLong(r[4]), Long.parseLong(r[5])));
        }
        return out;
    }

    private static java.util.List<Scexpf> readScexpf(String path) throws java.io.IOException {
        java.util.List<Scexpf> out = new java.util.ArrayList<Scexpf>();
        for (String[] r : readCsv(path)) {
            out.add(new Scexpf(r[0], java.time.LocalDate.parse(r[1]), Long.parseLong(r[2]), Long.parseLong(r[3]), Long.parseLong(r[4]), Integer.parseInt(r[5])));
        }
        return out;
    }

    private static java.util.List<Scposf> sampleScposf() {
        java.util.List<Scposf> v = new java.util.ArrayList<Scposf>();
        v.add(new Scposf("C000001", "1301", 12000L, 315000L, 1840000L));
        v.add(new Scposf("C000001", "7203", -8000L, 327500L, -920000L));
        v.add(new Scposf("C000002", "4565", 3000L, 421000L, 0L));
        v.add(new Scposf("C000002", "INT001", 100000L, 10000L, 0L));
        v.add(new Scposf("C000003", "9984", 7000L, 982000L, 1220000L));
        return v;
    }

    private static java.util.List<Scm2mf> sampleScm2mf(java.time.LocalDate d) {
        java.util.List<Scm2mf> v = new java.util.ArrayList<Scm2mf>();
        v.add(new Scm2mf("C000001", "1301", d, 12000L, 318000L, 381600000L, 3600000L));
        v.add(new Scm2mf("C000001", "7203", d, -8000L, 330000L, -264000000L, -2000000L));
        v.add(new Scm2mf("C000002", "4565", d, 3000L, 418500L, 125550000L, -750000L));
        v.add(new Scm2mf("C000002", "INT001", d, 100000L, 10000L, 1000000000L, 0L));
        v.add(new Scm2mf("C000003", "9984", d, 7000L, 991000L, 693700000L, 6300000L));
        return v;
    }

    private static java.util.List<Scpnlf> sampleScpnlf(java.time.LocalDate d) {
        java.util.List<Scpnlf> v = new java.util.ArrayList<Scpnlf>();
        v.add(new Scpnlf("C000001", "1301", d, 1840000L, 3600000L, 28000L, d.atTime(15, 4, 0)));
        v.add(new Scpnlf("C000001", "7203", d, -920000L, -2000000L, 31000L, d.atTime(15, 4, 1)));
        v.add(new Scpnlf("C000002", "4565", d, 0L, -750000L, 12000L, d.atTime(15, 4, 2)));
        v.add(new Scpnlf("C000003", "9984", d, 1220000L, 6300000L, 45000L, d.minusDays(1).atTime(23, 55, 0)));
        return v;
    }

    private static java.util.List<Schldf> sampleSchldf(java.time.LocalDate d) {
        java.util.List<Schldf> v = new java.util.ArrayList<Schldf>();
        v.add(new Schldf("C000001", "1301", d, 10000L, 2000L, 1000L));
        v.add(new Schldf("C000001", "7203", d, -7000L, -1000L, 0L));
        v.add(new Schldf("C000002", "4565", d, 2000L, 1000L, 0L));
        v.add(new Schldf("C000003", "9984", d, 7000L, 0L, 500L));
        return v;
    }

    private static java.util.List<Scexpf> sampleScexpf(java.time.LocalDate d) {
        java.util.List<Scexpf> v = new java.util.ArrayList<Scexpf>();
        v.add(new Scexpf("C000001", d, 381600000L, 264000000L, 117600000L, 7350));
        v.add(new Scexpf("C000002", d, 125550000L, 0L, 125550000L, 2511));
        v.add(new Scexpf("C000003", d, 693700000L, 0L, 693700000L, 9800));
        return v;
    }

    private static final class Scposf {
        final String cifNo;
        final String instrCode;
        final long netQty;
        final long avgAmt;
        final long rlzdAmt;

        Scposf(String cifNo, String instrCode, long netQty, long avgAmt, long rlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.rlzdAmt = rlzdAmt;
        }
    }

    private static final class Scm2mf {
        final String cifNo;
        final String instrCode;
        final java.time.LocalDate sessDt;
        final long netQty;
        final long markAmt;
        final long markNotionalAmt;
        final long unrlzdAmt;

        Scm2mf(String cifNo, String instrCode, java.time.LocalDate sessDt, long netQty, long markAmt, long markNotionalAmt, long unrlzdAmt) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sessDt = sessDt;
            this.netQty = netQty;
            this.markAmt = markAmt;
            this.markNotionalAmt = markNotionalAmt;
            this.unrlzdAmt = unrlzdAmt;
        }
    }

    private static final class Scpnlf {
        final String cifNo;
        final String instrCode;
        final java.time.LocalDate sessDt;
        final long rlzdAmt;
        final long unrlzdAmt;
        final long feeAmt;
        final java.time.LocalDateTime calcTs;

        Scpnlf(String cifNo, String instrCode, java.time.LocalDate sessDt, long rlzdAmt, long unrlzdAmt, long feeAmt, java.time.LocalDateTime calcTs) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sessDt = sessDt;
            this.rlzdAmt = rlzdAmt;
            this.unrlzdAmt = unrlzdAmt;
            this.feeAmt = feeAmt;
            this.calcTs = calcTs;
        }
    }

    private static final class Schldf {
        final String cifNo;
        final String instrCode;
        final java.time.LocalDate asofDt;
        final long settledQty;
        final long tradeQty;
        final long restrictedQty;

        Schldf(String cifNo, String instrCode, java.time.LocalDate asofDt, long settledQty, long tradeQty, long restrictedQty) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.asofDt = asofDt;
            this.settledQty = settledQty;
            this.tradeQty = tradeQty;
            this.restrictedQty = restrictedQty;
        }
    }

    private static final class Scexpf {
        final String cifNo;
        final java.time.LocalDate sessDt;
        final long grossLongAmt;
        final long grossShortAmt;
        final long netExposureAmt;
        final int limitUtilPct;

        Scexpf(String cifNo, java.time.LocalDate sessDt, long grossLongAmt, long grossShortAmt, long netExposureAmt, int limitUtilPct) {
            this.cifNo = cifNo;
            this.sessDt = sessDt;
            this.grossLongAmt = grossLongAmt;
            this.grossShortAmt = grossShortAmt;
            this.netExposureAmt = netExposureAmt;
            this.limitUtilPct = limitUtilPct;
        }
    }

    private static final class ReportLine {
        final String cifNo;
        final String instrCode;
        final java.time.LocalDate sessDt;
        final long netQty;
        final long avgAmt;
        final long markAmt;
        final long markNotionalAmt;
        final long rlzdAmt;
        final long unrlzdAmt;
        final long availableQty;
        final long settledQty;
        final long tradeQty;
        final long restrictedQty;
        final long grossLongAmt;
        final long grossShortAmt;
        final long netExposureAmt;
        final int limitUtilPct;
        final long marginReqAmt;
        final int decisionCd;

        ReportLine(String cifNo, String instrCode, java.time.LocalDate sessDt, long netQty, long avgAmt, long markAmt, long markNotionalAmt, long rlzdAmt, long unrlzdAmt, long availableQty, long settledQty, long tradeQty, long restrictedQty, long grossLongAmt, long grossShortAmt, long netExposureAmt, int limitUtilPct, long marginReqAmt, int decisionCd) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.sessDt = sessDt;
            this.netQty = netQty;
            this.avgAmt = avgAmt;
            this.markAmt = markAmt;
            this.markNotionalAmt = markNotionalAmt;
            this.rlzdAmt = rlzdAmt;
            this.unrlzdAmt = unrlzdAmt;
            this.availableQty = availableQty;
            this.settledQty = settledQty;
            this.tradeQty = tradeQty;
            this.restrictedQty = restrictedQty;
            this.grossLongAmt = grossLongAmt;
            this.grossShortAmt = grossShortAmt;
            this.netExposureAmt = netExposureAmt;
            this.limitUtilPct = limitUtilPct;
            this.marginReqAmt = marginReqAmt;
            this.decisionCd = decisionCd;
        }
    }
}
