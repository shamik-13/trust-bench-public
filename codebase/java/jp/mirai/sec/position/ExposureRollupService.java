package jp.mirai.sec.position;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2025-01-21  藤田 和也 (E-271)  初版作成
 */
public class ExposureRollupService {
    private static final BigDecimal ZERO = BigDecimal.ZERO.setScale(0, RoundingMode.HALF_UP);
    private static final BigDecimal ONE_HUNDRED = new BigDecimal("100.00");
    private static final BigDecimal MIHFT_MAX_NOTIONAL = new BigDecimal("500000000");
    private static final DateTimeFormatter TS_FMT = DateTimeFormatter.ofPattern("yyyyMMddHHmmss", Locale.JAPAN);

    private static final String DECISION_ACCEPT = "0";
    private static final String DECISION_REJECT_MARGIN = "4";
    private static final String DECISION_REJECT_NOTIONAL = "8";
    private static final String DECISION_REJECT_TICK = "12";

    public static void main(String[] a) {
        LocalDate sessDt = a.length == 0 ? LocalDate.of(2026, 6, 27) : LocalDate.parse(a[0]);
        ExposureRollupService service = new ExposureRollupService();
        RollupResult result = service.run(sessDt, seedScExpf(sessDt), seedScm2mf(sessDt), seedSccust());
        for (Map<String, String> row : result.scExpfWritten) {
            System.out.println("SCEXPF更新 CIF-NO=" + row.get("CIF-NO")
                    + " SESS-DT=" + row.get("SESS-DT")
                    + " NET-EXPOSURE-AMT=" + row.get("NET-EXPOSURE-AMT")
                    + " LIMIT-UTIL-PCT=" + row.get("LIMIT-UTIL-PCT"));
        }
        for (Map<String, String> row : result.scRiskf2Written) {
            System.out.println("SCRISKF2追加 RISK-EVENT-ID=" + row.get("RISK-EVENT-ID")
                    + " CIF-NO=" + row.get("CIF-NO")
                    + " INSTR-CODE=" + row.get("INSTR-CODE")
                    + " DECISION-KBN=" + row.get("DECISION-KBN"));
        }
    }

    private RollupResult run(LocalDate sessDt,
                             List<Map<String, String>> scExpf,
                             List<Map<String, String>> scm2mf,
                             List<Map<String, String>> sccust) {
        Map<String, CustomerLimit> limits = loadCustomerLimits(sccust);
        Map<String, ExposureAccumulator> calculated = aggregateByCif(sessDt, scm2mf);
        Map<String, Map<String, String>> hotPath = indexByCifAndDate(scExpf);

        List<Map<String, String>> scExpfWritten = new ArrayList<>();
        List<Map<String, String>> scRiskf2Written = new ArrayList<>();

        List<String> cifNos = new ArrayList<>(calculated.keySet());
        Collections.sort(cifNos);

        int eventSeq = 1;
        for (String cifNo : cifNos) {
            ExposureAccumulator acc = calculated.get(cifNo);
            CustomerLimit limit = limits.get(cifNo);
            if (limit == null) {
                continue;
            }

            BigDecimal baseUsed = limit.groupUsedAmt.add(limit.acctUsedAmt);
            BigDecimal usedAmt = baseUsed.add(acc.marginExposure());
            BigDecimal utilPct = pct(usedAmt, limit.groupLimit);
            Map<String, String> updated = scExpfRow(cifNo, sessDt, acc.grossLongAmt, acc.grossShortAmt, acc.netExposureAmt(), utilPct);
            scExpfWritten.add(updated);

            Map<String, String> previous = hotPath.get(cifKey(cifNo, sessDt));
            String decision = decide(limit, usedAmt, previous, updated, acc);
            if (!DECISION_ACCEPT.equals(decision)) {
                scRiskf2Written.add(riskRow(eventSeq++, cifNo, acc.primaryInstrCode(), limit.groupLimit, usedAmt, decision));
            }
        }

        scExpfWritten.sort(Comparator.comparing(r -> r.get("CIF-NO")));
        return new RollupResult(scExpfWritten, scRiskf2Written);
    }

    private Map<String, CustomerLimit> loadCustomerLimits(List<Map<String, String>> sccust) {
        Map<String, CustomerLimit> out = new HashMap<>();
        for (Map<String, String> row : sccust) {
            String cifNo = required(row, "CIF-NO");
            BigDecimal groupLimit = amount(row, "GROUP-LIMIT");
            BigDecimal groupUsedAmt = amount(row, "GROUP-USED-AMT");
            BigDecimal acctUsedAmt = amount(row, "ACCT-USED-AMT");
            if (groupLimit.signum() <= 0 || groupUsedAmt.signum() < 0 || acctUsedAmt.signum() < 0) {
                continue;
            }
            out.put(cifNo, new CustomerLimit(groupLimit, groupUsedAmt, acctUsedAmt));
        }
        return out;
    }

    private Map<String, ExposureAccumulator> aggregateByCif(LocalDate sessDt, List<Map<String, String>> scm2mf) {
        Map<String, ExposureAccumulator> out = new HashMap<>();
        for (Map<String, String> row : scm2mf) {
            if (!sessDt.toString().equals(required(row, "SESS-DT"))) {
                continue;
            }

            String cifNo = required(row, "CIF-NO");
            String instrCode = required(row, "INSTR-CODE");
            BigDecimal netQty = amount(row, "NET-QTY");
            BigDecimal markAmt = amount(row, "MARK-AMT");
            BigDecimal markNotionalAmt = amount(row, "MARK-NOTIONAL-AMT");
            BigDecimal unrlzdAmt = amount(row, "UNRLZD-AMT");

            int tier = instrTier(instrCode);
            BigDecimal margin = markNotionalAmt.multiply(new BigDecimal(rateBp(tier)))
                    .divide(new BigDecimal("10000"), 0, RoundingMode.HALF_UP);

            ExposureAccumulator acc = out.computeIfAbsent(cifNo, k -> new ExposureAccumulator());
            acc.add(instrCode, netQty, markAmt, markNotionalAmt, unrlzdAmt, margin);
        }
        return out;
    }

    private String decide(CustomerLimit limit,
                          BigDecimal usedAmt,
                          Map<String, String> previous,
                          Map<String, String> updated,
                          ExposureAccumulator acc) {
        if (acc.maxInstrumentNotional.compareTo(MIHFT_MAX_NOTIONAL) > 0) {
            return DECISION_REJECT_NOTIONAL;
        }
        if (hasTickViolation(acc)) {
            return DECISION_REJECT_TICK;
        }
        if (usedAmt.compareTo(limit.groupLimit) > 0) {
            return DECISION_REJECT_MARGIN;
        }
        if (previous == null) {
            return DECISION_ACCEPT;
        }

        BigDecimal oldNet = amount(previous, "NET-EXPOSURE-AMT");
        BigDecimal newNet = amount(updated, "NET-EXPOSURE-AMT");
        BigDecimal oldUtil = amount(previous, "LIMIT-UTIL-PCT");
        BigDecimal newUtil = amount(updated, "LIMIT-UTIL-PCT");
        BigDecimal netGap = newNet.subtract(oldNet).abs();
        BigDecimal utilGap = newUtil.subtract(oldUtil).abs();

        if (netGap.compareTo(new BigDecimal("1000000")) > 0 || utilGap.compareTo(new BigDecimal("1.00")) > 0) {
            return DECISION_ACCEPT;
        }
        return DECISION_ACCEPT;
    }

    private boolean hasTickViolation(ExposureAccumulator acc) {
        for (Map.Entry<String, BigDecimal> e : acc.lastPriceByInstr.entrySet()) {
            int tier = instrTier(e.getKey());
            BigDecimal tick = new BigDecimal(tickSize(tier));
            if (e.getValue().remainder(tick).compareTo(BigDecimal.ZERO) != 0) {
                return true;
            }
        }
        return false;
    }

    private static Map<String, Map<String, String>> indexByCifAndDate(List<Map<String, String>> scExpf) {
        Map<String, Map<String, String>> out = new HashMap<>();
        for (Map<String, String> row : scExpf) {
            out.put(cifKey(required(row, "CIF-NO"), LocalDate.parse(required(row, "SESS-DT"))), row);
        }
        return out;
    }

    private static Map<String, String> scExpfRow(String cifNo,
                                                 LocalDate sessDt,
                                                 BigDecimal grossLongAmt,
                                                 BigDecimal grossShortAmt,
                                                 BigDecimal netExposureAmt,
                                                 BigDecimal limitUtilPct) {
        Map<String, String> row = new LinkedHashMap<>();
        row.put("CIF-NO", cifNo);
        row.put("SESS-DT", sessDt.toString());
        row.put("GROSS-LONG-AMT", money(grossLongAmt));
        row.put("GROSS-SHORT-AMT", money(grossShortAmt));
        row.put("NET-EXPOSURE-AMT", money(netExposureAmt));
        row.put("LIMIT-UTIL-PCT", pctText(limitUtilPct));
        return row;
    }

    private static Map<String, String> riskRow(int seq,
                                               String cifNo,
                                               String instrCode,
                                               BigDecimal limitAmt,
                                               BigDecimal usedAmt,
                                               String decisionKbn) {
        Map<String, String> row = new LinkedHashMap<>();
        row.put("RISK-EVENT-ID", "ER" + LocalDateTime.now().format(TS_FMT) + String.format("%04d", seq));
        row.put("CIF-NO", cifNo);
        row.put("INSTR-CODE", instrCode);
        row.put("EVENT-TS", LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
        row.put("LIMIT-AMT", money(limitAmt));
        row.put("USED-AMT", money(usedAmt));
        row.put("DECISION-KBN", decisionKbn);
        return row;
    }

    private static List<Map<String, String>> seedScExpf(LocalDate sessDt) {
        List<Map<String, String>> rows = new ArrayList<>();
        rows.add(row("CIF-NO", "10000001", "SESS-DT", sessDt.toString(), "GROSS-LONG-AMT", "142000000",
                "GROSS-SHORT-AMT", "42000000", "NET-EXPOSURE-AMT", "100000000", "LIMIT-UTIL-PCT", "68.10"));
        rows.add(row("CIF-NO", "10000002", "SESS-DT", sessDt.toString(), "GROSS-LONG-AMT", "860000000",
                "GROSS-SHORT-AMT", "90000000", "NET-EXPOSURE-AMT", "770000000", "LIMIT-UTIL-PCT", "93.20"));
        return rows;
    }

    private static List<Map<String, String>> seedScm2mf(LocalDate sessDt) {
        List<Map<String, String>> rows = new ArrayList<>();
        rows.add(row("CIF-NO", "10000001", "INSTR-CODE", "7203", "SESS-DT", sessDt.toString(), "NET-QTY", "120000",
                "MARK-AMT", "3300", "MARK-NOTIONAL-AMT", "396000000", "UNRLZD-AMT", "18000000"));
        rows.add(row("CIF-NO", "10000001", "INSTR-CODE", "8306", "SESS-DT", sessDt.toString(), "NET-QTY", "-80000",
                "MARK-AMT", "1550", "MARK-NOTIONAL-AMT", "124000000", "UNRLZD-AMT", "-6000000"));
        rows.add(row("CIF-NO", "10000002", "INSTR-CODE", "4478", "SESS-DT", sessDt.toString(), "NET-QTY", "90000",
                "MARK-AMT", "5420", "MARK-NOTIONAL-AMT", "487800000", "UNRLZD-AMT", "25000000"));
        rows.add(row("CIF-NO", "10000002", "INSTR-CODE", "9984", "SESS-DT", sessDt.toString(), "NET-QTY", "40000",
                "MARK-AMT", "12600", "MARK-NOTIONAL-AMT", "504000000", "UNRLZD-AMT", "31000000"));
        return rows;
    }

    private static List<Map<String, String>> seedSccust() {
        List<Map<String, String>> rows = new ArrayList<>();
        rows.add(row("CIF-NO", "10000001", "GROUP-LIMIT", "800000000", "GROUP-USED-AMT", "410000000", "ACCT-USED-AMT", "50000000"));
        rows.add(row("CIF-NO", "10000002", "GROUP-LIMIT", "1200000000", "GROUP-USED-AMT", "780000000", "ACCT-USED-AMT", "90000000"));
        return rows;
    }

    private static Map<String, String> row(String... kv) {
        if (kv.length % 2 != 0) {
            throw new IllegalArgumentException("項目数不正");
        }
        Map<String, String> row = new LinkedHashMap<>();
        for (int i = 0; i < kv.length; i += 2) {
            row.put(kv[i], kv[i + 1]);
        }
        return row;
    }

    private static String required(Map<String, String> row, String key) {
        String v = row.get(key);
        if (v == null || v.trim().isEmpty()) {
            throw new IllegalArgumentException("必須項目なし:" + key);
        }
        return v.trim();
    }

    private static BigDecimal amount(Map<String, String> row, String key) {
        return new BigDecimal(required(row, key)).setScale(0, RoundingMode.HALF_UP);
    }

    private static BigDecimal pct(BigDecimal numerator, BigDecimal denominator) {
        if (denominator.signum() == 0) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        return numerator.multiply(ONE_HUNDRED).divide(denominator, 2, RoundingMode.HALF_UP);
    }

    private static String money(BigDecimal v) {
        return v.setScale(0, RoundingMode.HALF_UP).toPlainString();
    }

    private static String pctText(BigDecimal v) {
        return v.setScale(2, RoundingMode.HALF_UP).toPlainString();
    }

    private static String cifKey(String cifNo, LocalDate sessDt) {
        return cifNo + "|" + sessDt;
    }

    private static int instrTier(String instrCode) {
        Objects.requireNonNull(instrCode, "銘柄コード");
        char c = instrCode.charAt(instrCode.length() - 1);
        int digit = Character.isDigit(c) ? c - '0' : 0;
        if (digit <= 3) {
            return 1;
        }
        if (digit <= 6) {
            return 2;
        }
        return 3;
    }

    private static int rateBp(int tier) {
        switch (tier) {
            case 1:
                return 1000;
            case 2:
                return 2000;
            case 3:
                return 4000;
            default:
                throw new IllegalArgumentException("階層不正:" + tier);
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
                throw new IllegalArgumentException("階層不正:" + tier);
        }
    }

    private static final class ExposureAccumulator {
        private BigDecimal grossLongAmt = ZERO;
        private BigDecimal grossShortAmt = ZERO;
        private BigDecimal totalMargin = ZERO;
        private BigDecimal maxInstrumentNotional = ZERO;
        private String primaryInstrCode = "";
        private final Map<String, BigDecimal> lastPriceByInstr = new HashMap<>();

        private void add(String instrCode,
                         BigDecimal netQty,
                         BigDecimal markAmt,
                         BigDecimal markNotionalAmt,
                         BigDecimal unrlzdAmt,
                         BigDecimal margin) {
            BigDecimal signedExposure = markNotionalAmt.add(unrlzdAmt);
            if (netQty.signum() >= 0) {
                grossLongAmt = grossLongAmt.add(signedExposure.max(BigDecimal.ZERO));
            } else {
                grossShortAmt = grossShortAmt.add(signedExposure.abs());
            }
            totalMargin = totalMargin.add(margin);
            lastPriceByInstr.put(instrCode, markAmt);
            if (markNotionalAmt.compareTo(maxInstrumentNotional) > 0) {
                maxInstrumentNotional = markNotionalAmt;
                primaryInstrCode = instrCode;
            }
        }

        private BigDecimal netExposureAmt() {
            return grossLongAmt.subtract(grossShortAmt);
        }

        private BigDecimal marginExposure() {
            return totalMargin;
        }

        private String primaryInstrCode() {
            return primaryInstrCode.isEmpty() ? "0000" : primaryInstrCode;
        }
    }

    private static final class CustomerLimit {
        private final BigDecimal groupLimit;
        private final BigDecimal groupUsedAmt;
        private final BigDecimal acctUsedAmt;

        private CustomerLimit(BigDecimal groupLimit, BigDecimal groupUsedAmt, BigDecimal acctUsedAmt) {
            this.groupLimit = groupLimit;
            this.groupUsedAmt = groupUsedAmt;
            this.acctUsedAmt = acctUsedAmt;
        }
    }

    private static final class RollupResult {
        private final List<Map<String, String>> scExpfWritten;
        private final List<Map<String, String>> scRiskf2Written;

        private RollupResult(List<Map<String, String>> scExpfWritten, List<Map<String, String>> scRiskf2Written) {
            this.scExpfWritten = scExpfWritten;
            this.scRiskf2Written = scRiskf2Written;
        }
    }
}
