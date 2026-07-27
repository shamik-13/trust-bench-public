/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2020-03-10  大野 修 (E-225)  初版作成
 */

package jp.mirai.sec.pretrade;

public class RiskEventMonitorService {
    private static final java.math.BigDecimal ZERO = java.math.BigDecimal.ZERO;
    private static final java.math.BigDecimal HUNDRED = new java.math.BigDecimal("100");
    private static final java.math.BigDecimal LIMIT_WARN_RATE = new java.math.BigDecimal("0.80");
    private static final java.math.BigDecimal LIMIT_CRIT_RATE = new java.math.BigDecimal("0.95");
    private static final java.math.BigDecimal REJECT_WARN_RATE = new java.math.BigDecimal("0.20");
    private static final java.math.BigDecimal REJECT_CRIT_RATE = new java.math.BigDecimal("0.35");
    private static final java.math.BigDecimal CONCENT_WARN_RATE = new java.math.BigDecimal("0.55");
    private static final java.math.BigDecimal CONCENT_CRIT_RATE = new java.math.BigDecimal("0.70");
    private static final int MIN_REJECT_DENOMINATOR = 3;

    public static void main(String[] a) {
        MonitorContext context = syntheticBenchmarkContext();
        java.util.List<Scrisk> events = new RiskEventMonitorService().monitor(context);
        for (Scrisk event : events) {
            System.out.println(event.toLine());
        }
    }

    private java.util.List<Scrisk> monitor(MonitorContext context) {
        java.util.Map<String, RefInstrument> refByInstrument = context.refByInstrument;
        java.util.Map<String, Sclmtf> limitByCifTier = indexLimit(context.limits);
        java.util.Map<String, Scexpr> exposureByCifInstr = indexExposure(context.exposures);

        java.util.List<Scrisk> out = new java.util.ArrayList<Scrisk>();
        java.util.Map<String, DecisionStat> statByCifInstr = aggregateDecisionStat(context.decisions, context.rejects);
        java.util.Map<String, java.math.BigDecimal> totalExposureByCif = aggregateTotalExposure(context.exposures);

        java.time.LocalDateTime batchTs = java.time.LocalDateTime.parse("2025-01-15T08:45:00");

        for (java.util.Map.Entry<String, DecisionStat> entry : statByCifInstr.entrySet()) {
            DecisionStat stat = entry.getValue();
            RefInstrument ref = refByInstrument.get(stat.instrCode);
            if (ref == null) {
                continue;
            }

            Sclmtf limit = limitByCifTier.get(limitKey(stat.cifNo, ref.instrTier));
            if (limit == null) {
                continue;
            }

            if (stat.attemptCount >= MIN_REJECT_DENOMINATOR && stat.rejectCount > 0) {
                java.math.BigDecimal rejectRate = ratio(stat.rejectCount, stat.attemptCount);
                if (rejectRate.compareTo(REJECT_WARN_RATE) >= 0) {
                    out.add(new Scrisk(
                            nextEventId(out.size()),
                            stat.lastOrderId,
                            stat.cifNo,
                            stat.instrCode,
                            "RJCT_RATE",
                            severityByRate(rejectRate, REJECT_CRIT_RATE),
                            percent(rejectRate),
                            percent(REJECT_WARN_RATE),
                            maxTs(stat.lastDecisionTs, stat.lastRejectTs, batchTs)));
                }
            }

            Scexpr exposure = exposureByCifInstr.get(cifInstrKey(stat.cifNo, stat.instrCode));
            java.math.BigDecimal observed = max(stat.maxLimitUsedAmt, exposure == null ? ZERO : exposure.netNotionalAmt);
            if (limit.maxNotionalAmt.signum() > 0) {
                java.math.BigDecimal usedRate = observed.divide(limit.maxNotionalAmt, 6, java.math.RoundingMode.HALF_UP);
                if (usedRate.compareTo(LIMIT_WARN_RATE) >= 0) {
                    out.add(new Scrisk(
                            nextEventId(out.size()),
                            stat.lastOrderId,
                            stat.cifNo,
                            stat.instrCode,
                            "LIMIT_NEAR",
                            severityByRate(usedRate, LIMIT_CRIT_RATE),
                            observed,
                            limit.maxNotionalAmt,
                            maxTs(stat.lastDecisionTs, exposure == null ? null : exposure.updatedTs, batchTs)));
                }
            }
        }

        for (Scexpr exposure : context.exposures) {
            java.math.BigDecimal total = totalExposureByCif.get(exposure.cifNo);
            if (total == null || total.signum() <= 0 || exposure.netNotionalAmt.signum() <= 0) {
                continue;
            }
            java.math.BigDecimal concentrationRate = exposure.netNotionalAmt.divide(total, 6, java.math.RoundingMode.HALF_UP);
            if (concentrationRate.compareTo(CONCENT_WARN_RATE) >= 0) {
                out.add(new Scrisk(
                        nextEventId(out.size()),
                        "",
                        exposure.cifNo,
                        exposure.instrCode,
                        "CONCENT",
                        severityByRate(concentrationRate, CONCENT_CRIT_RATE),
                        percent(concentrationRate),
                        percent(CONCENT_WARN_RATE),
                        exposure.updatedTs));
            }
        }

        out.sort(new java.util.Comparator<Scrisk>() {
            public int compare(Scrisk l, Scrisk r) {
                int ts = l.eventTs.compareTo(r.eventTs);
                if (ts != 0) {
                    return ts;
                }
                return l.eventId.compareTo(r.eventId);
            }
        });
        return out;
    }

    private static MonitorContext syntheticBenchmarkContext() {
        java.util.Map<String, RefInstrument> ref = new java.util.HashMap<String, RefInstrument>();
        ref.put("7203", new RefInstrument("7203", "PRIME", "輸送用機器"));
        ref.put("6758", new RefInstrument("6758", "PRIME", "電気機器"));
        ref.put("9984", new RefInstrument("9984", "GROWTH", "情報通信"));
        ref.put("8306", new RefInstrument("8306", "PRIME", "銀行"));

        java.util.List<Hfdec> decisions = new java.util.ArrayList<Hfdec>();
        decisions.add(new Hfdec("D000001", "O100001", "CIF0001", "7203", "ACPT", "", bd("42000000"), bd("162000000"), ts("2025-01-15T08:31:02")));
        decisions.add(new Hfdec("D000002", "O100002", "CIF0001", "7203", "ACPT", "", bd("31000000"), bd("193000000"), ts("2025-01-15T08:31:08")));
        decisions.add(new Hfdec("D000003", "O100003", "CIF0001", "6758", "ACPT", "", bd("18000000"), bd("74000000"), ts("2025-01-15T08:31:10")));
        decisions.add(new Hfdec("D000004", "O100004", "CIF0002", "9984", "RJCT", "LMT", bd("15000000"), bd("65000000"), ts("2025-01-15T08:32:01")));
        decisions.add(new Hfdec("D000005", "O100005", "CIF0002", "9984", "RJCT", "LMT", bd("12000000"), bd("65000000"), ts("2025-01-15T08:32:05")));
        decisions.add(new Hfdec("D000006", "O100006", "CIF0002", "9984", "ACPT", "", bd("9000000"), bd("74000000"), ts("2025-01-15T08:32:11")));
        decisions.add(new Hfdec("D000007", "O100007", "CIF0003", "8306", "ACPT", "", bd("24000000"), bd("87000000"), ts("2025-01-15T08:33:20")));
        decisions.add(new Hfdec("D000008", "O100008", "CIF0003", "8306", "RJCT", "RATE", bd("26000000"), bd("87000000"), ts("2025-01-15T08:33:31")));

        java.util.List<Hfrjct> rejects = new java.util.ArrayList<Hfrjct>();
        rejects.add(new Hfrjct("R000001", "O100004", "CIF0002", "9984", "LMT", "NOTIONAL", ts("2025-01-15T08:32:02")));
        rejects.add(new Hfrjct("R000002", "O100005", "CIF0002", "9984", "LMT", "NOTIONAL", ts("2025-01-15T08:32:06")));
        rejects.add(new Hfrjct("R000003", "O100008", "CIF0003", "8306", "RATE", "COUNT", ts("2025-01-15T08:33:32")));

        java.util.List<Scexpr> exposures = new java.util.ArrayList<Scexpr>();
        exposures.add(new Scexpr("CIF0001", "7203", bd("221000000"), bd("232000000"), bd("11000000"), ts("2025-01-15T08:34:00")));
        exposures.add(new Scexpr("CIF0001", "6758", bd("79000000"), bd("79000000"), bd("0"), ts("2025-01-15T08:34:00")));
        exposures.add(new Scexpr("CIF0002", "9984", bd("76000000"), bd("81000000"), bd("5000000"), ts("2025-01-15T08:34:02")));
        exposures.add(new Scexpr("CIF0003", "8306", bd("87000000"), bd("90000000"), bd("3000000"), ts("2025-01-15T08:34:04")));

        java.util.List<Sclmtf> limits = new java.util.ArrayList<Sclmtf>();
        limits.add(new Sclmtf("CIF0001", "PRIME", bd("240000000"), 100000, 30, ts("2025-01-15T08:00:00")));
        limits.add(new Sclmtf("CIF0002", "GROWTH", bd("80000000"), 20000, 12, ts("2025-01-15T08:00:00")));
        limits.add(new Sclmtf("CIF0003", "PRIME", bd("100000000"), 50000, 18, ts("2025-01-15T08:00:00")));

        return new MonitorContext(decisions, rejects, exposures, limits, ref);
    }

    private static java.util.Map<String, Sclmtf> indexLimit(java.util.List<Sclmtf> limits) {
        java.util.Map<String, Sclmtf> map = new java.util.HashMap<String, Sclmtf>();
        for (Sclmtf limit : limits) {
            map.put(limitKey(limit.cifNo, limit.instrTier), limit);
        }
        return map;
    }

    private static java.util.Map<String, Scexpr> indexExposure(java.util.List<Scexpr> exposures) {
        java.util.Map<String, Scexpr> map = new java.util.HashMap<String, Scexpr>();
        for (Scexpr exposure : exposures) {
            map.put(cifInstrKey(exposure.cifNo, exposure.instrCode), exposure);
        }
        return map;
    }

    private static java.util.Map<String, DecisionStat> aggregateDecisionStat(java.util.List<Hfdec> decisions, java.util.List<Hfrjct> rejects) {
        java.util.Map<String, DecisionStat> map = new java.util.HashMap<String, DecisionStat>();
        for (Hfdec decision : decisions) {
            String key = cifInstrKey(decision.cifNo, decision.instrCode);
            DecisionStat stat = map.get(key);
            if (stat == null) {
                stat = new DecisionStat(decision.cifNo, decision.instrCode);
                map.put(key, stat);
            }
            stat.attemptCount++;
            stat.lastOrderId = decision.orderId;
            stat.lastDecisionTs = maxTs(stat.lastDecisionTs, decision.decisionTs, decision.decisionTs);
            stat.maxLimitUsedAmt = max(stat.maxLimitUsedAmt, decision.limitUsedAmt);
        }
        for (Hfrjct reject : rejects) {
            String key = cifInstrKey(reject.cifNo, reject.instrCode);
            DecisionStat stat = map.get(key);
            if (stat == null) {
                stat = new DecisionStat(reject.cifNo, reject.instrCode);
                map.put(key, stat);
            }
            stat.rejectCount++;
            stat.lastOrderId = reject.orderId;
            stat.lastRejectTs = maxTs(stat.lastRejectTs, reject.rejectTs, reject.rejectTs);
        }
        return map;
    }

    private static java.util.Map<String, java.math.BigDecimal> aggregateTotalExposure(java.util.List<Scexpr> exposures) {
        java.util.Map<String, java.math.BigDecimal> map = new java.util.HashMap<String, java.math.BigDecimal>();
        for (Scexpr exposure : exposures) {
            java.math.BigDecimal current = map.get(exposure.cifNo);
            map.put(exposure.cifNo, (current == null ? ZERO : current).add(exposure.netNotionalAmt.max(ZERO)));
        }
        return map;
    }

    private static String severityByRate(java.math.BigDecimal rate, java.math.BigDecimal criticalRate) {
        return rate.compareTo(criticalRate) >= 0 ? "H" : "M";
    }

    private static java.math.BigDecimal ratio(int numerator, int denominator) {
        if (denominator <= 0) {
            return ZERO;
        }
        return new java.math.BigDecimal(numerator).divide(new java.math.BigDecimal(denominator), 6, java.math.RoundingMode.HALF_UP);
    }

    private static java.math.BigDecimal percent(java.math.BigDecimal rate) {
        return rate.multiply(HUNDRED).setScale(2, java.math.RoundingMode.HALF_UP);
    }

    private static java.math.BigDecimal max(java.math.BigDecimal l, java.math.BigDecimal r) {
        if (l == null) {
            return r == null ? ZERO : r;
        }
        if (r == null) {
            return l;
        }
        return l.compareTo(r) >= 0 ? l : r;
    }

    private static java.time.LocalDateTime maxTs(java.time.LocalDateTime a, java.time.LocalDateTime b, java.time.LocalDateTime fallback) {
        java.time.LocalDateTime result = fallback;
        if (a != null && (result == null || a.compareTo(result) > 0)) {
            result = a;
        }
        if (b != null && (result == null || b.compareTo(result) > 0)) {
            result = b;
        }
        return result;
    }

    private static String nextEventId(int index) {
        return String.format("EV%06d", index + 1);
    }

    private static String cifInstrKey(String cifNo, String instrCode) {
        return cifNo + "|" + instrCode;
    }

    private static String limitKey(String cifNo, String tier) {
        return cifNo + "|" + tier;
    }

    private static java.math.BigDecimal bd(String value) {
        return new java.math.BigDecimal(value);
    }

    private static java.time.LocalDateTime ts(String value) {
        return java.time.LocalDateTime.parse(value);
    }

    private static final class MonitorContext {
        final java.util.List<Hfdec> decisions;
        final java.util.List<Hfrjct> rejects;
        final java.util.List<Scexpr> exposures;
        final java.util.List<Sclmtf> limits;
        final java.util.Map<String, RefInstrument> refByInstrument;

        MonitorContext(java.util.List<Hfdec> decisions, java.util.List<Hfrjct> rejects, java.util.List<Scexpr> exposures,
                java.util.List<Sclmtf> limits, java.util.Map<String, RefInstrument> refByInstrument) {
            this.decisions = decisions;
            this.rejects = rejects;
            this.exposures = exposures;
            this.limits = limits;
            this.refByInstrument = refByInstrument;
        }
    }

    private static final class DecisionStat {
        final String cifNo;
        final String instrCode;
        int attemptCount;
        int rejectCount;
        String lastOrderId = "";
        java.math.BigDecimal maxLimitUsedAmt = ZERO;
        java.time.LocalDateTime lastDecisionTs;
        java.time.LocalDateTime lastRejectTs;

        DecisionStat(String cifNo, String instrCode) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
        }
    }

    private static final class Hfdec {
        final String decisionId;
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String decisionCd;
        final String reasonCd;
        final java.math.BigDecimal notionalAmt;
        final java.math.BigDecimal limitUsedAmt;
        final java.time.LocalDateTime decisionTs;

        Hfdec(String decisionId, String orderId, String cifNo, String instrCode, String decisionCd, String reasonCd,
                java.math.BigDecimal notionalAmt, java.math.BigDecimal limitUsedAmt, java.time.LocalDateTime decisionTs) {
            this.decisionId = decisionId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.decisionCd = decisionCd;
            this.reasonCd = reasonCd;
            this.notionalAmt = notionalAmt;
            this.limitUsedAmt = limitUsedAmt;
            this.decisionTs = decisionTs;
        }
    }

    private static final class Hfrjct {
        final String rejectId;
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String rejectCd;
        final String detailCd;
        final java.time.LocalDateTime rejectTs;

        Hfrjct(String rejectId, String orderId, String cifNo, String instrCode, String rejectCd, String detailCd,
                java.time.LocalDateTime rejectTs) {
            this.rejectId = rejectId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.rejectCd = rejectCd;
            this.detailCd = detailCd;
            this.rejectTs = rejectTs;
        }
    }

    private static final class Scexpr {
        final String cifNo;
        final String instrCode;
        final java.math.BigDecimal netNotionalAmt;
        final java.math.BigDecimal buyOpenAmt;
        final java.math.BigDecimal sellOpenAmt;
        final java.time.LocalDateTime updatedTs;

        Scexpr(String cifNo, String instrCode, java.math.BigDecimal netNotionalAmt, java.math.BigDecimal buyOpenAmt,
                java.math.BigDecimal sellOpenAmt, java.time.LocalDateTime updatedTs) {
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.netNotionalAmt = netNotionalAmt;
            this.buyOpenAmt = buyOpenAmt;
            this.sellOpenAmt = sellOpenAmt;
            this.updatedTs = updatedTs;
        }
    }

    private static final class Sclmtf {
        final String cifNo;
        final String instrTier;
        final java.math.BigDecimal maxNotionalAmt;
        final int maxOrderQty;
        final int maxRateCnt;
        final java.time.LocalDateTime updatedTs;

        Sclmtf(String cifNo, String instrTier, java.math.BigDecimal maxNotionalAmt, int maxOrderQty, int maxRateCnt,
                java.time.LocalDateTime updatedTs) {
            this.cifNo = cifNo;
            this.instrTier = instrTier;
            this.maxNotionalAmt = maxNotionalAmt;
            this.maxOrderQty = maxOrderQty;
            this.maxRateCnt = maxRateCnt;
            this.updatedTs = updatedTs;
        }
    }

    private static final class Scrisk {
        final String eventId;
        final String orderId;
        final String cifNo;
        final String instrCode;
        final String riskCd;
        final String severityKbn;
        final java.math.BigDecimal observedAmt;
        final java.math.BigDecimal thresholdAmt;
        final java.time.LocalDateTime eventTs;

        Scrisk(String eventId, String orderId, String cifNo, String instrCode, String riskCd, String severityKbn,
                java.math.BigDecimal observedAmt, java.math.BigDecimal thresholdAmt, java.time.LocalDateTime eventTs) {
            this.eventId = eventId;
            this.orderId = orderId;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.riskCd = riskCd;
            this.severityKbn = severityKbn;
            this.observedAmt = observedAmt;
            this.thresholdAmt = thresholdAmt;
            this.eventTs = eventTs;
        }

        String toLine() {
            return eventId + "," + orderId + "," + cifNo + "," + instrCode + "," + riskCd + "," + severityKbn + ","
                    + observedAmt.toPlainString() + "," + thresholdAmt.toPlainString() + "," + eventTs;
        }
    }

    private static final class RefInstrument {
        final String instrCode;
        final String instrTier;
        final String sectorKbn;

        RefInstrument(String instrCode, String instrTier, String sectorKbn) {
            this.instrCode = instrCode;
            this.instrTier = instrTier;
            this.sectorKbn = sectorKbn;
        }
    }
}
