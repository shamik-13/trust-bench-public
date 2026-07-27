/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2021-07-15  小林 直樹 (E-252)  初版作成。寄り付き直後の実績を含む事前判定用のスロットル方針分析を実装。
 */

package jp.mirai.sec.pretrade;

public class ThrottlePolicyService {
    private static final java.time.ZoneId ZONE = java.time.ZoneId.of("Asia/Tokyo");
    private static final java.time.LocalTime OPEN_START = java.time.LocalTime.of(9, 0);
    private static final java.time.LocalTime OPEN_END = java.time.LocalTime.of(9, 15);
    private static final int MIN_RATE = 20;
    private static final int MAX_RATE = 800;

    public static void main(String[] a) {
        java.util.List<HfRateRow> hfRate = sampleHfRate();
        java.util.List<ScRiskRow> scRisk = sampleScRisk();
        java.util.List<ScLmtfRow> currentLimits = sampleScLmtf();

        java.time.ZonedDateTime updatedAt = java.time.ZonedDateTime.of(
                2026, 6, 27, 9, 16, 0, 0, ZONE);

        java.util.List<ScLmtfRow> adjusted = adjustLimits(hfRate, scRisk, currentLimits, updatedAt);
        for (ScLmtfRow row : adjusted) {
            System.out.println(row.toOutputLine());
        }
    }

    private static java.util.List<ScLmtfRow> adjustLimits(
            java.util.List<HfRateRow> hfRate,
            java.util.List<ScRiskRow> scRisk,
            java.util.List<ScLmtfRow> currentLimits,
            java.time.ZonedDateTime updatedAt) {

        java.util.Map<String, BucketStats> rateByCifTier = new java.util.LinkedHashMap<String, BucketStats>();
        for (HfRateRow row : hfRate) {
            if (!isValid(row)) {
                continue;
            }
            String key = row.cifNo + "|" + row.instrTier;
            BucketStats stats = rateByCifTier.get(key);
            if (stats == null) {
                stats = new BucketStats();
                rateByCifTier.put(key, stats);
            }
            stats.accept(row);
        }

        java.util.Map<String, RiskStats> riskByCifTier = new java.util.LinkedHashMap<String, RiskStats>();
        for (ScRiskRow row : scRisk) {
            if (!isValid(row)) {
                continue;
            }
            String key = row.cifNo + "|" + tierOf(row.instrCode);
            RiskStats stats = riskByCifTier.get(key);
            if (stats == null) {
                stats = new RiskStats();
                riskByCifTier.put(key, stats);
            }
            stats.accept(row);
        }

        java.util.List<ScLmtfRow> result = new java.util.ArrayList<ScLmtfRow>();
        for (ScLmtfRow limit : currentLimits) {
            if (!isValid(limit)) {
                continue;
            }

            String key = limit.cifNo + "|" + limit.instrTier;
            BucketStats bucket = rateByCifTier.get(key);
            RiskStats risk = riskByCifTier.get(key);

            int nextRate = decideRate(limit.maxRateCnt, limit.cifNo, bucket, risk);
            result.add(new ScLmtfRow(
                    limit.cifNo,
                    limit.instrTier,
                    limit.maxNotionalAmt,
                    limit.maxOrderQty,
                    nextRate,
                    updatedAt));
        }
        return result;
    }

    private static int decideRate(int currentRate, String cifNo, BucketStats bucket, RiskStats risk) {
        double consumeRatio = bucket == null ? 0.0d : bucket.consumeRatio();
        double dropRatio = bucket == null ? 0.0d : bucket.dropRatio();
        double rejectRatio = risk == null ? 0.0d : risk.rejectRatio();
        int severeCount = risk == null ? 0 : risk.severeCount;

        int next = currentRate;

        if (severeCount >= 2 || rejectRatio >= 0.35d || dropRatio >= 0.18d) {
            next = (int) Math.floor(currentRate * 0.75d);
        } else if (consumeRatio >= 0.92d && dropRatio >= 0.08d) {
            next = (int) Math.floor(currentRate * 0.85d);
        } else if (consumeRatio >= 0.82d && rejectRatio >= 0.15d) {
            next = (int) Math.floor(currentRate * 0.90d);
        } else if (consumeRatio < 0.45d && rejectRatio == 0.0d && dropRatio < 0.02d) {
            next = (int) Math.ceil(currentRate * 1.08d);
        } else if (consumeRatio < 0.60d && rejectRatio < 0.05d) {
            next = (int) Math.ceil(currentRate * 1.03d);
        }

        int tierCap = tierCap(cifNo);
        next = Math.min(next, tierCap);
        next = Math.max(MIN_RATE, next);
        next = Math.min(MAX_RATE, next);
        return roundToFive(next);
    }

    private static int roundToFive(int value) {
        return ((value + 2) / 5) * 5;
    }

    private static int tierCap(String cifNo) {
        if (cifNo.startsWith("PB")) {
            return 600;
        }
        if (cifNo.startsWith("HL")) {
            return 420;
        }
        if (cifNo.startsWith("RT")) {
            return 180;
        }
        return 260;
    }

    private static boolean isValid(HfRateRow row) {
        return row != null
                && row.cifNo != null
                && row.instrTier != null
                && row.windowTs != null
                && row.orderCnt >= 0
                && row.notionalAmt.signum() >= 0
                && row.dropCnt >= 0
                && row.dropCnt <= row.orderCnt;
    }

    private static boolean isValid(ScRiskRow row) {
        return row != null
                && row.eventId != null
                && row.orderId != null
                && row.cifNo != null
                && row.instrCode != null
                && row.riskCd != null
                && row.severityKbn != null
                && row.observedAmt.signum() >= 0
                && row.thresholdAmt.signum() > 0
                && row.eventTs != null;
    }

    private static boolean isValid(ScLmtfRow row) {
        return row != null
                && row.cifNo != null
                && row.instrTier != null
                && row.maxNotionalAmt.signum() > 0
                && row.maxOrderQty > 0
                && row.maxRateCnt > 0
                && row.updatedTs != null;
    }

    private static boolean isOpeningWindow(java.time.ZonedDateTime ts) {
        java.time.LocalTime t = ts.withZoneSameInstant(ZONE).toLocalTime();
        return !t.isBefore(OPEN_START) && t.isBefore(OPEN_END);
    }

    private static String tierOf(String instrCode) {
        if (instrCode.startsWith("13") || instrCode.startsWith("15")) {
            return "ETF";
        }
        if (instrCode.startsWith("9")) {
            return "LOW";
        }
        return "CORE";
    }

    private static java.util.List<HfRateRow> sampleHfRate() {
        java.util.List<HfRateRow> rows = new java.util.ArrayList<HfRateRow>();
        rows.add(new HfRateRow("PB10001|CORE|0900", "PB10001", "CORE", zdt(2026, 6, 27, 9, 0), 520, bd("1842000000"), 47));
        rows.add(new HfRateRow("PB10001|CORE|0905", "PB10001", "CORE", zdt(2026, 6, 27, 9, 5), 498, bd("1769000000"), 52));
        rows.add(new HfRateRow("PB10001|ETF|0900", "PB10001", "ETF", zdt(2026, 6, 27, 9, 0), 210, bd("638000000"), 8));
        rows.add(new HfRateRow("HL20007|CORE|0900", "HL20007", "CORE", zdt(2026, 6, 27, 9, 0), 355, bd("942000000"), 71));
        rows.add(new HfRateRow("HL20007|CORE|0910", "HL20007", "CORE", zdt(2026, 6, 27, 9, 10), 331, bd("887000000"), 66));
        rows.add(new HfRateRow("RT30044|CORE|0900", "RT30044", "CORE", zdt(2026, 6, 27, 9, 0), 128, bd("81000000"), 3));
        rows.add(new HfRateRow("RT30044|LOW|0930", "RT30044", "LOW", zdt(2026, 6, 27, 9, 30), 36, bd("12000000"), 0));
        rows.add(new HfRateRow("CP40012|ETF|0910", "CP40012", "ETF", zdt(2026, 6, 27, 9, 10), 92, bd("252000000"), 1));
        return rows;
    }

    private static java.util.List<ScRiskRow> sampleScRisk() {
        java.util.List<ScRiskRow> rows = new java.util.ArrayList<ScRiskRow>();
        rows.add(new ScRiskRow("EVT000001", "ORD900001", "PB10001", "7203", "NOMINAL_OVER", "2", bd("690000000"), bd("600000000"), zdt(2026, 6, 27, 9, 2)));
        rows.add(new ScRiskRow("EVT000002", "ORD900188", "PB10001", "7203", "PRICE_BAND", "1", bd("405000000"), bd("400000000"), zdt(2026, 6, 27, 9, 7)));
        rows.add(new ScRiskRow("EVT000003", "ORD900220", "HL20007", "9984", "NOMINAL_OVER", "3", bd("820000000"), bd("500000000"), zdt(2026, 6, 27, 9, 4)));
        rows.add(new ScRiskRow("EVT000004", "ORD900231", "HL20007", "9984", "CONCENTRATION", "3", bd("910000000"), bd("500000000"), zdt(2026, 6, 27, 9, 6)));
        rows.add(new ScRiskRow("EVT000005", "ORD900312", "RT30044", "9432", "PRICE_BAND", "1", bd("16000000"), bd("15000000"), zdt(2026, 6, 27, 9, 8)));
        rows.add(new ScRiskRow("EVT000006", "ORD900411", "CP40012", "1321", "ETF_NAV", "1", bd("88000000"), bd("85000000"), zdt(2026, 6, 27, 9, 12)));
        return rows;
    }

    private static java.util.List<ScLmtfRow> sampleScLmtf() {
        java.util.List<ScLmtfRow> rows = new java.util.ArrayList<ScLmtfRow>();
        rows.add(new ScLmtfRow("PB10001", "CORE", bd("2500000000"), 5000, 560, zdt(2026, 6, 26, 15, 10)));
        rows.add(new ScLmtfRow("PB10001", "ETF", bd("1200000000"), 4000, 260, zdt(2026, 6, 26, 15, 10)));
        rows.add(new ScLmtfRow("HL20007", "CORE", bd("1400000000"), 3000, 360, zdt(2026, 6, 26, 15, 10)));
        rows.add(new ScLmtfRow("RT30044", "CORE", bd("180000000"), 900, 140, zdt(2026, 6, 26, 15, 10)));
        rows.add(new ScLmtfRow("RT30044", "LOW", bd("70000000"), 500, 80, zdt(2026, 6, 26, 15, 10)));
        rows.add(new ScLmtfRow("CP40012", "ETF", bd("650000000"), 1800, 150, zdt(2026, 6, 26, 15, 10)));
        return rows;
    }

    private static java.time.ZonedDateTime zdt(int y, int m, int d, int h, int min) {
        return java.time.ZonedDateTime.of(y, m, d, h, min, 0, 0, ZONE);
    }

    private static java.math.BigDecimal bd(String value) {
        return new java.math.BigDecimal(value);
    }

    private static final class BucketStats {
        private int openOrderCnt;
        private int normalOrderCnt;
        private int openDropCnt;
        private int normalDropCnt;
        private java.math.BigDecimal notionalAmt = java.math.BigDecimal.ZERO;

        private void accept(HfRateRow row) {
            if (isOpeningWindow(row.windowTs)) {
                openOrderCnt += row.orderCnt;
                openDropCnt += row.dropCnt;
            } else {
                normalOrderCnt += row.orderCnt;
                normalDropCnt += row.dropCnt;
            }
            notionalAmt = notionalAmt.add(row.notionalAmt);
        }

        private double consumeRatio() {
            int weighted = openOrderCnt * 2 + normalOrderCnt;
            return weighted / 1000.0d;
        }

        private double dropRatio() {
            int orders = openOrderCnt + normalOrderCnt;
            if (orders == 0) {
                return 0.0d;
            }
            return (openDropCnt + normalDropCnt) / (double) orders;
        }
    }

    private static final class RiskStats {
        private int eventCount;
        private int severeCount;
        private int rejectedCount;

        private void accept(ScRiskRow row) {
            eventCount++;
            int severity = Integer.parseInt(row.severityKbn);
            if (severity >= 3) {
                severeCount++;
            }
            if (severity >= 2 || row.observedAmt.compareTo(row.thresholdAmt.multiply(new java.math.BigDecimal("1.10"))) > 0) {
                rejectedCount++;
            }
        }

        private double rejectRatio() {
            if (eventCount == 0) {
                return 0.0d;
            }
            return rejectedCount / (double) eventCount;
        }
    }

    private static final class HfRateRow {
        private final String bucketKey;
        private final String cifNo;
        private final String instrTier;
        private final java.time.ZonedDateTime windowTs;
        private final int orderCnt;
        private final java.math.BigDecimal notionalAmt;
        private final int dropCnt;

        private HfRateRow(String bucketKey, String cifNo, String instrTier, java.time.ZonedDateTime windowTs,
                          int orderCnt, java.math.BigDecimal notionalAmt, int dropCnt) {
            this.bucketKey = bucketKey;
            this.cifNo = cifNo;
            this.instrTier = instrTier;
            this.windowTs = windowTs;
            this.orderCnt = orderCnt;
            this.notionalAmt = notionalAmt;
            this.dropCnt = dropCnt;
        }
    }

    private static final class ScRiskRow {
        private final String eventId;
        private final String orderId;
        private final String cifNo;
        private final String instrCode;
        private final String riskCd;
        private final String severityKbn;
        private final java.math.BigDecimal observedAmt;
        private final java.math.BigDecimal thresholdAmt;
        private final java.time.ZonedDateTime eventTs;

        private ScRiskRow(String eventId, String orderId, String cifNo, String instrCode, String riskCd,
                          String severityKbn, java.math.BigDecimal observedAmt,
                          java.math.BigDecimal thresholdAmt, java.time.ZonedDateTime eventTs) {
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
    }

    private static final class ScLmtfRow {
        private final String cifNo;
        private final String instrTier;
        private final java.math.BigDecimal maxNotionalAmt;
        private final int maxOrderQty;
        private final int maxRateCnt;
        private final java.time.ZonedDateTime updatedTs;

        private ScLmtfRow(String cifNo, String instrTier, java.math.BigDecimal maxNotionalAmt,
                          int maxOrderQty, int maxRateCnt, java.time.ZonedDateTime updatedTs) {
            this.cifNo = cifNo;
            this.instrTier = instrTier;
            this.maxNotionalAmt = maxNotionalAmt;
            this.maxOrderQty = maxOrderQty;
            this.maxRateCnt = maxRateCnt;
            this.updatedTs = updatedTs;
        }

        private String toOutputLine() {
            return "SCLMTF 書込 CIF-NO=" + cifNo
                    + " INSTR-TIER=" + instrTier
                    + " MAX-NOTIONAL-AMT=" + maxNotionalAmt.toPlainString()
                    + " MAX-ORDER-QTY=" + maxOrderQty
                    + " MAX-RATE-CNT=" + maxRateCnt
                    + " UPDATED-TS=" + updatedTs;
        }
    }
}
