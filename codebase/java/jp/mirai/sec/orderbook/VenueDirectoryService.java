/************************************************************
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2020-03-10  福田 亮太 (E-211)    初版作成
 ************************************************************/

package jp.mirai.sec.orderbook;


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
import java.util.Map;

public class VenueDirectoryService {
    private static final DateTimeFormatter TS_FMT = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
    private static final LocalDateTime BATCH_TS = LocalDate.now().atTime(13, 25, 40);

    private VenueDirectoryService() {
    }

    public static void main(String[] a) {
        List<VenueRow> scvenf = new ArrayList<VenueRow>();
        scvenf.add(new VenueRow("TSE", "PRM", 420, new BigDecimal("0.32"), "1", 1800000L));
        scvenf.add(new VenueRow("OSE", "STD", 610, new BigDecimal("0.44"), "1", 900000L));
        scvenf.add(new VenueRow("JNX", "PTS", 310, new BigDecimal("0.28"), "1", 620000L));
        scvenf.add(new VenueRow("CBO", "ETF", 870, new BigDecimal("0.55"), "0", 240000L));
        scvenf.add(new VenueRow("NAG", "NXT", 760, new BigDecimal("0.38"), "1", 130000L));

        List<FeeRow> scfeef = new ArrayList<FeeRow>();
        scfeef.add(new FeeRow("PRM", new BigDecimal("0.0032"), 55));
        scfeef.add(new FeeRow("STD", new BigDecimal("0.0044"), 55));
        scfeef.add(new FeeRow("PTS", new BigDecimal("0.0028"), 35));
        scfeef.add(new FeeRow("ETF", new BigDecimal("0.0055"), 45));

        List<VenueChange> requests = new ArrayList<VenueChange>();
        requests.add(new VenueChange("CBO", "ETF", 830, new BigDecimal("0.55"), "1", 260000L, "運用卓Ａ"));
        requests.add(new VenueChange("JNX", "PTS", 330, new BigDecimal("0.28"), "0", 0L, "運用卓Ｂ"));
        requests.add(new VenueChange("NAG", "NXT", 750, new BigDecimal("0.38"), "0", 0L, "運用卓Ｃ"));
        requests.add(new VenueChange("TSE", "PRM", 390, new BigDecimal("0.32"), "1", 1950000L, "運用卓Ａ"));
        requests.add(new VenueChange("OSE", "STD", 620, new BigDecimal("0.41"), "0", 500000L, "運用卓Ｄ"));

        Map<String, FeeRow> feeByBoard = buildFeeMap(scfeef);
        Map<String, VenueRow> venueByCode = buildVenueMap(scvenf);
        List<AuditRow> scaudf = new ArrayList<AuditRow>();

        for (VenueChange request : requests) {
            ValidationResult result = validateChange(request, feeByBoard);
            if (!result.accepted) {
                System.out.println("却下 " + request.venueCode + " " + result.detailCd);
                continue;
            }

            VenueRow before = venueByCode.get(request.venueCode);
            VenueRow after = new VenueRow(
                    request.venueCode,
                    request.boardCode,
                    request.latencyUs,
                    normalizeBps(request.feeBps),
                    request.enabledKbn,
                    request.capacityQty);

            venueByCode.put(after.venueCode, after);

            if (before != null && !before.enabledKbn.equals(after.enabledKbn)) {
                scaudf.add(new AuditRow(
                        auditId(scaudf.size() + 1),
                        "0",
                        "場状態変更",
                        "0",
                        after.venueCode,
                        BATCH_TS.plusSeconds(scaudf.size()),
                        after.enabledKbn.equals("1") ? "有効化" : "停止"));
            }
        }

        List<VenueRow> updated = new ArrayList<VenueRow>(venueByCode.values());
        Collections.sort(updated, Comparator.comparing(VenueRow::venueCode));

        long enabledCapacity = 0L;
        long weightedLatency = 0L;
        for (VenueRow row : updated) {
            if ("1".equals(row.enabledKbn)) {
                enabledCapacity += row.capacityQty;
                weightedLatency += row.capacityQty * row.latencyUs;
            }
        }

        long avgLatency = enabledCapacity == 0L ? 0L : weightedLatency / enabledCapacity;

        System.out.println("ＳＣＶＥＮＦ出力");
        for (VenueRow row : updated) {
            System.out.println(row.toLine());
        }

        System.out.println("ＳＣＡＵＤＦ出力");
        for (AuditRow row : scaudf) {
            System.out.println(row.toLine());
        }

        System.out.println("集計 有効容量=" + enabledCapacity + " 加重遅延ＵＳ=" + avgLatency);
    }

    private static Map<String, FeeRow> buildFeeMap(List<FeeRow> feeRows) {
        Map<String, FeeRow> feeByBoard = new HashMap<String, FeeRow>();
        for (FeeRow row : feeRows) {
            feeByBoard.put(row.boardCode, row);
        }
        return feeByBoard;
    }

    private static Map<String, VenueRow> buildVenueMap(List<VenueRow> venueRows) {
        Map<String, VenueRow> venueByCode = new LinkedHashMap<String, VenueRow>();
        for (VenueRow row : venueRows) {
            venueByCode.put(row.venueCode, row);
        }
        return venueByCode;
    }

    private static ValidationResult validateChange(VenueChange request, Map<String, FeeRow> feeByBoard) {
        if (request.venueCode == null || request.venueCode.trim().isEmpty()) {
            return ValidationResult.reject("場コード未設定");
        }
        if (!"0".equals(request.enabledKbn) && !"1".equals(request.enabledKbn)) {
            return ValidationResult.reject("有効区分不正");
        }
        if (request.latencyUs <= 0 || request.latencyUs > 5000) {
            return ValidationResult.reject("遅延範囲外");
        }
        if (request.capacityQty < 0) {
            return ValidationResult.reject("容量負値");
        }
        if ("1".equals(request.enabledKbn) && request.capacityQty == 0L) {
            return ValidationResult.reject("有効時容量ゼロ");
        }

        FeeRow fee = feeByBoard.get(request.boardCode);
        if (fee == null) {
            return ValidationResult.reject("板コード未解決");
        }

        BigDecimal expectedBps = fee.feeRate.movePointRight(2).setScale(2, RoundingMode.HALF_UP);
        if (expectedBps.compareTo(normalizeBps(request.feeBps)) != 0) {
            return ValidationResult.reject("手数料不整合");
        }

        if (fee.minFeeAmt < 0) {
            return ValidationResult.reject("最低手数料不正");
        }

        return ValidationResult.accept();
    }

    private static BigDecimal normalizeBps(BigDecimal value) {
        return value.setScale(2, RoundingMode.HALF_UP);
    }

    private static String auditId(int seq) {
        return "監査" + String.format("%08d", seq);
    }

    private static final class VenueRow {
        private final String venueCode;
        private final String boardCode;
        private final int latencyUs;
        private final BigDecimal feeBps;
        private final String enabledKbn;
        private final long capacityQty;

        private VenueRow(String venueCode, String boardCode, int latencyUs, BigDecimal feeBps, String enabledKbn, long capacityQty) {
            this.venueCode = venueCode;
            this.boardCode = boardCode;
            this.latencyUs = latencyUs;
            this.feeBps = feeBps;
            this.enabledKbn = enabledKbn;
            this.capacityQty = capacityQty;
        }

        private String venueCode() {
            return venueCode;
        }

        private String toLine() {
            return venueCode + "," + boardCode + "," + latencyUs + "," + feeBps + "," + enabledKbn + "," + capacityQty;
        }
    }

    private static final class FeeRow {
        private final String boardCode;
        private final BigDecimal feeRate;
        private final long minFeeAmt;

        private FeeRow(String boardCode, BigDecimal feeRate, long minFeeAmt) {
            this.boardCode = boardCode;
            this.feeRate = feeRate;
            this.minFeeAmt = minFeeAmt;
        }
    }

    private static final class AuditRow {
        private final String auditId;
        private final String orderId;
        private final String eventKbn;
        private final String cifNo;
        private final String instrCode;
        private final LocalDateTime eventTs;
        private final String detailCd;

        private AuditRow(String auditId, String orderId, String eventKbn, String cifNo, String instrCode, LocalDateTime eventTs, String detailCd) {
            this.auditId = auditId;
            this.orderId = orderId;
            this.eventKbn = eventKbn;
            this.cifNo = cifNo;
            this.instrCode = instrCode;
            this.eventTs = eventTs;
            this.detailCd = detailCd;
        }

        private String toLine() {
            return auditId + "," + orderId + "," + eventKbn + "," + cifNo + "," + instrCode + "," + eventTs.format(TS_FMT) + "," + detailCd;
        }
    }

    private static final class VenueChange {
        private final String venueCode;
        private final String boardCode;
        private final int latencyUs;
        private final BigDecimal feeBps;
        private final String enabledKbn;
        private final long capacityQty;
        private final String operatorId;

        private VenueChange(String venueCode, String boardCode, int latencyUs, BigDecimal feeBps, String enabledKbn, long capacityQty, String operatorId) {
            this.venueCode = venueCode;
            this.boardCode = boardCode;
            this.latencyUs = latencyUs;
            this.feeBps = feeBps;
            this.enabledKbn = enabledKbn;
            this.capacityQty = capacityQty;
            this.operatorId = operatorId;
        }
    }

    private static final class ValidationResult {
        private final boolean accepted;
        private final String detailCd;

        private ValidationResult(boolean accepted, String detailCd) {
            this.accepted = accepted;
            this.detailCd = detailCd;
        }

        private static ValidationResult accept() {
            return new ValidationResult(true, "承認");
        }

        private static ValidationResult reject(String detailCd) {
            return new ValidationResult(false, detailCd);
        }
    }
}
