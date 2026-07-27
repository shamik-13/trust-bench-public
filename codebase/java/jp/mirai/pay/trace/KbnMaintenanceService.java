package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2025/03/05  みらいペイ システム部 精算・連携チーム  初版作成
 */
public class KbnMaintenanceService {
    private static final java.time.format.DateTimeFormatter DATE_FORMAT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    private final java.util.Map<String, java.util.List<KbnDefinition>> pckbnf =
            new java.util.LinkedHashMap<>();

    public void run() {
        loadSyntheticPckbnf();

        java.util.List<KbnChangeRequest> requests = java.util.Arrays.asList(
                new KbnChangeRequest("10", "加盟店通常精算", true, "0.0185", "20260401", "20270331"),
                new KbnChangeRequest("20", "翌日精算特約", true, "0.0240", "20260701", "20270630"),
                new KbnChangeRequest("30", "月次相殺対象外", false, "0.0100", "20260401", "20270331"),
                new KbnChangeRequest("20", "翌日精算特約改定", true, "0.0230", "20260601", "20261231")
        );

        int ok = 0;
        int ng = 0;
        for (KbnChangeRequest request : requests) {
            MaintenanceResult result = apply(request);
            if (result.accepted) {
                ok++;
            } else {
                ng++;
            }
            System.out.println(result.message);
        }

        System.out.println("処理件数=" + requests.size() + " 正常=" + ok + " 否認=" + ng);
    }

    public MaintenanceResult apply(KbnChangeRequest request) {
        java.util.List<String> errors = validate(request);
        if (!errors.isEmpty()) {
            return MaintenanceResult.rejected(
                    request.settleKbn,
                    "入力検証エラー " + String.join("、", errors));
        }

        KbnDefinition candidate = request.toDefinition();
        ImpactScope impact = confirmImpact(candidate);
        if (impact.blockingDetailCount > 0) {
            return MaintenanceResult.rejected(
                    candidate.settleKbn,
                    "既存明細影響あり 件数=" + impact.blockingDetailCount
                            + " 金額=" + impact.blockingAmount
                            + " 期間=" + format(candidate.validFrom) + "-" + format(candidate.validTo));
        }

        if (hasOverlap(candidate)) {
            return MaintenanceResult.rejected(
                    candidate.settleKbn,
                    "有効期間重複のためPCKBNF未更新 期間="
                            + format(candidate.validFrom) + "-" + format(candidate.validTo));
        }

        java.util.List<KbnDefinition> rows = pckbnf.computeIfAbsent(
                candidate.settleKbn,
                k -> new java.util.ArrayList<>());
        rows.add(candidate);
        rows.sort(java.util.Comparator.comparing(k -> k.validFrom));

        return MaintenanceResult.accepted(
                candidate.settleKbn,
                "PCKBNF更新完了 名称=" + candidate.kbnName
                        + " 料率=" + candidate.feeRate
                        + " ネット対象=" + (candidate.nettableFlag ? "1" : "0"));
    }

    private java.util.List<String> validate(KbnChangeRequest request) {
        java.util.List<String> errors = new java.util.ArrayList<>();

        if (request == null) {
            errors.add("要求なし");
            return errors;
        }
        if (request.settleKbn == null || !request.settleKbn.matches("[0-9A-Z]{2}")) {
            errors.add("精算区分不正");
        }
        if (request.kbnName == null || request.kbnName.trim().isEmpty()
                || request.kbnName.length() > 40) {
            errors.add("区分名称不正");
        }

        java.math.BigDecimal rate = parseRate(request.feeRateText, errors);
        if (rate != null
                && (rate.compareTo(java.math.BigDecimal.ZERO) < 0
                || rate.compareTo(new java.math.BigDecimal("0.1500")) > 0)) {
            errors.add("料率範囲外");
        }

        java.time.LocalDate from = parseDate(request.validFromText, "開始日", errors);
        java.time.LocalDate to = parseDate(request.validToText, "終了日", errors);
        if (from != null && to != null && from.isAfter(to)) {
            errors.add("有効期間逆転");
        }

        return errors;
    }

    private java.math.BigDecimal parseRate(String text, java.util.List<String> errors) {
        if (text == null || text.trim().isEmpty()) {
            errors.add("料率未設定");
            return null;
        }
        try {
            java.math.BigDecimal rate = new java.math.BigDecimal(text).setScale(4);
            if (rate.precision() - rate.scale() > 1) {
                errors.add("料率桁数不正");
            }
            return rate;
        } catch (ArithmeticException | NumberFormatException e) {
            errors.add("料率形式不正");
            return null;
        }
    }

    private java.time.LocalDate parseDate(String text, String name, java.util.List<String> errors) {
        if (text == null || !text.matches("[0-9]{8}")) {
            errors.add(name + "形式不正");
            return null;
        }
        try {
            return java.time.LocalDate.parse(text, DATE_FORMAT);
        } catch (java.time.DateTimeException e) {
            errors.add(name + "暦日不正");
            return null;
        }
    }

    private ImpactScope confirmImpact(KbnDefinition candidate) {
        java.util.List<PostedDetail> details = syntheticPostedDetails();
        int count = 0;
        java.math.BigDecimal amount = java.math.BigDecimal.ZERO;

        for (PostedDetail detail : details) {
            if (!detail.settleKbn.equals(candidate.settleKbn)) {
                continue;
            }
            if (detail.businessDate.isBefore(candidate.validFrom)
                    || detail.businessDate.isAfter(candidate.validTo)) {
                continue;
            }
            if (detail.closed || detail.nettingFixed) {
                count++;
                amount = amount.add(detail.amount);
            }
        }

        return new ImpactScope(count, amount);
    }

    private boolean hasOverlap(KbnDefinition candidate) {
        java.util.List<KbnDefinition> rows = pckbnf.get(candidate.settleKbn);
        if (rows == null) {
            return false;
        }
        for (KbnDefinition row : rows) {
            boolean separated = candidate.validTo.isBefore(row.validFrom)
                    || candidate.validFrom.isAfter(row.validTo);
            if (!separated) {
                return true;
            }
        }
        return false;
    }

    private void loadSyntheticPckbnf() {
        addExisting("10", "加盟店通常精算", true, "0.0180", "20250401", "20260331");
        addExisting("20", "翌日精算特約", true, "0.0250", "20250401", "20260630");
        addExisting("30", "月次相殺対象外", false, "0.0100", "20250401", "20260331");
    }

    private void addExisting(
            String settleKbn,
            String kbnName,
            boolean nettableFlag,
            String feeRate,
            String validFrom,
            String validTo) {
        KbnDefinition row = new KbnDefinition(
                settleKbn,
                kbnName,
                nettableFlag,
                new java.math.BigDecimal(feeRate).setScale(4),
                java.time.LocalDate.parse(validFrom, DATE_FORMAT),
                java.time.LocalDate.parse(validTo, DATE_FORMAT));
        pckbnf.computeIfAbsent(settleKbn, k -> new java.util.ArrayList<>()).add(row);
    }

    private java.util.List<PostedDetail> syntheticPostedDetails() {
        return java.util.Arrays.asList(
                new PostedDetail("10", java.time.LocalDate.of(2026, 4, 3),
                        new java.math.BigDecimal("1284000"), false, false),
                new PostedDetail("20", java.time.LocalDate.of(2026, 6, 10),
                        new java.math.BigDecimal("764000"), true, true),
                new PostedDetail("20", java.time.LocalDate.of(2026, 7, 8),
                        new java.math.BigDecimal("512000"), false, false),
                new PostedDetail("30", java.time.LocalDate.of(2026, 5, 20),
                        new java.math.BigDecimal("99000"), false, true));
    }

    private static String format(java.time.LocalDate date) {
        return DATE_FORMAT.format(date);
    }

    public static final class KbnChangeRequest {
        public final String settleKbn;
        public final String kbnName;
        public final boolean nettableFlag;
        public final String feeRateText;
        public final String validFromText;
        public final String validToText;

        public KbnChangeRequest(
                String settleKbn,
                String kbnName,
                boolean nettableFlag,
                String feeRateText,
                String validFromText,
                String validToText) {
            this.settleKbn = settleKbn;
            this.kbnName = kbnName;
            this.nettableFlag = nettableFlag;
            this.feeRateText = feeRateText;
            this.validFromText = validFromText;
            this.validToText = validToText;
        }

        private KbnDefinition toDefinition() {
            return new KbnDefinition(
                    settleKbn,
                    kbnName.trim(),
                    nettableFlag,
                    new java.math.BigDecimal(feeRateText).setScale(4),
                    java.time.LocalDate.parse(validFromText, DATE_FORMAT),
                    java.time.LocalDate.parse(validToText, DATE_FORMAT));
        }
    }

    public static final class MaintenanceResult {
        public final String settleKbn;
        public final boolean accepted;
        public final String message;

        private MaintenanceResult(String settleKbn, boolean accepted, String message) {
            this.settleKbn = settleKbn;
            this.accepted = accepted;
            this.message = message;
        }

        private static MaintenanceResult accepted(String settleKbn, String message) {
            return new MaintenanceResult(settleKbn, true, "正常 区分=" + settleKbn + " " + message);
        }

        private static MaintenanceResult rejected(String settleKbn, String message) {
            return new MaintenanceResult(settleKbn, false, "否認 区分=" + settleKbn + " " + message);
        }
    }

    private static final class KbnDefinition {
        private final String settleKbn;
        private final String kbnName;
        private final boolean nettableFlag;
        private final java.math.BigDecimal feeRate;
        private final java.time.LocalDate validFrom;
        private final java.time.LocalDate validTo;

        private KbnDefinition(
                String settleKbn,
                String kbnName,
                boolean nettableFlag,
                java.math.BigDecimal feeRate,
                java.time.LocalDate validFrom,
                java.time.LocalDate validTo) {
            this.settleKbn = settleKbn;
            this.kbnName = kbnName;
            this.nettableFlag = nettableFlag;
            this.feeRate = feeRate;
            this.validFrom = validFrom;
            this.validTo = validTo;
        }
    }

    private static final class PostedDetail {
        private final String settleKbn;
        private final java.time.LocalDate businessDate;
        private final java.math.BigDecimal amount;
        private final boolean closed;
        private final boolean nettingFixed;

        private PostedDetail(
                String settleKbn,
                java.time.LocalDate businessDate,
                java.math.BigDecimal amount,
                boolean closed,
                boolean nettingFixed) {
            this.settleKbn = settleKbn;
            this.businessDate = businessDate;
            this.amount = amount;
            this.closed = closed;
            this.nettingFixed = nettingFixed;
        }
    }

    private static final class ImpactScope {
        private final int blockingDetailCount;
        private final java.math.BigDecimal blockingAmount;

        private ImpactScope(int blockingDetailCount, java.math.BigDecimal blockingAmount) {
            this.blockingDetailCount = blockingDetailCount;
            this.blockingAmount = blockingAmount;
        }
    }
}
