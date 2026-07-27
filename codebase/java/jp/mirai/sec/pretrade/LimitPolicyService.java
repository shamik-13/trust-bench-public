package jp.mirai.sec.pretrade;

public class LimitPolicyService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2021/07/15  村上 健司 (E-301)    顧客限度ポリシー算出処理の新規作成
     */
    private static final java.math.BigDecimal MIHFT_MAX_NOTIONAL = new java.math.BigDecimal("500000000");
    private static final java.nio.charset.Charset MS932 = java.nio.charset.Charset.forName("MS932");

    private static final String[] SCLMTF_HEADER = {
            "CIF-NO",
            "INSTR-TIER",
            "MAX-NOTIONAL-AMT",
            "MAX-ORDER-QTY",
            "MAX-RATE-CNT",
            "UPDATED-TS"
    };

    public static void main(String[] a) throws Exception {
        if (a.length != 4) {
            throw new IllegalArgumentException("引数は SCCUST SCLMTF SCINSTF 出力SCLMTF の順で指定してください");
        }

        java.nio.file.Path sccustPath = java.nio.file.Paths.get(a[0]);
        java.nio.file.Path sclmtfPath = java.nio.file.Paths.get(a[1]);
        java.nio.file.Path scinstfPath = java.nio.file.Paths.get(a[2]);
        java.nio.file.Path outputPath = java.nio.file.Paths.get(a[3]);

        java.util.Map<String, Customer> customers = readCustomers(sccustPath);
        java.util.Map<Key, LimitRow> currentLimits = readLimits(sclmtfPath);
        java.util.Map<Integer, TierReference> tierReferences = readTierReferences(scinstfPath);
        java.time.LocalDateTime now = java.time.LocalDateTime.now(java.time.Clock.systemDefaultZone());

        java.util.List<LimitRow> output = new java.util.ArrayList<>();
        for (Customer customer : customers.values()) {
            for (int tier = 1; tier <= 3; tier++) {
                TierRule rule = TierRule.of(tier);
                TierReference reference = tierReferences.get(tier);
                if (reference == null) {
                    throw new IllegalStateException("銘柄階層の参照データが不足しています: INSTR-TIER=" + tier);
                }

                Key key = new Key(customer.cifNo, tier);
                LimitRow old = currentLimits.get(key);
                output.add(calculateLimit(customer, old, rule, reference, now));
            }
        }

        writeLimits(outputPath, output);
        System.out.println("SCLMTF反映件数=" + output.size());
    }

    private static LimitRow calculateLimit(
            Customer customer,
            LimitRow old,
            TierRule rule,
            TierReference reference,
            java.time.LocalDateTime now) {
        java.math.BigDecimal groupLimit = nonNegative(customer.groupLimit);
        java.math.BigDecimal groupUsed = nonNegative(customer.groupUsedAmount);
        java.math.BigDecimal accountUsed = nonNegative(customer.accountUsedAmount);
        java.math.BigDecimal groupRemaining = nonNegative(groupLimit.subtract(groupUsed));
        java.math.BigDecimal accountHeadroom = nonNegative(groupLimit.subtract(accountUsed));
        java.math.BigDecimal usableMargin = groupRemaining.min(accountHeadroom);

        java.math.BigDecimal pressure = ratio(groupUsed, groupLimit);
        java.math.BigDecimal shrink = shrinkFactor(pressure);
        java.math.BigDecimal marginAfterPressure = usableMargin.multiply(shrink);

        java.math.BigDecimal notional = marginAfterPressure
                .multiply(new java.math.BigDecimal("10000"))
                .divide(new java.math.BigDecimal(rule.marginRateBp), 0, java.math.RoundingMode.DOWN)
                .min(MIHFT_MAX_NOTIONAL);

        notional = roundDown(notional, new java.math.BigDecimal(rule.tickAmount));
        notional = roundDown(notional, reference.tickAmount);

        long quantity = notional
                .divide(reference.tickAmount, 0, java.math.RoundingMode.DOWN)
                .longValue();

        quantity = (quantity / reference.lotQuantity) * reference.lotQuantity;

        int rateCount = calculateRateCount(tierWeight(rule.tier), pressure, quantity);
        if (quantity == 0L || notional.signum() == 0) {
            rateCount = 0;
        }

        if (old != null) {
            notional = notional.min(old.maxNotionalAmount.max(java.math.BigDecimal.ZERO));
            quantity = Math.min(quantity, Math.max(0L, old.maxOrderQuantity));
            rateCount = Math.min(rateCount, Math.max(0, old.maxRateCount));
        }

        return new LimitRow(
                customer.cifNo,
                rule.tier,
                notional.setScale(0, java.math.RoundingMode.DOWN),
                quantity,
                rateCount,
                now.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
        );
    }

    private static int calculateRateCount(int tierWeight, java.math.BigDecimal pressure, long quantity) {
        int base = Math.max(1, 120 / tierWeight);
        if (pressure.compareTo(new java.math.BigDecimal("0.9500")) >= 0) {
            base /= 4;
        } else if (pressure.compareTo(new java.math.BigDecimal("0.9000")) >= 0) {
            base /= 2;
        } else if (pressure.compareTo(new java.math.BigDecimal("0.8000")) >= 0) {
            base = (base * 3) / 4;
        }
        if (quantity < 1000L) {
            base = Math.min(base, 5);
        }
        return Math.max(0, base);
    }

    private static int tierWeight(int tier) {
        return tier == 1 ? 1 : tier == 2 ? 2 : 4;
    }

    private static java.math.BigDecimal shrinkFactor(java.math.BigDecimal pressure) {
        if (pressure.compareTo(new java.math.BigDecimal("0.9800")) >= 0) {
            return new java.math.BigDecimal("0.1000");
        }
        if (pressure.compareTo(new java.math.BigDecimal("0.9500")) >= 0) {
            return new java.math.BigDecimal("0.2500");
        }
        if (pressure.compareTo(new java.math.BigDecimal("0.9000")) >= 0) {
            return new java.math.BigDecimal("0.5000");
        }
        if (pressure.compareTo(new java.math.BigDecimal("0.8000")) >= 0) {
            return new java.math.BigDecimal("0.7500");
        }
        return java.math.BigDecimal.ONE;
    }

    private static java.math.BigDecimal ratio(java.math.BigDecimal numerator, java.math.BigDecimal denominator) {
        if (denominator.signum() <= 0) {
            return java.math.BigDecimal.ONE;
        }
        return numerator.divide(denominator, 8, java.math.RoundingMode.HALF_UP);
    }

    private static java.math.BigDecimal nonNegative(java.math.BigDecimal value) {
        return value.signum() < 0 ? java.math.BigDecimal.ZERO : value;
    }

    private static java.math.BigDecimal roundDown(java.math.BigDecimal value, java.math.BigDecimal unit) {
        if (unit.signum() <= 0) {
            throw new IllegalArgumentException("丸め単位が不正です: " + unit);
        }
        return value.divide(unit, 0, java.math.RoundingMode.DOWN).multiply(unit);
    }

    private static java.util.Map<String, Customer> readCustomers(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<java.util.Map<String, String>> rows = readCsv(path);
        java.util.Map<String, Customer> result = new java.util.TreeMap<>();
        for (java.util.Map<String, String> row : rows) {
            String cifNo = required(row, "CIF-NO");
            Customer customer = new Customer(
                    cifNo,
                    amount(row, "GROUP-LIMIT"),
                    amount(row, "GROUP-USED-AMT"),
                    amount(row, "ACCT-USED-AMT")
            );
            if (customer.groupLimit.signum() < 0 || customer.groupUsedAmount.signum() < 0 || customer.accountUsedAmount.signum() < 0) {
                throw new IllegalArgumentException("SCCUST金額が不正です: CIF-NO=" + cifNo);
            }
            result.put(cifNo, customer);
        }
        return result;
    }

    private static java.util.Map<Key, LimitRow> readLimits(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<java.util.Map<String, String>> rows = readCsv(path);
        java.util.Map<Key, LimitRow> result = new java.util.HashMap<>();
        for (java.util.Map<String, String> row : rows) {
            String cifNo = required(row, "CIF-NO");
            int tier = integer(row, "INSTR-TIER");
            TierRule.of(tier);
            LimitRow limit = new LimitRow(
                    cifNo,
                    tier,
                    amount(row, "MAX-NOTIONAL-AMT"),
                    longValue(row, "MAX-ORDER-QTY"),
                    integer(row, "MAX-RATE-CNT"),
                    optional(row, "UPDATED-TS")
            );
            result.put(new Key(cifNo, tier), limit);
        }
        return result;
    }

    private static java.util.Map<Integer, TierReference> readTierReferences(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<java.util.Map<String, String>> rows = readCsv(path);
        java.util.Map<Integer, TierReference> result = new java.util.HashMap<>();
        for (java.util.Map<String, String> row : rows) {
            int tier = integer(row, "INSTR-TIER");
            TierRule rule = TierRule.of(tier);
            java.math.BigDecimal tickAmount = amount(row, "TICK-AMT");
            long lotQuantity = longValue(row, "LOT-QTY");
            String boardCode = required(row, "BOARD-CODE");

            if (!"T1".equals(boardCode) && !"ST".equals(boardCode) && !"ETF".equals(boardCode)) {
                throw new IllegalArgumentException("BOARD-CODEが不正です: " + boardCode);
            }
            if (tickAmount.compareTo(new java.math.BigDecimal(rule.tickAmount)) != 0) {
                throw new IllegalArgumentException("TICK-AMTが階層定義と不一致です: INSTR-TIER=" + tier);
            }
            if (lotQuantity <= 0L) {
                throw new IllegalArgumentException("LOT-QTYが不正です: INSTR-TIER=" + tier);
            }

            TierReference existing = result.get(tier);
            if (existing == null) {
                result.put(tier, new TierReference(tier, tickAmount, lotQuantity));
            } else {
                long mergedLot = lcm(existing.lotQuantity, lotQuantity);
                result.put(tier, new TierReference(tier, tickAmount.min(existing.tickAmount), mergedLot));
            }
        }
        return result;
    }

    private static long lcm(long left, long right) {
        return Math.multiplyExact(left / gcd(left, right), right);
    }

    private static long gcd(long left, long right) {
        long a = Math.abs(left);
        long b = Math.abs(right);
        while (b != 0L) {
            long t = a % b;
            a = b;
            b = t;
        }
        return a;
    }

    private static java.util.List<java.util.Map<String, String>> readCsv(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(path, MS932);
        java.util.List<java.util.Map<String, String>> rows = new java.util.ArrayList<>();
        if (lines.isEmpty()) {
            return rows;
        }

        java.util.List<String> header = parseCsvLine(lines.get(0));
        java.util.Map<String, Integer> indexes = new java.util.HashMap<>();
        for (int i = 0; i < header.size(); i++) {
            indexes.put(header.get(i), i);
        }

        for (int i = 1; i < lines.size(); i++) {
            if (lines.get(i).trim().isEmpty()) {
                continue;
            }
            java.util.List<String> fields = parseCsvLine(lines.get(i));
            java.util.Map<String, String> row = new java.util.HashMap<>();
            for (java.util.Map.Entry<String, Integer> entry : indexes.entrySet()) {
                int index = entry.getValue();
                row.put(entry.getKey(), index < fields.size() ? fields.get(index) : "");
            }
            rows.add(row);
        }
        return rows;
    }

    private static void writeLimits(java.nio.file.Path path, java.util.List<LimitRow> rows) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add(String.join(",", SCLMTF_HEADER));
        rows.sort(java.util.Comparator
                .comparing((LimitRow r) -> r.cifNo)
                .thenComparingInt(r -> r.instrTier));

        for (LimitRow row : rows) {
            lines.add(toCsvLine(new String[] {
                    row.cifNo,
                    String.valueOf(row.instrTier),
                    row.maxNotionalAmount.toPlainString(),
                    String.valueOf(row.maxOrderQuantity),
                    String.valueOf(row.maxRateCount),
                    row.updatedTimestamp
            }));
        }
        java.nio.file.Files.write(path, lines, MS932);
    }

    private static java.util.List<String> parseCsvLine(String line) {
        java.util.List<String> result = new java.util.ArrayList<>();
        StringBuilder field = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quoted) {
                if (c == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        field.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    field.append(c);
                }
            } else {
                if (c == ',') {
                    result.add(field.toString().trim());
                    field.setLength(0);
                } else if (c == '"') {
                    quoted = true;
                } else {
                    field.append(c);
                }
            }
        }
        result.add(field.toString().trim());
        return result;
    }

    private static String toCsvLine(String[] values) {
        java.util.List<String> escaped = new java.util.ArrayList<>();
        for (String value : values) {
            String v = value == null ? "" : value;
            if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
                escaped.add("\"" + v.replace("\"", "\"\"") + "\"");
            } else {
                escaped.add(v);
            }
        }
        return String.join(",", escaped);
    }

    private static String required(java.util.Map<String, String> row, String name) {
        String value = row.get(name);
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException("必須項目が未設定です: " + name);
        }
        return value.trim();
    }

    private static String optional(java.util.Map<String, String> row, String name) {
        String value = row.get(name);
        return value == null ? "" : value.trim();
    }

    private static java.math.BigDecimal amount(java.util.Map<String, String> row, String name) {
        try {
            return new java.math.BigDecimal(required(row, name).replace(",", ""));
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("金額項目が不正です: " + name, ex);
        }
    }

    private static int integer(java.util.Map<String, String> row, String name) {
        try {
            return Integer.parseInt(required(row, name));
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("数値項目が不正です: " + name, ex);
        }
    }

    private static long longValue(java.util.Map<String, String> row, String name) {
        try {
            return Long.parseLong(required(row, name));
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("数値項目が不正です: " + name, ex);
        }
    }

    private static final class Customer {
        private final String cifNo;
        private final java.math.BigDecimal groupLimit;
        private final java.math.BigDecimal groupUsedAmount;
        private final java.math.BigDecimal accountUsedAmount;

        private Customer(String cifNo, java.math.BigDecimal groupLimit, java.math.BigDecimal groupUsedAmount, java.math.BigDecimal accountUsedAmount) {
            this.cifNo = cifNo;
            this.groupLimit = groupLimit;
            this.groupUsedAmount = groupUsedAmount;
            this.accountUsedAmount = accountUsedAmount;
        }
    }

    private static final class LimitRow {
        private final String cifNo;
        private final int instrTier;
        private final java.math.BigDecimal maxNotionalAmount;
        private final long maxOrderQuantity;
        private final int maxRateCount;
        private final String updatedTimestamp;

        private LimitRow(String cifNo, int instrTier, java.math.BigDecimal maxNotionalAmount, long maxOrderQuantity, int maxRateCount, String updatedTimestamp) {
            this.cifNo = cifNo;
            this.instrTier = instrTier;
            this.maxNotionalAmount = maxNotionalAmount;
            this.maxOrderQuantity = maxOrderQuantity;
            this.maxRateCount = maxRateCount;
            this.updatedTimestamp = updatedTimestamp;
        }
    }

    private static final class TierReference {
        private final int tier;
        private final java.math.BigDecimal tickAmount;
        private final long lotQuantity;

        private TierReference(int tier, java.math.BigDecimal tickAmount, long lotQuantity) {
            this.tier = tier;
            this.tickAmount = tickAmount;
            this.lotQuantity = lotQuantity;
        }
    }

    private static final class TierRule {
        private final int tier;
        private final int marginRateBp;
        private final int tickAmount;

        private TierRule(int tier, int marginRateBp, int tickAmount) {
            this.tier = tier;
            this.marginRateBp = marginRateBp;
            this.tickAmount = tickAmount;
        }

        private static TierRule of(int tier) {
            switch (tier) {
                case 1:
                    return new TierRule(1, 1000, 100);
                case 2:
                    return new TierRule(2, 2000, 500);
                case 3:
                    return new TierRule(3, 4000, 1000);
                default:
                    throw new IllegalArgumentException("INSTR-TIERが不正です: " + tier);
            }
        }
    }

    private static final class Key {
        private final String cifNo;
        private final int instrTier;

        private Key(String cifNo, int instrTier) {
            this.cifNo = cifNo;
            this.instrTier = instrTier;
        }

        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof Key)) {
                return false;
            }
            Key key = (Key) other;
            return instrTier == key.instrTier && java.util.Objects.equals(cifNo, key.cifNo);
        }

        public int hashCode() {
            return java.util.Objects.hash(cifNo, instrTier);
        }
    }
}
