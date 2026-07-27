public class AuthHoldStore {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2026-06-28  基盤担当  初版作成。CDAUTHFの承認済み円貨ホールド集計を実装。
     */

    private static final java.time.ZoneId TOKYO = java.time.ZoneId.of("Asia/Tokyo");

    private final java.util.List<HoldRow> rows;

    public AuthHoldStore(java.util.List<String> cdAuthfRows) {
        if (cdAuthfRows == null) {
            throw new IllegalArgumentException("CDAUTHF行一覧が未指定です");
        }

        java.util.List<HoldRow> parsed = new java.util.ArrayList<>();
        int lineNo = 0;
        for (String line : cdAuthfRows) {
            lineNo++;
            if (line == null || line.trim().isEmpty()) {
                continue;
            }
            parsed.add(parse(line, lineNo));
        }
        this.rows = java.util.Collections.unmodifiableList(parsed);
    }

    public long activeHoldTotal(String cardId, java.time.Instant now) {
        if (cardId == null || cardId.trim().isEmpty()) {
            throw new IllegalArgumentException("カードIDが未指定です");
        }
        if (now == null) {
            throw new IllegalArgumentException("基準時刻が未指定です");
        }

        long total = 0L;
        for (HoldRow row : rows) {
            if (row.cardId.equals(cardId)
                    && row.approved
                    && "JPY".equals(row.currency)
                    && now.isBefore(row.expiresAt)) {
                total = Math.addExact(total, row.amount);
            }
        }
        return total;
    }

    public static void main(String[] a) {
        java.util.List<String> data = java.util.Arrays.asList(
                "AH000001,CARD-0001,APPROVED,JPY,125000,2026-06-28T23:59:59+09:00",
                "AH000002,CARD-0001,APPROVED,JPY,43000,2026-06-28T15:30:00+09:00",
                "AH000003,CARD-0001,DECLINED,JPY,900000,2026-06-28T23:59:59+09:00",
                "AH000004,CARD-0002,APPROVED,JPY,78000,2026-06-29T09:00:00+09:00",
                "AH000005,CARD-0002,APPROVED,USD,1200,2026-06-29T09:00:00+09:00",
                "AH000006,CARD-0003,APPROVED,JPY,210000,2026-06-27T23:59:59+09:00",
                "AH000007,CARD-0003,APPROVED,JPY,36000,2026-06-28T18:45:00+09:00",
                "AH000008,CARD-0004,APPROVED,JPY,500000,2026-06-30T00:00:00+09:00"
        );

        AuthHoldStore store = new AuthHoldStore(data);
        java.time.Instant 基準時刻 = java.time.ZonedDateTime
                .of(2026, 6, 28, 14, 0, 0, 0, TOKYO)
                .toInstant();

        String[] cards = {"CARD-0001", "CARD-0002", "CARD-0003", "CARD-0004"};
        for (String card : cards) {
            System.out.println(card + " 有効ホールド合計=" + store.activeHoldTotal(card, 基準時刻));
        }
    }

    private static HoldRow parse(String line, int lineNo) {
        String[] parts = line.split(",", -1);
        if (parts.length != 6) {
            throw new IllegalArgumentException("CDAUTHF形式不正 行=" + lineNo);
        }

        String authId = required(parts[0], "承認ID", lineNo);
        String cardId = required(parts[1], "カードID", lineNo);
        String status = required(parts[2], "状態", lineNo);
        String currency = required(parts[3], "通貨", lineNo);
        long amount = parseAmount(parts[4], lineNo);
        java.time.Instant expiresAt = parseExpiry(parts[5], lineNo);

        if (!"APPROVED".equals(status) && !"DECLINED".equals(status) && !"REVERSED".equals(status)) {
            throw new IllegalArgumentException("状態コード不正 行=" + lineNo);
        }
        if (currency.length() != 3) {
            throw new IllegalArgumentException("通貨コード不正 行=" + lineNo);
        }

        return new HoldRow(authId, cardId, "APPROVED".equals(status), currency, amount, expiresAt);
    }

    private static String required(String value, String name, int lineNo) {
        String trimmed = value == null ? "" : value.trim();
        if (trimmed.isEmpty()) {
            throw new IllegalArgumentException(name + "未設定 行=" + lineNo);
        }
        return trimmed;
    }

    private static long parseAmount(String value, int lineNo) {
        String trimmed = required(value, "金額", lineNo);
        try {
            long amount = Long.parseLong(trimmed);
            if (amount < 0L) {
                throw new IllegalArgumentException("金額が負数です 行=" + lineNo);
            }
            return amount;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("金額形式不正 行=" + lineNo, e);
        }
    }

    private static java.time.Instant parseExpiry(String value, int lineNo) {
        String trimmed = required(value, "失効時刻", lineNo);
        try {
            return java.time.OffsetDateTime.parse(trimmed).toInstant();
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException("失効時刻形式不正 行=" + lineNo, e);
        }
    }

    private static final class HoldRow {
        private final String authId;
        private final String cardId;
        private final boolean approved;
        private final String currency;
        private final long amount;
        private final java.time.Instant expiresAt;

        private HoldRow(String authId, String cardId, boolean approved, String currency,
                        long amount, java.time.Instant expiresAt) {
            this.authId = authId;
            this.cardId = cardId;
            this.approved = approved;
            this.currency = currency;
            this.amount = amount;
            this.expiresAt = expiresAt;
        }
    }
}
