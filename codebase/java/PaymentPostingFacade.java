/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/03/05  開発担当  初版作成。入金要求を検証しCDHISTFへ受付/対象外イベントを追記する。
 * 1.01  2024/07/22  決済運用  重複PAY-ID判定と項目数不正のスキップ件数計上を追加。
 */
public class PaymentPostingFacade {
    private static final String SOURCE_PROGRAM = "PaymentPostingFacade";
    private static final String EVENT_TYPE_FULL = "NYUKIN_F";
    private static final String EVENT_TYPE_PARTIAL = "NYUKIN_P";
    private static final String EVENT_TYPE_OVER = "NYUKIN_O";
    private static final String EVENT_TYPE_SKIP = "NYUKIN_S";

    public static void main(String[] a) throws Exception {
        if (a.length != 3) {
            System.err.println("引数不正: CDPAYF CDOSF CDHISTF を指定してください");
            System.exit(2);
        }

        java.nio.file.Path cdpayf = java.nio.file.Paths.get(a[0]);
        java.nio.file.Path cdosf = java.nio.file.Paths.get(a[1]);
        java.nio.file.Path cdhistf = java.nio.file.Paths.get(a[2]);

        java.util.Map<String, CardOpenState> cardStates = readCardOpenStates(cdosf);
        HistoryState historyState = readHistoryState(cdhistf);

        java.util.List<HistoryEvent> appendEvents = new java.util.ArrayList<>();
        int readCount = 0;
        int writeCount = 0;
        int skipCount = 0;

        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(cdpayf, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isSkippable(line)) {
                    continue;
                }

                PaymentRequest request;
                try {
                    request = parsePaymentRequest(line);
                } catch (RuntimeException e) {
                    skipCount++;
                    appendEvents.add(skipEvent("", "PAY読込不正-" + lineNo, java.math.BigDecimal.ZERO, java.time.LocalDate.now(), historyState));
                    continue;
                }

                readCount++;

                if (historyState.postedPayIds.contains(request.payId)) {
                    skipCount++;
                    continue;
                }

                ValidationResult validation = validate(request, cardStates.get(request.cardNo));
                if (!validation.accepted) {
                    skipCount++;
                    appendEvents.add(newHistoryEvent(
                            request.cardNo,
                            request.payId,
                            EVENT_TYPE_SKIP,
                            request.payAmount,
                            request.payDate,
                            historyState));
                    continue;
                }

                HistoryEvent event = createAcceptedEvent(request, validation.cardState, historyState);
                appendEvents.add(event);
                historyState.postedPayIds.add(request.payId);
                writeCount++;
            }
        }

        if (!appendEvents.isEmpty()) {
            boolean exists = java.nio.file.Files.exists(cdhistf) && java.nio.file.Files.size(cdhistf) > 0;
            try (java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(
                    cdhistf,
                    java.nio.charset.StandardCharsets.UTF_8,
                    java.nio.file.StandardOpenOption.CREATE,
                    java.nio.file.StandardOpenOption.APPEND)) {
                if (exists) {
                    writer.newLine();
                }
                for (int i = 0; i < appendEvents.size(); i++) {
                    if (i > 0) {
                        writer.newLine();
                    }
                    writer.write(formatHistoryEvent(appendEvents.get(i)));
                }
            }
        }

        System.out.println("処理終了: 読込件数=" + readCount + " 書込件数=" + writeCount + " 対象外件数=" + skipCount);
    }

    private static java.util.Map<String, CardOpenState> readCardOpenStates(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, CardOpenState> result = new java.util.HashMap<>();
        if (!java.nio.file.Files.exists(path)) {
            return result;
        }

        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (isSkippable(line)) {
                    continue;
                }
                CardOpenState state = parseCardOpenState(line);
                result.put(state.cardNo, state);
            }
        }
        return result;
    }

    private static HistoryState readHistoryState(java.nio.file.Path path) throws java.io.IOException {
        HistoryState state = new HistoryState();
        if (!java.nio.file.Files.exists(path)) {
            return state;
        }

        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (isSkippable(line)) {
                    continue;
                }
                HistoryEvent event = parseHistoryEvent(line);
                state.postedPayIds.add(event.payId);
                String key = event.cardNo + "\u0001" + event.payId;
                Integer current = state.maxSequenceByKey.get(key);
                if (current == null || current < event.eventSeq) {
                    state.maxSequenceByKey.put(key, event.eventSeq);
                }
            }
        }
        return state;
    }

    private static PaymentRequest parsePaymentRequest(String line) {
        String[] p = splitCsv(line, 5);
        return new PaymentRequest(
                p[0].trim(),
                p[1].trim(),
                amount(p[2]),
                date(p[3]),
                p[4].trim());
    }

    private static CardOpenState parseCardOpenState(String line) {
        String[] p = splitCsv(line, 5);
        return new CardOpenState(
                p[0].trim(),
                amount(p[1]),
                amount(p[2]),
                amount(p[3]),
                date(p[4]));
    }

    private static HistoryEvent parseHistoryEvent(String line) {
        String[] p = splitCsv(line, 7);
        return new HistoryEvent(
                p[0].trim(),
                p[1].trim(),
                Integer.parseInt(p[2].trim()),
                p[3].trim(),
                amount(p[4]),
                date(p[5]),
                p[6].trim());
    }

    private static ValidationResult validate(PaymentRequest request, CardOpenState state) {
        if (request.payId.isEmpty() || request.cardNo.isEmpty()) {
            return ValidationResult.rejected();
        }
        if (request.payAmount.signum() <= 0) {
            return ValidationResult.rejected();
        }
        if (!"10".equals(request.payMethod) && !"20".equals(request.payMethod) && !"30".equals(request.payMethod)) {
            return ValidationResult.rejected();
        }
        if (state == null) {
            return ValidationResult.rejected();
        }
        if (request.payDate.isBefore(state.cycleDate)) {
            return ValidationResult.rejected();
        }
        if (state.totalBalance().signum() <= 0) {
            return ValidationResult.rejected();
        }
        return ValidationResult.accepted(state);
    }

    private static HistoryEvent createAcceptedEvent(PaymentRequest request, CardOpenState state, HistoryState historyState) {
        java.math.BigDecimal balance = state.totalBalance();
        String eventType;
        int comparison = request.payAmount.compareTo(balance);
        if (comparison == 0) {
            eventType = EVENT_TYPE_FULL;
        } else if (comparison < 0) {
            eventType = EVENT_TYPE_PARTIAL;
        } else {
            eventType = EVENT_TYPE_OVER;
        }
        return newHistoryEvent(request.cardNo, request.payId, eventType, request.payAmount, request.payDate, historyState);
    }

    private static HistoryEvent skipEvent(String cardNo, String payId, java.math.BigDecimal amount, java.time.LocalDate date, HistoryState historyState) {
        return newHistoryEvent(cardNo, payId, EVENT_TYPE_SKIP, amount, date, historyState);
    }

    private static HistoryEvent newHistoryEvent(
            String cardNo,
            String payId,
            String eventType,
            java.math.BigDecimal amount,
            java.time.LocalDate date,
            HistoryState historyState) {
        String key = cardNo + "\u0001" + payId;
        int seq = historyState.maxSequenceByKey.getOrDefault(key, 0) + 1;
        historyState.maxSequenceByKey.put(key, seq);
        return new HistoryEvent(cardNo, payId, seq, eventType, amount, date, SOURCE_PROGRAM);
    }

    private static String formatHistoryEvent(HistoryEvent event) {
        return csv(event.cardNo)
                + "," + csv(event.payId)
                + "," + event.eventSeq
                + "," + csv(event.eventType)
                + "," + event.eventAmount.toPlainString()
                + "," + event.eventDate
                + "," + csv(event.sourceProgram);
    }

    private static String[] splitCsv(String line, int expected) {
        java.util.List<String> out = new java.util.ArrayList<>();
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
            } else if (c == '"') {
                quoted = true;
            } else if (c == ',') {
                out.add(field.toString());
                field.setLength(0);
            } else {
                field.append(c);
            }
        }
        out.add(field.toString());

        if (out.size() != expected) {
            throw new IllegalArgumentException("項目数不正");
        }
        return out.toArray(new String[0]);
    }

    private static String csv(String value) {
        if (value.indexOf(',') < 0 && value.indexOf('"') < 0 && value.indexOf('\n') < 0 && value.indexOf('\r') < 0) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }

    private static java.math.BigDecimal amount(String value) {
        return new java.math.BigDecimal(value.trim()).setScale(0, java.math.RoundingMode.UNNECESSARY);
    }

    private static java.time.LocalDate date(String value) {
        String v = value.trim();
        if (v.length() == 8 && v.chars().allMatch(Character::isDigit)) {
            return java.time.LocalDate.of(
                    Integer.parseInt(v.substring(0, 4)),
                    Integer.parseInt(v.substring(4, 6)),
                    Integer.parseInt(v.substring(6, 8)));
        }
        return java.time.LocalDate.parse(v);
    }

    private static boolean isSkippable(String line) {
        String trimmed = line.trim();
        return trimmed.isEmpty() || trimmed.startsWith("#");
    }

    private static final class PaymentRequest {
        private final String payId;
        private final String cardNo;
        private final java.math.BigDecimal payAmount;
        private final java.time.LocalDate payDate;
        private final String payMethod;

        private PaymentRequest(String payId, String cardNo, java.math.BigDecimal payAmount, java.time.LocalDate payDate, String payMethod) {
            this.payId = payId;
            this.cardNo = cardNo;
            this.payAmount = payAmount;
            this.payDate = payDate;
            this.payMethod = payMethod;
        }
    }

    private static final class CardOpenState {
        private final String cardNo;
        private final java.math.BigDecimal feeBalanceAmount;
        private final java.math.BigDecimal interestBalanceAmount;
        private final java.math.BigDecimal principalBalanceAmount;
        private final java.time.LocalDate cycleDate;

        private CardOpenState(
                String cardNo,
                java.math.BigDecimal feeBalanceAmount,
                java.math.BigDecimal interestBalanceAmount,
                java.math.BigDecimal principalBalanceAmount,
                java.time.LocalDate cycleDate) {
            this.cardNo = cardNo;
            this.feeBalanceAmount = feeBalanceAmount;
            this.interestBalanceAmount = interestBalanceAmount;
            this.principalBalanceAmount = principalBalanceAmount;
            this.cycleDate = cycleDate;
        }

        private java.math.BigDecimal totalBalance() {
            return feeBalanceAmount.add(interestBalanceAmount).add(principalBalanceAmount);
        }
    }

    private static final class HistoryEvent {
        private final String cardNo;
        private final String payId;
        private final int eventSeq;
        private final String eventType;
        private final java.math.BigDecimal eventAmount;
        private final java.time.LocalDate eventDate;
        private final String sourceProgram;

        private HistoryEvent(
                String cardNo,
                String payId,
                int eventSeq,
                String eventType,
                java.math.BigDecimal eventAmount,
                java.time.LocalDate eventDate,
                String sourceProgram) {
            this.cardNo = cardNo;
            this.payId = payId;
            this.eventSeq = eventSeq;
            this.eventType = eventType;
            this.eventAmount = eventAmount;
            this.eventDate = eventDate;
            this.sourceProgram = sourceProgram;
        }
    }

    private static final class HistoryState {
        private final java.util.Set<String> postedPayIds = new java.util.HashSet<>();
        private final java.util.Map<String, Integer> maxSequenceByKey = new java.util.HashMap<>();
    }

    private static final class ValidationResult {
        private final boolean accepted;
        private final CardOpenState cardState;

        private ValidationResult(boolean accepted, CardOpenState cardState) {
            this.accepted = accepted;
            this.cardState = cardState;
        }

        private static ValidationResult accepted(CardOpenState cardState) {
            return new ValidationResult(true, cardState);
        }

        private static ValidationResult rejected() {
            return new ValidationResult(false, null);
        }
    }
}
