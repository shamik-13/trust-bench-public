public class AuthService {
    /**
     * 変更履歴
     * 版数  年月日      担当          概要
     * 1.00  2022-04-18  オーソリ基盤  オーソリ与信判定サービス新規作成
     * 1.01  2023-09-05  オーソリ基盤  通貨・カード状態チェックを整理
     * 1.02  2024-06-12  オーソリ基盤  利用可能枠に未確定ホールドの控除を追加
     */

    private static final String CARD_STATUS_VALID = "01";
    private static final String BASE_CURRENCY = "JPY";
    private static final String HOLD_APPROVED = "00";

    private static final String DECISION_APPROVE = "A";
    private static final String DECISION_DECLINE = "D";

    private static final String REASON_LIMIT = "LIM";
    private static final String REASON_STATUS = "STS";
    private static final String REASON_CURRENCY = "CUR";
    private static final String REASON_NONE = "";

    public static void main(String[] a) throws Exception {
        java.util.Map<String, Card> cards = readCards("CDCARDF");
        java.util.Map<String, Balance> balances = readBalances("CDBALF");
        java.util.List<Auth> auths = readAuths("CDAUTHF");
        for (Response r : decide(cards, balances, auths)) {
            System.out.println(r.authId + "|" + r.decisionKbn + "|" + r.declineReason);
        }
    }

    private static java.util.List<Response> decide(
            java.util.Map<String, Card> cards,
            java.util.Map<String, Balance> balances,
            java.util.List<Auth> auths) {
        java.util.List<Response> responses = new java.util.ArrayList<Response>();

        for (Auth request : auths) {
            Card card = cards.get(request.cardNo);
            Balance balance = balances.get(request.cardNo);

            if (card == null || !CARD_STATUS_VALID.equals(card.cardStatus)) {
                responses.add(new Response(request.authId, request.cardNo, DECISION_DECLINE, 0L,
                        request.authAmt, REASON_STATUS));
                continue;
            }

            if (!BASE_CURRENCY.equals(request.currencyCd)) {
                long available = computeAvailable(card, balance, auths, request);
                responses.add(new Response(request.authId, request.cardNo, DECISION_DECLINE, available,
                        request.authAmt, REASON_CURRENCY));
                continue;
            }

            long available = computeAvailable(card, balance, auths, request);
            if (request.authAmt <= available) {
                responses.add(new Response(request.authId, request.cardNo, DECISION_APPROVE, available,
                        request.authAmt, REASON_NONE));
            } else {
                responses.add(new Response(request.authId, request.cardNo, DECISION_DECLINE, available,
                        request.authAmt, REASON_LIMIT));
            }
        }

        return responses;
    }

    private static long computeAvailable(Card card, Balance balance, java.util.List<Auth> auths, Auth request) {
        long currentBalance = balance == null ? 0L : balance.currentBalAmt;
        long activeHolds = 0L;

        for (Auth hold : auths) {
            if (!request.cardNo.equals(hold.cardNo)) {
                continue;
            }
            if (!HOLD_APPROVED.equals(hold.authResult)) {
                continue;
            }
            if (!BASE_CURRENCY.equals(hold.currencyCd)) {
                continue;
            }
            if (hold.holdExpDt.compareTo(request.requestDate()) < 0) {
                continue;
            }
            activeHolds += hold.authAmt;
        }

        return card.creditLimit - currentBalance - activeHolds;
    }

    private static java.util.Map<String, Card> readCards(String path) throws java.io.IOException {
        java.util.Map<String, Card> cards = new java.util.LinkedHashMap<String, Card>();
        for (String line : readDataLines(path)) {
            java.util.List<String> f = parseCsv(line);
            requireSize(path, f, 5);
            cards.put(f.get(0), new Card(f.get(0), f.get(1), f.get(2), parseMoney(path, f.get(3)), f.get(4)));
        }
        return cards;
    }

    private static java.util.Map<String, Balance> readBalances(String path) throws java.io.IOException {
        java.util.Map<String, Balance> balances = new java.util.LinkedHashMap<String, Balance>();
        for (String line : readDataLines(path)) {
            java.util.List<String> f = parseCsv(line);
            requireSize(path, f, 4);
            balances.put(f.get(0), new Balance(f.get(0), parseMoney(path, f.get(1)),
                    parseMoney(path, f.get(2)), f.get(3)));
        }
        return balances;
    }

    private static java.util.List<Auth> readAuths(String path) throws java.io.IOException {
        java.util.List<Auth> auths = new java.util.ArrayList<Auth>();
        for (String line : readDataLines(path)) {
            java.util.List<String> f = parseCsv(line);
            requireSize(path, f, 8);
            auths.add(new Auth(f.get(0), f.get(1), parseMoney(path, f.get(2)), f.get(3),
                    f.get(4), f.get(5), f.get(6), f.get(7)));
        }
        return auths;
    }

    private static void writeResponses(String path, java.util.List<Response> responses) throws java.io.IOException {
        java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        try {
            for (Response r : responses) {
                writer.write(csv(r.authId));
                writer.write(',');
                writer.write(csv(r.cardNo));
                writer.write(',');
                writer.write(csv(r.decisionKbn));
                writer.write(',');
                writer.write(Long.toString(r.availAmt));
                writer.write(',');
                writer.write(Long.toString(r.authAmt));
                writer.write(',');
                writer.write(csv(r.declineReason));
                writer.newLine();
            }
        } finally {
            writer.close();
        }
    }

    private static java.util.List<String> readDataLines(String path) throws java.io.IOException {
        java.util.List<String> lines = java.nio.file.Files.readAllLines(
                java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<String> data = new java.util.ArrayList<String>();
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.isEmpty() || trimmed.startsWith("#")) {
                continue;
            }
            data.add(line);
        }
        return data;
    }

    private static void requireSize(String path, java.util.List<String> fields, int size) {
        if (fields.size() != size) {
            throw new IllegalArgumentException(path + " の項目数が不正です");
        }
    }

    private static long parseMoney(String path, String value) {
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(path + " の金額が不正です: " + value, e);
        }
    }

    private static java.util.List<String> parseCsv(String line) {
        java.util.List<String> fields = new java.util.ArrayList<String>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quoted) {
                if (c == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        current.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    current.append(c);
                }
            } else {
                if (c == ',') {
                    fields.add(current.toString().trim());
                    current.setLength(0);
                } else if (c == '"') {
                    quoted = true;
                } else {
                    current.append(c);
                }
            }
        }

        if (quoted) {
            throw new IllegalArgumentException("CSV引用符が不正です");
        }

        fields.add(current.toString().trim());
        return fields;
    }

    private static String csv(String value) {
        if (value == null) {
            return "";
        }
        boolean quote = value.indexOf(',') >= 0 || value.indexOf('"') >= 0
                || value.indexOf('\n') >= 0 || value.indexOf('\r') >= 0;
        if (!quote) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }

    private static final class Card {
        final String cardNo;
        final String memberId;
        final String cardStatus;
        final long creditLimit;
        final String memberNameKana;

        Card(String cardNo, String memberId, String cardStatus, long creditLimit, String memberNameKana) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
            this.creditLimit = creditLimit;
            this.memberNameKana = memberNameKana;
        }
    }

    private static final class Balance {
        final String cardNo;
        final long currentBalAmt;
        final long lastStmtAmt;
        final String cycleDt;

        Balance(String cardNo, long currentBalAmt, long lastStmtAmt, String cycleDt) {
            this.cardNo = cardNo;
            this.currentBalAmt = currentBalAmt;
            this.lastStmtAmt = lastStmtAmt;
            this.cycleDt = cycleDt;
        }
    }

    private static final class Auth {
        final String authId;
        final String cardNo;
        final long authAmt;
        final String authResult;
        final String merchantCode;
        final String currencyCd;
        final String authTs;
        final String holdExpDt;

        Auth(String authId, String cardNo, long authAmt, String authResult,
             String merchantCode, String currencyCd, String authTs, String holdExpDt) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.authAmt = authAmt;
            this.authResult = authResult;
            this.merchantCode = merchantCode;
            this.currencyCd = currencyCd;
            this.authTs = authTs;
            this.holdExpDt = holdExpDt;
        }

        String requestDate() {
            String digits = authTs.replaceAll("[^0-9]", "");
            if (digits.length() < 8) {
                throw new IllegalArgumentException("オーソリ日時が不正です: " + authTs);
            }
            return digits.substring(0, 8);
        }
    }

    private static final class Response {
        final String authId;
        final String cardNo;
        final String decisionKbn;
        final long availAmt;
        final long authAmt;
        final String declineReason;

        Response(String authId, String cardNo, String decisionKbn, long availAmt,
                 long authAmt, String declineReason) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.decisionKbn = decisionKbn;
            this.availAmt = availAmt;
            this.authAmt = authAmt;
            this.declineReason = declineReason;
        }
    }
}
