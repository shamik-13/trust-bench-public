public class RevolvingStatementInquiryService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2018-09-14  開発担当  リボ明細照会（請求ヘッダ・売上・リボ確定行の時系列表示）を新規作成
     * 1.01  2020-04-22  開発担当  確定済みリボ行を起点とした将来見込みの試算を追加
     * 1.02  2023-06-30  開発担当  リボ状態が無効の場合の対象外表示を見直し
     */

    private static final java.math.BigDecimal REV_MONTHLY_FEE_RATE = new java.math.BigDecimal("0.0125");
    private static final java.time.format.DateTimeFormatter DATE_FMT = java.time.format.DateTimeFormatter.ISO_LOCAL_DATE;
    private static final java.util.Set<String> VALID_SLIDE_TIERS =
            new java.util.LinkedHashSet<>(java.util.Arrays.asList("T1", "T2", "T3", "T4"));

    public String inquire(String cardNo,
                          java.time.YearMonth cycleDt,
                          java.util.List<Cdstmtf2> cdstmtf2,
                          java.util.List<Cdrsldf> cdrsldf,
                          java.util.List<Cdcaptf> cdcaptf,
                          java.util.List<Cdrevf> cdrevf) {
        validateRequest(cardNo, cycleDt);

        Cdstmtf2 statement = cdstmtf2.stream()
                .filter(r -> r.cardNo.equals(cardNo) && r.cycleDt.equals(cycleDt))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("請求ヘッダが存在しません"));

        Cdrevf revolving = cdrevf.stream()
                .filter(r -> r.cardNo.equals(cardNo))
                .findFirst()
                .orElse(null);

        java.util.List<Cdcaptf> captures = cdcaptf.stream()
                .filter(r -> r.cardNo.equals(cardNo))
                .filter(r -> java.time.YearMonth.from(r.salesDt).equals(cycleDt))
                .filter(r -> "確定".equals(r.captureStatus))
                .sorted(java.util.Comparator.comparing((Cdcaptf r) -> r.salesDt).thenComparing(r -> r.captureId))
                .collect(java.util.stream.Collectors.toList());

        int captureTotal = captures.stream().mapToInt(r -> r.captureAmt).sum();

        java.util.List<Cdrsldf> revolvingDetails = cdrsldf.stream()
                .filter(r -> r.cardNo.equals(cardNo) && r.cycleDt.equals(cycleDt))
                .sorted(java.util.Comparator.comparing((Cdrsldf r) -> r.programId).thenComparing(r -> r.slideTier))
                .collect(java.util.stream.Collectors.toList());

        if (revolving != null && !"01".equals(revolving.revStatus)) {
            revolvingDetails = java.util.Collections.singletonList(
                    new Cdrsldf(cardNo, cycleDt, 0, 0, 0, "T1", "S", "CB290S"));
        }

        validateStatement(statement);
        for (Cdcaptf capture : captures) {
            validateCapture(capture);
        }
        for (Cdrsldf line : revolvingDetails) {
            validateRevolvingLine(line);
        }

        java.util.List<Event> events = new java.util.ArrayList<>();
        events.add(new Event(statement.dueDt, "請求ヘッダ", statementJson(statement, captureTotal)));

        for (Cdcaptf capture : captures) {
            events.add(new Event(capture.salesDt, "売上明細", captureJson(capture)));
        }

        for (Cdrsldf line : revolvingDetails) {
            events.add(new Event(cycleDt.atEndOfMonth(), "リボ請求確定行", revolvingLineJson(line, revolving)));
        }

        if (revolving != null && "01".equals(revolving.revStatus) && !revolvingDetails.isEmpty()) {
            Cdrsldf base = revolvingDetails.get(revolvingDetails.size() - 1);
            events.addAll(projectFuture(cardNo, cycleDt, base));
        }

        events.sort(java.util.Comparator.comparing((Event e) -> e.date).thenComparing(e -> e.kind));

        StringBuilder json = new StringBuilder();
        json.append("{");
        appendField(json, "カード番号", cardNo).append(",");
        appendField(json, "対象月", cycleDt.toString()).append(",");
        appendField(json, "会員番号", revolving == null ? "" : revolving.memberId).append(",");
        appendField(json, "会員カナ", revolving == null ? "" : revolving.memberNameKana).append(",");
        json.append("\"時系列\":[");
        for (int i = 0; i < events.size(); i++) {
            if (i > 0) {
                json.append(",");
            }
            Event e = events.get(i);
            json.append("{");
            appendField(json, "日付", DATE_FMT.format(e.date)).append(",");
            appendField(json, "種別", e.kind).append(",");
            json.append("\"内容\":").append(e.body);
            json.append("}");
        }
        json.append("]}");
        return json.toString();
    }

    private java.util.List<Event> projectFuture(String cardNo, java.time.YearMonth cycleDt, Cdrsldf base) {
        java.util.List<Event> events = new java.util.ArrayList<>();
        int balance = Math.max(0, base.prinAmt * 5);
        int monthlyPrincipal = base.prinAmt;

        for (int i = 1; i <= 3 && balance > 0; i++) {
            java.time.YearMonth next = cycleDt.plusMonths(i);
            int principal = Math.min(monthlyPrincipal, balance);
            int fee = fee(balance);
            int pay = principal + fee;
            balance -= principal;

            StringBuilder body = new StringBuilder();
            body.append("{");
            appendField(body, "カード番号", cardNo).append(",");
            appendField(body, "見込月", next.toString()).append(",");
            appendNumber(body, "元金", principal).append(",");
            appendNumber(body, "手数料", fee).append(",");
            appendNumber(body, "支払額", pay).append(",");
            appendNumber(body, "見込残高", balance).append(",");
            appendField(body, "算出根拠", "確定済みCDRSLDF起点");
            body.append("}");
            events.add(new Event(next.atEndOfMonth(), "リボ将来見込み", body.toString()));
        }
        return events;
    }

    private int fee(int revBalance) {
        return new java.math.BigDecimal(revBalance).multiply(REV_MONTHLY_FEE_RATE).setScale(0, java.math.RoundingMode.DOWN).intValueExact();
    }

    private void validateRequest(String cardNo, java.time.YearMonth cycleDt) {
        if (cardNo == null || !cardNo.matches("[0-9]{16}")) {
            throw new IllegalArgumentException("カード番号が不正です");
        }
        if (cycleDt == null) {
            throw new IllegalArgumentException("対象月が不正です");
        }
    }

    private void validateStatement(Cdstmtf2 r) {
        if (r.billAmt < 0 || r.minPayAmt < 0 || r.delinqDays < 0) {
            throw new IllegalArgumentException("請求ヘッダの金額または延滞日数が不正です");
        }
    }

    private void validateCapture(Cdcaptf r) {
        if (r.captureAmt <= 0) {
            throw new IllegalArgumentException("売上明細の金額が不正です");
        }
        if (isBlank(r.captureId) || isBlank(r.authNo) || isBlank(r.merchantId)) {
            throw new IllegalArgumentException("売上明細のキー項目が不正です");
        }
    }

    private void validateRevolvingLine(Cdrsldf r) {
        if (!VALID_SLIDE_TIERS.contains(r.slideTier)) {
            throw new IllegalArgumentException("リボ区分が不正です");
        }
        if (!"C".equals(r.rsldStatus) && !"S".equals(r.rsldStatus)) {
            throw new IllegalArgumentException("リボ確定状態が不正です");
        }
        if (r.prinAmt < 0 || r.feeAmt < 0 || r.payAmt < 0) {
            throw new IllegalArgumentException("リボ金額が不正です");
        }
        if ("S".equals(r.rsldStatus) && (r.prinAmt != 0 || r.feeAmt != 0 || r.payAmt != 0)) {
            throw new IllegalArgumentException("対象外リボ行の金額が不正です");
        }
    }

    private String statementJson(Cdstmtf2 r, int captureTotal) {
        StringBuilder json = new StringBuilder();
        json.append("{");
        appendField(json, "カード番号", r.cardNo).append(",");
        appendField(json, "締年月", r.cycleDt.toString()).append(",");
        appendNumber(json, "請求額", r.billAmt).append(",");
        appendNumber(json, "最低支払額", r.minPayAmt).append(",");
        appendField(json, "支払期日", DATE_FMT.format(r.dueDt)).append(",");
        appendField(json, "請求状態", r.stmtStatus).append(",");
        appendNumber(json, "延滞日数", r.delinqDays).append(",");
        appendNumber(json, "売上合計", captureTotal);
        json.append("}");
        return json.toString();
    }

    private String captureJson(Cdcaptf r) {
        StringBuilder json = new StringBuilder();
        json.append("{");
        appendField(json, "売上ＩＤ", r.captureId).append(",");
        appendField(json, "承認番号", r.authNo).append(",");
        appendField(json, "売上日", DATE_FMT.format(r.salesDt)).append(",");
        appendNumber(json, "売上金額", r.captureAmt).append(",");
        appendField(json, "加盟店ＩＤ", r.merchantId).append(",");
        appendField(json, "売上状態", r.captureStatus);
        json.append("}");
        return json.toString();
    }

    private String revolvingLineJson(Cdrsldf r, Cdrevf rev) {
        StringBuilder json = new StringBuilder();
        json.append("{");
        appendField(json, "カード番号", r.cardNo).append(",");
        appendField(json, "締年月", r.cycleDt.toString()).append(",");
        appendNumber(json, "元金", r.prinAmt).append(",");
        appendNumber(json, "手数料", r.feeAmt).append(",");
        appendNumber(json, "支払額", r.payAmt).append(",");
        appendField(json, "区分", r.slideTier).append(",");
        appendField(json, "確定状態", r.rsldStatus).append(",");
        appendField(json, "作成プログラム", r.programId).append(",");
        appendField(json, "リボ状態", rev == null ? "" : rev.revStatus).append(",");
        appendField(json, "リボコース", rev == null ? "" : rev.revCourseCd);
        json.append("}");
        return json.toString();
    }

    private static StringBuilder appendField(StringBuilder json, String name, String value) {
        json.append("\"").append(escape(name)).append("\":\"").append(escape(value)).append("\"");
        return json;
    }

    private static StringBuilder appendNumber(StringBuilder json, String name, int value) {
        json.append("\"").append(escape(name)).append("\":").append(value);
        return json;
    }

    private static String escape(String value) {
        if (value == null) {
            return "";
        }
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c == '"' || c == '\\') {
                out.append('\\').append(c);
            } else if (c == '\b') {
                out.append("\\b");
            } else if (c == '\f') {
                out.append("\\f");
            } else if (c == '\n') {
                out.append("\\n");
            } else if (c == '\r') {
                out.append("\\r");
            } else if (c == '\t') {
                out.append("\\t");
            } else if (c < 0x20) {
                out.append(String.format("\\u%04x", (int) c));
            } else {
                out.append(c);
            }
        }
        return out.toString();
    }

    private static boolean isBlank(String v) {
        return v == null || v.trim().isEmpty();
    }

    private static final class Event {
        private final java.time.LocalDate date;
        private final String kind;
        private final String body;

        private Event(java.time.LocalDate date, String kind, String body) {
            this.date = date;
            this.kind = kind;
            this.body = body;
        }
    }

    static final class Cdstmtf2 {
        private final String cardNo;
        private final java.time.YearMonth cycleDt;
        private final int billAmt;
        private final int minPayAmt;
        private final java.time.LocalDate dueDt;
        private final String stmtStatus;
        private final int delinqDays;

        Cdstmtf2(String cardNo, java.time.YearMonth cycleDt, int billAmt, int minPayAmt,
                         java.time.LocalDate dueDt, String stmtStatus, int delinqDays) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.billAmt = billAmt;
            this.minPayAmt = minPayAmt;
            this.dueDt = dueDt;
            this.stmtStatus = stmtStatus;
            this.delinqDays = delinqDays;
        }
    }

    static final class Cdrsldf {
        private final String cardNo;
        private final java.time.YearMonth cycleDt;
        private final int prinAmt;
        private final int feeAmt;
        private final int payAmt;
        private final String slideTier;
        private final String rsldStatus;
        private final String programId;

        Cdrsldf(String cardNo, java.time.YearMonth cycleDt, int prinAmt, int feeAmt,
                        int payAmt, String slideTier, String rsldStatus, String programId) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.prinAmt = prinAmt;
            this.feeAmt = feeAmt;
            this.payAmt = payAmt;
            this.slideTier = slideTier;
            this.rsldStatus = rsldStatus;
            this.programId = programId;
        }
    }

    static final class Cdcaptf {
        private final String captureId;
        private final String authNo;
        private final String cardNo;
        private final java.time.LocalDate salesDt;
        private final int captureAmt;
        private final String merchantId;
        private final String captureStatus;

        Cdcaptf(String captureId, String authNo, String cardNo, java.time.LocalDate salesDt,
                        int captureAmt, String merchantId, String captureStatus) {
            this.captureId = captureId;
            this.authNo = authNo;
            this.cardNo = cardNo;
            this.salesDt = salesDt;
            this.captureAmt = captureAmt;
            this.merchantId = merchantId;
            this.captureStatus = captureStatus;
        }
    }

    static final class Cdrevf {
        private final String cardNo;
        private final String memberId;
        private final String revStatus;
        private final String revCourseCd;
        private final String memberNameKana;
        private final java.time.LocalDate revStartDt;

        Cdrevf(String cardNo, String memberId, String revStatus, String revCourseCd,
                       String memberNameKana, java.time.LocalDate revStartDt) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.revStatus = revStatus;
            this.revCourseCd = revCourseCd;
            this.memberNameKana = memberNameKana;
            this.revStartDt = revStartDt;
        }
    }
}
