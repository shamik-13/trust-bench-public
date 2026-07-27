public class ChargebackCaseService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2024/02/14  開発一課  初版作成
     * 1.01  2024/08/22  保守二課  返品反映済の例外区分を追加
     * 1.02  2025/02/10  保守二課  高リスク判定の連携区分を整理
     */

    private static final String PGM_ID = "CDCBK-LNK";
    private static final String BASE_CURRENCY = "JPY";
    private static final String CAP_CONFIRMED = "C";
    private static final String CAP_SKIP = "S";
    private static final String CAP_HOLD = "H";

    private static final java.time.format.DateTimeFormatter DATE_FMT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    public static void main(String[] a) throws Exception {
        java.util.List<ChargebackCase> chargebacks;
        java.util.List<Capture> captures;
        java.util.List<FraudCase> fraudCases;
        java.nio.file.Path exceptionPath = null;

        if (a.length >= 4) {
            chargebacks = readChargebacks(java.nio.file.Paths.get(a[0]));
            captures = readCaptures(java.nio.file.Paths.get(a[1]));
            fraudCases = readFraudCases(java.nio.file.Paths.get(a[2]));
            exceptionPath = java.nio.file.Paths.get(a[3]);
        } else {
            chargebacks = sampleChargebacks();
            captures = sampleCaptures();
            fraudCases = sampleFraudCases();
        }

        Result result = judge(chargebacks, captures, fraudCases);

        if (exceptionPath != null) {
            writeExceptions(exceptionPath, result.exceptions);
        } else {
            for (ExceptionRecord e : result.exceptions) {
                System.out.println(toCsv(e));
            }
        }

        for (WorkflowCase c : result.workflowCases) {
            System.out.println("連携対象," + c.chargebackId + "," + c.saleId + "," + maskCard(c.cardNo)
                    + "," + c.claimAmt + "," + c.billedAmt + "," + c.feeAmt + ","
                    + c.currencyCd + "," + c.riskScore + "," + c.decisionCd);
        }

        System.out.println("処理件数," + chargebacks.size()
                + ",連携件数," + result.workflowCases.size()
                + ",例外件数," + result.exceptions.size());
    }

    private static Result judge(java.util.List<ChargebackCase> chargebacks,
                                java.util.List<Capture> captures,
                                java.util.List<FraudCase> fraudCases) {
        java.util.Map<String, Capture> captureBySale = new java.util.HashMap<>();
        for (Capture c : captures) {
            Capture old = captureBySale.get(c.saleId);
            if (old == null || priority(c.capStatus) > priority(old.capStatus)) {
                captureBySale.put(c.saleId, c);
            }
        }

        java.util.Map<String, FraudCase> fraudBySale = new java.util.HashMap<>();
        for (FraudCase f : fraudCases) {
            FraudCase old = fraudBySale.get(f.saleId);
            if (old == null || f.riskScore > old.riskScore) {
                fraudBySale.put(f.saleId, f);
            }
        }

        java.util.List<WorkflowCase> workflow = new java.util.ArrayList<>();
        java.util.List<ExceptionRecord> exceptions = new java.util.ArrayList<>();
        java.util.Set<String> exceptionKeys = new java.util.HashSet<>();

        int seq = 1;
        for (ChargebackCase cb : chargebacks) {
            Capture cap = captureBySale.get(cb.saleId);
            FraudCase fraud = fraudBySale.get(cb.saleId);

            if (!validChargeback(cb)) {
                addException(exceptions, exceptionKeys, seq++, cb, "入力不備");
                continue;
            }

            if (cap == null || !sameCard(cb.cardNo, cap.cardNo)) {
                addException(exceptions, exceptionKeys, seq++, cb, "請求未検出");
                continue;
            }

            if (isReturned(cb.caseStatus) || CAP_SKIP.equals(cap.capStatus)) {
                addException(exceptions, exceptionKeys, seq++, cb, "返品反映済");
                continue;
            }

            if (CAP_HOLD.equals(cap.capStatus)) {
                addException(exceptions, exceptionKeys, seq++, cb, "精算保留中");
                continue;
            }

            if (!CAP_CONFIRMED.equals(cap.capStatus)) {
                addException(exceptions, exceptionKeys, seq++, cb, "請求状態不正");
                continue;
            }

            long fee = feeAmount(cap);
            int risk = fraud == null ? 0 : fraud.riskScore;
            String decision = decision(cb, cap, fraud, fee);

            if ("連携不可".equals(decision)) {
                addException(exceptions, exceptionKeys, seq++, cb, "金額不整合");
                continue;
            }

            workflow.add(new WorkflowCase(cb.chargebackId, cb.saleId, cb.cardNo, cb.claimAmt,
                    cap.billedAmt, fee, cap.currencyCd, risk, decision));
        }

        return new Result(workflow, exceptions);
    }

    private static boolean validChargeback(ChargebackCase cb) {
        return notBlank(cb.chargebackId)
                && notBlank(cb.saleId)
                && notBlank(cb.cardNo)
                && notBlank(cb.merchantCode)
                && cb.claimAmt > 0
                && notBlank(cb.claimReason)
                && notBlank(cb.caseStatus);
    }

    private static String decision(ChargebackCase cb, Capture cap, FraudCase fraud, long fee) {
        long upper = cap.billedAmt + fee;
        if (cb.claimAmt > upper) {
            return "連携不可";
        }
        int risk = fraud == null ? 0 : fraud.riskScore;
        if (risk >= 80) {
            return "高リスク連携";
        }
        if (risk >= 50 || cb.claimAmt >= upper * 8 / 10) {
            return "要確認連携";
        }
        return "通常連携";
    }

    private static long feeAmount(Capture cap) {
        if (BASE_CURRENCY.equals(cap.currencyCd)) {
            return 0L;
        }
        return cap.feeAmt > 0 ? cap.feeAmt : Math.round(cap.billedAmt * 0.025d);
    }

    private static void addException(java.util.List<ExceptionRecord> out, java.util.Set<String> keys,
                                     int seq, ChargebackCase cb, String reason) {
        String key = cb.saleId + "|" + cb.cardNo + "|" + reason;
        if (!keys.add(key)) {
            return;
        }
        String id = "EX" + java.time.LocalDate.now().format(DATE_FMT) + String.format("%06d", seq);
        out.add(new ExceptionRecord(id, cb.saleId, cb.cardNo, reason, PGM_ID,
                java.time.LocalDate.now().format(DATE_FMT), "未処理"));
    }

    private static boolean sameCard(String a, String b) {
        return normalize(a).equals(normalize(b));
    }

    private static boolean isReturned(String status) {
        String s = normalize(status);
        return "返品反映済".equals(s) || "RETURNED".equals(s) || "R".equals(s);
    }

    private static int priority(String capStatus) {
        if (CAP_HOLD.equals(capStatus)) return 3;
        if (CAP_CONFIRMED.equals(capStatus)) return 2;
        if (CAP_SKIP.equals(capStatus)) return 1;
        return 0;
    }

    private static java.util.List<ChargebackCase> readChargebacks(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<ChargebackCase> list = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(p, java.nio.charset.StandardCharsets.UTF_8)) {
            if (skip(line)) continue;
            java.util.List<String> f = splitCsv(line);
            if (f.size() < 7 || "CHARGEBACK-ID".equals(f.get(0))) continue;
            list.add(new ChargebackCase(f.get(0), f.get(1), f.get(2), f.get(3),
                    yen(f.get(4)), f.get(5), f.get(6)));
        }
        return list;
    }

    private static java.util.List<Capture> readCaptures(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<Capture> list = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(p, java.nio.charset.StandardCharsets.UTF_8)) {
            if (skip(line)) continue;
            java.util.List<String> f = splitCsv(line);
            if (f.size() < 7 || "SALE-ID".equals(f.get(0))) continue;
            list.add(new Capture(f.get(0), f.get(1), yen(f.get(2)), yen(f.get(3)),
                    f.get(4), f.get(5), f.get(6)));
        }
        return list;
    }

    private static java.util.List<FraudCase> readFraudCases(java.nio.file.Path p) throws java.io.IOException {
        java.util.List<FraudCase> list = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(p, java.nio.charset.StandardCharsets.UTF_8)) {
            if (skip(line)) continue;
            java.util.List<String> f = splitCsv(line);
            if (f.size() < 7 || "FRAUD-CASE-ID".equals(f.get(0))) continue;
            list.add(new FraudCase(f.get(0), f.get(1), f.get(2), f.get(3),
                    Integer.parseInt(normalize(f.get(4))), f.get(5), f.get(6)));
        }
        return list;
    }

    private static void writeExceptions(java.nio.file.Path p, java.util.List<ExceptionRecord> rows)
            throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        lines.add("EXCEPTION-ID,SALE-ID,CARD-NO,REASON-CD,DETECTED-PGM,EXCEPTION-DT,ACTION-STATUS");
        for (ExceptionRecord e : rows) {
            lines.add(toCsv(e));
        }
        java.nio.file.Files.write(p, lines, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static String toCsv(ExceptionRecord e) {
        return joinCsv(e.exceptionId, e.saleId, e.cardNo, e.reasonCd, e.detectedPgm,
                e.exceptionDt, e.actionStatus);
    }

    private static String joinCsv(String... f) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < f.length; i++) {
            if (i > 0) b.append(',');
            String s = f[i] == null ? "" : f[i];
            if (s.indexOf(',') >= 0 || s.indexOf('"') >= 0 || s.indexOf('\n') >= 0) {
                b.append('"').append(s.replace("\"", "\"\"")).append('"');
            } else {
                b.append(s);
            }
        }
        return b.toString();
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (quoted) {
                if (ch == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        cur.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    cur.append(ch);
                }
            } else if (ch == ',') {
                out.add(cur.toString().trim());
                cur.setLength(0);
            } else if (ch == '"') {
                quoted = true;
            } else {
                cur.append(ch);
            }
        }
        out.add(cur.toString().trim());
        return out;
    }

    private static long yen(String s) {
        String n = normalize(s).replace(",", "");
        if (n.isEmpty()) return 0L;
        return Long.parseLong(n);
    }

    private static boolean skip(String line) {
        return line == null || line.trim().isEmpty() || line.trim().startsWith("#");
    }

    private static boolean notBlank(String s) {
        return s != null && !s.trim().isEmpty();
    }

    private static String normalize(String s) {
        return s == null ? "" : s.trim();
    }

    private static String maskCard(String cardNo) {
        String s = normalize(cardNo).replace("-", "");
        if (s.length() <= 8) return s;
        return s.substring(0, 6) + "******" + s.substring(s.length() - 4);
    }

    private static java.util.List<ChargebackCase> sampleChargebacks() {
        java.util.List<ChargebackCase> l = new java.util.ArrayList<>();
        l.add(new ChargebackCase("CB202606280001", "S202606270015", "4111111111111111", "M10001", 120000, "不正利用申立", "受付"));
        l.add(new ChargebackCase("CB202606280002", "S202606270016", "5555444433332222", "M20020", 48000, "商品未着", "受付"));
        l.add(new ChargebackCase("CB202606280003", "S202606270017", "3566002020360505", "M30030", 75000, "二重請求", "返品反映済"));
        l.add(new ChargebackCase("CB202606280004", "S202606270018", "4000000000000002", "M40040", 98000, "加盟店異議", "受付"));
        return l;
    }

    private static java.util.List<Capture> sampleCaptures() {
        java.util.List<Capture> l = new java.util.ArrayList<>();
        l.add(new Capture("S202606270015", "4111111111111111", 120000, 0, "JPY", "C", "CDCAPPF"));
        l.add(new Capture("S202606270016", "5555444433332222", 47000, 1300, "USD", "C", "CDCAPPF"));
        l.add(new Capture("S202606270017", "3566002020360505", 75000, 0, "JPY", "S", "CDCAPPF"));
        l.add(new Capture("S202606270018", "4000000000000002", 98000, 0, "JPY", "H", "CDCAPPF"));
        return l;
    }

    private static java.util.List<FraudCase> sampleFraudCases() {
        java.util.List<FraudCase> l = new java.util.ArrayList<>();
        l.add(new FraudCase("FR202606270301", "S202606270015", "4111111111111111", "M10001", 86, "IP距離", "調査中"));
        l.add(new FraudCase("FR202606270302", "S202606270016", "5555444433332222", "M20020", 42, "配送先変更", "監視"));
        return l;
    }

    private static final class ChargebackCase {
        final String chargebackId;
        final String saleId;
        final String cardNo;
        final String merchantCode;
        final long claimAmt;
        final String claimReason;
        final String caseStatus;

        ChargebackCase(String chargebackId, String saleId, String cardNo, String merchantCode,
                       long claimAmt, String claimReason, String caseStatus) {
            this.chargebackId = normalize(chargebackId);
            this.saleId = normalize(saleId);
            this.cardNo = normalize(cardNo);
            this.merchantCode = normalize(merchantCode);
            this.claimAmt = claimAmt;
            this.claimReason = normalize(claimReason);
            this.caseStatus = normalize(caseStatus);
        }
    }

    private static final class Capture {
        final String saleId;
        final String cardNo;
        final long billedAmt;
        final long feeAmt;
        final String currencyCd;
        final String capStatus;
        final String programId;

        Capture(String saleId, String cardNo, long billedAmt, long feeAmt,
                String currencyCd, String capStatus, String programId) {
            this.saleId = normalize(saleId);
            this.cardNo = normalize(cardNo);
            this.billedAmt = billedAmt;
            this.feeAmt = feeAmt;
            this.currencyCd = normalize(currencyCd);
            this.capStatus = normalize(capStatus);
            this.programId = normalize(programId);
        }
    }

    private static final class FraudCase {
        final String fraudCaseId;
        final String saleId;
        final String cardNo;
        final String merchantCode;
        final int riskScore;
        final String ruleHitCd;
        final String caseStatus;

        FraudCase(String fraudCaseId, String saleId, String cardNo, String merchantCode,
                  int riskScore, String ruleHitCd, String caseStatus) {
            this.fraudCaseId = normalize(fraudCaseId);
            this.saleId = normalize(saleId);
            this.cardNo = normalize(cardNo);
            this.merchantCode = normalize(merchantCode);
            this.riskScore = Math.max(0, Math.min(100, riskScore));
            this.ruleHitCd = normalize(ruleHitCd);
            this.caseStatus = normalize(caseStatus);
        }
    }

    private static final class ExceptionRecord {
        final String exceptionId;
        final String saleId;
        final String cardNo;
        final String reasonCd;
        final String detectedPgm;
        final String exceptionDt;
        final String actionStatus;

        ExceptionRecord(String exceptionId, String saleId, String cardNo, String reasonCd,
                        String detectedPgm, String exceptionDt, String actionStatus) {
            this.exceptionId = exceptionId;
            this.saleId = saleId;
            this.cardNo = cardNo;
            this.reasonCd = reasonCd;
            this.detectedPgm = detectedPgm;
            this.exceptionDt = exceptionDt;
            this.actionStatus = actionStatus;
        }
    }

    private static final class WorkflowCase {
        final String chargebackId;
        final String saleId;
        final String cardNo;
        final long claimAmt;
        final long billedAmt;
        final long feeAmt;
        final String currencyCd;
        final int riskScore;
        final String decisionCd;

        WorkflowCase(String chargebackId, String saleId, String cardNo, long claimAmt,
                     long billedAmt, long feeAmt, String currencyCd, int riskScore, String decisionCd) {
            this.chargebackId = chargebackId;
            this.saleId = saleId;
            this.cardNo = cardNo;
            this.claimAmt = claimAmt;
            this.billedAmt = billedAmt;
            this.feeAmt = feeAmt;
            this.currencyCd = currencyCd;
            this.riskScore = riskScore;
            this.decisionCd = decisionCd;
        }
    }

    private static final class Result {
        final java.util.List<WorkflowCase> workflowCases;
        final java.util.List<ExceptionRecord> exceptions;

        Result(java.util.List<WorkflowCase> workflowCases, java.util.List<ExceptionRecord> exceptions) {
            this.workflowCases = workflowCases;
            this.exceptions = exceptions;
        }
    }
}
