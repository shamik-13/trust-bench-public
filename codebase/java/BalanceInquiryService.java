/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/01/29  開発担当  残高照会サービス初版作成。CDOSF残高にCDLATEFの最新延滞利息を加算し直近消込を併記する。
 * 1.01  2024/06/11  決済運用  直近消込の未確定表示判定を追加。
 */
public class BalanceInquiryService {
    private static final java.math.RoundingMode 通貨丸め = java.math.RoundingMode.HALF_UP;
    private static final java.time.format.DateTimeFormatter 日時書式 =
            java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    private final java.util.Map<String, CDOSFRecord> cdosf;
    private final java.util.Map<String, java.util.List<CDLATEFRecord>> cdlatef;
    private final java.util.List<CDAPPFRecord> cdappf;

    public BalanceInquiryService() {
        this.cdosf = 作成CDOSF();
        this.cdlatef = 作成CDLATEF();
        this.cdappf = 作成CDAPPF();
    }

    public static void main(String[] a) {
        BalanceInquiryService service = new BalanceInquiryService();
        String cardNo = a.length == 0 ? "4900000000000001" : a[0];
        BalanceResponse response = service.inquire(cardNo);
        System.out.println(response.toOperatorText());
    }

    public BalanceResponse inquire(String cardNo) {
        String normalizedCardNo = normalizeCardNo(cardNo);
        CDOSFRecord balance = cdosf.get(normalizedCardNo);
        if (balance == null) {
            throw new IllegalArgumentException("カード番号が登録されていません: " + maskCardNo(normalizedCardNo));
        }

        java.math.BigDecimal fee = money(balance.feeBalAmt);
        java.math.BigDecimal interest = money(balance.interestBalAmt);
        java.math.BigDecimal principal = money(balance.principalBalAmt);
        java.math.BigDecimal lateInterest = 最新延滞利息(normalizedCardNo, balance.cycleDt);
        java.math.BigDecimal currentBalance = fee.add(interest).add(principal).add(lateInterest).setScale(0, 通貨丸め);

        ApplySummary summary = 直近消込(normalizedCardNo);
        boolean unsettled = summary.latestAppliedAt != null
                && !summary.latestAppliedAt.isBefore(java.time.LocalDateTime.now().minusMinutes(20))
                && !"F".equals(summary.status);

        return new BalanceResponse(
                normalizedCardNo,
                balance.cycleDt,
                currentBalance,
                fee,
                interest,
                principal,
                lateInterest,
                summary,
                unsettled
        );
    }

    private static String normalizeCardNo(String cardNo) {
        if (cardNo == null) {
            throw new IllegalArgumentException("カード番号が未指定です");
        }
        String normalized = cardNo.replaceAll("[^0-9]", "");
        if (normalized.length() != 16) {
            throw new IllegalArgumentException("カード番号は16桁で指定してください");
        }
        return normalized;
    }

    private java.math.BigDecimal 最新延滞利息(String cardNo, java.time.LocalDate cycleDt) {
        return cdlatef.getOrDefault(cardNo, java.util.Collections.emptyList()).stream()
                .filter(r -> !r.cycleDt.isAfter(cycleDt))
                .max(java.util.Comparator.comparing((CDLATEFRecord r) -> r.calcDt)
                        .thenComparing(r -> r.cycleDt))
                .map(r -> money(r.lateInterestAmt))
                .orElse(java.math.BigDecimal.ZERO.setScale(0, 通貨丸め));
    }

    private ApplySummary 直近消込(String cardNo) {
        java.util.List<CDAPPFRecord> rows = new java.util.ArrayList<>();
        for (CDAPPFRecord row : cdappf) {
            if (row.cardNo.equals(cardNo) && !"S".equals(row.appStatus)) {
                rows.add(row);
            }
        }
        rows.sort(java.util.Comparator.comparing((CDAPPFRecord r) -> r.appliedAt).reversed()
                .thenComparing(r -> r.payId));

        if (rows.isEmpty()) {
            return new ApplySummary("", "", null, java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO,
                    java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO);
        }

        CDAPPFRecord latest = rows.get(0);
        java.math.BigDecimal fee = java.math.BigDecimal.ZERO;
        java.math.BigDecimal interest = java.math.BigDecimal.ZERO;
        java.math.BigDecimal principal = java.math.BigDecimal.ZERO;
        java.math.BigDecimal remain = java.math.BigDecimal.ZERO;

        for (CDAPPFRecord row : rows) {
            if (!row.payId.equals(latest.payId)) {
                continue;
            }
            fee = fee.add(money(row.appliedFeeAmt));
            interest = interest.add(money(row.appliedIntAmt));
            principal = principal.add(money(row.appliedPrinAmt));
            remain = remain.add(money(row.remainAmt));
        }

        return new ApplySummary(latest.payId, latest.appStatus, latest.appliedAt, fee, interest, principal, remain);
    }

    private static java.math.BigDecimal money(String value) {
        return new java.math.BigDecimal(value).setScale(0, 通貨丸め);
    }

    private static String maskCardNo(String cardNo) {
        return cardNo.substring(0, 6) + "******" + cardNo.substring(12);
    }

    private static java.util.Map<String, CDOSFRecord> 作成CDOSF() {
        java.util.Map<String, CDOSFRecord> rows = new java.util.LinkedHashMap<>();
        rows.put("4900000000000001", new CDOSFRecord("4900000000000001", "1200.4", "3480.5", "183420.0",
                java.time.LocalDate.of(2026, 6, 25)));
        rows.put("4900000000000002", new CDOSFRecord("4900000000000002", "0", "0", "0",
                java.time.LocalDate.of(2026, 6, 25)));
        rows.put("4900000000000003", new CDOSFRecord("4900000000000003", "660.0", "1050.2", "78200.8",
                java.time.LocalDate.of(2026, 6, 24)));
        return rows;
    }

    private static java.util.Map<String, java.util.List<CDLATEFRecord>> 作成CDLATEF() {
        java.util.Map<String, java.util.List<CDLATEFRecord>> rows = new java.util.LinkedHashMap<>();
        rows.put("4900000000000001", java.util.Arrays.asList(
                new CDLATEFRecord("4900000000000001", java.time.LocalDate.of(2026, 6, 20), 5,
                        "825.4", "165080", java.time.LocalDate.of(2026, 6, 21)),
                new CDLATEFRecord("4900000000000001", java.time.LocalDate.of(2026, 6, 25), 10,
                        "1668.5", "183420", java.time.LocalDate.of(2026, 6, 26))
        ));
        rows.put("4900000000000003", java.util.Collections.singletonList(
                new CDLATEFRecord("4900000000000003", java.time.LocalDate.of(2026, 6, 24), 3,
                        "193.4", "78200", java.time.LocalDate.of(2026, 6, 25))
        ));
        return rows;
    }

    private static java.util.List<CDAPPFRecord> 作成CDAPPF() {
        java.time.LocalDateTime now = java.time.LocalDateTime.now();
        java.util.List<CDAPPFRecord> rows = new java.util.ArrayList<>();
        rows.add(new CDAPPFRecord("PY202606280001", "4900000000000001", "0", "2000", "40000",
                "0", "P", "CDPAYAP", now.minusMinutes(8)));
        rows.add(new CDAPPFRecord("PY202606250031", "4900000000000001", "1200", "1480", "15000",
                "0", "P", "CDPAYAP", java.time.LocalDateTime.of(2026, 6, 25, 2, 14, 3)));
        rows.add(new CDAPPFRecord("PY202606250040", "4900000000000002", "0", "0", "0",
                "3000", "O", "CDPAYAP", java.time.LocalDateTime.of(2026, 6, 25, 2, 18, 44)));
        rows.add(new CDAPPFRecord("PY202606240021", "4900000000000003", "660", "1050", "78201",
                "0", "F", "CDPAYAP", java.time.LocalDateTime.of(2026, 6, 24, 2, 5, 12)));
        rows.add(new CDAPPFRecord("PY202606230010", "4900000000000003", "0", "0", "0",
                "0", "S", "CDPAYEX", java.time.LocalDateTime.of(2026, 6, 23, 23, 50, 0)));
        return rows;
    }

    private static final class CDOSFRecord {
        final String cardNo;
        final String feeBalAmt;
        final String interestBalAmt;
        final String principalBalAmt;
        final java.time.LocalDate cycleDt;

        CDOSFRecord(String cardNo, String feeBalAmt, String interestBalAmt, String principalBalAmt,
                    java.time.LocalDate cycleDt) {
            this.cardNo = cardNo;
            this.feeBalAmt = feeBalAmt;
            this.interestBalAmt = interestBalAmt;
            this.principalBalAmt = principalBalAmt;
            this.cycleDt = cycleDt;
        }
    }

    private static final class CDLATEFRecord {
        final String cardNo;
        final java.time.LocalDate cycleDt;
        final int delinqDays;
        final String lateInterestAmt;
        final String calcBaseAmt;
        final java.time.LocalDate calcDt;

        CDLATEFRecord(String cardNo, java.time.LocalDate cycleDt, int delinqDays, String lateInterestAmt,
                      String calcBaseAmt, java.time.LocalDate calcDt) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.delinqDays = delinqDays;
            this.lateInterestAmt = lateInterestAmt;
            this.calcBaseAmt = calcBaseAmt;
            this.calcDt = calcDt;
        }
    }

    private static final class CDAPPFRecord {
        final String payId;
        final String cardNo;
        final String appliedFeeAmt;
        final String appliedIntAmt;
        final String appliedPrinAmt;
        final String remainAmt;
        final String appStatus;
        final String programId;
        final java.time.LocalDateTime appliedAt;

        CDAPPFRecord(String payId, String cardNo, String appliedFeeAmt, String appliedIntAmt,
                     String appliedPrinAmt, String remainAmt, String appStatus, String programId,
                     java.time.LocalDateTime appliedAt) {
            this.payId = payId;
            this.cardNo = cardNo;
            this.appliedFeeAmt = appliedFeeAmt;
            this.appliedIntAmt = appliedIntAmt;
            this.appliedPrinAmt = appliedPrinAmt;
            this.remainAmt = remainAmt;
            this.appStatus = appStatus;
            this.programId = programId;
            this.appliedAt = appliedAt;
        }
    }

    private static final class ApplySummary {
        final String payId;
        final String status;
        final java.time.LocalDateTime latestAppliedAt;
        final java.math.BigDecimal appliedFee;
        final java.math.BigDecimal appliedInterest;
        final java.math.BigDecimal appliedPrincipal;
        final java.math.BigDecimal remainAmount;

        ApplySummary(String payId, String status, java.time.LocalDateTime latestAppliedAt,
                     java.math.BigDecimal appliedFee, java.math.BigDecimal appliedInterest,
                     java.math.BigDecimal appliedPrincipal, java.math.BigDecimal remainAmount) {
            this.payId = payId;
            this.status = status;
            this.latestAppliedAt = latestAppliedAt;
            this.appliedFee = appliedFee;
            this.appliedInterest = appliedInterest;
            this.appliedPrincipal = appliedPrincipal;
            this.remainAmount = remainAmount;
        }
    }

    private static final class BalanceResponse {
        final String cardNo;
        final java.time.LocalDate cycleDt;
        final java.math.BigDecimal currentBalance;
        final java.math.BigDecimal feeBalance;
        final java.math.BigDecimal interestBalance;
        final java.math.BigDecimal principalBalance;
        final java.math.BigDecimal lateInterest;
        final ApplySummary applySummary;
        final boolean unsettled;

        BalanceResponse(String cardNo, java.time.LocalDate cycleDt, java.math.BigDecimal currentBalance,
                        java.math.BigDecimal feeBalance, java.math.BigDecimal interestBalance,
                        java.math.BigDecimal principalBalance, java.math.BigDecimal lateInterest,
                        ApplySummary applySummary, boolean unsettled) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.currentBalance = currentBalance;
            this.feeBalance = feeBalance;
            this.interestBalance = interestBalance;
            this.principalBalance = principalBalance;
            this.lateInterest = lateInterest;
            this.applySummary = applySummary;
            this.unsettled = unsettled;
        }

        String toOperatorText() {
            String appliedAt = applySummary.latestAppliedAt == null ? "" : applySummary.latestAppliedAt.format(日時書式);
            return "残高照会応答"
                    + "\nカード番号=" + maskCardNo(cardNo)
                    + "\n請求サイクル日=" + cycleDt
                    + "\n現残高=" + currentBalance
                    + "\n手数料残高=" + feeBalance
                    + "\n利息残高=" + interestBalance
                    + "\n元本残高=" + principalBalance
                    + "\n延滞利息=" + lateInterest
                    + "\n直近消込ID=" + applySummary.payId
                    + "\n直近消込状態=" + applySummary.status
                    + "\n直近処理日時=" + appliedAt
                    + "\n消込手数料=" + applySummary.appliedFee
                    + "\n消込利息=" + applySummary.appliedInterest
                    + "\n消込元本=" + applySummary.appliedPrincipal
                    + "\n残余入金=" + applySummary.remainAmount
                    + "\n未確定表示=" + (unsettled ? "対象" : "対象外");
        }
    }
}
