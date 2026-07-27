public class PaymentPlanProjectionService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2019/02/18  開発部    リボ支払予定の見込み試算処理を新規作成
     * 1.01  2021/07/05  開発部    確定明細重複時の例外処理を追加
     * 1.02  2024/03/11  開発部    一時増枠を加味した利用可能リボ枠の算出に改定
     */

    private static final double MONTHLY_FEE_RATE = 0.0125d;

    public static java.util.List<ProjectionResult> project(
            java.util.List<Cdrsldf> statementFile,
            java.util.List<Cdrbalf> balanceFile,
            java.util.Map<String, Cdcourf> courseFile,
            java.util.Map<String, Cdlimtf> limitFile,
            String confirmedCycle) {

        if (statementFile == null || balanceFile == null || courseFile == null || limitFile == null) {
            throw new IllegalArgumentException("入力ファイルが未設定です");
        }
        if (confirmedCycle == null || !confirmedCycle.matches("[0-9]{6}")) {
            throw new IllegalArgumentException("対象年月が不正です");
        }

        java.util.Map<String, Cdrbalf> balances = new java.util.HashMap<String, Cdrbalf>();
        for (Cdrbalf b : balanceFile) {
            validateBalance(b);
            if (confirmedCycle.equals(b.cycleDt)) {
                balances.put(key(b.cardNo, b.cycleDt), b);
            }
        }

        java.util.List<ProjectionResult> out = new java.util.ArrayList<ProjectionResult>();
        java.util.Set<String> processed = new java.util.HashSet<String>();

        for (Cdrsldf s : statementFile) {
            validateStatement(s);
            if (!confirmedCycle.equals(s.cycleDt)) {
                continue;
            }

            String rowKey = key(s.cardNo, s.cycleDt);
            if (!processed.add(rowKey)) {
                throw new IllegalStateException("確定明細が重複しています: " + s.cardNo + "/" + s.cycleDt);
            }

            Cdrbalf b = balances.get(rowKey);
            if (b == null) {
                throw new IllegalStateException("残高明細が未登録です: " + s.cardNo + "/" + s.cycleDt);
            }

            Cdlimtf limit = limitFile.get(s.cardNo);
            if (limit == null) {
                throw new IllegalStateException("限度額明細が未登録です: " + s.cardNo);
            }
            validateLimit(limit);

            boolean active = "01".equals(limit.limitStatus);
            if (!active) {
                out.add(ProjectionResult.skipped(s.cardNo, s.cycleDt, b.revBalAmt));
                continue;
            }

            Cdcourf course = courseFile.get(s.slideTier);
            if (course == null) {
                throw new IllegalStateException("コース明細が未登録です: " + s.slideTier);
            }
            validateCourse(course);

            if (!"01".equals(course.courseStatus) || !"C".equals(s.rsldStatus)) {
                out.add(ProjectionResult.skipped(s.cardNo, s.cycleDt, b.revBalAmt));
                continue;
            }

            long recomputedFee = floorFee(b.revBalAmt);
            long feeAmt = s.feeAmt;
            if (feeAmt != recomputedFee) {
                feeAmt = recomputedFee;
            }

            long billedPrincipal = Math.max(0L, s.prinAmt);
            long billedPay = billedPrincipal + feeAmt;
            long availableRevolvingLimit = Math.max(0L, limit.revLimitAmt + limit.tempLimitAmt - limit.usedAmt);
            long nextBalanceLow = Math.max(0L, b.revBalAmt + b.newRevAmt - billedPrincipal);
            long nextBalanceHigh = Math.max(nextBalanceLow, b.revBalAmt + b.newRevAmt + b.carriedFeeAmt);
            long nextPayLow = Math.max(course.minPayAmt, billedPrincipal);
            long nextPayHigh = Math.max(nextPayLow, billedPay + floorFee(nextBalanceHigh));

            out.add(new ProjectionResult(
                    s.cardNo,
                    s.cycleDt,
                    "C",
                    b.revBalAmt,
                    billedPrincipal,
                    feeAmt,
                    billedPay,
                    availableRevolvingLimit,
                    course.courseName,
                    s.slideTier,
                    nextBalanceLow,
                    nextBalanceHigh,
                    nextPayLow,
                    nextPayHigh));
        }

        java.util.Collections.sort(out);
        return out;
    }

    private static long floorFee(long revolvingBalance) {
        return (long) Math.floor(revolvingBalance * MONTHLY_FEE_RATE);
    }

    private static String key(String cardNo, String cycleDt) {
        return cardNo + "|" + cycleDt;
    }

    private static void validateStatement(Cdrsldf s) {
        if (s == null) {
            throw new IllegalArgumentException("支払明細が未設定です");
        }
        requireCardNo(s.cardNo);
        requireCycle(s.cycleDt);
        requireNonNegative(s.prinAmt, "請求済み元金");
        requireNonNegative(s.feeAmt, "手数料");
        requireNonNegative(s.payAmt, "支払額");
        if (!isSlideTier(s.slideTier)) {
            throw new IllegalArgumentException("区分が不正です: " + s.slideTier);
        }
        if (!"C".equals(s.rsldStatus) && !"S".equals(s.rsldStatus)) {
            throw new IllegalArgumentException("確定状態が不正です: " + s.rsldStatus);
        }
        if (s.programId == null || s.programId.trim().isEmpty()) {
            throw new IllegalArgumentException("プログラムIDが未設定です");
        }
    }

    private static void validateBalance(Cdrbalf b) {
        if (b == null) {
            throw new IllegalArgumentException("残高明細が未設定です");
        }
        requireCardNo(b.cardNo);
        requireCycle(b.cycleDt);
        requireNonNegative(b.revBalAmt, "リボ残高");
        requireNonNegative(b.carriedFeeAmt, "繰越手数料");
        requireNonNegative(b.newRevAmt, "新規リボ利用額");
    }

    private static void validateCourse(Cdcourf c) {
        if (c == null) {
            throw new IllegalArgumentException("コース明細が未設定です");
        }
        if (!isSlideTier(c.courseCd)) {
            throw new IllegalArgumentException("コースコードが不正です: " + c.courseCd);
        }
        if (c.courseName == null || c.courseName.trim().isEmpty()) {
            throw new IllegalArgumentException("コース名称が未設定です");
        }
        if (c.feeRate < 0.0d) {
            throw new IllegalArgumentException("手数料率が不正です");
        }
        requireNonNegative(c.minPayAmt, "最低支払額");
        if (!"01".equals(c.courseStatus) && !"02".equals(c.courseStatus) && !"03".equals(c.courseStatus)) {
            throw new IllegalArgumentException("コース状態が不正です: " + c.courseStatus);
        }
    }

    private static void validateLimit(Cdlimtf l) {
        if (l == null) {
            throw new IllegalArgumentException("限度額明細が未設定です");
        }
        requireCardNo(l.cardNo);
        requireNonNegative(l.totalLimitAmt, "総枠");
        requireNonNegative(l.revLimitAmt, "リボ枠");
        requireNonNegative(l.usedAmt, "利用額");
        requireNonNegative(l.tempLimitAmt, "一時増枠");
        if (!"01".equals(l.limitStatus) && !"02".equals(l.limitStatus) && !"03".equals(l.limitStatus)) {
            throw new IllegalArgumentException("リボ状態が不正です: " + l.limitStatus);
        }
    }

    private static void requireCardNo(String cardNo) {
        if (cardNo == null || !cardNo.matches("[0-9]{16}")) {
            throw new IllegalArgumentException("カード番号が不正です");
        }
    }

    private static void requireCycle(String cycleDt) {
        if (cycleDt == null || !cycleDt.matches("[0-9]{6}")) {
            throw new IllegalArgumentException("年月が不正です");
        }
    }

    private static void requireNonNegative(long value, String name) {
        if (value < 0L) {
            throw new IllegalArgumentException(name + "がマイナスです");
        }
    }

    private static boolean isSlideTier(String v) {
        return "T1".equals(v) || "T2".equals(v) || "T3".equals(v) || "T4".equals(v);
    }

    public static final class Cdrsldf {
        public final String cardNo;
        public final String cycleDt;
        public final long prinAmt;
        public final long feeAmt;
        public final long payAmt;
        public final String slideTier;
        public final String rsldStatus;
        public final String programId;

        public Cdrsldf(String cardNo, String cycleDt, long prinAmt, long feeAmt, long payAmt,
                       String slideTier, String rsldStatus, String programId) {
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

    public static final class Cdrbalf {
        public final String cardNo;
        public final String cycleDt;
        public final long revBalAmt;
        public final long carriedFeeAmt;
        public final long newRevAmt;

        public Cdrbalf(String cardNo, String cycleDt, long revBalAmt, long carriedFeeAmt, long newRevAmt) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.revBalAmt = revBalAmt;
            this.carriedFeeAmt = carriedFeeAmt;
            this.newRevAmt = newRevAmt;
        }
    }

    public static final class Cdcourf {
        public final String courseCd;
        public final String courseName;
        public final double feeRate;
        public final long minPayAmt;
        public final String courseStatus;

        public Cdcourf(String courseCd, String courseName, double feeRate, long minPayAmt, String courseStatus) {
            this.courseCd = courseCd;
            this.courseName = courseName;
            this.feeRate = feeRate;
            this.minPayAmt = minPayAmt;
            this.courseStatus = courseStatus;
        }
    }

    public static final class Cdlimtf {
        public final String cardNo;
        public final long totalLimitAmt;
        public final long revLimitAmt;
        public final long usedAmt;
        public final long tempLimitAmt;
        public final String limitStatus;

        public Cdlimtf(String cardNo, long totalLimitAmt, long revLimitAmt, long usedAmt,
                       long tempLimitAmt, String limitStatus) {
            this.cardNo = cardNo;
            this.totalLimitAmt = totalLimitAmt;
            this.revLimitAmt = revLimitAmt;
            this.usedAmt = usedAmt;
            this.tempLimitAmt = tempLimitAmt;
            this.limitStatus = limitStatus;
        }
    }

    public static final class ProjectionResult implements Comparable<ProjectionResult> {
        public final String cardNo;
        public final String cycleDt;
        public final String rsldStatus;
        public final long revolvingBalance;
        public final long billedPrincipal;
        public final long feeAmount;
        public final long payAmount;
        public final long availableRevolvingLimit;
        public final String courseName;
        public final String slideTier;
        public final long nextBalanceLow;
        public final long nextBalanceHigh;
        public final long nextPayLow;
        public final long nextPayHigh;

        public ProjectionResult(String cardNo, String cycleDt, String rsldStatus, long revolvingBalance,
                                long billedPrincipal, long feeAmount, long payAmount,
                                long availableRevolvingLimit, String courseName, String slideTier,
                                long nextBalanceLow, long nextBalanceHigh, long nextPayLow, long nextPayHigh) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.rsldStatus = rsldStatus;
            this.revolvingBalance = revolvingBalance;
            this.billedPrincipal = billedPrincipal;
            this.feeAmount = feeAmount;
            this.payAmount = payAmount;
            this.availableRevolvingLimit = availableRevolvingLimit;
            this.courseName = courseName;
            this.slideTier = slideTier;
            this.nextBalanceLow = nextBalanceLow;
            this.nextBalanceHigh = nextBalanceHigh;
            this.nextPayLow = nextPayLow;
            this.nextPayHigh = nextPayHigh;
        }

        public static ProjectionResult skipped(String cardNo, String cycleDt, long revolvingBalance) {
            return new ProjectionResult(cardNo, cycleDt, "S", revolvingBalance, 0L, 0L, 0L,
                    0L, "", "", 0L, 0L, 0L, 0L);
        }

        public String toDisplayLine() {
            return "カード番号=" + cardNo
                    + ", 年月=" + cycleDt
                    + ", 状態=" + rsldStatus
                    + ", 残高=" + revolvingBalance
                    + ", 請求済み元金=" + billedPrincipal
                    + ", 手数料=" + feeAmount
                    + ", 支払額=" + payAmount
                    + ", 利用可能リボ枠=" + availableRevolvingLimit
                    + ", 翌月残高見込=" + nextBalanceLow + "から" + nextBalanceHigh
                    + ", 翌月支払見込=" + nextPayLow + "から" + nextPayHigh;
        }

        @Override
        public int compareTo(ProjectionResult other) {
            int c = this.cardNo.compareTo(other.cardNo);
            if (c != 0) {
                return c;
            }
            return this.cycleDt.compareTo(other.cycleDt);
        }
    }
}
