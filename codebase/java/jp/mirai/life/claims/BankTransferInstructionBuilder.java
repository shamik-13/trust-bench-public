package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-03-15  保険金システムG  初版作成
 */
public class BankTransferInstructionBuilder {
    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final int PAYMENT_RATIO_AFTER_ONE_YEAR = 100;
    private static final int KANA_LENGTH = 20;

    private static final java.util.Map<String, java.util.Set<String>> BANK_BRANCH_MASTER =
            new java.util.HashMap<String, java.util.Set<String>>();
    private static final java.util.Set<java.time.MonthDay> FIXED_HOLIDAYS =
            new java.util.HashSet<java.time.MonthDay>();

    static {
        BANK_BRANCH_MASTER.put("0001", setOf("001", "005", "101"));
        BANK_BRANCH_MASTER.put("0005", setOf("103", "118", "221"));
        BANK_BRANCH_MASTER.put("0009", setOf("015", "084", "201"));
        BANK_BRANCH_MASTER.put("0010", setOf("112", "214", "315"));

        FIXED_HOLIDAYS.add(java.time.MonthDay.of(1, 1));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(1, 2));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(1, 3));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(2, 11));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(2, 23));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(4, 29));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(5, 3));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(5, 4));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(5, 5));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(8, 11));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(11, 3));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(11, 23));
        FIXED_HOLIDAYS.add(java.time.MonthDay.of(12, 31));
    }

    public java.util.List<Lfxfrf> build(java.util.List<Lfbenf> beneficiaries, java.util.List<Lfpayf> payments) {
        java.util.Map<String, Lfbenf> beneficiaryById = new java.util.HashMap<String, Lfbenf>();
        for (Lfbenf beneficiary : beneficiaries) {
            beneficiaryById.put(beneficiary.beneficiaryId, beneficiary);
        }

        java.time.LocalDate transferDate = nextBusinessDay(java.time.LocalDate.now());
        java.util.List<Lfxfrf> transferRecords = new java.util.ArrayList<Lfxfrf>();

        for (Lfpayf payment : payments) {
            if (!CLAIM_STATUS_PAYABLE.equals(payment.claimStatusKbn)) {
                continue;
            }

            Lfbenf beneficiary = beneficiaryById.get(payment.beneficiaryId);
            if (beneficiary == null) {
                throw new IllegalStateException("受取人マスタ未登録: " + payment.beneficiaryId);
            }

            validateBankAndBranch(beneficiary.bankCd, beneficiary.branchCd);
            validateAccountNo(beneficiary.acctNo);
            validatePayoutAmount(payment.payoutAmt);

            transferRecords.add(new Lfxfrf(
                    "XF" + payment.payId.substring(Math.max(0, payment.payId.length() - 6)),
                    payment.payId,
                    beneficiary.bankCd,
                    beneficiary.branchCd,
                    beneficiary.acctNo,
                    toZenginKana20(beneficiary.nameKana),
                    formatAmount(payment.payoutAmt),
                    transferDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
            ));
        }

        return transferRecords;
    }

    private static java.util.Set<String> setOf(String... values) {
        java.util.Set<String> set = new java.util.HashSet<String>();
        java.util.Collections.addAll(set, values);
        return java.util.Collections.unmodifiableSet(set);
    }

    private static void validateBankAndBranch(String bankCd, String branchCd) {
        if (!bankCd.matches("\\d{4}")) {
            throw new IllegalArgumentException("金融機関コード形式不正: " + bankCd);
        }
        if (!branchCd.matches("\\d{3}")) {
            throw new IllegalArgumentException("支店コード形式不正: " + branchCd);
        }

        java.util.Set<String> branches = BANK_BRANCH_MASTER.get(bankCd);
        if (branches == null) {
            throw new IllegalArgumentException("金融機関コード未登録: " + bankCd);
        }
        if (!branches.contains(branchCd)) {
            throw new IllegalArgumentException("支店コード未登録: " + bankCd + "-" + branchCd);
        }
    }

    private static void validateAccountNo(String acctNo) {
        if (acctNo == null || !acctNo.matches("\\d{7}")) {
            throw new IllegalArgumentException("口座番号形式不正: " + acctNo);
        }
    }

    private static void validatePayoutAmount(long payoutAmt) {
        if (payoutAmt < 0 || payoutAmt > 9999999999L) {
            throw new IllegalArgumentException("支払金額範囲外: " + payoutAmt);
        }
    }

    private static String toZenginKana20(String nameKana) {
        if (nameKana == null || nameKana.trim().isEmpty()) {
            throw new IllegalArgumentException("口座名義カナ未設定");
        }

        String normalized = java.text.Normalizer.normalize(nameKana, java.text.Normalizer.Form.NFKC)
                .replace('　', ' ')
                .trim();

        StringBuilder kana = new StringBuilder();
        for (int i = 0; i < normalized.length(); i++) {
            char ch = normalized.charAt(i);
            if (ch >= 'ぁ' && ch <= 'ゖ') {
                ch = (char) (ch + ('ァ' - 'ぁ'));
            }
            if (isAllowedZenginKana(ch)) {
                kana.append(ch);
            } else if (ch == ' ') {
                kana.append(' ');
            } else {
                throw new IllegalArgumentException("口座名義カナ使用不可文字: " + ch);
            }
        }

        String value = kana.toString().replaceAll(" +", " ");
        if (value.length() > KANA_LENGTH) {
            value = value.substring(value.length() - KANA_LENGTH);
        }

        StringBuilder padded = new StringBuilder();
        for (int i = value.length(); i < KANA_LENGTH; i++) {
            padded.append(' ');
        }
        padded.append(value);
        return padded.toString();
    }

    private static boolean isAllowedZenginKana(char ch) {
        return (ch >= 'ァ' && ch <= 'ヶ')
                || (ch >= 'Ａ' && ch <= 'Ｚ')
                || (ch >= '０' && ch <= '９')
                || ch == 'ー'
                || ch == '（'
                || ch == '）'
                || ch == '．'
                || ch == '，'
                || ch == '／'
                || ch == '「'
                || ch == '」';
    }

    private static String formatAmount(long payoutAmt) {
        return String.format(java.util.Locale.ROOT, "%010d", payoutAmt);
    }

    private static java.time.LocalDate nextBusinessDay(java.time.LocalDate baseDate) {
        java.time.LocalDate date = baseDate.plusDays(1);
        while (!isBusinessDay(date)) {
            date = date.plusDays(1);
        }
        return date;
    }

    private static boolean isBusinessDay(java.time.LocalDate date) {
        java.time.DayOfWeek dayOfWeek = date.getDayOfWeek();
        if (dayOfWeek == java.time.DayOfWeek.SATURDAY || dayOfWeek == java.time.DayOfWeek.SUNDAY) {
            return false;
        }
        return !FIXED_HOLIDAYS.contains(java.time.MonthDay.from(date));
    }

    private static final class Lfbenf {
        private final String polNo;
        private final String beneficiaryId;
        private final String nameKana;
        private final String relationshipKbn;
        private final String bankCd;
        private final String branchCd;
        private final String acctNo;
        private final int paymentPriority;

        private Lfbenf(String polNo, String beneficiaryId, String nameKana, String relationshipKbn,
                       String bankCd, String branchCd, String acctNo, int paymentPriority) {
            this.polNo = polNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.paymentPriority = paymentPriority;
        }
    }

    private static final class Lfpayf {
        private final String payId;
        private final String claimId;
        private final String beneficiaryId;
        private final String claimStatusKbn;
        private final long grossAmt;
        private final int reductionRate;
        private final long payoutAmt;

        private Lfpayf(String payId, String claimId, String beneficiaryId, String claimStatusKbn,
                       long grossAmt, int reductionRate, long payoutAmt) {
            this.payId = payId;
            this.claimId = claimId;
            this.beneficiaryId = beneficiaryId;
            this.claimStatusKbn = claimStatusKbn;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
        }
    }

    private static final class Lfxfrf {
        private final String transferId;
        private final String payId;
        private final String bankCd;
        private final String branchCd;
        private final String acctNo;
        private final String acctHolderKna;
        private final String amount;
        private final String transferDt;

        private Lfxfrf(String transferId, String payId, String bankCd, String branchCd, String acctNo,
                       String acctHolderKna, String amount, String transferDt) {
            this.transferId = transferId;
            this.payId = payId;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.acctHolderKna = acctHolderKna;
            this.amount = amount;
            this.transferDt = transferDt;
        }

        private String toLine() {
            return transferId
                    + "," + payId
                    + "," + bankCd
                    + "," + branchCd
                    + "," + acctNo
                    + "," + acctHolderKna
                    + "," + amount
                    + "," + transferDt;
        }
    }
}
