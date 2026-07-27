package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.00  2024/03/15  保険金システムG  源泉徴収税額計算サービスの初版作成
 */
public class WithholdingTaxCalculator {
    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final String RELATIONSHIP_DIRECT_ASCENDANT = "01";
    private static final String TAX_EXEMPT = "1";
    private static final String TAXABLE = "0";
    private static final java.math.BigDecimal ONE_YEAR_OR_MORE_PAYMENT_RATE = new java.math.BigDecimal("1.00");
    private static final java.math.BigDecimal ARTICLE_207_TAX_RATE = new java.math.BigDecimal("0.2042");

    private static final java.util.Map<String, java.math.BigDecimal> EXEMPT_THRESHOLD_BY_RELATIONSHIP;
    static {
        java.util.Map<String, java.math.BigDecimal> table = new java.util.HashMap<>();
        table.put("01", new java.math.BigDecimal("0"));
        table.put("02", new java.math.BigDecimal("500000"));
        table.put("03", new java.math.BigDecimal("300000"));
        table.put("04", new java.math.BigDecimal("100000"));
        table.put("99", new java.math.BigDecimal("0"));
        EXEMPT_THRESHOLD_BY_RELATIONSHIP = java.util.Collections.unmodifiableMap(table);
    }

    private static java.util.List<WithholdingTaxRecord> calculate(
            java.util.Collection<?> lfpays,
            java.util.Collection<?> lfbenfs,
            java.util.Collection<?> lfclmfs,
            java.time.LocalDate processDate) {

        if (lfpays == null || lfbenfs == null || lfclmfs == null || processDate == null) {
            throw new IllegalArgumentException("入力レコードまたは処理日が未設定です。");
        }

        java.util.Map<String, Object> claimById = new java.util.HashMap<>();
        for (Object claim : lfclmfs) {
            String claimId = text(claim, "claimId", "CLAIM_ID");
            require(claimId, "CLAIM-ID");
            claimById.put(claimId, claim);
        }

        java.util.Map<String, java.util.List<Object>> beneficiariesByPolicy = new java.util.HashMap<>();
        for (Object beneficiary : lfbenfs) {
            String polNo = text(beneficiary, "polNo", "POL_NO");
            require(polNo, "POL-NO");
            beneficiariesByPolicy.computeIfAbsent(polNo, k -> new java.util.ArrayList<>()).add(beneficiary);
        }
        for (java.util.List<Object> beneficiaries : beneficiariesByPolicy.values()) {
            beneficiaries.sort(java.util.Comparator.comparingInt(b -> number(b, "paymentPriority", "PAYMENT_PRIORITY").intValue()));
        }

        java.util.List<WithholdingTaxRecord> writes = new java.util.ArrayList<>();
        java.util.Map<String, java.math.BigDecimal> taxableTotalByBeneficiary = new java.util.HashMap<>();

        for (Object pay : lfpays) {
            String payId = text(pay, "payId", "PAY_ID");
            String claimId = text(pay, "claimId", "CLAIM_ID");
            require(payId, "PAY-ID");
            require(claimId, "CLAIM-ID");

            Object claim = claimById.get(claimId);
            if (claim == null) {
                throw new IllegalStateException("請求レコードが見つかりません。CLAIM-ID=" + claimId);
            }

            String status = text(claim, "claimStatusKbn", "CLAIM_STATUS_KBN");
            if (!CLAIM_STATUS_PAYABLE.equals(status)) {
                continue;
            }

            String polNo = text(claim, "polNo", "POL_NO");
            require(polNo, "POL-NO");
            java.util.List<Object> beneficiaries = beneficiariesByPolicy.get(polNo);
            if (beneficiaries == null || beneficiaries.isEmpty()) {
                throw new IllegalStateException("受取人レコードが見つかりません。POL-NO=" + polNo);
            }

            Object beneficiary = beneficiaries.get(0);
            String beneficiaryId = text(beneficiary, "beneficiaryId", "BENEFICIARY_ID");
            String relationship = text(beneficiary, "relationshipKbn", "RELATIONSHIP_KBN");
            require(beneficiaryId, "BENEFICIARY-ID");
            require(relationship, "RELATIONSHIP-KBN");

            java.math.BigDecimal payoutAmount = money(pay, "payoutAmt", "PAYOUT_AMT");
            validatePayout(pay, payoutAmount);

            String taxExemptFlag = RELATIONSHIP_DIRECT_ASCENDANT.equals(relationship) ? TAX_EXEMPT : TAXABLE;
            java.math.BigDecimal taxableAmount = java.math.BigDecimal.ZERO;
            java.math.BigDecimal taxAmount = java.math.BigDecimal.ZERO;

            if (TAXABLE.equals(taxExemptFlag)) {
                java.math.BigDecimal threshold = EXEMPT_THRESHOLD_BY_RELATIONSHIP.getOrDefault(
                        relationship, EXEMPT_THRESHOLD_BY_RELATIONSHIP.get("99"));
                taxableAmount = payoutAmount.subtract(threshold).max(java.math.BigDecimal.ZERO)
                        .setScale(0, java.math.RoundingMode.DOWN);
                taxAmount = taxableAmount.multiply(ARTICLE_207_TAX_RATE)
                        .setScale(0, java.math.RoundingMode.DOWN);
            }

            taxableTotalByBeneficiary.merge(beneficiaryId, taxableAmount, java.math.BigDecimal::add);
            writes.add(new WithholdingTaxRecord(
                    reportId(processDate, payId),
                    payId,
                    beneficiaryId,
                    taxableAmount,
                    taxAmount,
                    String.valueOf(processDate.getYear()),
                    taxExemptFlag));
        }

        for (java.util.Map.Entry<String, java.math.BigDecimal> entry : taxableTotalByBeneficiary.entrySet()) {
            if (entry.getValue().signum() < 0) {
                throw new IllegalStateException("課税対象額集計が不正です。BENEFICIARY-ID=" + entry.getKey());
            }
        }

        return java.util.Collections.unmodifiableList(writes);
    }

    private static void validatePayout(Object pay, java.math.BigDecimal payoutAmount) {
        java.math.BigDecimal reductionRate = money(pay, "reductionRate", "REDUCTION_RATE");
        if (payoutAmount.signum() < 0) {
            throw new IllegalArgumentException("支払額が不正です。");
        }
        if (reductionRate.compareTo(java.math.BigDecimal.ZERO) < 0
                || reductionRate.compareTo(ONE_YEAR_OR_MORE_PAYMENT_RATE) > 0) {
            throw new IllegalArgumentException("支払削減割合が不正です。");
        }
    }

    private static String reportId(java.time.LocalDate processDate, String payId) {
        return "WT" + processDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE) + "-" + payId;
    }

    private static void require(String value, String itemName) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(itemName + "が未設定です。");
        }
    }

    private static String text(Object record, String camelName, String upperName) {
        Object value = value(record, camelName, upperName);
        return value == null ? null : String.valueOf(value).trim();
    }

    private static java.math.BigDecimal money(Object record, String camelName, String upperName) {
        Object value = value(record, camelName, upperName);
        if (value == null) {
            throw new IllegalArgumentException(upperName + "が未設定です。");
        }
        if (value instanceof java.math.BigDecimal) {
            return (java.math.BigDecimal) value;
        }
        if (value instanceof Number) {
            return new java.math.BigDecimal(value.toString());
        }
        return new java.math.BigDecimal(String.valueOf(value).trim());
    }

    private static Number number(Object record, String camelName, String upperName) {
        Object value = value(record, camelName, upperName);
        if (value == null) {
            throw new IllegalArgumentException(upperName + "が未設定です。");
        }
        if (value instanceof Number) {
            return (Number) value;
        }
        return Integer.valueOf(String.valueOf(value).trim());
    }

    private static Object value(Object record, String camelName, String upperName) {
        if (record == null) {
            throw new IllegalArgumentException("入力レコードが未設定です。");
        }
        for (String name : new String[] {camelName, upperName, lowerSnake(upperName)}) {
            try {
                java.lang.reflect.Method method = record.getClass().getMethod(name);
                return method.invoke(record);
            } catch (ReflectiveOperationException ignored) {
                // ClaimModelの命名差異を吸収する。
            }
        }
        throw new IllegalArgumentException(upperName + "を参照できません。");
    }

    private static String lowerSnake(String upperName) {
        return upperName.toLowerCase(java.util.Locale.ROOT).replace('-', '_');
    }

    private static final class WithholdingTaxRecord {
        private final String reportId;
        private final String payId;
        private final String beneficiaryId;
        private final java.math.BigDecimal taxableAmount;
        private final java.math.BigDecimal taxAmount;
        private final String taxYear;
        private final String taxExemptFlag;

        private WithholdingTaxRecord(
                String reportId,
                String payId,
                String beneficiaryId,
                java.math.BigDecimal taxableAmount,
                java.math.BigDecimal taxAmount,
                String taxYear,
                String taxExemptFlag) {
            this.reportId = reportId;
            this.payId = payId;
            this.beneficiaryId = beneficiaryId;
            this.taxableAmount = taxableAmount;
            this.taxAmount = taxAmount;
            this.taxYear = taxYear;
            this.taxExemptFlag = taxExemptFlag;
        }

        @Override
        public String toString() {
            return reportId + "," + payId + "," + beneficiaryId + "," + taxableAmount
                    + "," + taxAmount + "," + taxYear + "," + taxExemptFlag;
        }
    }
}
