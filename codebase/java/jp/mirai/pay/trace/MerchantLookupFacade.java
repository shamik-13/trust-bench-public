package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2024/12/10  みらいペイ システム部 精算・連携チーム  初版作成。加盟店照会の事前判定版を作成。
 */
public class MerchantLookupFacade {
    private static final java.time.format.DateTimeFormatter DATE_FMT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
    private static final int MERCHANT_CODE_LENGTH = 10;

    public void run() {
        java.util.List<Pjmstf> pjmstf = java.util.Arrays.asList(
                new Pjmstf("M000000001", "青葉文具店", "0005", "123456789012", true, "B"),
                new Pjmstf("M000000002", "みらい珈琲", "0001", "998877665544", true, "A"),
                new Pjmstf("M000000003", "東都雑貨", "0138", "111122223333", false, "C"),
                new Pjmstf("M000000004", "北浜薬局", "0009", "444455556666", true, "D"),
                new Pjmstf("M000000005", "七福食堂", "0033", "777788889999", true, "B")
        );

        java.util.List<Pcsumf> pcsumf = java.util.Arrays.asList(
                new Pcsumf("M000000001", date("20241125"), "10", 42, 318500L, 0L),
                new Pcsumf("M000000001", date("20241126"), "10", 38, 291000L, 12000L),
                new Pcsumf("M000000001", date("20241126"), "20", 4, -9200L, 0L),
                new Pcsumf("M000000002", date("20241126"), "10", 91, 812300L, 0L),
                new Pcsumf("M000000003", date("20241124"), "10", 17, 140000L, 5000L),
                new Pcsumf("M000000004", date("20241126"), "10", 6, 75500L, 38000L),
                new Pcsumf("M000000005", date("20241125"), "10", 23, 166400L, 0L)
        );

        java.util.List<Pccarf> pccarf = java.util.Arrays.asList(
                new Pccarf("C202411260001", "M000000001", "10", 12000L, "証跡確認待ち", date("20241130")),
                new Pccarf("C202411260002", "M000000004", "10", 38000L, "口座照合保留", date("20241201")),
                new Pccarf("C202411240009", "M000000003", "10", 5000L, "停止先残高", date("20241130"))
        );

        java.util.Set<String> cIndex = new java.util.LinkedHashSet<String>();
        cIndex.add("M000000001");
        cIndex.add("M000000004");
        cIndex.add("M000000009");

        for (String merchantCode : cIndex) {
            LookupResponse response = lookup(merchantCode, cIndex, pjmstf, pcsumf, pccarf);
            System.out.println(response.toOperatorLine());
        }
    }

    private LookupResponse lookup(String merchantCode,
                                  java.util.Set<String> cIndex,
                                  java.util.List<Pjmstf> pjmstf,
                                  java.util.List<Pcsumf> pcsumf,
                                  java.util.List<Pccarf> pccarf) {
        String normalizedCode = normalizeMerchantCode(merchantCode);
        if (!cIndex.contains(normalizedCode)) {
            return LookupResponse.error(normalizedCode, "C索引未登録");
        }

        Pjmstf merchant = findMerchant(normalizedCode, pjmstf);
        if (merchant == null) {
            return LookupResponse.error(normalizedCode, "PJMSTF未確認");
        }
        if (!merchant.active) {
            return LookupResponse.error(normalizedCode, "加盟店停止中");
        }

        SettlementSummary summary = summarize(normalizedCode, pcsumf);
        CarrySummary carry = summarizeCarry(normalizedCode, pccarf);
        String accountSuffix = maskAccount(merchant.accountNo);

        return LookupResponse.ok(
                merchant.merchantCode,
                merchant.merchantName,
                merchant.bankCode,
                accountSuffix,
                merchant.riskRank,
                summary.latestSettleDate,
                summary.settleKbn,
                summary.txnCount,
                summary.totalAmount,
                summary.carryAmount,
                carry.carryAmount,
                carry.nextSettleDate,
                carry.reason
        );
    }

    private static String normalizeMerchantCode(String merchantCode) {
        if (merchantCode == null) {
            throw new IllegalArgumentException("加盟店コード未設定");
        }
        String trimmed = merchantCode.trim();
        if (trimmed.length() != MERCHANT_CODE_LENGTH) {
            throw new IllegalArgumentException("加盟店コード桁数不正");
        }
        for (int i = 0; i < trimmed.length(); i++) {
            char c = trimmed.charAt(i);
            if (!(c >= '0' && c <= '9') && c != 'M') {
                throw new IllegalArgumentException("加盟店コード文字種不正");
            }
        }
        return trimmed;
    }

    private static Pjmstf findMerchant(String merchantCode, java.util.List<Pjmstf> pjmstf) {
        for (Pjmstf row : pjmstf) {
            if (row.merchantCode.equals(merchantCode)) {
                return row;
            }
        }
        return null;
    }

    private static SettlementSummary summarize(String merchantCode, java.util.List<Pcsumf> rows) {
        java.time.LocalDate latestDate = null;
        String latestKbn = "";
        int count = 0;
        long totalAmount = 0L;
        long carryAmount = 0L;

        for (Pcsumf row : rows) {
            if (!row.merchantCode.equals(merchantCode)) {
                continue;
            }
            if (latestDate == null || row.settleDate.isAfter(latestDate)) {
                latestDate = row.settleDate;
                latestKbn = row.settleKbn;
                count = row.txnCount;
                totalAmount = row.totalAmount;
                carryAmount = row.carryAmount;
            } else if (row.settleDate.equals(latestDate)) {
                count += row.txnCount;
                totalAmount += row.totalAmount;
                carryAmount += row.carryAmount;
                if (latestKbn.compareTo(row.settleKbn) > 0) {
                    latestKbn = row.settleKbn;
                }
            }
        }

        if (latestDate == null) {
            return new SettlementSummary(null, "", 0, 0L, 0L);
        }
        return new SettlementSummary(latestDate, latestKbn, count, totalAmount, carryAmount);
    }

    private static CarrySummary summarizeCarry(String merchantCode, java.util.List<Pccarf> rows) {
        long carryAmount = 0L;
        java.time.LocalDate nextSettleDate = null;
        java.util.List<String> reasons = new java.util.ArrayList<String>();

        for (Pccarf row : rows) {
            if (!row.merchantCode.equals(merchantCode)) {
                continue;
            }
            carryAmount += row.carryAmount;
            if (nextSettleDate == null || row.nextSettleDate.isBefore(nextSettleDate)) {
                nextSettleDate = row.nextSettleDate;
            }
            if (!reasons.contains(row.carryReason)) {
                reasons.add(row.carryReason);
            }
        }

        return new CarrySummary(carryAmount, nextSettleDate, String.join("、", reasons));
    }

    private static String maskAccount(String accountNo) {
        if (accountNo == null || accountNo.length() < 4) {
            return "****";
        }
        return accountNo.substring(accountNo.length() - 4);
    }

    private static java.time.LocalDate date(String yyyymmdd) {
        return java.time.LocalDate.parse(yyyymmdd, DATE_FMT);
    }

    private static String dateText(java.time.LocalDate date) {
        return date == null ? "" : DATE_FMT.format(date);
    }

    private static final class Pjmstf {
        private final String merchantCode;
        private final String merchantName;
        private final String bankCode;
        private final String accountNo;
        private final boolean active;
        private final String riskRank;

        private Pjmstf(String merchantCode, String merchantName, String bankCode,
                      String accountNo, boolean active, String riskRank) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.bankCode = bankCode;
            this.accountNo = accountNo;
            this.active = active;
            this.riskRank = riskRank;
        }
    }

    private static final class Pcsumf {
        private final String merchantCode;
        private final java.time.LocalDate settleDate;
        private final String settleKbn;
        private final int txnCount;
        private final long totalAmount;
        private final long carryAmount;

        private Pcsumf(String merchantCode, java.time.LocalDate settleDate, String settleKbn,
                      int txnCount, long totalAmount, long carryAmount) {
            this.merchantCode = merchantCode;
            this.settleDate = settleDate;
            this.settleKbn = settleKbn;
            this.txnCount = txnCount;
            this.totalAmount = totalAmount;
            this.carryAmount = carryAmount;
        }
    }

    private static final class Pccarf {
        private final String carryId;
        private final String merchantCode;
        private final String settleKbn;
        private final long carryAmount;
        private final String carryReason;
        private final java.time.LocalDate nextSettleDate;

        private Pccarf(String carryId, String merchantCode, String settleKbn,
                      long carryAmount, String carryReason, java.time.LocalDate nextSettleDate) {
            this.carryId = carryId;
            this.merchantCode = merchantCode;
            this.settleKbn = settleKbn;
            this.carryAmount = carryAmount;
            this.carryReason = carryReason;
            this.nextSettleDate = nextSettleDate;
        }
    }

    private static final class SettlementSummary {
        private final java.time.LocalDate latestSettleDate;
        private final String settleKbn;
        private final int txnCount;
        private final long totalAmount;
        private final long carryAmount;

        private SettlementSummary(java.time.LocalDate latestSettleDate, String settleKbn,
                                  int txnCount, long totalAmount, long carryAmount) {
            this.latestSettleDate = latestSettleDate;
            this.settleKbn = settleKbn;
            this.txnCount = txnCount;
            this.totalAmount = totalAmount;
            this.carryAmount = carryAmount;
        }
    }

    private static final class CarrySummary {
        private final long carryAmount;
        private final java.time.LocalDate nextSettleDate;
        private final String reason;

        private CarrySummary(long carryAmount, java.time.LocalDate nextSettleDate, String reason) {
            this.carryAmount = carryAmount;
            this.nextSettleDate = nextSettleDate;
            this.reason = reason;
        }
    }

    private static final class LookupResponse {
        private final boolean success;
        private final String merchantCode;
        private final String message;
        private final String merchantName;
        private final String bankCode;
        private final String accountLast4;
        private final String riskRank;
        private final java.time.LocalDate settleDate;
        private final String settleKbn;
        private final int txnCount;
        private final long totalAmount;
        private final long pcsumCarryAmount;
        private final long pccarCarryAmount;
        private final java.time.LocalDate nextSettleDate;
        private final String carryReason;

        private LookupResponse(boolean success, String merchantCode, String message,
                               String merchantName, String bankCode, String accountLast4,
                               String riskRank, java.time.LocalDate settleDate, String settleKbn,
                               int txnCount, long totalAmount, long pcsumCarryAmount,
                               long pccarCarryAmount, java.time.LocalDate nextSettleDate,
                               String carryReason) {
            this.success = success;
            this.merchantCode = merchantCode;
            this.message = message;
            this.merchantName = merchantName;
            this.bankCode = bankCode;
            this.accountLast4 = accountLast4;
            this.riskRank = riskRank;
            this.settleDate = settleDate;
            this.settleKbn = settleKbn;
            this.txnCount = txnCount;
            this.totalAmount = totalAmount;
            this.pcsumCarryAmount = pcsumCarryAmount;
            this.pccarCarryAmount = pccarCarryAmount;
            this.nextSettleDate = nextSettleDate;
            this.carryReason = carryReason;
        }

        private static LookupResponse ok(String merchantCode, String merchantName, String bankCode,
                                         String accountLast4, String riskRank,
                                         java.time.LocalDate settleDate, String settleKbn,
                                         int txnCount, long totalAmount, long pcsumCarryAmount,
                                         long pccarCarryAmount, java.time.LocalDate nextSettleDate,
                                         String carryReason) {
            return new LookupResponse(true, merchantCode, "正常", merchantName, bankCode,
                    accountLast4, riskRank, settleDate, settleKbn, txnCount, totalAmount,
                    pcsumCarryAmount, pccarCarryAmount, nextSettleDate, carryReason);
        }

        private static LookupResponse error(String merchantCode, String message) {
            return new LookupResponse(false, merchantCode, message, "", "", "", "",
                    null, "", 0, 0L, 0L, 0L, null, "");
        }

        private String toOperatorLine() {
            if (!success) {
                return "照会結果=不可, 加盟店=" + merchantCode + ", 事由=" + message;
            }
            return "照会結果=正常"
                    + ", 加盟店=" + merchantCode
                    + ", 名称=" + merchantName
                    + ", 銀行=" + bankCode
                    + ", 口座下4桁=" + accountLast4
                    + ", リスク=" + riskRank
                    + ", 精算日=" + dateText(settleDate)
                    + ", 精算区分=" + settleKbn
                    + ", 件数=" + txnCount
                    + ", 金額=" + totalAmount
                    + ", 精算繰越=" + pcsumCarryAmount
                    + ", 繰越残高=" + pccarCarryAmount
                    + ", 次回精算日=" + dateText(nextSettleDate)
                    + ", 繰越理由=" + carryReason;
        }
    }
}
