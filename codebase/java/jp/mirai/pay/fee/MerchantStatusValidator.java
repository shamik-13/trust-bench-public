package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数    年月日        担当        概要
 * 1.0     2024-09-09    みらいペイ システム部 加盟店・手数料チーム  加盟店状態検証サービス初版
 */
public class MerchantStatusValidator {

    private static final String STS_VALID = "01";
    private static final String STS_SUSPENDED = "02";
    private static final String STS_TERMINATED = "09";

    private static final String CAT_RETAIL = "C1";
    private static final String CAT_RESTAURANT = "C2";
    private static final String CAT_PUBLIC = "C3";
    private static final String CAT_EC = "C4";
    private static final String CAT_HIGH_RISK = "C5";

    private enum Business {
        BILLING("請求"),
        SETTLEMENT("精算"),
        FEE_CALCULATION("手数料計算");

        private final String label;

        Business(String label) {
            this.label = label;
        }
    }

    private static final class PfmerfRecord {
        private final String merchantCode;
        private final String merchantName;
        private final String merchantCategory;
        private final String merchantStatus;

        private PfmerfRecord(String merchantCode, String merchantName, String merchantCategory, String merchantStatus) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merchantCategory = merchantCategory;
            this.merchantStatus = merchantStatus;
        }
    }

    private static final class ValidationResult {
        private final String merchantCode;
        private final String merchantName;
        private final Business business;
        private final boolean accepted;
        private final boolean suppressMdrInput;
        private final String reason;

        private ValidationResult(String merchantCode, String merchantName, Business business,
                                 boolean accepted, boolean suppressMdrInput, String reason) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.business = business;
            this.accepted = accepted;
            this.suppressMdrInput = suppressMdrInput;
            this.reason = reason;
        }
    }

    private static final class Summary {
        private int total;
        private int accepted;
        private int rejected;
        private int mdrSuppressed;

        private void add(ValidationResult result) {
            total++;
            if (result.accepted) {
                accepted++;
            } else {
                rejected++;
            }
            if (result.suppressMdrInput) {
                mdrSuppressed++;
            }
        }
    }

    public static void main(String[] a) {
        PfmerfRecord[] records = readPfmerf();
        Summary billing = new Summary();
        Summary settlement = new Summary();
        Summary feeCalculation = new Summary();

        for (PfmerfRecord record : records) {
            ValidationResult billingResult = validate(record, Business.BILLING);
            ValidationResult settlementResult = validate(record, Business.SETTLEMENT);
            ValidationResult feeResult = validate(record, Business.FEE_CALCULATION);

            billing.add(billingResult);
            settlement.add(settlementResult);
            feeCalculation.add(feeResult);

            printRejected(billingResult);
            printRejected(settlementResult);
            printRejected(feeResult);
        }

        printSummary(Business.BILLING, billing);
        printSummary(Business.SETTLEMENT, settlement);
        printSummary(Business.FEE_CALCULATION, feeCalculation);
    }

    private static PfmerfRecord[] readPfmerf() {
        return new PfmerfRecord[] {
                new PfmerfRecord("M000001", "青葉文具店", CAT_RETAIL, STS_VALID),
                new PfmerfRecord("M000002", "銀座食堂", CAT_RESTAURANT, STS_VALID),
                new PfmerfRecord("M000003", "東都水道料金センター", CAT_PUBLIC, STS_VALID),
                new PfmerfRecord("M000004", "みらい通販", CAT_EC, STS_SUSPENDED),
                new PfmerfRecord("M000005", "湾岸チケット販売", CAT_HIGH_RISK, STS_VALID),
                new PfmerfRecord("M000006", "北町薬局", CAT_RETAIL, STS_TERMINATED),
                new PfmerfRecord("M000007", "桜カフェ", CAT_RESTAURANT, STS_SUSPENDED),
                new PfmerfRecord("M000008", "中央市公金窓口", CAT_PUBLIC, STS_TERMINATED)
        };
    }

    private static ValidationResult validate(PfmerfRecord record, Business business) {
        if (!isKnownCategory(record.merchantCategory)) {
            return rejected(record, business, "業種区分が未定義");
        }

        if (!isKnownStatus(record.merchantStatus)) {
            return rejected(record, business, "加盟店状態が未定義");
        }

        if (Business.BILLING == business) {
            if (STS_TERMINATED.equals(record.merchantStatus)) {
                return rejected(record, business, "解約加盟店のため請求対象外");
            }
            return accepted(record, business);
        }

        if (Business.SETTLEMENT == business) {
            if (STS_VALID.equals(record.merchantStatus)) {
                return accepted(record, business);
            }
            if (STS_SUSPENDED.equals(record.merchantStatus)) {
                return rejected(record, business, "停止加盟店のため精算保留");
            }
            return rejected(record, business, "解約加盟店のため精算不可");
        }

        if (STS_VALID.equals(record.merchantStatus)) {
            return accepted(record, business);
        }
        return rejected(record, business, "有効状態ではないためMDR算定投入抑止");
    }

    private static ValidationResult accepted(PfmerfRecord record, Business business) {
        return new ValidationResult(record.merchantCode, record.merchantName, business, true, false, "許容");
    }

    private static ValidationResult rejected(PfmerfRecord record, Business business, String reason) {
        return new ValidationResult(record.merchantCode, record.merchantName, business, false, true, reason);
    }

    private static boolean isKnownCategory(String category) {
        return CAT_RETAIL.equals(category)
                || CAT_RESTAURANT.equals(category)
                || CAT_PUBLIC.equals(category)
                || CAT_EC.equals(category)
                || CAT_HIGH_RISK.equals(category);
    }

    private static boolean isKnownStatus(String status) {
        return STS_VALID.equals(status)
                || STS_SUSPENDED.equals(status)
                || STS_TERMINATED.equals(status);
    }

    private static void printRejected(ValidationResult result) {
        if (result.accepted) {
            return;
        }
        System.out.println("検証NG 業務=" + result.business.label
                + " 加盟店コード=" + result.merchantCode
                + " 加盟店名=" + result.merchantName
                + " 理由=" + result.reason
                + " MDR投入抑止=" + (result.suppressMdrInput ? "有" : "無"));
    }

    private static void printSummary(Business business, Summary summary) {
        System.out.println("集計 業務=" + business.label
                + " 総件数=" + summary.total
                + " 許容=" + summary.accepted
                + " 停止=" + summary.rejected
                + " MDR投入抑止=" + summary.mdrSuppressed);
    }
}
