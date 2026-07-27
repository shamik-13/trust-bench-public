package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024-09-24  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TransactionEligibilityService {
    private static final String STATUS_CHARGEABLE = "01";
    private static final DateTimeFormatter DATE_FORMAT = DateTimeFormatter.BASIC_ISO_DATE;

    public static void main(String[] a) {
        Map<String, MerchantRecord> merchantFile = loadPfmerf();
        List<TransactionRecord> transactionFile = loadPftxnf();

        EligibilitySummary summary = judge(transactionFile, merchantFile);

        System.out.println("手数料対象取引判定サービス");
        System.out.println("読込件数=" + summary.readCount);
        System.out.println("対象件数=" + summary.eligibleRows.size());
        System.out.println("除外件数=" + summary.excludedRows.size());
        System.out.println("対象金額合計=" + summary.totalEligibleAmount.toPlainString());

        for (ExcludedRow row : summary.excludedRows) {
            System.out.println("除外 TXN-ID=" + row.transaction.txnId
                    + " MERCHANT-CODE=" + row.transaction.merchantCode
                    + " 理由=" + row.reason);
        }

        for (FeeInputRow row : summary.eligibleRows) {
            System.out.println("FEE入力 TXN-ID=" + row.txnId
                    + " MERCHANT-CODE=" + row.merchantCode
                    + " MER-CATEGORY=" + row.merchantCategory
                    + " TXN-AMT=" + row.amount.toPlainString()
                    + " TXN-DT=" + row.transactionDate.format(DATE_FORMAT));
        }
    }

    private static EligibilitySummary judge(List<TransactionRecord> transactions,
                                            Map<String, MerchantRecord> merchants) {
        List<FeeInputRow> eligibleRows = new ArrayList<>();
        List<ExcludedRow> excludedRows = new ArrayList<>();
        BigDecimal totalEligibleAmount = BigDecimal.ZERO;

        for (TransactionRecord transaction : transactions) {
            MerchantRecord merchant = merchants.get(transaction.merchantCode);
            String reason = exclusionReason(transaction, merchant);

            if (reason != null) {
                excludedRows.add(new ExcludedRow(transaction, reason));
                continue;
            }

            FeeInputRow feeInput = new FeeInputRow(
                    transaction.txnId,
                    transaction.merchantCode,
                    merchant.merchantCategory,
                    transaction.amount,
                    transaction.transactionDate
            );
            eligibleRows.add(feeInput);
            totalEligibleAmount = totalEligibleAmount.add(transaction.amount);
        }

        return new EligibilitySummary(transactions.size(), eligibleRows, excludedRows, totalEligibleAmount);
    }

    private static String exclusionReason(TransactionRecord transaction, MerchantRecord merchant) {
        if (transaction.amount.signum() < 0) {
            return "取消取引";
        }
        if (transaction.amount.signum() == 0) {
            return "ゼロ円取引";
        }
        if (merchant == null) {
            return "加盟店未登録";
        }
        if (!STATUS_CHARGEABLE.equals(merchant.merchantStatus)) {
            return "加盟店状態対象外:" + merchant.merchantStatus;
        }
        return null;
    }

    private static List<TransactionRecord> loadPftxnf() {
        return Arrays.asList(
                new TransactionRecord("T202606280001", "M000001", new BigDecimal("12800"), LocalDate.parse("20260628", DATE_FORMAT)),
                new TransactionRecord("T202606280002", "M000002", new BigDecimal("4200"), LocalDate.parse("20260628", DATE_FORMAT)),
                new TransactionRecord("T202606280003", "M000003", new BigDecimal("0"), LocalDate.parse("20260628", DATE_FORMAT)),
                new TransactionRecord("T202606280004", "M000004", new BigDecimal("-12800"), LocalDate.parse("20260628", DATE_FORMAT)),
                new TransactionRecord("T202606280005", "M000005", new BigDecimal("98000"), LocalDate.parse("20260628", DATE_FORMAT)),
                new TransactionRecord("T202606280006", "M999999", new BigDecimal("3500"), LocalDate.parse("20260628", DATE_FORMAT)),
                new TransactionRecord("T202606280007", "M000006", new BigDecimal("7600"), LocalDate.parse("20260628", DATE_FORMAT))
        );
    }

    private static Map<String, MerchantRecord> loadPfmerf() {
        Map<String, MerchantRecord> merchants = new LinkedHashMap<>();
        merchants.put("M000001", new MerchantRecord("M000001", "未来百貨店", "C1", "01"));
        merchants.put("M000002", new MerchantRecord("M000002", "東京食堂", "C2", "01"));
        merchants.put("M000003", new MerchantRecord("M000003", "水道局収納", "C3", "01"));
        merchants.put("M000004", new MerchantRecord("M000004", "未来百貨店", "C1", "01"));
        merchants.put("M000005", new MerchantRecord("M000005", "高額決済ネット", "C5", "02"));
        merchants.put("M000006", new MerchantRecord("M000006", "通信販売みらい", "C4", "09"));
        return merchants;
    }

    private static final class TransactionRecord {
        private final String txnId;
        private final String merchantCode;
        private final BigDecimal amount;
        private final LocalDate transactionDate;

        private TransactionRecord(String txnId, String merchantCode, BigDecimal amount, LocalDate transactionDate) {
            this.txnId = txnId;
            this.merchantCode = merchantCode;
            this.amount = amount;
            this.transactionDate = transactionDate;
        }
    }

    private static final class MerchantRecord {
        private final String merchantCode;
        private final String merchantName;
        private final String merchantCategory;
        private final String merchantStatus;

        private MerchantRecord(String merchantCode, String merchantName, String merchantCategory, String merchantStatus) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merchantCategory = merchantCategory;
            this.merchantStatus = merchantStatus;
        }
    }

    private static final class FeeInputRow {
        private final String txnId;
        private final String merchantCode;
        private final String merchantCategory;
        private final BigDecimal amount;
        private final LocalDate transactionDate;

        private FeeInputRow(String txnId, String merchantCode, String merchantCategory,
                            BigDecimal amount, LocalDate transactionDate) {
            this.txnId = txnId;
            this.merchantCode = merchantCode;
            this.merchantCategory = merchantCategory;
            this.amount = amount;
            this.transactionDate = transactionDate;
        }
    }

    private static final class ExcludedRow {
        private final TransactionRecord transaction;
        private final String reason;

        private ExcludedRow(TransactionRecord transaction, String reason) {
            this.transaction = transaction;
            this.reason = reason;
        }
    }

    private static final class EligibilitySummary {
        private final int readCount;
        private final List<FeeInputRow> eligibleRows;
        private final List<ExcludedRow> excludedRows;
        private final BigDecimal totalEligibleAmount;

        private EligibilitySummary(int readCount, List<FeeInputRow> eligibleRows,
                                   List<ExcludedRow> excludedRows, BigDecimal totalEligibleAmount) {
            this.readCount = readCount;
            this.eligibleRows = eligibleRows;
            this.excludedRows = excludedRows;
            this.totalEligibleAmount = totalEligibleAmount;
        }
    }
}
