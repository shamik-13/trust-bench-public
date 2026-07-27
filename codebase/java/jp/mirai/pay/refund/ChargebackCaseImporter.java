package jp.mirai.pay.refund;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-05-20  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class ChargebackCaseImporter {
    private static final String REASON_CHARGEBACK = "30";
    private static final String STATUS_LINKED = "9";

    public static void main(String[] a) {
        List<PrcbfRecord> input = Arrays.asList(
                new PrcbfRecord("CB202606010001", "TXN202605180001", "VISA", 12800, LocalDate.of(2026, 6, 1), "1"),
                new PrcbfRecord("CB202606010002", "TXN202605180002", "VISA", 0, LocalDate.of(2026, 6, 1), "1"),
                new PrcbfRecord("CB202606010003", "TXN202605180003", "VISA", 9500, LocalDate.of(2026, 6, 1), "C"),
                new PrcbfRecord("CB202606020001", "TXN202605190001", "MC", 24000, LocalDate.of(2026, 6, 2), "2"),
                new PrcbfRecord("CB202606020002", "TXN202605190002", "MC", 17500, LocalDate.of(2026, 6, 2), "E"),
                new PrcbfRecord("CB202606030001", "TXN202605200001", "JCB", 6800, LocalDate.of(2026, 6, 3), "A"),
                new PrcbfRecord("CB202606030002", "TXN202605200002", "JCB", 10300, LocalDate.of(2026, 6, 3), "Z"),
                new PrcbfRecord("CB202606040001", "TXN202605210001", "AMEX", 31200, LocalDate.of(2026, 6, 4), "OPEN"),
                new PrcbfRecord("CB202606040002", "TXN202605210002", "AMEX", 4400, LocalDate.of(2026, 6, 4), "NODOC"),
                new PrcbfRecord("CB202606050001", "TXN202605220001", "VISA", 5700, LocalDate.of(2026, 6, 5), STATUS_LINKED),
                new PrcbfRecord("CB202606050002", "TXN202605220002", "MC", -1200, LocalDate.of(2026, 6, 5), "2")
        );

        ImportResult result = importCases(input, LocalDate.of(2026, 6, 28));

        System.out.println("取込件数=" + result.totalCount);
        System.out.println("依頼生成件数=" + result.requestRecords.size());
        System.out.println("除外件数=" + result.excludedCount);
        System.out.println("合計返金額=" + result.totalRefundAmount);

        for (PrreqfRecord record : result.requestRecords) {
            System.out.println(record.toLine());
        }
    }

    private static ImportResult importCases(List<PrcbfRecord> input, LocalDate requestDate) {
        List<PrreqfRecord> output = new ArrayList<PrreqfRecord>();
        int excludedCount = 0;
        long totalRefundAmount = 0;

        for (PrcbfRecord source : input) {
            if (isLinked(source) || source.disputeAmount <= 0 || !isRequestableStatus(source.cardScheme, source.statusKbn)) {
                excludedCount++;
                continue;
            }

            String requestId = "RQ" + requestDate.format(DateTimeFormatter.BASIC_ISO_DATE)
                    + String.format("%06d", output.size() + 1);
            PrreqfRecord request = new PrreqfRecord(
                    requestId,
                    source.originalTransactionId,
                    source.disputeAmount,
                    requestDate,
                    REASON_CHARGEBACK
            );
            output.add(request);
            totalRefundAmount += request.refundAmount;
        }

        return new ImportResult(input.size(), excludedCount, totalRefundAmount, output);
    }

    private static boolean isLinked(PrcbfRecord record) {
        return STATUS_LINKED.equals(record.statusKbn);
    }

    private static boolean isRequestableStatus(String cardScheme, String statusKbn) {
        if ("VISA".equals(cardScheme)) {
            return "1".equals(statusKbn) || "2".equals(statusKbn);
        }
        if ("MC".equals(cardScheme)) {
            return "2".equals(statusKbn) || "3".equals(statusKbn);
        }
        if ("JCB".equals(cardScheme)) {
            return "A".equals(statusKbn) || "B".equals(statusKbn);
        }
        if ("AMEX".equals(cardScheme)) {
            return "OPEN".equals(statusKbn) || "REVIEW".equals(statusKbn);
        }
        return false;
    }

    private static final class PrcbfRecord {
        private final String caseId;
        private final String originalTransactionId;
        private final String cardScheme;
        private final long disputeAmount;
        private final LocalDate disputeDate;
        private final String statusKbn;

        private PrcbfRecord(String caseId, String originalTransactionId, String cardScheme,
                            long disputeAmount, LocalDate disputeDate, String statusKbn) {
            this.caseId = caseId;
            this.originalTransactionId = originalTransactionId;
            this.cardScheme = cardScheme;
            this.disputeAmount = disputeAmount;
            this.disputeDate = disputeDate;
            this.statusKbn = statusKbn;
        }
    }

    private static final class PrreqfRecord {
        private final String requestId;
        private final String originalTransactionId;
        private final long refundAmount;
        private final LocalDate requestDate;
        private final String requestReason;

        private PrreqfRecord(String requestId, String originalTransactionId, long refundAmount,
                             LocalDate requestDate, String requestReason) {
            this.requestId = requestId;
            this.originalTransactionId = originalTransactionId;
            this.refundAmount = refundAmount;
            this.requestDate = requestDate;
            this.requestReason = requestReason;
        }

        private String toLine() {
            return requestId + "," + originalTransactionId + "," + refundAmount + ","
                    + requestDate.format(DateTimeFormatter.BASIC_ISO_DATE) + "," + requestReason;
        }
    }

    private static final class ImportResult {
        private final int totalCount;
        private final int excludedCount;
        private final long totalRefundAmount;
        private final List<PrreqfRecord> requestRecords;

        private ImportResult(int totalCount, int excludedCount, long totalRefundAmount,
                             List<PrreqfRecord> requestRecords) {
            this.totalCount = totalCount;
            this.excludedCount = excludedCount;
            this.totalRefundAmount = totalRefundAmount;
            this.requestRecords = requestRecords;
        }
    }
}
