package jp.mirai.pay.refund;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2024-12-03  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class RefundBalanceProbeService {
    private static final String[] PRBALF_SAMPLE = {
            "WLT0000001,125000,18000,20260627",
            "WLT0000002,8000,12000,20260627",
            "WLT0000003,540000,0,20260625",
            "WLT0000004,33000,3000,20260626",
            "WLT0000005,0,4500,20260624"
    };

    private static final String[] REFUND_LEDGER_SAMPLE = {
            "WLT0000001,12000",
            "WLT0000001,6000",
            "WLT0000002,5000",
            "WLT0000002,2000",
            "WLT0000004,3000",
            "WLT0000005,1500"
    };

    public static void main(String[] a) {
        Map<String, BalanceLine> balanceByWallet = readPrbalf(PRBALF_SAMPLE);
        Map<String, Long> ledgerByWallet = aggregateRefundLedger(REFUND_LEDGER_SAMPLE);

        for (BalanceLine line : balanceByWallet.values()) {
            long refundActual = ledgerByWallet.getOrDefault(line.walletId, 0L);
            ProbeResult result = probe(line, refundActual);
            System.out.println(format(result));
        }
    }

    private static Map<String, BalanceLine> readPrbalf(String[] rows) {
        Map<String, BalanceLine> result = new LinkedHashMap<>();
        for (String row : rows) {
            String[] columns = row.split(",", -1);
            if (columns.length != 4) {
                throw new IllegalArgumentException("PRBALF項目数不正:" + row);
            }

            String walletId = requireWalletId(columns[0]);
            long availableBalance = parseAmount(columns[1], "AVAILABLE-BAL", walletId);
            long pendingRefundAmount = parseAmount(columns[2], "PENDING-REFUND-AMT", walletId);
            LocalDate lastAdjustDate = parseDate(columns[3], "LAST-ADJ-DT", walletId);

            if (result.containsKey(walletId)) {
                throw new IllegalArgumentException("PRBALFキー重複:" + walletId);
            }

            result.put(walletId, new BalanceLine(walletId, availableBalance, pendingRefundAmount, lastAdjustDate));
        }
        return result;
    }

    private static Map<String, Long> aggregateRefundLedger(String[] rows) {
        Map<String, Long> result = new HashMap<>();
        for (String row : rows) {
            String[] columns = row.split(",", -1);
            if (columns.length != 2) {
                throw new IllegalArgumentException("返金実績項目数不正:" + row);
            }

            String walletId = requireWalletId(columns[0]);
            long amount = parseAmount(columns[1], "REFUND-AMT", walletId);
            result.merge(walletId, amount, RefundBalanceProbeService::checkedAdd);
        }
        return result;
    }

    private static ProbeResult probe(BalanceLine line, long refundActual) {
        long availableAfterPending = checkedSubtract(line.availableBalance, line.pendingRefundAmount);
        long referenceCapacity = checkedSubtract(availableAfterPending, refundActual);
        String status = referenceCapacity < 0 ? "不足参考" : "余力参考";
        return new ProbeResult(
                line.walletId,
                line.availableBalance,
                line.pendingRefundAmount,
                refundActual,
                referenceCapacity,
                line.lastAdjustDate,
                status
        );
    }

    private static String requireWalletId(String value) {
        if (value == null || !value.matches("WLT[0-9]{7}")) {
            throw new IllegalArgumentException("ウォレットID不正:" + value);
        }
        return value;
    }

    private static long parseAmount(String value, String itemName, String walletId) {
        try {
            long amount = Long.parseLong(value);
            if (amount < 0L) {
                throw new IllegalArgumentException(itemName + "負値:" + walletId);
            }
            return amount;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(itemName + "数値不正:" + walletId, e);
        }
    }

    private static LocalDate parseDate(String value, String itemName, String walletId) {
        try {
            return LocalDate.parse(value, DateTimeFormatter.BASIC_ISO_DATE);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(itemName + "日付不正:" + walletId, e);
        }
    }

    private static long checkedAdd(long left, long right) {
        return Math.addExact(left, right);
    }

    private static long checkedSubtract(long left, long right) {
        return Math.subtractExact(left, right);
    }

    private static String format(ProbeResult result) {
        return String.join(",",
                "ウォレットID=" + result.walletId,
                "利用可能残高=" + result.availableBalance,
                "保留中返金額=" + result.pendingRefundAmount,
                "返金実績=" + result.refundActual,
                "返金余力参考値=" + result.referenceCapacity,
                "最終調整日=" + result.lastAdjustDate.format(DateTimeFormatter.BASIC_ISO_DATE),
                "判定区分=" + result.status);
    }

    private static final class BalanceLine {
        private final String walletId;
        private final long availableBalance;
        private final long pendingRefundAmount;
        private final LocalDate lastAdjustDate;

        private BalanceLine(String walletId, long availableBalance, long pendingRefundAmount, LocalDate lastAdjustDate) {
            this.walletId = walletId;
            this.availableBalance = availableBalance;
            this.pendingRefundAmount = pendingRefundAmount;
            this.lastAdjustDate = lastAdjustDate;
        }
    }

    private static final class ProbeResult {
        private final String walletId;
        private final long availableBalance;
        private final long pendingRefundAmount;
        private final long refundActual;
        private final long referenceCapacity;
        private final LocalDate lastAdjustDate;
        private final String status;

        private ProbeResult(String walletId, long availableBalance, long pendingRefundAmount, long refundActual,
                            long referenceCapacity, LocalDate lastAdjustDate, String status) {
            this.walletId = walletId;
            this.availableBalance = availableBalance;
            this.pendingRefundAmount = pendingRefundAmount;
            this.refundActual = refundActual;
            this.referenceCapacity = referenceCapacity;
            this.lastAdjustDate = lastAdjustDate;
            this.status = status;
        }
    }
}
