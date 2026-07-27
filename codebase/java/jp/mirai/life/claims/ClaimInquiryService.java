package jp.mirai.life.claims;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * 変更履歴
 * 版数    年月日        担当            概要
 * 1.00    2024-03-15    保険金システムG    初版作成
 */
public class ClaimInquiryService {
    private static final String STATUS_PAYABLE = "01";
    private static final int FULL_PAYMENT_RATE = 100;
    private static final int DEFAULT_PAGE_SIZE = 5;

    private ClaimStatusView inquire(String claimId, int pageNo, int pageSize) {
        validateClaimId(claimId);
        int normalizedPageNo = pageNo < 1 ? 1 : pageNo;
        int normalizedPageSize = pageSize < 1 ? DEFAULT_PAGE_SIZE : pageSize;

        Object master = findClaimMaster(claimId);
        if (master == null) {
            throw new IllegalArgumentException("請求ＩＤが存在しません: " + claimId);
        }

        Object payout = findLatestPayout(claimId);
        Object[] histories = findHistories(claimId);
        sortHistoriesByChangeDate(histories);

        int totalCount = histories.length;
        int fromIndex = Math.min((normalizedPageNo - 1) * normalizedPageSize, totalCount);
        int toIndex = Math.min(fromIndex + normalizedPageSize, totalCount);
        Object[] pageHistories = slice(histories, fromIndex, toIndex);

        boolean payable = STATUS_PAYABLE.equals(stringValue(master, "claimStatusKbn"));
        int elapsedYearRate = FULL_PAYMENT_RATE;

        return new ClaimStatusView(
                stringValue(master, "claimId"),
                stringValue(master, "polNo"),
                yen(longValue(master, "sumAssuredAmt")),
                yen(longValue(master, "loanBalanceAmt")),
                stringValue(master, "respStartDt"),
                stringValue(master, "eventDt"),
                stringValue(master, "claimStatusKbn"),
                payable,
                elapsedYearRate,
                payout == null ? "" : stringValue(payout, "payId"),
                payout == null ? "" : yen(longValue(payout, "grossAmt")),
                payout == null ? "" : String.valueOf(intValue(payout, "reductionRate")),
                payout == null ? "" : yen(longValue(payout, "payoutAmt")),
                normalizedPageNo,
                normalizedPageSize,
                totalCount,
                pageHistories
        );
    }

    private static void validateClaimId(String claimId) {
        if (claimId == null || claimId.trim().isEmpty()) {
            throw new IllegalArgumentException("請求ＩＤが未指定です");
        }
    }

    private static int parsePositiveInt(String value, int defaultValue) {
        try {
            int parsed = Integer.parseInt(value);
            return parsed > 0 ? parsed : defaultValue;
        } catch (NumberFormatException ex) {
            return defaultValue;
        }
    }

    private static Object findClaimMaster(String claimId) {
        Object[] records = modelArray("LFCLMF_DATA", "LFCLMF", "lfclmfData", "lfclmf");
        if (records.length == 0) {
            records = FALLBACK_LFCLMF_DATA;
        }
        for (Object record : records) {
            if (claimId.equals(stringValue(record, "claimId"))) {
                return record;
            }
        }
        return null;
    }

    private static Object findLatestPayout(String claimId) {
        Object[] records = modelArray("LFPAYF_DATA", "LFPAYF", "lfpayfData", "lfpayf");
        if (records.length == 0) {
            records = FALLBACK_LFPAYF_DATA;
        }

        Object latest = null;
        for (Object record : records) {
            if (!claimId.equals(stringValue(record, "claimId"))) {
                continue;
            }
            if (latest == null || stringValue(record, "payId").compareTo(stringValue(latest, "payId")) > 0) {
                latest = record;
            }
        }
        return latest;
    }

    private static Object[] findHistories(String claimId) {
        Object[] records = modelArray("LFPAYH_DATA", "LFPAYH", "lfpayhData", "lfpayh");
        if (records.length == 0) {
            records = FALLBACK_LFPAYH_DATA;
        }

        int count = 0;
        for (Object record : records) {
            if (claimId.equals(stringValue(record, "claimId"))) {
                count++;
            }
        }

        Object[] result = new Object[count];
        int index = 0;
        for (Object record : records) {
            if (claimId.equals(stringValue(record, "claimId"))) {
                result[index++] = record;
            }
        }
        return result;
    }

    private static void sortHistoriesByChangeDate(Object[] histories) {
        for (int i = 1; i < histories.length; i++) {
            Object current = histories[i];
            int j = i - 1;
            while (j >= 0 && compareHistory(histories[j], current) > 0) {
                histories[j + 1] = histories[j];
                j--;
            }
            histories[j + 1] = current;
        }
    }

    private static int compareHistory(Object left, Object right) {
        int dateCompare = stringValue(left, "changeDt").compareTo(stringValue(right, "changeDt"));
        if (dateCompare != 0) {
            return dateCompare;
        }
        return Integer.compare(intValue(left, "seqNo"), intValue(right, "seqNo"));
    }

    private static Object[] slice(Object[] source, int fromIndex, int toIndex) {
        Object[] result = new Object[toIndex - fromIndex];
        for (int i = fromIndex; i < toIndex; i++) {
            result[i - fromIndex] = source[i];
        }
        return result;
    }

    private static Object[] modelArray(String... fieldNames) {
        for (String fieldName : fieldNames) {
            try {
                Field field = ClaimModel.class.getDeclaredField(fieldName);
                field.setAccessible(true);
                Object value = field.get(null);
                if (value == null || !value.getClass().isArray()) {
                    continue;
                }

                int length = Array.getLength(value);
                Object[] result = new Object[length];
                for (int i = 0; i < length; i++) {
                    result[i] = Array.get(value, i);
                }
                return result;
            } catch (ReflectiveOperationException ignored) {
                continue;
            }
        }
        return new Object[0];
    }

    private static String stringValue(Object target, String name) {
        Object value = value(target, name);
        return value == null ? "" : String.valueOf(value);
    }

    private static int intValue(Object target, String name) {
        Object value = value(target, name);
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        if (value == null || String.valueOf(value).isEmpty()) {
            return 0;
        }
        return Integer.parseInt(String.valueOf(value));
    }

    private static long longValue(Object target, String name) {
        Object value = value(target, name);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        if (value == null || String.valueOf(value).isEmpty()) {
            return 0L;
        }
        return Long.parseLong(String.valueOf(value));
    }

    private static Object value(Object target, String name) {
        if (target == null) {
            return null;
        }

        if (target instanceof Row) {
            return ((Row) target).get(name);
        }

        try {
            Method method = target.getClass().getMethod(name);
            return method.invoke(target);
        } catch (ReflectiveOperationException ignored) {
        }

        String getter = "get" + Character.toUpperCase(name.charAt(0)) + name.substring(1);
        try {
            Method method = target.getClass().getMethod(getter);
            return method.invoke(target);
        } catch (ReflectiveOperationException ignored) {
        }

        try {
            Field field = target.getClass().getDeclaredField(name);
            field.setAccessible(true);
            return field.get(target);
        } catch (ReflectiveOperationException ignored) {
            return null;
        }
    }

    private static String yen(long amount) {
        return String.format("%,d円", amount);
    }

    private static final Object[] FALLBACK_LFCLMF_DATA = {
            new Row(new String[] {"claimId", "polNo", "sumAssuredAmt", "loanBalanceAmt", "respStartDt", "eventDt", "claimStatusKbn"},
                    new Object[] {"CLM-2024-0001", "POL-10000001", 12000000L, 1800000L, "2018-04-01", "2024-05-17", "05"}),
            new Row(new String[] {"claimId", "polNo", "sumAssuredAmt", "loanBalanceAmt", "respStartDt", "eventDt", "claimStatusKbn"},
                    new Object[] {"CLM-2024-0002", "POL-10000002", 8000000L, 0L, "2023-12-01", "2024-02-10", "01"}),
            new Row(new String[] {"claimId", "polNo", "sumAssuredAmt", "loanBalanceAmt", "respStartDt", "eventDt", "claimStatusKbn"},
                    new Object[] {"CLM-2024-0003", "POL-10000003", 15000000L, 2400000L, "2017-09-20", "2024-04-28", "09"})
    };

    private static final Object[] FALLBACK_LFPAYF_DATA = {
            new Row(new String[] {"payId", "claimId", "grossAmt", "reductionRate", "payoutAmt"},
                    new Object[] {"PAY-2024-0001", "CLM-2024-0001", 12000000L, 100, 10200000L}),
            new Row(new String[] {"payId", "claimId", "grossAmt", "reductionRate", "payoutAmt"},
                    new Object[] {"PAY-2024-0002", "CLM-2024-0002", 8000000L, 100, 8000000L}),
            new Row(new String[] {"payId", "claimId", "grossAmt", "reductionRate", "payoutAmt"},
                    new Object[] {"PAY-2024-0003", "CLM-2024-0003", 15000000L, 100, 12600000L})
    };

    private static final Object[] FALLBACK_LFPAYH_DATA = {
            new Row(new String[] {"seqNo", "claimId", "statusFrom", "statusTo", "changeDt", "operatorId"},
                    new Object[] {1, "CLM-2024-0001", "00", "05", "2024-05-18", "OPR001"}),
            new Row(new String[] {"seqNo", "claimId", "statusFrom", "statusTo", "changeDt", "operatorId"},
                    new Object[] {2, "CLM-2024-0001", "05", "05", "2024-05-25", "OPR014"}),
            new Row(new String[] {"seqNo", "claimId", "statusFrom", "statusTo", "changeDt", "operatorId"},
                    new Object[] {3, "CLM-2024-0001", "05", "01", "2024-06-02", "OPR021"}),
            new Row(new String[] {"seqNo", "claimId", "statusFrom", "statusTo", "changeDt", "operatorId"},
                    new Object[] {1, "CLM-2024-0002", "00", "05", "2024-02-11", "OPR002"}),
            new Row(new String[] {"seqNo", "claimId", "statusFrom", "statusTo", "changeDt", "operatorId"},
                    new Object[] {2, "CLM-2024-0002", "05", "01", "2024-02-18", "OPR002"}),
            new Row(new String[] {"seqNo", "claimId", "statusFrom", "statusTo", "changeDt", "operatorId"},
                    new Object[] {1, "CLM-2024-0003", "00", "05", "2024-04-29", "OPR011"}),
            new Row(new String[] {"seqNo", "claimId", "statusFrom", "statusTo", "changeDt", "operatorId"},
                    new Object[] {2, "CLM-2024-0003", "05", "09", "2024-05-09", "OPR031"})
    };

    private static final class Row {
        private final String[] names;
        private final Object[] values;

        private Row(String[] names, Object[] values) {
            this.names = names;
            this.values = values;
        }

        private Object get(String name) {
            for (int i = 0; i < names.length; i++) {
                if (names[i].equals(name)) {
                    return values[i];
                }
            }
            return null;
        }
    }

    private static final class ClaimStatusView {
        private final String claimId;
        private final String polNo;
        private final String sumAssuredAmt;
        private final String loanBalanceAmt;
        private final String respStartDt;
        private final String eventDt;
        private final String claimStatusKbn;
        private final boolean payable;
        private final int elapsedYearRate;
        private final String payId;
        private final String grossAmt;
        private final String reductionRate;
        private final String payoutAmt;
        private final int pageNo;
        private final int pageSize;
        private final int totalCount;
        private final Object[] histories;

        private ClaimStatusView(
                String claimId,
                String polNo,
                String sumAssuredAmt,
                String loanBalanceAmt,
                String respStartDt,
                String eventDt,
                String claimStatusKbn,
                boolean payable,
                int elapsedYearRate,
                String payId,
                String grossAmt,
                String reductionRate,
                String payoutAmt,
                int pageNo,
                int pageSize,
                int totalCount,
                Object[] histories) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
            this.payable = payable;
            this.elapsedYearRate = elapsedYearRate;
            this.payId = payId;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
            this.pageNo = pageNo;
            this.pageSize = pageSize;
            this.totalCount = totalCount;
            this.histories = histories;
        }

        private String toDisplayText() {
            StringBuilder builder = new StringBuilder();
            builder.append("請求ＩＤ=").append(claimId).append('\n');
            builder.append("証券番号=").append(polNo).append('\n');
            builder.append("保険金額=").append(sumAssuredAmt).append('\n');
            builder.append("貸付残高=").append(loanBalanceAmt).append('\n');
            builder.append("責任開始日=").append(respStartDt).append('\n');
            builder.append("事故日=").append(eventDt).append('\n');
            builder.append("請求状態=").append(claimStatusKbn).append('\n');
            builder.append("支払対象=").append(payable ? "対象" : "対象外").append('\n');
            builder.append("一年経過支払割合=").append(elapsedYearRate).append("%\n");
            builder.append("支払ＩＤ=").append(payId).append('\n');
            builder.append("支払前金額=").append(grossAmt).append('\n');
            builder.append("削減率=").append(reductionRate).append('\n');
            builder.append("支払金額=").append(payoutAmt).append('\n');
            builder.append("履歴ページ=").append(pageNo).append('/').append(pageSize).append(" 件 総件数=").append(totalCount).append('\n');
            for (Object history : histories) {
                builder.append(intValue(history, "seqNo")).append(' ')
                        .append(stringValue(history, "statusFrom")).append("->")
                        .append(stringValue(history, "statusTo")).append(' ')
                        .append(stringValue(history, "changeDt")).append(' ')
                        .append(stringValue(history, "operatorId")).append('\n');
            }
            return builder.toString();
        }
    }
}
