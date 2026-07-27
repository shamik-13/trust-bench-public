package jp.mirai.pay.settlement;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/04/01  加盟店精算チーム  初版作成
 * 1.01  2024/06/18  加盟店精算チーム  許容遡及日数の参照を追加
 */
public class FxRateReferenceService {

    private static final String LOAD_STATUS_ACTIVE = "有効";
    private static final String CONF_KEY_LOOKBACK_DAYS = "FX_RATE_LOOKBACK_DAYS";

    private static final PsfxrfRow[] PSFXRF = {
            new PsfxrfRow("USDJPY", "20240624", "159.620000", "銀行TTM", LOAD_STATUS_ACTIVE),
            new PsfxrfRow("USDJPY", "20240625", "159.840000", "銀行TTM", LOAD_STATUS_ACTIVE),
            new PsfxrfRow("USDJPY", "20240626", "160.120000", "銀行TTM", "取込中"),
            new PsfxrfRow("USDJPY", "20240621", "158.910000", "銀行仲値", LOAD_STATUS_ACTIVE),
            new PsfxrfRow("EURJPY", "20240624", "170.740000", "銀行TTM", LOAD_STATUS_ACTIVE),
            new PsfxrfRow("EURJPY", "20240625", "171.030000", "銀行TTM", LOAD_STATUS_ACTIVE),
            new PsfxrfRow("EURJPY", "20240620", "170.210000", "銀行仲値", LOAD_STATUS_ACTIVE),
            new PsfxrfRow("EURUSD", "20240624", "1.069700", "市場仲値", LOAD_STATUS_ACTIVE),
            new PsfxrfRow("EURUSD", "20240625", "1.070200", "市場仲値", LOAD_STATUS_ACTIVE)
    };

    private static final PsconfRow[] PSCONF = {
            new PsconfRow(CONF_KEY_LOOKBACK_DAYS, "3", "20240401", "20240630", "20240401090000"),
            new PsconfRow(CONF_KEY_LOOKBACK_DAYS, "5", "20240701", "99991231", "20240620170000"),
            new PsconfRow("FX_RATE_SOURCE_PRIORITY", "銀行TTM,銀行仲値,市場仲値", "20240401", "99991231", "20240401090000")
    };

    public static void main(String[] a) {
        String ccyPair = a.length > 0 ? a[0] : "USDJPY";
        String baseDate = a.length > 1 ? a[1] : "20240626";

        try {
            RateReference result = findRate(ccyPair, baseDate);
            System.out.println("通貨ペア=" + result.ccyPair
                    + ", 基準日=" + result.requestDate
                    + ", 採用日=" + result.rateDate
                    + ", レート=" + result.ttmRate
                    + ", レート元=" + result.sourceCode
                    + ", 判定=" + result.decisionCode);
        } catch (IllegalArgumentException ex) {
            System.err.println("入力不正: " + ex.getMessage());
            System.exit(2);
        } catch (IllegalStateException ex) {
            System.err.println("参照失敗: " + ex.getMessage());
            System.exit(1);
        }
    }

    private static RateReference findRate(String ccyPair, String baseDate) {
        String normalizedPair = normalizePair(ccyPair);
        validateDate(baseDate, "基準日");

        int lookbackDays = resolveLookbackDays(baseDate);
        PsfxrfRow best = null;
        int bestDistance = Integer.MAX_VALUE;

        for (int i = 0; i < PSFXRF.length; i++) {
            PsfxrfRow row = PSFXRF[i];
            if (!normalizedPair.equals(row.ccyPair)) {
                continue;
            }
            if (!LOAD_STATUS_ACTIVE.equals(row.loadStatus)) {
                continue;
            }
            if (compareDate(row.rateDate, baseDate) > 0) {
                continue;
            }

            int distance = daysBetween(row.rateDate, baseDate);
            if (distance > lookbackDays) {
                continue;
            }

            if (best == null
                    || distance < bestDistance
                    || (distance == bestDistance && isPreferredSource(row.sourceCode, best.sourceCode))) {
                best = row;
                bestDistance = distance;
            }
        }

        if (best == null) {
            throw new IllegalStateException("許容遡及日数内の有効レートなし 通貨ペア=" + normalizedPair
                    + " 基準日=" + baseDate + " 許容遡及日数=" + lookbackDays);
        }

        String decisionCode = best.rateDate.equals(baseDate) ? "当日採用" : "過去採用";
        return new RateReference(normalizedPair, baseDate, best.rateDate, best.ttmRate, best.sourceCode, decisionCode);
    }

    private static int resolveLookbackDays(String baseDate) {
        PsconfRow selected = null;

        for (int i = 0; i < PSCONF.length; i++) {
            PsconfRow row = PSCONF[i];
            if (!CONF_KEY_LOOKBACK_DAYS.equals(row.confKey)) {
                continue;
            }
            if (compareDate(row.applyDate, baseDate) <= 0 && compareDate(baseDate, row.expireDate) <= 0) {
                if (selected == null || compareText(row.updatedAt, selected.updatedAt) > 0) {
                    selected = row;
                }
            }
        }

        if (selected == null) {
            throw new IllegalStateException("許容遡及日数の設定なし 基準日=" + baseDate);
        }

        int days;
        try {
            days = Integer.parseInt(selected.confValue);
        } catch (NumberFormatException ex) {
            throw new IllegalStateException("許容遡及日数が数値でない 設定値=" + selected.confValue);
        }

        if (days < 0 || days > 31) {
            throw new IllegalStateException("許容遡及日数が範囲外 設定値=" + days);
        }
        return days;
    }

    private static String normalizePair(String ccyPair) {
        if (ccyPair == null) {
            throw new IllegalArgumentException("通貨ペア未指定");
        }
        String value = ccyPair.trim().toUpperCase();
        if (value.length() != 6) {
            throw new IllegalArgumentException("通貨ペア桁数不正 値=" + ccyPair);
        }
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if (ch < 'A' || ch > 'Z') {
                throw new IllegalArgumentException("通貨ペア文字種不正 値=" + ccyPair);
            }
        }
        return value;
    }

    private static void validateDate(String value, String itemName) {
        if (value == null || value.length() != 8) {
            throw new IllegalArgumentException(itemName + "桁数不正 値=" + value);
        }
        for (int i = 0; i < value.length(); i++) {
            if (!Character.isDigit(value.charAt(i))) {
                throw new IllegalArgumentException(itemName + "文字種不正 値=" + value);
            }
        }

        int year = parseInt(value, 0, 4);
        int month = parseInt(value, 4, 6);
        int day = parseInt(value, 6, 8);
        if (month < 1 || month > 12) {
            throw new IllegalArgumentException(itemName + "月不正 値=" + value);
        }
        int maxDay = daysInMonth(year, month);
        if (day < 1 || day > maxDay) {
            throw new IllegalArgumentException(itemName + "日不正 値=" + value);
        }
    }

    private static int compareDate(String left, String right) {
        validateDate(left, "比較日");
        validateDate(right, "比較日");
        return compareText(left, right);
    }

    private static int compareText(String left, String right) {
        return left.compareTo(right);
    }

    private static int daysBetween(String fromDate, String toDate) {
        return toSerialDay(toDate) - toSerialDay(fromDate);
    }

    private static int toSerialDay(String yyyymmdd) {
        validateDate(yyyymmdd, "日付");
        int year = parseInt(yyyymmdd, 0, 4);
        int month = parseInt(yyyymmdd, 4, 6);
        int day = parseInt(yyyymmdd, 6, 8);

        int serial = day;
        for (int y = 1900; y < year; y++) {
            serial += isLeapYear(y) ? 366 : 365;
        }
        for (int m = 1; m < month; m++) {
            serial += daysInMonth(year, m);
        }
        return serial;
    }

    private static int parseInt(String value, int begin, int end) {
        int result = 0;
        for (int i = begin; i < end; i++) {
            result = result * 10 + (value.charAt(i) - '0');
        }
        return result;
    }

    private static int daysInMonth(int year, int month) {
        switch (month) {
            case 2:
                return isLeapYear(year) ? 29 : 28;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            default:
                return 31;
        }
    }

    private static boolean isLeapYear(int year) {
        return year % 400 == 0 || (year % 4 == 0 && year % 100 != 0);
    }

    private static boolean isPreferredSource(String candidate, String current) {
        return sourceRank(candidate) < sourceRank(current);
    }

    private static int sourceRank(String sourceCode) {
        if ("銀行TTM".equals(sourceCode)) {
            return 1;
        }
        if ("銀行仲値".equals(sourceCode)) {
            return 2;
        }
        if ("市場仲値".equals(sourceCode)) {
            return 3;
        }
        return 9;
    }

    private static final class PsfxrfRow {
        private final String ccyPair;
        private final String rateDate;
        private final String ttmRate;
        private final String sourceCode;
        private final String loadStatus;

        private PsfxrfRow(String ccyPair, String rateDate, String ttmRate, String sourceCode, String loadStatus) {
            this.ccyPair = ccyPair;
            this.rateDate = rateDate;
            this.ttmRate = ttmRate;
            this.sourceCode = sourceCode;
            this.loadStatus = loadStatus;
        }
    }

    private static final class PsconfRow {
        private final String confKey;
        private final String confValue;
        private final String applyDate;
        private final String expireDate;
        private final String updatedAt;

        private PsconfRow(String confKey, String confValue, String applyDate, String expireDate, String updatedAt) {
            this.confKey = confKey;
            this.confValue = confValue;
            this.applyDate = applyDate;
            this.expireDate = expireDate;
            this.updatedAt = updatedAt;
        }
    }

    private static final class RateReference {
        private final String ccyPair;
        private final String requestDate;
        private final String rateDate;
        private final String ttmRate;
        private final String sourceCode;
        private final String decisionCode;

        private RateReference(String ccyPair, String requestDate, String rateDate,
                              String ttmRate, String sourceCode, String decisionCode) {
            this.ccyPair = ccyPair;
            this.requestDate = requestDate;
            this.rateDate = rateDate;
            this.ttmRate = ttmRate;
            this.sourceCode = sourceCode;
            this.decisionCode = decisionCode;
        }
    }
}
