package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025/06/29  共通基盤  初版作成
 */
public class CommonCodeLookupService {

    private static final long CACHE_TTL_MILLIS = 3_000L;
    private static final java.time.format.DateTimeFormatter DATE_FORMAT =
            java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

    private static final java.util.List<CmcodfRecord> CMCODF =
            java.util.Collections.unmodifiableList(loadSyntheticCmcodf());

    private static final java.util.Map<String, CmcodfRecord> KSDS =
            java.util.Collections.unmodifiableMap(buildKsds(CMCODF));

    private static final java.util.Map<String, CacheEntry> CACHE =
            new java.util.LinkedHashMap<String, CacheEntry>(256, 0.75f, true) {
                @Override
                protected boolean removeEldestEntry(java.util.Map.Entry<String, CacheEntry> eldest) {
                    return size() > 512;
                }
            };

    private CommonCodeLookupService() {
    }

    public static void main(String[] a) {
        java.time.LocalDate kijunbi = a.length >= 3 ? parseDate(a[2], "有効日") : java.time.LocalDate.now();
        java.util.List<StandardCode> result = lookupAvailableStandardCodes(a.length >= 1 ? a[0] : "KINYU_KBN",
                a.length >= 2 ? a[1] : "01", kijunbi);

        for (StandardCode code : result) {
            System.out.println("コード種別=" + code.codeType
                    + ", コード値=" + code.codeValue
                    + ", 標準区分=" + code.standardKbn
                    + ", 適用開始日=" + DATE_FORMAT.format(code.validFrom)
                    + ", 適用終了日=" + DATE_FORMAT.format(code.validTo));
        }
    }

    private static java.util.List<StandardCode> lookupAvailableStandardCodes(
            String codeType, String codeValue, java.time.LocalDate effectiveDate) {
        String normalizedType = requireCode("コード種別", codeType, 20);
        String normalizedValue = requireCode("コード値", codeValue, 20);
        if (effectiveDate == null) {
            throw new IllegalArgumentException("有効日が未設定です。");
        }

        String cacheKey = normalizedType + '\u001f' + normalizedValue + '\u001f' + DATE_FORMAT.format(effectiveDate);
        long now = System.currentTimeMillis();

        synchronized (CACHE) {
            CacheEntry cached = CACHE.get(cacheKey);
            if (cached != null && cached.expiresAtMillis > now) {
                return cached.values;
            }
        }

        java.util.List<CmcodfRecord> currentRecords = readKsdsCurrentValues(normalizedType, normalizedValue);
        java.util.List<StandardCode> available = new java.util.ArrayList<>();

        for (CmcodfRecord record : currentRecords) {
            if (record.validFrom.compareTo(effectiveDate) <= 0
                    && record.validTo.compareTo(effectiveDate) >= 0
                    && "1".equals(record.codeStatusKbn)
                    && isStandardKbn(record)) {
                available.add(new StandardCode(record.codeType, record.codeValue,
                        record.standardKbn, record.validFrom, record.validTo));
            }
        }

        available.sort(java.util.Comparator
                .comparing((StandardCode c) -> c.codeType)
                .thenComparing(c -> c.codeValue)
                .thenComparing(c -> c.standardKbn));

        java.util.List<StandardCode> fixed = java.util.Collections.unmodifiableList(available);
        synchronized (CACHE) {
            CACHE.put(cacheKey, new CacheEntry(fixed, now + CACHE_TTL_MILLIS));
        }
        return fixed;
    }

    private static java.util.List<CmcodfRecord> readKsdsCurrentValues(String codeType, String codeValue) {
        java.util.List<CmcodfRecord> records = new java.util.ArrayList<>();
        String prefix = codeType + '\u001f' + codeValue + '\u001f';

        for (java.util.Map.Entry<String, CmcodfRecord> entry : KSDS.entrySet()) {
            if (entry.getKey().startsWith(prefix)) {
                records.add(entry.getValue());
            }
        }
        return records;
    }

    private static boolean isStandardKbn(CmcodfRecord record) {
        return "STD".equals(record.standardKbn) || "ZK".equals(record.standardKbn);
    }

    private static String requireCode(String name, String value, int maxLength) {
        if (value == null) {
            throw new IllegalArgumentException(name + "が未設定です。");
        }
        String trimmed = value.trim();
        if (trimmed.isEmpty()) {
            throw new IllegalArgumentException(name + "が空です。");
        }
        if (trimmed.length() > maxLength) {
            throw new IllegalArgumentException(name + "が桁数上限を超過しています。");
        }
        for (int i = 0; i < trimmed.length(); i++) {
            char ch = trimmed.charAt(i);
            if (!(ch >= 'A' && ch <= 'Z') && !(ch >= '0' && ch <= '9') && ch != '_') {
                throw new IllegalArgumentException(name + "に使用できない文字があります。");
            }
        }
        return trimmed;
    }

    private static java.time.LocalDate parseDate(String value, String name) {
        try {
            return java.time.LocalDate.parse(requireCode(name, value, 8), DATE_FORMAT);
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException(name + "の日付形式が不正です。", e);
        }
    }

    private static java.util.Map<String, CmcodfRecord> buildKsds(java.util.List<CmcodfRecord> records) {
        java.util.Map<String, CmcodfRecord> map = new java.util.TreeMap<>();
        for (CmcodfRecord record : records) {
            String key = record.codeType + '\u001f'
                    + record.codeValue + '\u001f'
                    + DATE_FORMAT.format(record.validFrom);
            CmcodfRecord previous = map.put(key, record);
            if (previous != null) {
                throw new IllegalStateException("CMCODFのキーが重複しています。");
            }
        }
        return map;
    }

    private static java.util.List<CmcodfRecord> loadSyntheticCmcodf() {
        java.util.List<CmcodfRecord> records = new java.util.ArrayList<>();

        records.add(record("KINYU_KBN", "01", "20200101", "99991231", "1", "STD"));
        records.add(record("KINYU_KBN", "02", "20200101", "99991231", "1", "STD"));
        records.add(record("KINYU_KBN", "99", "20200101", "99991231", "0", "STD"));
        records.add(record("KINYU_KBN", "01", "20180401", "20191231", "1", "OLD"));

        records.add(record("KANJO_KBN", "100", "20210401", "99991231", "1", "ZK"));
        records.add(record("KANJO_KBN", "200", "20210401", "99991231", "1", "ZK"));
        records.add(record("KANJO_KBN", "900", "20210401", "99991231", "1", "LOCAL"));

        records.add(record("SOSA_KBN", "A1", "20220401", "99991231", "1", "STD"));
        records.add(record("SOSA_KBN", "A2", "20220401", "20250331", "1", "STD"));
        records.add(record("SOSA_KBN", "A2", "20250401", "99991231", "2", "STD"));

        return records;
    }

    private static CmcodfRecord record(String codeType, String codeValue, String validFrom,
                                      String validTo, String status, String standardKbn) {
        return new CmcodfRecord(codeType, codeValue,
                parseDate(validFrom, "適用開始日"),
                parseDate(validTo, "適用終了日"),
                status,
                standardKbn);
    }

    private static final class CacheEntry {
        private final java.util.List<StandardCode> values;
        private final long expiresAtMillis;

        private CacheEntry(java.util.List<StandardCode> values, long expiresAtMillis) {
            this.values = values;
            this.expiresAtMillis = expiresAtMillis;
        }
    }

    private static final class StandardCode {
        private final String codeType;
        private final String codeValue;
        private final String standardKbn;
        private final java.time.LocalDate validFrom;
        private final java.time.LocalDate validTo;

        private StandardCode(String codeType, String codeValue, String standardKbn,
                             java.time.LocalDate validFrom, java.time.LocalDate validTo) {
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.standardKbn = standardKbn;
            this.validFrom = validFrom;
            this.validTo = validTo;
        }
    }

    private static final class CmcodfRecord {
        private final String codeType;
        private final String codeValue;
        private final java.time.LocalDate validFrom;
        private final java.time.LocalDate validTo;
        private final String codeStatusKbn;
        private final String standardKbn;

        private CmcodfRecord(String codeType, String codeValue,
                             java.time.LocalDate validFrom, java.time.LocalDate validTo,
                             String codeStatusKbn, String standardKbn) {
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.codeStatusKbn = codeStatusKbn;
            this.standardKbn = standardKbn;
        }
    }
}
