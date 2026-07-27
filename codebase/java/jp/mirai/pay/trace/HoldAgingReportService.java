package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2025/07/08  みらいペイ システム部 精算・連携チーム  初版作成
 */
public class HoldAgingReportService {
    private static final String HD_STATUS_APPROVED = "00";
    private static final String HD_STATUS_SETTLED = "30";
    private static final String HD_STATUS_CANCELLED = "20";

    private static final String SETTLE_KBN_IMMEDIATE = "1";
    private static final String SETTLE_KBN_CARRY_OVER = "2";
    private static final String SETTLE_KBN_EXCLUDED = "9";

    private static final String ACTIVE = "1";
    private static final String EXPIRE_DONE = "90";
    private static final String EXPIRE_PENDING = "00";

    private static final java.time.LocalDate BUSINESS_DATE = java.time.LocalDate.of(2025, 6, 28);

    public void run() {
        java.util.List<PtholdfRow> holdRows = loadPtholdf();
        java.util.List<PthxpfRow> expireRows = loadPthxpf();
        java.util.Map<String, PjmstfRow> merchantRows = loadPjmstf();

        java.util.Map<String, PthxpfRow> expireByHoldId = new java.util.HashMap<String, PthxpfRow>();
        for (PthxpfRow row : expireRows) {
            if (isBlank(row.holdId) || isBlank(row.merchantCode) || row.expireAt == null) {
                continue;
            }
            expireByHoldId.put(row.holdId, row);
        }

        java.util.Map<String, Summary> summaries = new java.util.TreeMap<String, Summary>();
        for (PtholdfRow hold : holdRows) {
            if (!isValidHold(hold)) {
                continue;
            }

            PjmstfRow merchant = merchantRows.get(hold.merchantCode);
            if (merchant == null || !ACTIVE.equals(merchant.activeFlag)) {
                continue;
            }

            Summary summary = summaries.get(hold.merchantCode);
            if (summary == null) {
                summary = new Summary(merchant);
                summaries.put(hold.merchantCode, summary);
            }

            if (HD_STATUS_APPROVED.equals(hold.holdStatus)) {
                summary.openHoldCount++;
                summary.holdAmountTotal = summary.holdAmountTotal.add(hold.holdAmount);
                java.time.LocalDate holdDate = parseHoldDate(hold.holdId);
                if (holdDate != null && (summary.oldestHoldDate == null || holdDate.isBefore(summary.oldestHoldDate))) {
                    summary.oldestHoldDate = holdDate;
                }

                PthxpfRow expire = expireByHoldId.get(hold.holdId);
                if (expire != null && EXPIRE_PENDING.equals(expire.expireStatus)
                        && !expire.expireAt.isAfter(BUSINESS_DATE.plusDays(3))) {
                    summary.expireCandidateCount++;
                }
            }
        }

        for (PthxpfRow expire : expireRows) {
            PjmstfRow merchant = merchantRows.get(expire.merchantCode);
            if (merchant == null || !ACTIVE.equals(merchant.activeFlag)) {
                continue;
            }
            if (EXPIRE_DONE.equals(expire.expireStatus)) {
                Summary summary = summaries.get(expire.merchantCode);
                if (summary == null) {
                    summary = new Summary(merchant);
                    summaries.put(expire.merchantCode, summary);
                }
                summary.expiredCount++;
            }
        }

        System.out.println("加盟店コード,加盟店名,銀行コード,リスクランク,未確定ホールド件数,失効候補件数,失効済み件数,ホールド金額合計,最古ホールド日,精算区分");
        for (Summary summary : summaries.values()) {
            System.out.println(summary.toCsv());
        }
    }

    private static boolean isValidHold(PtholdfRow row) {
        if (row == null || isBlank(row.holdId) || isBlank(row.walletId) || isBlank(row.merchantCode)) {
            return false;
        }
        if (row.holdAmount == null || row.holdAmount.signum() < 0) {
            return false;
        }
        return HD_STATUS_APPROVED.equals(row.holdStatus)
                || HD_STATUS_SETTLED.equals(row.holdStatus)
                || HD_STATUS_CANCELLED.equals(row.holdStatus);
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static java.time.LocalDate parseHoldDate(String holdId) {
        if (holdId == null || holdId.length() < 8) {
            return null;
        }
        String yyyymmdd = holdId.substring(0, 8);
        try {
            return java.time.LocalDate.parse(yyyymmdd, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        } catch (java.time.DateTimeException e) {
            return null;
        }
    }

    private static java.util.List<PtholdfRow> loadPtholdf() {
        java.util.List<PtholdfRow> rows = new java.util.ArrayList<PtholdfRow>();
        rows.add(new PtholdfRow("202506010001", "WLT10001", "MRC001", bd("18420"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506050002", "WLT10002", "MRC001", bd("9600"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506120003", "WLT10003", "MRC002", bd("23500"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506140004", "WLT10004", "MRC002", bd("4100"), HD_STATUS_SETTLED));
        rows.add(new PtholdfRow("202506170005", "WLT10005", "MRC003", bd("7800"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506190006", "WLT10006", "MRC003", bd("12200"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506200007", "WLT10007", "MRC004", bd("6400"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506210008", "WLT10008", "MRC005", bd("19800"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506230009", "WLT10009", "MRC006", bd("5100"), HD_STATUS_CANCELLED));
        rows.add(new PtholdfRow("202506240010", "WLT10010", "MRC006", bd("33000"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506250011", "WLT10011", "MRC007", bd("2700"), HD_STATUS_APPROVED));
        rows.add(new PtholdfRow("202506260012", "WLT10012", "MRC008", bd("44200"), HD_STATUS_APPROVED));
        return rows;
    }

    private static java.util.List<PthxpfRow> loadPthxpf() {
        java.util.List<PthxpfRow> rows = new java.util.ArrayList<PthxpfRow>();
        rows.add(new PthxpfRow("202506010001", "WLT10001", "MRC001", java.time.LocalDate.of(2025, 6, 29), "01", EXPIRE_PENDING));
        rows.add(new PthxpfRow("202506050002", "WLT10002", "MRC001", java.time.LocalDate.of(2025, 7, 4), "01", EXPIRE_PENDING));
        rows.add(new PthxpfRow("202506120003", "WLT10003", "MRC002", java.time.LocalDate.of(2025, 6, 30), "02", EXPIRE_PENDING));
        rows.add(new PthxpfRow("202506170005", "WLT10005", "MRC003", java.time.LocalDate.of(2025, 6, 26), "03", EXPIRE_DONE));
        rows.add(new PthxpfRow("202506190006", "WLT10006", "MRC003", java.time.LocalDate.of(2025, 7, 1), "01", EXPIRE_PENDING));
        rows.add(new PthxpfRow("202506210008", "WLT10008", "MRC005", java.time.LocalDate.of(2025, 6, 27), "02", EXPIRE_DONE));
        rows.add(new PthxpfRow("202506240010", "WLT10010", "MRC006", java.time.LocalDate.of(2025, 7, 2), "01", EXPIRE_PENDING));
        rows.add(new PthxpfRow("202506260012", "WLT10012", "MRC008", java.time.LocalDate.of(2025, 6, 28), "04", EXPIRE_PENDING));
        return rows;
    }

    private static java.util.Map<String, PjmstfRow> loadPjmstf() {
        java.util.Map<String, PjmstfRow> rows = new java.util.HashMap<String, PjmstfRow>();
        rows.put("MRC001", new PjmstfRow("MRC001", "青葉書店", "0005", "1023344", ACTIVE, "B"));
        rows.put("MRC002", new PjmstfRow("MRC002", "東都薬局", "0009", "9032211", ACTIVE, "A"));
        rows.put("MRC003", new PjmstfRow("MRC003", "南町電機", "0017", "7730100", ACTIVE, "C"));
        rows.put("MRC004", new PjmstfRow("MRC004", "北浜雑貨", "0033", "2250901", "0", "B"));
        rows.put("MRC005", new PjmstfRow("MRC005", "みらい用品", "0005", "6641207", ACTIVE, "D"));
        rows.put("MRC006", new PjmstfRow("MRC006", "中央旅行社", "0042", "1800234", ACTIVE, "C"));
        rows.put("MRC007", new PjmstfRow("MRC007", "港町食堂", "0036", "4507782", "0", "A"));
        rows.put("MRC008", new PjmstfRow("MRC008", "桜橋家具", "0001", "9987012", ACTIVE, "B"));
        return rows;
    }

    private static java.math.BigDecimal bd(String value) {
        return new java.math.BigDecimal(value);
    }

    private static String settlementKbn(PjmstfRow merchant) {
        if ("D".equals(merchant.riskRank)) {
            return SETTLE_KBN_EXCLUDED;
        }
        if ("C".equals(merchant.riskRank)) {
            return SETTLE_KBN_CARRY_OVER;
        }
        return SETTLE_KBN_IMMEDIATE;
    }

    private static final class PtholdfRow {
        private final String holdId;
        private final String walletId;
        private final String merchantCode;
        private final java.math.BigDecimal holdAmount;
        private final String holdStatus;

        private PtholdfRow(String holdId, String walletId, String merchantCode,
                           java.math.BigDecimal holdAmount, String holdStatus) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.holdAmount = holdAmount;
            this.holdStatus = holdStatus;
        }
    }

    private static final class PthxpfRow {
        private final String holdId;
        private final String walletId;
        private final String merchantCode;
        private final java.time.LocalDate expireAt;
        private final String reasonCode;
        private final String expireStatus;

        private PthxpfRow(String holdId, String walletId, String merchantCode,
                          java.time.LocalDate expireAt, String reasonCode, String expireStatus) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.expireAt = expireAt;
            this.reasonCode = reasonCode;
            this.expireStatus = expireStatus;
        }
    }

    private static final class PjmstfRow {
        private final String merchantCode;
        private final String merchantName;
        private final String bankCode;
        private final String accountNo;
        private final String activeFlag;
        private final String riskRank;

        private PjmstfRow(String merchantCode, String merchantName, String bankCode,
                          String accountNo, String activeFlag, String riskRank) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.bankCode = bankCode;
            this.accountNo = accountNo;
            this.activeFlag = activeFlag;
            this.riskRank = riskRank;
        }
    }

    private static final class Summary {
        private final PjmstfRow merchant;
        private int openHoldCount;
        private int expireCandidateCount;
        private int expiredCount;
        private java.math.BigDecimal holdAmountTotal = java.math.BigDecimal.ZERO;
        private java.time.LocalDate oldestHoldDate;

        private Summary(PjmstfRow merchant) {
            this.merchant = merchant;
        }

        private String toCsv() {
            return merchant.merchantCode + ","
                    + merchant.merchantName + ","
                    + merchant.bankCode + ","
                    + merchant.riskRank + ","
                    + openHoldCount + ","
                    + expireCandidateCount + ","
                    + expiredCount + ","
                    + holdAmountTotal.toPlainString() + ","
                    + (oldestHoldDate == null ? "" : oldestHoldDate) + ","
                    + settlementKbn(merchant);
        }
    }
}
