public class MerchantSettlementFacade {
    /**
     * 変更履歴
     * 版数  年月日      担当        概要
     * 1.00  2024/03/06  保守二課    加盟店精算照会ファサードの初版作成
     * 1.01  2025/01/27  保守二課    未決チャージバックの保留控除を追加
     */
    private static final java.math.BigDecimal ZERO = java.math.BigDecimal.ZERO;
    private static final java.time.format.DateTimeFormatter DATE_FMT =
            java.time.format.DateTimeFormatter.ofPattern("uuuu-MM-dd");

    public static void main(String[] a) {
        java.util.List<SettlementRecord> settlements = readCdsetlf();
        java.util.List<ChargebackRecord> chargebacks = readCdcbkpf();

        java.util.List<MerchantSettlementView> views =
               照会一覧を作成(settlements, chargebacks, java.time.LocalDate.of(2026, 6, 28));

        for (MerchantSettlementView view : views) {
            System.out.println(view.toOperatorLine());
        }
    }

    public static java.util.List<MerchantSettlementView> 照会一覧を作成(
            java.util.List<SettlementRecord> settlements,
            java.util.List<ChargebackRecord> chargebacks,
            java.time.LocalDate businessDate) {

        if (settlements == null) {
            throw new IllegalArgumentException("精算明細が未設定です");
        }
        if (chargebacks == null) {
            throw new IllegalArgumentException("チャージバック明細が未設定です");
        }
        if (businessDate == null) {
            throw new IllegalArgumentException("処理日が未設定です");
        }

        java.util.Map<String, java.math.BigDecimal> pendingByMerchant = new java.util.HashMap<>();
        java.util.Map<String, Integer> pendingCountByMerchant = new java.util.HashMap<>();

        for (ChargebackRecord cb : chargebacks) {
            cb.validate();
            if (cb.isPending()) {
                pendingByMerchant.merge(cb.merchantCode, cb.claimAmt, java.math.BigDecimal::add);
                pendingCountByMerchant.merge(cb.merchantCode, 1, Integer::sum);
            }
        }

        java.util.List<MerchantSettlementView> result = new java.util.ArrayList<>();
        java.util.Set<String> seenSettlementIds = new java.util.HashSet<>();

        for (SettlementRecord settlement : settlements) {
            settlement.validate();

            if (!seenSettlementIds.add(settlement.settlementId)) {
                throw new IllegalStateException("精算ＩＤが重複しています: " + settlement.settlementId);
            }

            java.math.BigDecimal holdAmount =
                    pendingByMerchant.getOrDefault(settlement.merchantCode, ZERO);
            int holdCount = pendingCountByMerchant.getOrDefault(settlement.merchantCode, 0);

            java.math.BigDecimal scheduledDeposit = settlement.netAmt.subtract(holdAmount);
            if (scheduledDeposit.signum() < 0) {
                scheduledDeposit = ZERO;
            }

            String auditText = "精算ＩＤ=" + settlement.settlementId
                    + ",総額=" + money(settlement.grossAmt)
                    + ",純額=" + money(settlement.netAmt)
                    + ",調整額=" + money(settlement.adjAmt)
                    + ",未決保留=" + money(holdAmount);

            result.add(new MerchantSettlementView(
                    settlement.settlementId,
                    settlement.merchantCode,
                    settlement.settleDt,
                    settlement.settleStatus,
                    settlement.grossAmt,
                    settlement.netAmt,
                    settlement.adjAmt,
                    scheduledDeposit,
                    holdAmount,
                    holdCount,
                    auditText));
        }

        result.sort(java.util.Comparator
                .comparing((MerchantSettlementView v) -> v.settleDt)
                .thenComparing(v -> v.merchantCode)
                .thenComparing(v -> v.settlementId));

        return java.util.Collections.unmodifiableList(result);
    }

    private static java.util.List<SettlementRecord> readCdsetlf() {
        java.util.List<SettlementRecord> rows = new java.util.ArrayList<>();
        rows.add(new SettlementRecord("ST202606270001", "MRC-TKY-0001",
                java.time.LocalDate.of(2026, 6, 27),
                bd("15428000"), bd("15311240"), bd("-42000"), "確定"));
        rows.add(new SettlementRecord("ST202606270002", "MRC-OSA-0107",
                java.time.LocalDate.of(2026, 6, 27),
                bd("8935000"), bd("8870410"), bd("0"), "確定"));
        rows.add(new SettlementRecord("ST202606270003", "MRC-FUK-0044",
                java.time.LocalDate.of(2026, 6, 27),
                bd("4210000"), bd("4179120"), bd("-18000"), "確定"));
        rows.add(new SettlementRecord("ST202606280001", "MRC-TKY-0001",
                java.time.LocalDate.of(2026, 6, 28),
                bd("11248000"), bd("11166360"), bd("0"), "作成済"));
        return rows;
    }

    private static java.util.List<ChargebackRecord> readCdcbkpf() {
        java.util.List<ChargebackRecord> rows = new java.util.ArrayList<>();
        rows.add(new ChargebackRecord("CB202606250017", "SL202606210944", "4980********1124",
                "MRC-TKY-0001", bd("128000"), "本人利用否認", "未決"));
        rows.add(new ChargebackRecord("CB202606260031", "SL202606221102", "4980********8831",
                "MRC-TKY-0001", bd("64000"), "商品未着", "未決"));
        rows.add(new ChargebackRecord("CB202606260044", "SL202606221477", "4980********2190",
                "MRC-OSA-0107", bd("92500"), "二重請求", "未決"));
        rows.add(new ChargebackRecord("CB202606240009", "SL202606190377", "4980********4458",
                "MRC-FUK-0044", bd("31000"), "金額相違", "解決"));
        return rows;
    }

    private static java.math.BigDecimal bd(String value) {
        return new java.math.BigDecimal(value).setScale(0);
    }

    private static String money(java.math.BigDecimal value) {
        return value.setScale(0, java.math.RoundingMode.UNNECESSARY).toPlainString();
    }

    public static final class SettlementRecord {
        public final String settlementId;
        public final String merchantCode;
        public final java.time.LocalDate settleDt;
        public final java.math.BigDecimal grossAmt;
        public final java.math.BigDecimal netAmt;
        public final java.math.BigDecimal adjAmt;
        public final String settleStatus;

        public SettlementRecord(String settlementId, String merchantCode, java.time.LocalDate settleDt,
                                java.math.BigDecimal grossAmt, java.math.BigDecimal netAmt,
                                java.math.BigDecimal adjAmt, String settleStatus) {
            this.settlementId = settlementId;
            this.merchantCode = merchantCode;
            this.settleDt = settleDt;
            this.grossAmt = grossAmt;
            this.netAmt = netAmt;
            this.adjAmt = adjAmt;
            this.settleStatus = settleStatus;
        }

        void validate() {
            requireText(settlementId, "精算ＩＤ");
            requireText(merchantCode, "加盟店コード");
            requireText(settleStatus, "精算状態");
            requireDate(settleDt, "精算日");
            requireAmount(grossAmt, "総額");
            requireAmount(netAmt, "純額");
            requireAmount(adjAmt, "調整額");

            if (grossAmt.signum() < 0 || netAmt.signum() < 0) {
                throw new IllegalArgumentException("精算金額に負数があります: " + settlementId);
            }
            if (!"作成済".equals(settleStatus) && !"確定".equals(settleStatus) && !"保留".equals(settleStatus)) {
                throw new IllegalArgumentException("精算状態が不正です: " + settlementId);
            }
        }
    }

    public static final class ChargebackRecord {
        public final String chargebackId;
        public final String saleId;
        public final String cardNo;
        public final String merchantCode;
        public final java.math.BigDecimal claimAmt;
        public final String claimReason;
        public final String caseStatus;

        public ChargebackRecord(String chargebackId, String saleId, String cardNo, String merchantCode,
                                java.math.BigDecimal claimAmt, String claimReason, String caseStatus) {
            this.chargebackId = chargebackId;
            this.saleId = saleId;
            this.cardNo = cardNo;
            this.merchantCode = merchantCode;
            this.claimAmt = claimAmt;
            this.claimReason = claimReason;
            this.caseStatus = caseStatus;
        }

        boolean isPending() {
            return "未決".equals(caseStatus) || "調査中".equals(caseStatus);
        }

        void validate() {
            requireText(chargebackId, "チャージバックＩＤ");
            requireText(saleId, "売上ＩＤ");
            requireText(cardNo, "カード番号");
            requireText(merchantCode, "加盟店コード");
            requireText(claimReason, "申立理由");
            requireText(caseStatus, "案件状態");
            requireAmount(claimAmt, "申立額");

            if (claimAmt.signum() <= 0) {
                throw new IllegalArgumentException("申立額が不正です: " + chargebackId);
            }
            if (!"未決".equals(caseStatus) && !"調査中".equals(caseStatus)
                    && !"解決".equals(caseStatus) && !"取下".equals(caseStatus)) {
                throw new IllegalArgumentException("案件状態が不正です: " + chargebackId);
            }
        }
    }

    public static final class MerchantSettlementView {
        public final String settlementId;
        public final String merchantCode;
        public final java.time.LocalDate settleDt;
        public final String settleStatus;
        public final java.math.BigDecimal grossAmt;
        public final java.math.BigDecimal netAmt;
        public final java.math.BigDecimal adjAmt;
        public final java.math.BigDecimal scheduledDepositAmt;
        public final java.math.BigDecimal holdAmt;
        public final int holdCaseCount;
        public final String auditText;

        MerchantSettlementView(String settlementId, String merchantCode, java.time.LocalDate settleDt,
                               String settleStatus, java.math.BigDecimal grossAmt, java.math.BigDecimal netAmt,
                               java.math.BigDecimal adjAmt, java.math.BigDecimal scheduledDepositAmt,
                               java.math.BigDecimal holdAmt, int holdCaseCount, String auditText) {
            this.settlementId = settlementId;
            this.merchantCode = merchantCode;
            this.settleDt = settleDt;
            this.settleStatus = settleStatus;
            this.grossAmt = grossAmt;
            this.netAmt = netAmt;
            this.adjAmt = adjAmt;
            this.scheduledDepositAmt = scheduledDepositAmt;
            this.holdAmt = holdAmt;
            this.holdCaseCount = holdCaseCount;
            this.auditText = auditText;
        }

        String toOperatorLine() {
            return "精算照会"
                    + " 精算日=" + DATE_FMT.format(settleDt)
                    + " 加盟店=" + merchantCode
                    + " 状態=" + settleStatus
                    + " 入金予定額=" + money(scheduledDepositAmt)
                    + " 保留額=" + money(holdAmt)
                    + " 未決件数=" + holdCaseCount
                    + " 監査=[" + auditText + "]";
        }
    }

    private static void requireText(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "が未設定です");
        }
    }

    private static void requireDate(java.time.LocalDate value, String name) {
        if (value == null) {
            throw new IllegalArgumentException(name + "が未設定です");
        }
    }

    private static void requireAmount(java.math.BigDecimal value, String name) {
        if (value == null) {
            throw new IllegalArgumentException(name + "が未設定です");
        }
        if (value.scale() > 0) {
            throw new IllegalArgumentException(name + "は円単位で設定してください");
        }
    }
}
