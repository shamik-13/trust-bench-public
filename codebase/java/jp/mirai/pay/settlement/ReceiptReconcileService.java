package jp.mirai.pay.settlement;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/09/10  加盟店精算チーム  入金消込照会サービス初版
 * 1.01  2025/03/04  加盟店精算チーム  手数料・支払予定は精算格納値を参照する方式に変更
 */
public class ReceiptReconcileService {
    private static final String STATUS_SETTLEABLE = "01";
    private static final java.time.format.DateTimeFormatter DATE_FMT = java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
    private static final java.time.format.DateTimeFormatter TS_FMT = java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

    public static void main(String[] a) {
        java.util.List<ReceiptRecord> receipts = new java.util.ArrayList<>();
        receipts.add(new ReceiptRecord("RC202606250001", "M000001", new java.math.BigDecimal("997000"), java.time.LocalDate.of(2026, 6, 25), "未消込", "ST20260625001"));
        receipts.add(new ReceiptRecord("RC202606250002", "M000002", new java.math.BigDecimal("498500"), java.time.LocalDate.of(2026, 6, 25), "銀行戻り待ち", "ST20260625002"));
        receipts.add(new ReceiptRecord("RC202606260001", "M000001", new java.math.BigDecimal("299100"), java.time.LocalDate.of(2026, 6, 26), "差額あり", "ST20260626001"));
        receipts.add(new ReceiptRecord("RC202606260002", "M000003", new java.math.BigDecimal("199400"), java.time.LocalDate.of(2026, 6, 26), "未消込", "ST20260626002"));

        java.util.List<SettlementRecord> settlements = new java.util.ArrayList<>();
        settlements.add(new SettlementRecord("ST20260625001", "M000001", new java.math.BigDecimal("1000000"), new java.math.BigDecimal("3000"), new java.math.BigDecimal("997000"), java.time.LocalDate.of(2026, 6, 25)));
        settlements.add(new SettlementRecord("ST20260625002", "M000002", new java.math.BigDecimal("500000"), new java.math.BigDecimal("1500"), new java.math.BigDecimal("498500"), java.time.LocalDate.of(2026, 6, 25)));
        settlements.add(new SettlementRecord("ST20260626001", "M000001", new java.math.BigDecimal("300000"), new java.math.BigDecimal("900"), new java.math.BigDecimal("299100"), java.time.LocalDate.of(2026, 6, 26)));
        settlements.add(new SettlementRecord("ST20260626002", "M000003", new java.math.BigDecimal("200000"), new java.math.BigDecimal("600"), new java.math.BigDecimal("199400"), java.time.LocalDate.of(2026, 6, 26)));

        java.util.List<PayoutRecord> payouts = new java.util.ArrayList<>();
        payouts.add(new PayoutRecord("PO202606260001", "M000001", "0001234567", new java.math.BigDecimal("997000"), java.time.LocalDate.of(2026, 6, 26), "00"));
        payouts.add(new PayoutRecord("PO202606260002", "M000002", "0002345678", new java.math.BigDecimal("498500"), java.time.LocalDate.of(2026, 6, 26), "00"));
        payouts.add(new PayoutRecord("PO202606270001", "M000001", "0001234567", new java.math.BigDecimal("298900"), java.time.LocalDate.of(2026, 6, 27), "00"));
        payouts.add(new PayoutRecord("PO202606270002", "M000003", "0003456789", new java.math.BigDecimal("199400"), java.time.LocalDate.of(2026, 6, 27), "91"));

        java.util.Map<String, String> merchantStatus = new java.util.HashMap<>();
        merchantStatus.put("M000001", "01");
        merchantStatus.put("M000002", "01");
        merchantStatus.put("M000003", "02");

        SearchCondition condition = SearchCondition.fromArgs(a);
        ReconcileStore store = new ReconcileStore(receipts, settlements, payouts, merchantStatus);
        java.util.List<ReceiptView> result = store.search(condition);

        System.out.println("入金消込照会サービス");
        System.out.println("検索条件 加盟店=" + valueOrAll(condition.merchantCode)
                + " 精算ID=" + valueOrAll(condition.settleId)
                + " 銀行結果=" + valueOrAll(condition.bankResultCode));

        if (result.isEmpty()) {
            System.out.println("対象データなし");
            return;
        }

        for (ReceiptView view : result) {
            System.out.println(view.toDisplayLine());
        }

        if (condition.confirmReceiptId != null && condition.confirmUser != null) {
            ReceiptRecord updated = store.markConfirmed(condition.confirmReceiptId, condition.confirmUser, java.time.LocalDateTime.now());
            System.out.println("確認反映 RECEIPT-ID=" + updated.receiptId + " MATCH-STATUS=" + updated.matchStatus);
        }
    }

    private static String valueOrAll(String v) {
        return v == null || v.trim().isEmpty() ? "全件" : v;
    }

    private static final class ReconcileStore {
        private final java.util.Map<String, ReceiptRecord> receiptById = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, SettlementRecord> settlementById = new java.util.HashMap<>();
        private final java.util.Map<String, java.util.List<PayoutRecord>> payoutsByMerchant = new java.util.HashMap<>();
        private final java.util.Map<String, String> merchantStatus;

        private ReconcileStore(java.util.List<ReceiptRecord> receipts,
                               java.util.List<SettlementRecord> settlements,
                               java.util.List<PayoutRecord> payouts,
                               java.util.Map<String, String> merchantStatus) {
            for (ReceiptRecord receipt : receipts) {
                validateReceipt(receipt);
                receiptById.put(receipt.receiptId, receipt);
            }
            for (SettlementRecord settlement : settlements) {
                validateSettlement(settlement);
                settlementById.put(settlement.settleId, settlement);
            }
            for (PayoutRecord payout : payouts) {
                validatePayout(payout);
                payoutsByMerchant.computeIfAbsent(payout.merchantCode, k -> new java.util.ArrayList<>()).add(payout);
            }
            this.merchantStatus = new java.util.HashMap<>(merchantStatus);
        }

        private java.util.List<ReceiptView> search(SearchCondition condition) {
            java.util.List<ReceiptView> views = new java.util.ArrayList<>();
            for (ReceiptRecord receipt : receiptById.values()) {
                if (condition.merchantCode != null && !condition.merchantCode.equals(receipt.merchantCode)) {
                    continue;
                }
                if (condition.settleId != null && !condition.settleId.equals(receipt.settleId)) {
                    continue;
                }

                SettlementRecord settlement = settlementById.get(receipt.settleId);
                if (settlement == null || !receipt.merchantCode.equals(settlement.merchantCode)) {
                    continue;
                }

                java.util.List<PayoutRecord> payouts = payoutsByMerchant.getOrDefault(receipt.merchantCode, java.util.Collections.emptyList());
                java.math.BigDecimal bankOkTotal = java.math.BigDecimal.ZERO;
                String bankResult = "該当なし";
                boolean bankResultMatched = condition.bankResultCode == null;

                for (PayoutRecord payout : payouts) {
                    if (payout.payoutDt.isBefore(settlement.settleDt)) {
                        continue;
                    }
                    if (condition.bankResultCode != null && condition.bankResultCode.equals(payout.bankResultCode)) {
                        bankResultMatched = true;
                    }
                    if ("00".equals(payout.bankResultCode)) {
                        bankOkTotal = bankOkTotal.add(payout.payoutAmt);
                    }
                    bankResult = payout.bankResultCode;
                }

                if (!bankResultMatched) {
                    continue;
                }

                // 手数料・支払予定額は精算（PSSETF）に格納済みの値を参照する。
                java.math.BigDecimal expectedPayout = settlement.payoutAmt;
                java.math.BigDecimal receiptDiff = receipt.receiptAmt.subtract(expectedPayout);
                // 精算ヘッダ内の整合（純額＝手数料＋支払予定）のみ確認する。
                boolean chargeValid = settlement.netAmt.subtract(settlement.chargeAmt).compareTo(settlement.payoutAmt) == 0;
                boolean payoutValid = receipt.receiptAmt.compareTo(settlement.payoutAmt) == 0;
                boolean merchantSettleable = STATUS_SETTLEABLE.equals(merchantStatus.get(receipt.merchantCode));

                views.add(new ReceiptView(receipt, settlement, bankOkTotal, bankResult, receiptDiff, chargeValid, payoutValid, merchantSettleable));
            }
            return views;
        }

        private ReceiptRecord markConfirmed(String receiptId, String user, java.time.LocalDateTime confirmedAt) {
            ReceiptRecord current = receiptById.get(receiptId);
            if (current == null) {
                throw new IllegalArgumentException("確認対象の入金が存在しません: " + receiptId);
            }
            if (user == null || user.trim().isEmpty()) {
                throw new IllegalArgumentException("確認者が未指定です");
            }
            String suffix = " 手動確認済(" + user.trim() + "," + confirmedAt.format(TS_FMT) + ")";
            String newStatus = current.matchStatus.contains("手動確認済") ? current.matchStatus : current.matchStatus + suffix;
            ReceiptRecord updated = new ReceiptRecord(current.receiptId, current.merchantCode, current.receiptAmt, current.receiptDt, newStatus, current.settleId);
            receiptById.put(receiptId, updated);
            return updated;
        }

        private static void validateReceipt(ReceiptRecord r) {
            requireText(r.receiptId, "RECEIPT-ID");
            requireText(r.merchantCode, "MERCHANT-CODE");
            requireText(r.matchStatus, "MATCH-STATUS");
            requireText(r.settleId, "SETTLE-ID");
            requireNonNegative(r.receiptAmt, "RECEIPT-AMT");
            requireDate(r.receiptDt, "RECEIPT-DT");
        }

        private static void validateSettlement(SettlementRecord s) {
            requireText(s.settleId, "SETTLE-ID");
            requireText(s.merchantCode, "MERCHANT-CODE");
            requireNonNegative(s.netAmt, "NET-AMT");
            requireNonNegative(s.chargeAmt, "CHARGE-AMT");
            requireNonNegative(s.payoutAmt, "PAYOUT-AMT");
            requireDate(s.settleDt, "SETTLE-DT");
            if (s.netAmt.subtract(s.chargeAmt).compareTo(s.payoutAmt) != 0) {
                throw new IllegalArgumentException("精算金額不整合: " + s.settleId);
            }
        }

        private static void validatePayout(PayoutRecord p) {
            requireText(p.payoutId, "PAYOUT-ID");
            requireText(p.merchantCode, "MERCHANT-CODE");
            requireText(p.bankAcctNo, "BANK-ACCT-NO");
            requireText(p.bankResultCode, "BANK-RESULT-CD");
            requireNonNegative(p.payoutAmt, "PAYOUT-AMT");
            requireDate(p.payoutDt, "PAYOUT-DT");
        }

        private static void requireText(String v, String name) {
            if (v == null || v.trim().isEmpty()) {
                throw new IllegalArgumentException(name + "が未設定です");
            }
        }

        private static void requireNonNegative(java.math.BigDecimal v, String name) {
            if (v == null || v.signum() < 0) {
                throw new IllegalArgumentException(name + "が不正です");
            }
        }

        private static void requireDate(java.time.LocalDate v, String name) {
            if (v == null) {
                throw new IllegalArgumentException(name + "が未設定です");
            }
        }
    }

    private static final class SearchCondition {
        private final String merchantCode;
        private final String settleId;
        private final String bankResultCode;
        private final String confirmReceiptId;
        private final String confirmUser;

        private SearchCondition(String merchantCode, String settleId, String bankResultCode, String confirmReceiptId, String confirmUser) {
            this.merchantCode = blankToNull(merchantCode);
            this.settleId = blankToNull(settleId);
            this.bankResultCode = blankToNull(bankResultCode);
            this.confirmReceiptId = blankToNull(confirmReceiptId);
            this.confirmUser = blankToNull(confirmUser);
        }

        private static SearchCondition fromArgs(String[] args) {
            java.util.Map<String, String> values = new java.util.HashMap<>();
            for (String arg : args) {
                int p = arg.indexOf('=');
                if (p > 0) {
                    values.put(arg.substring(0, p), arg.substring(p + 1));
                }
            }
            return new SearchCondition(
                    values.get("merchant"),
                    values.get("settle"),
                    values.get("bank"),
                    values.get("confirmReceipt"),
                    values.get("confirmUser"));
        }

        private static String blankToNull(String v) {
            return v == null || v.trim().isEmpty() ? null : v.trim();
        }
    }

    private static final class ReceiptView {
        private final ReceiptRecord receipt;
        private final SettlementRecord settlement;
        private final java.math.BigDecimal bankOkTotal;
        private final String bankResultCode;
        private final java.math.BigDecimal receiptDiff;
        private final boolean chargeValid;
        private final boolean payoutValid;
        private final boolean merchantSettleable;

        private ReceiptView(ReceiptRecord receipt,
                            SettlementRecord settlement,
                            java.math.BigDecimal bankOkTotal,
                            String bankResultCode,
                            java.math.BigDecimal receiptDiff,
                            boolean chargeValid,
                            boolean payoutValid,
                            boolean merchantSettleable) {
            this.receipt = receipt;
            this.settlement = settlement;
            this.bankOkTotal = bankOkTotal;
            this.bankResultCode = bankResultCode;
            this.receiptDiff = receiptDiff;
            this.chargeValid = chargeValid;
            this.payoutValid = payoutValid;
            this.merchantSettleable = merchantSettleable;
        }

        private String toDisplayLine() {
            String hantei = receiptDiff.signum() == 0 && chargeValid && payoutValid && merchantSettleable ? "照合可" : "確認要";
            return "RECEIPT-ID=" + receipt.receiptId
                    + " 加盟店=" + receipt.merchantCode
                    + " 入金額=" + receipt.receiptAmt.toPlainString()
                    + " 入金日=" + receipt.receiptDt.format(DATE_FMT)
                    + " 精算ID=" + receipt.settleId
                    + " 支払予定=" + settlement.payoutAmt.toPlainString()
                    + " 銀行正常合計=" + bankOkTotal.toPlainString()
                    + " 銀行結果=" + bankResultCode
                    + " 差額=" + receiptDiff.toPlainString()
                    + " 加盟店状態=" + (merchantSettleable ? "01" : "対象外")
                    + " 判定=" + hantei
                    + " 状態=" + receipt.matchStatus;
        }
    }

    private static final class ReceiptRecord {
        private final String receiptId;
        private final String merchantCode;
        private final java.math.BigDecimal receiptAmt;
        private final java.time.LocalDate receiptDt;
        private final String matchStatus;
        private final String settleId;

        private ReceiptRecord(String receiptId, String merchantCode, java.math.BigDecimal receiptAmt,
                              java.time.LocalDate receiptDt, String matchStatus, String settleId) {
            this.receiptId = receiptId;
            this.merchantCode = merchantCode;
            this.receiptAmt = receiptAmt;
            this.receiptDt = receiptDt;
            this.matchStatus = matchStatus;
            this.settleId = settleId;
        }
    }

    private static final class SettlementRecord {
        private final String settleId;
        private final String merchantCode;
        private final java.math.BigDecimal netAmt;
        private final java.math.BigDecimal chargeAmt;
        private final java.math.BigDecimal payoutAmt;
        private final java.time.LocalDate settleDt;

        private SettlementRecord(String settleId, String merchantCode, java.math.BigDecimal netAmt,
                                 java.math.BigDecimal chargeAmt, java.math.BigDecimal payoutAmt,
                                 java.time.LocalDate settleDt) {
            this.settleId = settleId;
            this.merchantCode = merchantCode;
            this.netAmt = netAmt;
            this.chargeAmt = chargeAmt;
            this.payoutAmt = payoutAmt;
            this.settleDt = settleDt;
        }
    }

    private static final class PayoutRecord {
        private final String payoutId;
        private final String merchantCode;
        private final String bankAcctNo;
        private final java.math.BigDecimal payoutAmt;
        private final java.time.LocalDate payoutDt;
        private final String bankResultCode;

        private PayoutRecord(String payoutId, String merchantCode, String bankAcctNo,
                             java.math.BigDecimal payoutAmt, java.time.LocalDate payoutDt, String bankResultCode) {
            this.payoutId = payoutId;
            this.merchantCode = merchantCode;
            this.bankAcctNo = bankAcctNo;
            this.payoutAmt = payoutAmt;
            this.payoutDt = payoutDt;
            this.bankResultCode = bankResultCode;
        }
    }
}
