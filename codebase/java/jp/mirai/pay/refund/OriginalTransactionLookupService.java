package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数 / 年月日 / 担当 / 概要
 * 1.0 / 2024-06-04 / みらいペイ システム部 返金・チャージバックチーム / 原取引照会サービス初版
 */
public class OriginalTransactionLookupService {

    private final java.util.Map<String, PrtxnfRecord> prtxnf;

    public OriginalTransactionLookupService() {
        this.prtxnf = loadBenchmarkPrtxnf();
    }

    public OriginalTransactionLookupService(java.util.Map<String, PrtxnfRecord> prtxnf) {
        if (prtxnf == null) {
            throw new IllegalArgumentException("PRTXNFが未設定です");
        }
        this.prtxnf = new java.util.LinkedHashMap<>(prtxnf);
    }

    public OriginalTransactionLookupResult lookup(String origTxnId) {
        if (origTxnId == null || origTxnId.trim().isEmpty()) {
            return OriginalTransactionLookupResult.notFound(origTxnId);
        }

        PrtxnfRecord record = prtxnf.get(origTxnId.trim());
        if (record == null) {
            return OriginalTransactionLookupResult.notFound(origTxnId.trim());
        }

        java.util.List<String> warnings = new java.util.ArrayList<>();
        if (record.origTxnAmt.compareTo(java.math.BigDecimal.ZERO) == 0) {
            warnings.add("原取引金額ゼロ");
        }
        if (record.merchantCode == null || record.merchantCode.trim().isEmpty()) {
            warnings.add("加盟店コード欠落");
        }

        return OriginalTransactionLookupResult.found(
                record.origTxnId,
                record.walletId,
                record.merchantCode,
                record.origTxnAmt,
                record.origTxnDt,
                warnings
        );
    }

    public static void main(String[] a) {
        OriginalTransactionLookupService service = new OriginalTransactionLookupService();
        String key = a.length == 0 ? "OTX-20260401-000001" : a[0];
        OriginalTransactionLookupResult result = service.lookup(key);
        System.out.println(result.toOperatorLine());
    }

    private static java.util.Map<String, PrtxnfRecord> loadBenchmarkPrtxnf() {
        java.util.Map<String, PrtxnfRecord> table = new java.util.LinkedHashMap<>();
        put(table, "OTX-20260401-000001", "WLT-10000001", "MRC-000271", "3280", "2026-04-01");
        put(table, "OTX-20260401-000002", "WLT-10000002", "MRC-000418", "12000", "2026-04-01");
        put(table, "OTX-20260402-000003", "WLT-10000003", "MRC-000102", "0", "2026-04-02");
        put(table, "OTX-20260402-000004", "WLT-10000004", "", "8450", "2026-04-02");
        put(table, "OTX-20260403-000005", "WLT-10000005", "MRC-000995", "199800", "2026-04-03");
        put(table, "OTX-20260404-000006", "WLT-10000006", "MRC-000271", "760", "2026-04-04");
        return table;
    }

    private static void put(java.util.Map<String, PrtxnfRecord> table,
                            String origTxnId,
                            String walletId,
                            String merchantCode,
                            String origTxnAmt,
                            String origTxnDt) {
        table.put(origTxnId, new PrtxnfRecord(
                origTxnId,
                walletId,
                merchantCode,
                new java.math.BigDecimal(origTxnAmt),
                java.time.LocalDate.parse(origTxnDt)
        ));
    }

    public static final class OriginalTransactionLookupResult {
        private final String origTxnId;
        private final String walletId;
        private final String merchantCode;
        private final java.math.BigDecimal origTxnAmt;
        private final java.time.LocalDate origTxnDt;
        private final boolean found;
        private final boolean amountZero;
        private final boolean merchantCodeMissing;
        private final java.util.List<String> warnings;

        private OriginalTransactionLookupResult(String origTxnId,
                                                String walletId,
                                                String merchantCode,
                                                java.math.BigDecimal origTxnAmt,
                                                java.time.LocalDate origTxnDt,
                                                boolean found,
                                                java.util.List<String> warnings) {
            this.origTxnId = origTxnId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
            this.found = found;
            this.amountZero = found && origTxnAmt.compareTo(java.math.BigDecimal.ZERO) == 0;
            this.merchantCodeMissing = found && (merchantCode == null || merchantCode.trim().isEmpty());
            this.warnings = java.util.Collections.unmodifiableList(new java.util.ArrayList<>(warnings));
        }

        private static OriginalTransactionLookupResult found(String origTxnId,
                                                             String walletId,
                                                             String merchantCode,
                                                             java.math.BigDecimal origTxnAmt,
                                                             java.time.LocalDate origTxnDt,
                                                             java.util.List<String> warnings) {
            return new OriginalTransactionLookupResult(origTxnId, walletId, merchantCode, origTxnAmt, origTxnDt, true, warnings);
        }

        private static OriginalTransactionLookupResult notFound(String origTxnId) {
            return new OriginalTransactionLookupResult(
                    origTxnId,
                    null,
                    null,
                    java.math.BigDecimal.ZERO,
                    null,
                    false,
                    java.util.Collections.singletonList("原取引なし")
            );
        }

        public String getOrigTxnId() {
            return origTxnId;
        }

        public String getWalletId() {
            return walletId;
        }

        public String getMerchantCode() {
            return merchantCode;
        }

        public java.math.BigDecimal getOrigTxnAmt() {
            return origTxnAmt;
        }

        public java.time.LocalDate getOrigTxnDt() {
            return origTxnDt;
        }

        public boolean isFound() {
            return found;
        }

        public boolean isAmountZero() {
            return amountZero;
        }

        public boolean isMerchantCodeMissing() {
            return merchantCodeMissing;
        }

        public java.util.List<String> getWarnings() {
            return warnings;
        }

        public String toOperatorLine() {
            if (!found) {
                return "照会結果=未存在, ORIG-TXN-ID=" + origTxnId + ", RS-DECLINE-REASON=TXN";
            }
            return "照会結果=存在"
                    + ", ORIG-TXN-ID=" + origTxnId
                    + ", WALLET-ID=" + walletId
                    + ", MERCHANT-CODE=" + (merchantCodeMissing ? "未設定" : merchantCode)
                    + ", ORIG-TXN-AMT=" + origTxnAmt
                    + ", ORIG-TXN-DT=" + origTxnDt
                    + ", 警告=" + warnings;
        }
    }

    public static final class PrtxnfRecord {
        private final String origTxnId;
        private final String walletId;
        private final String merchantCode;
        private final java.math.BigDecimal origTxnAmt;
        private final java.time.LocalDate origTxnDt;

        public PrtxnfRecord(String origTxnId,
                            String walletId,
                            String merchantCode,
                            java.math.BigDecimal origTxnAmt,
                            java.time.LocalDate origTxnDt) {
            if (origTxnId == null || origTxnId.trim().isEmpty()) {
                throw new IllegalArgumentException("ORIG-TXN-IDが未設定です");
            }
            if (walletId == null || walletId.trim().isEmpty()) {
                throw new IllegalArgumentException("WALLET-IDが未設定です");
            }
            if (origTxnAmt == null) {
                throw new IllegalArgumentException("ORIG-TXN-AMTが未設定です");
            }
            if (origTxnDt == null) {
                throw new IllegalArgumentException("ORIG-TXN-DTが未設定です");
            }
            this.origTxnId = origTxnId.trim();
            this.walletId = walletId.trim();
            this.merchantCode = merchantCode;
            this.origTxnAmt = origTxnAmt;
            this.origTxnDt = origTxnDt;
        }
    }
}
