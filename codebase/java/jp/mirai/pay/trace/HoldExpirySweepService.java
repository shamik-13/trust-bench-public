package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.00  2025/05/12  みらいペイ システム部 精算・連携チーム  初版作成
 */
public class HoldExpirySweepService {
    private static final String HD_STATUS_APPROVED = "00";
    private static final String HD_STATUS_SETTLED = "30";
    private static final String HD_STATUS_CANCELLED = "20";

    private static final String SETTLE_IMMEDIATE = "1";
    private static final String SETTLE_NEXT_MONTH = "2";
    private static final String SETTLE_OUT_OF_SCOPE = "9";

    private static final String EXPIRE_CANDIDATE = "10";
    private static final String EXPIRE_HOLD = "40";

    private static final String REASON_EXPIRED = "期限超過";
    private static final String REASON_TRACE_MISMATCH = "履歴不整合";
    private static final String REASON_SETTLED_POSSIBLE = "売上確認要";

    private static final java.time.LocalDateTime BUSINESS_NOW =
            java.time.LocalDateTime.of(2025, 6, 28, 2, 30, 0);

    public void run() {
        java.util.List<PtholdfRecord> holdFile = loadSyntheticPtholdf();
        java.util.Map<String, MerchantRule> merchantRules = loadSyntheticMerchantRules();
        java.util.Map<String, TraceView> traceIndex = loadSyntheticTraceIndex();

        SweepResult result = sweep(holdFile, merchantRules, traceIndex, BUSINESS_NOW);

        System.out.println("処理日時=" + BUSINESS_NOW);
        System.out.println("読込件数=" + result.readCount);
        System.out.println("登録件数=" + result.writeCount);
        System.out.println("保留件数=" + result.pendingCount);
        System.out.println("対象外件数=" + result.skipCount);
        System.out.println("加盟店別候補金額=" + result.amountByMerchant);

        for (PthxpfRecord record : result.expireFile) {
            System.out.println(record.toLine());
        }
    }

    private static SweepResult sweep(
            java.util.List<PtholdfRecord> holdFile,
            java.util.Map<String, MerchantRule> merchantRules,
            java.util.Map<String, TraceView> traceIndex,
            java.time.LocalDateTime now) {

        java.util.List<PthxpfRecord> expireFile = new java.util.ArrayList<>();
        java.util.Map<String, Long> amountByMerchant = new java.util.TreeMap<>();
        int pendingCount = 0;
        int skipCount = 0;

        for (PtholdfRecord hold : holdFile) {
            if (!HD_STATUS_APPROVED.equals(hold.holdStatus)) {
                skipCount++;
                continue;
            }

            MerchantRule rule = merchantRules.get(hold.merchantCode);
            if (rule == null || SETTLE_OUT_OF_SCOPE.equals(rule.settleKbn)) {
                skipCount++;
                continue;
            }

            java.time.LocalDateTime expireAt = hold.holdAt.plusDays(rule.graceDays);
            if (!expireAt.isBefore(now)) {
                skipCount++;
                continue;
            }

            TraceView trace = traceIndex.get(hold.holdId);
            Validation validation = validateTrace(hold, trace);
            String status = EXPIRE_CANDIDATE;
            String reason = REASON_EXPIRED;

            if (!validation.matched) {
                status = EXPIRE_HOLD;
                reason = REASON_TRACE_MISMATCH;
                pendingCount++;
            } else if (trace.salesConfirmed || HD_STATUS_SETTLED.equals(trace.holdStatus)
                    || SETTLE_NEXT_MONTH.equals(trace.settleKbn)) {
                status = EXPIRE_HOLD;
                reason = REASON_SETTLED_POSSIBLE;
                pendingCount++;
            }

            PthxpfRecord out = new PthxpfRecord(
                    hold.holdId,
                    hold.walletId,
                    hold.merchantCode,
                    expireAt,
                    reason,
                    status);
            expireFile.add(out);

            if (EXPIRE_CANDIDATE.equals(status)) {
                Long current = amountByMerchant.get(hold.merchantCode);
                amountByMerchant.put(
                        hold.merchantCode,
                        current == null ? hold.holdAmt : current + hold.holdAmt);
            }
        }

        return new SweepResult(
                holdFile.size(),
                expireFile.size(),
                pendingCount,
                skipCount,
                expireFile,
                amountByMerchant);
    }

    private static Validation validateTrace(PtholdfRecord hold, TraceView trace) {
        if (trace == null) {
            return new Validation(false);
        }
        boolean matched = hold.holdId.equals(trace.holdId)
                && hold.merchantCode.equals(trace.merchantCode)
                && hold.walletId.equals(trace.walletId);
        return new Validation(matched);
    }

    private static java.util.List<PtholdfRecord> loadSyntheticPtholdf() {
        java.util.List<PtholdfRecord> list = new java.util.ArrayList<>();
        list.add(new PtholdfRecord("HLD-20250620-0001", "WLT-100001", "MRC-0007", 12800L, HD_STATUS_APPROVED,
                java.time.LocalDateTime.of(2025, 6, 20, 10, 15)));
        list.add(new PtholdfRecord("HLD-20250622-0002", "WLT-100002", "MRC-0007", 4500L, HD_STATUS_APPROVED,
                java.time.LocalDateTime.of(2025, 6, 22, 9, 5)));
        list.add(new PtholdfRecord("HLD-20250623-0003", "WLT-100003", "MRC-0142", 33100L, HD_STATUS_APPROVED,
                java.time.LocalDateTime.of(2025, 6, 23, 22, 11)));
        list.add(new PtholdfRecord("HLD-20250624-0004", "WLT-100004", "MRC-0142", 8800L, HD_STATUS_SETTLED,
                java.time.LocalDateTime.of(2025, 6, 24, 12, 1)));
        list.add(new PtholdfRecord("HLD-20250618-0005", "WLT-100005", "MRC-0201", 152000L, HD_STATUS_APPROVED,
                java.time.LocalDateTime.of(2025, 6, 18, 8, 45)));
        list.add(new PtholdfRecord("HLD-20250627-0006", "WLT-100006", "MRC-0007", 990L, HD_STATUS_APPROVED,
                java.time.LocalDateTime.of(2025, 6, 27, 13, 30)));
        list.add(new PtholdfRecord("HLD-20250619-0007", "WLT-100007", "MRC-0999", 7200L, HD_STATUS_APPROVED,
                java.time.LocalDateTime.of(2025, 6, 19, 16, 0)));
        list.add(new PtholdfRecord("HLD-20250617-0008", "WLT-100008", "MRC-0201", 64000L, HD_STATUS_CANCELLED,
                java.time.LocalDateTime.of(2025, 6, 17, 7, 25)));
        return list;
    }

    private static java.util.Map<String, MerchantRule> loadSyntheticMerchantRules() {
        java.util.Map<String, MerchantRule> rules = new java.util.HashMap<>();
        rules.put("MRC-0007", new MerchantRule("MRC-0007", 5, SETTLE_IMMEDIATE));
        rules.put("MRC-0142", new MerchantRule("MRC-0142", 3, SETTLE_NEXT_MONTH));
        rules.put("MRC-0201", new MerchantRule("MRC-0201", 7, SETTLE_IMMEDIATE));
        rules.put("MRC-0999", new MerchantRule("MRC-0999", 2, SETTLE_OUT_OF_SCOPE));
        return rules;
    }

    private static java.util.Map<String, TraceView> loadSyntheticTraceIndex() {
        java.util.Map<String, TraceView> traces = new java.util.HashMap<>();
        traces.put("HLD-20250620-0001", new TraceView("HLD-20250620-0001", "WLT-100001", "MRC-0007",
                HD_STATUS_APPROVED, SETTLE_IMMEDIATE, false));
        traces.put("HLD-20250622-0002", new TraceView("HLD-20250622-0002", "WLT-100002", "MRC-0007",
                HD_STATUS_SETTLED, SETTLE_IMMEDIATE, true));
        traces.put("HLD-20250623-0003", new TraceView("HLD-20250623-0003", "WLT-100003", "MRC-0142",
                HD_STATUS_APPROVED, SETTLE_NEXT_MONTH, false));
        traces.put("HLD-20250618-0005", new TraceView("HLD-20250618-0005", "WLT-100005", "MRC-0209",
                HD_STATUS_APPROVED, SETTLE_IMMEDIATE, false));
        traces.put("HLD-20250619-0007", new TraceView("HLD-20250619-0007", "WLT-100007", "MRC-0999",
                HD_STATUS_APPROVED, SETTLE_OUT_OF_SCOPE, false));
        return traces;
    }

    private static final class PtholdfRecord {
        private final String holdId;
        private final String walletId;
        private final String merchantCode;
        private final long holdAmt;
        private final String holdStatus;
        private final java.time.LocalDateTime holdAt;

        private PtholdfRecord(
                String holdId,
                String walletId,
                String merchantCode,
                long holdAmt,
                String holdStatus,
                java.time.LocalDateTime holdAt) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.holdAmt = holdAmt;
            this.holdStatus = holdStatus;
            this.holdAt = holdAt;
        }
    }

    private static final class PthxpfRecord {
        private final String holdId;
        private final String walletId;
        private final String merchantCode;
        private final java.time.LocalDateTime expireAt;
        private final String reasonCode;
        private final String expireStatus;

        private PthxpfRecord(
                String holdId,
                String walletId,
                String merchantCode,
                java.time.LocalDateTime expireAt,
                String reasonCode,
                String expireStatus) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.expireAt = expireAt;
            this.reasonCode = reasonCode;
            this.expireStatus = expireStatus;
        }

        private String toLine() {
            return "PTHXPF{"
                    + "HOLD-ID=" + holdId
                    + ", WALLET-ID=" + walletId
                    + ", MERCHANT-CODE=" + merchantCode
                    + ", EXPIRE-AT=" + expireAt
                    + ", REASON-CODE=" + reasonCode
                    + ", EXPIRE-STATUS=" + expireStatus
                    + '}';
        }
    }

    private static final class MerchantRule {
        private final String merchantCode;
        private final int graceDays;
        private final String settleKbn;

        private MerchantRule(String merchantCode, int graceDays, String settleKbn) {
            this.merchantCode = merchantCode;
            this.graceDays = graceDays;
            this.settleKbn = settleKbn;
        }
    }

    private static final class TraceView {
        private final String holdId;
        private final String walletId;
        private final String merchantCode;
        private final String holdStatus;
        private final String settleKbn;
        private final boolean salesConfirmed;

        private TraceView(
                String holdId,
                String walletId,
                String merchantCode,
                String holdStatus,
                String settleKbn,
                boolean salesConfirmed) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.holdStatus = holdStatus;
            this.settleKbn = settleKbn;
            this.salesConfirmed = salesConfirmed;
        }
    }

    private static final class Validation {
        private final boolean matched;

        private Validation(boolean matched) {
            this.matched = matched;
        }
    }

    private static final class SweepResult {
        private final int readCount;
        private final int writeCount;
        private final int pendingCount;
        private final int skipCount;
        private final java.util.List<PthxpfRecord> expireFile;
        private final java.util.Map<String, Long> amountByMerchant;

        private SweepResult(
                int readCount,
                int writeCount,
                int pendingCount,
                int skipCount,
                java.util.List<PthxpfRecord> expireFile,
                java.util.Map<String, Long> amountByMerchant) {
            this.readCount = readCount;
            this.writeCount = writeCount;
            this.pendingCount = pendingCount;
            this.skipCount = skipCount;
            this.expireFile = expireFile;
            this.amountByMerchant = amountByMerchant;
        }
    }
}
