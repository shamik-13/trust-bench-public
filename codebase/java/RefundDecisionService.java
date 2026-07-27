public class RefundDecisionService {
    /**
     * 変更履歴
     * 版数    年月日      担当      概要
     * 1.00    2024/05/13  開発担当  初版作成。返金候補の登録口座、取消受付、既返金状態、承認権限を検証する。
     */
    private static final String STATUS_WAITING = "承認待";
    private static final String STATUS_APPROVED = "承認済";
    private static final String STATUS_HOLD = "保留";
    private static final String STATUS_REJECTED = "否認";

    private static final String TRANSFER_ACTIVE = "有効";

    private static final String EVENT_APPROVED = "返金承認";
    private static final String EVENT_HOLD = "返金保留";
    private static final String EVENT_REJECTED = "返金否認";

    private static final String SOURCE_PROGRAM = "返金判定サービス";

    public static void main(String[] a) {
        DataStore store = DataStore.synthetic();
        DecisionEngine engine = new DecisionEngine(store);

        int approved = 0;
        int held = 0;
        int rejected = 0;

        for (RefundRecord refund : store.refunds.values()) {
            DecisionResult result = engine.decide(refund.refundId, "課長A", 500000);
            if (STATUS_APPROVED.equals(result.status)) {
                approved++;
            } else if (STATUS_HOLD.equals(result.status)) {
                held++;
            } else {
                rejected++;
            }
            System.out.println(result.message);
        }

        System.out.println("処理結果 承認=" + approved + " 保留=" + held + " 否認=" + rejected);
    }

    public static DecisionResult approveRefund(String refundId, String approverId, long approvalLimit) {
        DataStore store = DataStore.synthetic();
        return new DecisionEngine(store).decide(refundId, approverId, approvalLimit);
    }

    private static final class DecisionEngine {
        private final DataStore store;

        private DecisionEngine(DataStore store) {
            this.store = store;
        }

        private DecisionResult decide(String refundId, String approverId, long approvalLimit) {
            RefundRecord refund = store.refunds.get(refundId);
            if (refund == null) {
                return new DecisionResult(refundId, STATUS_REJECTED, "否認: 返金候補が存在しません REFUND-ID=" + refundId);
            }

            if (!STATUS_WAITING.equals(refund.refundStatus)) {
                writeHistory(refund, EVENT_REJECTED, 0);
                return reject(refund, "否認: 返金候補は承認待ではありません REFUND-ID=" + refund.refundId);
            }

            AccountRecord account = store.findActiveAccount(refund.cardNo, refund.bankCd, refund.accountNo);
            if (account == null) {
                refund.refundStatus = STATUS_HOLD;
                writeHistory(refund, EVENT_HOLD, refund.refundAmt);
                return new DecisionResult(refund.refundId, STATUS_HOLD, "保留: 登録口座または振込状態を確認してください REFUND-ID=" + refund.refundId);
            }

            long cancelTotal = store.sumCancelAmount(refund.payId, refund.cardNo);
            if (cancelTotal <= 0) {
                return reject(refund, "否認: 取消受付が存在しません REFUND-ID=" + refund.refundId);
            }

            if (refund.refundAmt <= 0 || refund.refundAmt > cancelTotal) {
                return reject(refund, "否認: 返金額が取消金額を超過しています REFUND-ID=" + refund.refundId);
            }

            long alreadyApproved = store.sumApprovedRefundAmount(refund.payId, refund.cardNo, refund.refundId);
            if (alreadyApproved + refund.refundAmt > cancelTotal) {
                return reject(refund, "否認: 既返金額を含めると取消金額を超過します REFUND-ID=" + refund.refundId);
            }

            if (approvalLimit < refund.refundAmt) {
                refund.refundStatus = STATUS_HOLD;
                writeHistory(refund, EVENT_HOLD, refund.refundAmt);
                return new DecisionResult(refund.refundId, STATUS_HOLD, "保留: 承認限度額を超過しています REFUND-ID=" + refund.refundId);
            }

            refund.refundStatus = STATUS_APPROVED;
            refund.approvalId = approverId + "-" + refund.refundId;
            writeHistory(refund, EVENT_APPROVED, refund.refundAmt);
            return new DecisionResult(refund.refundId, STATUS_APPROVED, "承認: 返金ステータスを更新しました REFUND-ID=" + refund.refundId);
        }

        private DecisionResult reject(RefundRecord refund, String message) {
            refund.refundStatus = STATUS_REJECTED;
            writeHistory(refund, EVENT_REJECTED, refund.refundAmt);
            return new DecisionResult(refund.refundId, STATUS_REJECTED, message);
        }

        private void writeHistory(RefundRecord refund, String eventType, long amount) {
            int seq = store.nextEventSeq(refund.cardNo, refund.payId);
            store.histories.add(new HistoryRecord(
                    refund.cardNo,
                    refund.payId,
                    seq,
                    eventType,
                    amount,
                    java.time.LocalDateTime.now(),
                    SOURCE_PROGRAM
            ));
        }
    }

    public static final class DecisionResult {
        public final String refundId;
        public final String status;
        public final String message;

        private DecisionResult(String refundId, String status, String message) {
            this.refundId = refundId;
            this.status = status;
            this.message = message;
        }
    }

    private static final class DataStore {
        private final java.util.Map<String, RefundRecord> refunds = new java.util.LinkedHashMap<>();
        private final java.util.Map<String, AccountRecord> accounts = new java.util.LinkedHashMap<>();
        private final java.util.List<CancelRecord> cancels = new java.util.ArrayList<>();
        private final java.util.List<HistoryRecord> histories = new java.util.ArrayList<>();

        private static DataStore synthetic() {
            DataStore store = new DataStore();

            store.accounts.put("A001", new AccountRecord("A001", "4900000000010001", "0001", "101", "普通", "1234567", "ヤマダタロウ", "有効"));
            store.accounts.put("A002", new AccountRecord("A002", "4900000000010002", "0005", "201", "普通", "7654321", "サトウハナコ", "停止"));
            store.accounts.put("A003", new AccountRecord("A003", "4900000000010003", "0001", "102", "当座", "2222333", "スズキイチロウ", "有効"));

            store.cancels.add(new CancelRecord("C001", "P1001", "4900000000010001", 120000, "注文取消", "端末01", "2026-06-27T09:15:00"));
            store.cancels.add(new CancelRecord("C002", "P1002", "4900000000010002", 80000, "二重約定", "端末02", "2026-06-27T10:20:00"));
            store.cancels.add(new CancelRecord("C003", "P1003", "4900000000010003", 300000, "価格訂正", "端末03", "2026-06-27T11:05:00"));

            store.refunds.put("R001", new RefundRecord("R001", "4900000000010001", "P1001", 120000, "0001", "1234567", STATUS_WAITING, ""));
            store.refunds.put("R002", new RefundRecord("R002", "4900000000010002", "P1002", 80000, "0005", "7654321", STATUS_WAITING, ""));
            store.refunds.put("R003", new RefundRecord("R003", "4900000000010003", "P1003", 350000, "0001", "2222333", STATUS_WAITING, ""));
            store.refunds.put("R004", new RefundRecord("R004", "4900000000010001", "P1999", 10000, "0001", "1234567", STATUS_WAITING, ""));

            return store;
        }

        private AccountRecord findActiveAccount(String cardNo, String bankCd, String accountNo) {
            for (AccountRecord account : accounts.values()) {
                if (account.cardNo.equals(cardNo)
                        && account.bankCd.equals(bankCd)
                        && account.accountNo.equals(accountNo)
                        && TRANSFER_ACTIVE.equals(account.transferStatus)) {
                    return account;
                }
            }
            return null;
        }

        private long sumCancelAmount(String payId, String cardNo) {
            long total = 0;
            for (CancelRecord cancel : cancels) {
                if (cancel.payId.equals(payId) && cancel.cardNo.equals(cardNo)) {
                    total += cancel.cancelAmt;
                }
            }
            return total;
        }

        private long sumApprovedRefundAmount(String payId, String cardNo, String excludingRefundId) {
            long total = 0;
            for (RefundRecord refund : refunds.values()) {
                if (!refund.refundId.equals(excludingRefundId)
                        && refund.payId.equals(payId)
                        && refund.cardNo.equals(cardNo)
                        && STATUS_APPROVED.equals(refund.refundStatus)) {
                    total += refund.refundAmt;
                }
            }
            return total;
        }

        private int nextEventSeq(String cardNo, String payId) {
            int max = 0;
            for (HistoryRecord history : histories) {
                if (history.cardNo.equals(cardNo) && history.payId.equals(payId)) {
                    max = Math.max(max, history.eventSeq);
                }
            }
            return max + 1;
        }
    }

    private static final class RefundRecord {
        private final String refundId;
        private final String cardNo;
        private final String payId;
        private final long refundAmt;
        private final String bankCd;
        private final String accountNo;
        private String refundStatus;
        private String approvalId;

        private RefundRecord(String refundId, String cardNo, String payId, long refundAmt, String bankCd,
                             String accountNo, String refundStatus, String approvalId) {
            this.refundId = refundId;
            this.cardNo = cardNo;
            this.payId = payId;
            this.refundAmt = refundAmt;
            this.bankCd = bankCd;
            this.accountNo = accountNo;
            this.refundStatus = refundStatus;
            this.approvalId = approvalId;
        }
    }

    private static final class AccountRecord {
        private final String accountId;
        private final String cardNo;
        private final String bankCd;
        private final String branchCd;
        private final String depositType;
        private final String accountNo;
        private final String holderKana;
        private final String transferStatus;

        private AccountRecord(String accountId, String cardNo, String bankCd, String branchCd, String depositType,
                              String accountNo, String holderKana, String transferStatus) {
            this.accountId = accountId;
            this.cardNo = cardNo;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.depositType = depositType;
            this.accountNo = accountNo;
            this.holderKana = holderKana;
            this.transferStatus = transferStatus;
        }
    }

    private static final class CancelRecord {
        private final String cancelId;
        private final String payId;
        private final String cardNo;
        private final long cancelAmt;
        private final String cancelReason;
        private final String requestUser;
        private final String cancelDt;

        private CancelRecord(String cancelId, String payId, String cardNo, long cancelAmt, String cancelReason,
                             String requestUser, String cancelDt) {
            this.cancelId = cancelId;
            this.payId = payId;
            this.cardNo = cardNo;
            this.cancelAmt = cancelAmt;
            this.cancelReason = cancelReason;
            this.requestUser = requestUser;
            this.cancelDt = cancelDt;
        }
    }

    private static final class HistoryRecord {
        private final String cardNo;
        private final String payId;
        private final int eventSeq;
        private final String eventType;
        private final long eventAmt;
        private final java.time.LocalDateTime eventDt;
        private final String sourceProgram;

        private HistoryRecord(String cardNo, String payId, int eventSeq, String eventType, long eventAmt,
                              java.time.LocalDateTime eventDt, String sourceProgram) {
            this.cardNo = cardNo;
            this.payId = payId;
            this.eventSeq = eventSeq;
            this.eventType = eventType;
            this.eventAmt = eventAmt;
            this.eventDt = eventDt;
            this.sourceProgram = sourceProgram;
        }
    }
}
