public class RetryInstructionService {
    /**
     * 変更履歴
     * 版数  年月日      担当        概要
     * 1.00  2026/01/15  勘定系基盤    初版作成。振替不能明細の再請求停止、再開、予定日変更を実装。
     * 1.01  2026/02/03  決済運用      口座状態と再請求回数の検証を追加。
     * 1.02  2026/02/18  取引統制      変更履歴出力と理由コード付与を追加。
     */

    private static final int MAX_RETRY_COUNT = 5;
    private static final long MAX_RETRY_AMOUNT = 9_999_999_999L;
    private static final String STATUS_ACTIVE = "01";
    private static final String STATUS_STOPPED = "02";
    private static final String STATUS_COMPLETED = "09";
    private static final String ACCOUNT_TRANSFER_OK = "0";
    private static final String ACCOUNT_TRANSFER_STOPPED = "7";
    private static final String ACCOUNT_TRANSFER_INVALID = "9";

    public static void main(String[] a) {
        RetryStore store = new RetryStore();

        store.addRetry(new RetryRecord("R202602180001", "4980123400010001", "Q202602010091", 2,
                toDate(2026, 2, 25), 128_400L, STATUS_ACTIVE));
        store.addRetry(new RetryRecord("R202602180002", "4980123400010002", "Q202602010104", 5,
                toDate(2026, 2, 24), 54_320L, STATUS_ACTIVE));
        store.addRetry(new RetryRecord("R202602180003", "4980123400010003", "Q202602010118", 1,
                toDate(2026, 2, 26), 8_210L, STATUS_STOPPED));

        store.addResult(new TransferResult("T202602020401", "Q202602010091", "4980123400010001",
                "R01", 0L, "残高不足", toDate(2026, 2, 2)));
        store.addResult(new TransferResult("T202602020418", "Q202602010104", "4980123400010002",
                "R03", 0L, "口座振替停止", toDate(2026, 2, 2)));
        store.addResult(new TransferResult("T202602020432", "Q202602010118", "4980123400010003",
                "R01", 0L, "残高不足", toDate(2026, 2, 2)));

        store.addAccount(new AccountRecord("A000000001", "4980123400010001", "0005", "013",
                "1", "1234567", "ヤマダタロウ", ACCOUNT_TRANSFER_OK));
        store.addAccount(new AccountRecord("A000000002", "4980123400010002", "0005", "021",
                "1", "2234567", "サトウハナコ", ACCOUNT_TRANSFER_STOPPED));
        store.addAccount(new AccountRecord("A000000003", "4980123400010003", "0001", "105",
                "1", "3234567", "スズキイチロウ", ACCOUNT_TRANSFER_OK));

        RetryInstructionService service = new RetryInstructionService(store);
        service.instruct(new Instruction("R202602180001", "4980123400010001", "予定日変更",
                toDate(2026, 3, 3), "顧客申出による入金予定日反映", toDate(2026, 2, 18)));
        service.instruct(new Instruction("R202602180003", "4980123400010003", "再開",
                toDate(2026, 2, 27), "入金確認後の再請求再開", toDate(2026, 2, 18)));
        service.instruct(new Instruction("R202602180002", "4980123400010002", "停止",
                0, "口座振替停止のため再請求打切り", toDate(2026, 2, 18)));

        store.printCurrentState();
    }

    private final RetryStore store;

    public RetryInstructionService(RetryStore store) {
        if (store == null) {
            throw new IllegalArgumentException("保管領域が未設定です");
        }
        this.store = store;
    }

    public InstructionResult instruct(Instruction instruction) {
        if (instruction == null) {
            return InstructionResult.reject("入力指示が未設定です");
        }

        RetryRecord retry = store.findRetry(instruction.retryId);
        if (retry == null) {
            return InstructionResult.reject("再請求明細が存在しません");
        }
        if (!retry.cardNo.equals(instruction.cardNo)) {
            return InstructionResult.reject("カード番号が再請求明細と一致しません");
        }

        AccountRecord account = store.findAccountByCardNo(instruction.cardNo);
        if (account == null) {
            return InstructionResult.reject("振替口座が存在しません");
        }

        TransferResult latestFailure = store.latestFailure(retry.originalRequestId, retry.cardNo);
        if (latestFailure == null) {
            return InstructionResult.reject("振替不能結果が存在しません");
        }

        String validationError = validateInstruction(instruction, retry, account);
        if (validationError != null) {
            return InstructionResult.reject(validationError);
        }

        long beforeDate = retry.nextRequestDate;
        String beforeStatus = retry.retryStatus;
        RetryRecord after = applyInstruction(instruction, retry);

        store.writeRetry(after);
        store.writeHistory(new HistoryRecord(
                after.cardNo,
                after.originalRequestId,
                store.nextHistorySeq(after.cardNo),
                eventType(instruction.instructionType, beforeStatus, after.retryStatus),
                after.retryAmount,
                instruction.instructionDate,
                "RetryInstructionService",
                beforeDate,
                after.nextRequestDate,
                instruction.reason
        ));

        return InstructionResult.accept(after.retryId, beforeDate, after.nextRequestDate, beforeStatus, after.retryStatus);
    }

    private String validateInstruction(Instruction instruction, RetryRecord retry, AccountRecord account) {
        if (retry.retryAmount <= 0 || retry.retryAmount > MAX_RETRY_AMOUNT) {
            return "再請求金額が許容範囲外です";
        }
        if (STATUS_COMPLETED.equals(retry.retryStatus)) {
            return "完了済明細は変更できません";
        }
        if (retry.retryCount >= MAX_RETRY_COUNT && !"停止".equals(instruction.instructionType)) {
            return "再請求上限回数に到達しています";
        }
        if (ACCOUNT_TRANSFER_INVALID.equals(account.transferStatus)) {
            return "口座状態が無効です";
        }
        if (ACCOUNT_TRANSFER_STOPPED.equals(account.transferStatus) && !"停止".equals(instruction.instructionType)) {
            return "振替停止口座は停止指示のみ可能です";
        }
        if ("停止".equals(instruction.instructionType)) {
            if (STATUS_STOPPED.equals(retry.retryStatus)) {
                return "既に停止済です";
            }
            return null;
        }
        if ("再開".equals(instruction.instructionType)) {
            if (!STATUS_STOPPED.equals(retry.retryStatus)) {
                return "停止中明細のみ再開できます";
            }
            if (instruction.newNextRequestDate <= instruction.instructionDate) {
                return "再開予定日は指示日より後の日付が必要です";
            }
            return null;
        }
        if ("予定日変更".equals(instruction.instructionType)) {
            if (!STATUS_ACTIVE.equals(retry.retryStatus)) {
                return "稼働中明細のみ予定日変更できます";
            }
            if (instruction.newNextRequestDate <= instruction.instructionDate) {
                return "変更後予定日は指示日より後の日付が必要です";
            }
            if (businessDaysBetween(instruction.instructionDate, instruction.newNextRequestDate) > 20) {
                return "変更後予定日が運用許容日数を超過しています";
            }
            return null;
        }
        return "指示区分が不正です";
    }

    private RetryRecord applyInstruction(Instruction instruction, RetryRecord retry) {
        if ("停止".equals(instruction.instructionType)) {
            return new RetryRecord(retry.retryId, retry.cardNo, retry.originalRequestId, retry.retryCount,
                    retry.nextRequestDate, retry.retryAmount, STATUS_STOPPED);
        }
        if ("再開".equals(instruction.instructionType)) {
            return new RetryRecord(retry.retryId, retry.cardNo, retry.originalRequestId, retry.retryCount,
                    instruction.newNextRequestDate, retry.retryAmount, STATUS_ACTIVE);
        }
        return new RetryRecord(retry.retryId, retry.cardNo, retry.originalRequestId, retry.retryCount,
                instruction.newNextRequestDate, retry.retryAmount, retry.retryStatus);
    }

    private String eventType(String instructionType, String beforeStatus, String afterStatus) {
        if ("停止".equals(instructionType)) {
            return "再請求停止";
        }
        if ("再開".equals(instructionType)) {
            return "再請求再開";
        }
        if (!beforeStatus.equals(afterStatus)) {
            return "状態変更";
        }
        return "予定日変更";
    }

    private static int businessDaysBetween(long fromYmd, long toYmd) {
        int days = 0;
        long cursor = fromYmd;
        while (cursor < toYmd) {
            cursor = addDays(cursor, 1);
            int week = dayOfWeek(cursor);
            if (week != 0 && week != 6) {
                days++;
            }
        }
        return days;
    }

    private static long addDays(long ymd, int days) {
        int y = (int) (ymd / 10000);
        int m = (int) ((ymd / 100) % 100);
        int d = (int) (ymd % 100);
        for (int i = 0; i < days; i++) {
            d++;
            if (d > lastDayOfMonth(y, m)) {
                d = 1;
                m++;
                if (m > 12) {
                    m = 1;
                    y++;
                }
            }
        }
        return toDate(y, m, d);
    }

    private static int dayOfWeek(long ymd) {
        int y = (int) (ymd / 10000);
        int m = (int) ((ymd / 100) % 100);
        int d = (int) (ymd % 100);
        if (m < 3) {
            m += 12;
            y--;
        }
        int k = y % 100;
        int j = y / 100;
        int h = (d + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 + 5 * j) % 7;
        return (h + 6) % 7;
    }

    private static int lastDayOfMonth(int y, int m) {
        switch (m) {
            case 2:
                return isLeapYear(y) ? 29 : 28;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            default:
                return 31;
        }
    }

    private static boolean isLeapYear(int y) {
        return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
    }

    private static long toDate(int y, int m, int d) {
        return y * 10000L + m * 100L + d;
    }

    static final class RetryStore {
        private final java.util.Map<String, RetryRecord> retryById = new java.util.LinkedHashMap<>();
        private final java.util.List<TransferResult> transferResults = new java.util.ArrayList<>();
        private final java.util.Map<String, AccountRecord> accountByCardNo = new java.util.LinkedHashMap<>();
        private final java.util.List<HistoryRecord> histories = new java.util.ArrayList<>();

        void addRetry(RetryRecord retry) {
            retryById.put(retry.retryId, retry);
        }

        void addResult(TransferResult result) {
            transferResults.add(result);
        }

        void addAccount(AccountRecord account) {
            accountByCardNo.put(account.cardNo, account);
        }

        RetryRecord findRetry(String retryId) {
            return retryById.get(retryId);
        }

        AccountRecord findAccountByCardNo(String cardNo) {
            return accountByCardNo.get(cardNo);
        }

        TransferResult latestFailure(String requestId, String cardNo) {
            TransferResult latest = null;
            for (TransferResult result : transferResults) {
                if (requestId.equals(result.requestId) && cardNo.equals(result.cardNo) && result.settledAmount == 0L) {
                    if (latest == null || result.resultDate > latest.resultDate) {
                        latest = result;
                    }
                }
            }
            return latest;
        }

        void writeRetry(RetryRecord retry) {
            retryById.put(retry.retryId, retry);
        }

        void writeHistory(HistoryRecord history) {
            histories.add(history);
        }

        int nextHistorySeq(String cardNo) {
            int max = 0;
            for (HistoryRecord history : histories) {
                if (cardNo.equals(history.cardNo) && history.eventSeq > max) {
                    max = history.eventSeq;
                }
            }
            return max + 1;
        }

        void printCurrentState() {
            for (RetryRecord retry : retryById.values()) {
                System.out.println("再請求明細 " + retry.retryId + " " + retry.cardNo + " 回数=" + retry.retryCount
                        + " 次回=" + retry.nextRequestDate + " 状態=" + retry.retryStatus);
            }
            for (HistoryRecord history : histories) {
                System.out.println("履歴 " + history.cardNo + " " + history.eventSeq + " " + history.eventType
                        + " 変更前=" + history.beforeNextRequestDate + " 変更後=" + history.afterNextRequestDate
                        + " 理由=" + history.reason);
            }
        }
    }

    public static final class Instruction {
        final String retryId;
        final String cardNo;
        final String instructionType;
        final long newNextRequestDate;
        final String reason;
        final long instructionDate;

        public Instruction(String retryId, String cardNo, String instructionType, long newNextRequestDate,
                           String reason, long instructionDate) {
            this.retryId = retryId;
            this.cardNo = cardNo;
            this.instructionType = instructionType;
            this.newNextRequestDate = newNextRequestDate;
            this.reason = reason;
            this.instructionDate = instructionDate;
        }
    }

    public static final class InstructionResult {
        final boolean accepted;
        final String retryId;
        final String message;
        final long beforeNextRequestDate;
        final long afterNextRequestDate;
        final String beforeStatus;
        final String afterStatus;

        private InstructionResult(boolean accepted, String retryId, String message, long beforeNextRequestDate,
                                  long afterNextRequestDate, String beforeStatus, String afterStatus) {
            this.accepted = accepted;
            this.retryId = retryId;
            this.message = message;
            this.beforeNextRequestDate = beforeNextRequestDate;
            this.afterNextRequestDate = afterNextRequestDate;
            this.beforeStatus = beforeStatus;
            this.afterStatus = afterStatus;
        }

        static InstructionResult accept(String retryId, long beforeNextRequestDate, long afterNextRequestDate,
                                        String beforeStatus, String afterStatus) {
            return new InstructionResult(true, retryId, "受付済", beforeNextRequestDate, afterNextRequestDate,
                    beforeStatus, afterStatus);
        }

        static InstructionResult reject(String message) {
            return new InstructionResult(false, "", message, 0L, 0L, "", "");
        }
    }

    static final class RetryRecord {
        final String retryId;
        final String cardNo;
        final String originalRequestId;
        final int retryCount;
        final long nextRequestDate;
        final long retryAmount;
        final String retryStatus;

        RetryRecord(String retryId, String cardNo, String originalRequestId, int retryCount,
                    long nextRequestDate, long retryAmount, String retryStatus) {
            this.retryId = retryId;
            this.cardNo = cardNo;
            this.originalRequestId = originalRequestId;
            this.retryCount = retryCount;
            this.nextRequestDate = nextRequestDate;
            this.retryAmount = retryAmount;
            this.retryStatus = retryStatus;
        }
    }

    static final class TransferResult {
        final String resultId;
        final String requestId;
        final String cardNo;
        final String resultCode;
        final long settledAmount;
        final String returnReason;
        final long resultDate;

        TransferResult(String resultId, String requestId, String cardNo, String resultCode,
                       long settledAmount, String returnReason, long resultDate) {
            this.resultId = resultId;
            this.requestId = requestId;
            this.cardNo = cardNo;
            this.resultCode = resultCode;
            this.settledAmount = settledAmount;
            this.returnReason = returnReason;
            this.resultDate = resultDate;
        }
    }

    static final class AccountRecord {
        final String accountId;
        final String cardNo;
        final String bankCode;
        final String branchCode;
        final String depositType;
        final String accountNo;
        final String holderKana;
        final String transferStatus;

        AccountRecord(String accountId, String cardNo, String bankCode, String branchCode,
                      String depositType, String accountNo, String holderKana, String transferStatus) {
            this.accountId = accountId;
            this.cardNo = cardNo;
            this.bankCode = bankCode;
            this.branchCode = branchCode;
            this.depositType = depositType;
            this.accountNo = accountNo;
            this.holderKana = holderKana;
            this.transferStatus = transferStatus;
        }
    }

    static final class HistoryRecord {
        final String cardNo;
        final String payId;
        final int eventSeq;
        final String eventType;
        final long eventAmount;
        final long eventDate;
        final String sourceProgram;
        final long beforeNextRequestDate;
        final long afterNextRequestDate;
        final String reason;

        HistoryRecord(String cardNo, String payId, int eventSeq, String eventType, long eventAmount,
                      long eventDate, String sourceProgram, long beforeNextRequestDate,
                      long afterNextRequestDate, String reason) {
            this.cardNo = cardNo;
            this.payId = payId;
            this.eventSeq = eventSeq;
            this.eventType = eventType;
            this.eventAmount = eventAmount;
            this.eventDate = eventDate;
            this.sourceProgram = sourceProgram;
            this.beforeNextRequestDate = beforeNextRequestDate;
            this.afterNextRequestDate = afterNextRequestDate;
            this.reason = reason;
        }
    }
}
