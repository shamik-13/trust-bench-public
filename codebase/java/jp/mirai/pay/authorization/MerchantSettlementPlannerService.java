package jp.mirai.pay.authorization;

/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2024-08-29  みらいペイ システム部  加盟店精算計画サービス初版
 */
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class MerchantSettlementPlannerService {
    private static final String MERCHANT_ACTIVE = "01";
    private static final String PEND_WAIT = "10";
    private static final String PEND_SETTLED = "30";
    private static final String TXN_CAPTURED = "30";
    private static final String NOTICE_UNSENT = "10";
    private static final DateTimeFormatter TS_FMT = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

    public static void main(String[] a) {
        MerchantSettlementPlannerService service = new MerchantSettlementPlannerService();
        List<MerchantFileRow> merchants = Arrays.asList(
                new MerchantFileRow("M10001", "01", "5411", money("5000000"), "B", "D"),
                new MerchantFileRow("M10002", "01", "5812", money("1200000"), "C", "W"),
                new MerchantFileRow("M10003", "09", "5999", money("300000"), "A", "D"),
                new MerchantFileRow("M10004", "01", "4899", money("8500000"), "S", "M"),
                new MerchantFileRow("M10005", "01", "5942", money("900000"), "B", "D")
        );
        List<TransactionFileRow> transactions = Arrays.asList(
                new TransactionFileRow("T202606270001", "R900001", "W0001001", "M10001", money("12800"), "30", LocalDate.parse("2026-06-27"), LocalDate.parse("2026-06-27")),
                new TransactionFileRow("T202606270002", "R900002", "W0001002", "M10001", money("25400"), "30", LocalDate.parse("2026-06-27"), LocalDate.parse("2026-06-27")),
                new TransactionFileRow("T202606270003", "R900003", "W0001003", "M10001", money("9800"), "20", LocalDate.parse("2026-06-27"), null),
                new TransactionFileRow("T202606260004", "R900004", "W0002001", "M10002", money("62000"), "30", LocalDate.parse("2026-06-26"), LocalDate.parse("2026-06-26")),
                new TransactionFileRow("T202606250005", "R900005", "W0003001", "M10003", money("10400"), "30", LocalDate.parse("2026-06-25"), LocalDate.parse("2026-06-25")),
                new TransactionFileRow("T202606270006", "R900006", "W0004001", "M10004", money("248000"), "30", LocalDate.parse("2026-06-27"), LocalDate.parse("2026-06-27")),
                new TransactionFileRow("T202606270007", "R900007", "W0005001", "M10005", money("31000"), "30", LocalDate.parse("2026-06-27"), LocalDate.parse("2026-06-27")),
                new TransactionFileRow("T202606270008", "R900008", "W0005002", "M10005", money("31000"), "30", LocalDate.parse("2026-06-27"), LocalDate.parse("2026-06-27"))
        );
        List<PendingFileRow> pendings = Arrays.asList(
                new PendingFileRow("P700001", "W0001001", money("12800"), "30", LocalDate.parse("2026-06-27")),
                new PendingFileRow("P700002", "W0001002", money("12000"), "10", LocalDate.parse("2026-06-27")),
                new PendingFileRow("P700003", "W0002001", money("62000"), "10", LocalDate.parse("2026-06-26")),
                new PendingFileRow("P700004", "W0003001", money("10400"), "10", LocalDate.parse("2026-06-25")),
                new PendingFileRow("P700005", "W0004001", money("248000"), "30", LocalDate.parse("2026-06-27")),
                new PendingFileRow("P700006", "W0005001", money("31000"), "30", LocalDate.parse("2026-06-27")),
                new PendingFileRow("P700007", "W0005002", money("30000"), "30", LocalDate.parse("2026-06-27"))
        );

        List<NoticeFileRow> notices = service.planSettlementNotices(
                merchants,
                transactions,
                pendings,
                LocalDate.parse("2026-06-28"),
                LocalDateTime.parse("2026-06-28T09:15:30")
        );

        for (NoticeFileRow notice : notices) {
            System.out.println(notice.toSequentialRecord());
        }
    }

    private List<NoticeFileRow> planSettlementNotices(List<MerchantFileRow> merchants,
                                                      List<TransactionFileRow> transactions,
                                                      List<PendingFileRow> pendings,
                                                      LocalDate businessDate,
                                                      LocalDateTime createTs) {
        Map<String, MerchantFileRow> merchantByCode = new LinkedHashMap<>();
        for (MerchantFileRow merchant : merchants) {
            merchantByCode.put(merchant.merchantCode, merchant);
        }

        Map<String, List<TransactionFileRow>> txnByMerchant = new HashMap<>();
        for (TransactionFileRow txn : transactions) {
            if (!TXN_CAPTURED.equals(txn.txnStatus) || txn.captureDate == null) {
                continue;
            }
            List<TransactionFileRow> list = txnByMerchant.get(txn.merchantCode);
            if (list == null) {
                list = new ArrayList<>();
                txnByMerchant.put(txn.merchantCode, list);
            }
            list.add(txn);
        }

        Map<String, List<PendingFileRow>> pendingByWallet = new HashMap<>();
        for (PendingFileRow pending : pendings) {
            List<PendingFileRow> list = pendingByWallet.get(pending.walletId);
            if (list == null) {
                list = new ArrayList<>();
                pendingByWallet.put(pending.walletId, list);
            }
            list.add(pending);
        }

        List<NoticeFileRow> notices = new ArrayList<>();
        int noticeSeq = 1;
        for (MerchantFileRow merchant : merchantByCode.values()) {
            List<TransactionFileRow> merchantTxns = txnByMerchant.get(merchant.merchantCode);
            if (merchantTxns == null || merchantTxns.isEmpty()) {
                continue;
            }
            if (!isSettlementTarget(merchant, businessDate)) {
                continue;
            }

            MerchantSettlement settlement = inspectMerchant(merchant, merchantTxns, pendingByWallet);
            if (!settlement.requiresNotice()) {
                continue;
            }

            String noticeKbn = settlement.noticeKind();
            String walletId = settlement.representativeWalletId();
            String text = buildNoticeText(merchant, settlement);
            notices.add(new NoticeFileRow(
                    "N" + businessDate.format(DateTimeFormatter.BASIC_ISO_DATE) + String.format("%05d", noticeSeq++),
                    walletId,
                    noticeKbn,
                    text,
                    NOTICE_UNSENT,
                    createTs
            ));
        }

        Collections.sort(notices, Comparator.comparing(n -> n.noticeId));
        return notices;
    }

    private MerchantSettlement inspectMerchant(MerchantFileRow merchant,
                                               List<TransactionFileRow> merchantTxns,
                                               Map<String, List<PendingFileRow>> pendingByWallet) {
        BigDecimal capturedTotal = BigDecimal.ZERO;
        BigDecimal pendingWaitTotal = BigDecimal.ZERO;
        BigDecimal settledPendingTotal = BigDecimal.ZERO;
        int pendingWaitCount = 0;
        int differenceCount = 0;
        String representativeWallet = "";
        List<String> reasons = new ArrayList<>();

        for (TransactionFileRow txn : merchantTxns) {
            capturedTotal = capturedTotal.add(txn.requestAmount);
            if (representativeWallet.isEmpty()) {
                representativeWallet = txn.walletId;
            }

            List<PendingFileRow> walletPendings = pendingByWallet.get(txn.walletId);
            if (walletPendings == null || walletPendings.isEmpty()) {
                differenceCount++;
                reasons.add("保留明細なし:" + txn.txnId);
                continue;
            }

            BigDecimal walletWait = BigDecimal.ZERO;
            BigDecimal walletSettled = BigDecimal.ZERO;
            for (PendingFileRow pending : walletPendings) {
                if (PEND_WAIT.equals(pending.pendingStatus)) {
                    walletWait = walletWait.add(pending.pendingAmount);
                    pendingWaitCount++;
                    if (isOldPending(txn, pending)) {
                        reasons.add("長期未確定:" + pending.pendingId);
                    } else {
                        reasons.add("未確定:" + pending.pendingId);
                    }
                } else if (PEND_SETTLED.equals(pending.pendingStatus)) {
                    walletSettled = walletSettled.add(pending.pendingAmount);
                }
            }

            pendingWaitTotal = pendingWaitTotal.add(walletWait);
            settledPendingTotal = settledPendingTotal.add(walletSettled);

            BigDecimal matchedAmount = walletWait.add(walletSettled);
            if (matchedAmount.compareTo(txn.requestAmount) != 0) {
                differenceCount++;
                reasons.add("金額差異:" + txn.txnId);
            }
        }

        if (!MERCHANT_ACTIVE.equals(merchant.merchantStatus)) {
            reasons.add("加盟店状態確認:" + merchant.merchantStatus);
        }
        if (capturedTotal.compareTo(merchant.dailyLimitAmount) > 0) {
            reasons.add("日次上限超過:" + merchant.dailyLimitAmount.toPlainString());
        }
        if ("S".equals(merchant.riskRank) && pendingWaitCount > 0) {
            reasons.add("高リスク保留確認:" + merchant.riskRank);
        }

        return new MerchantSettlement(
                merchant.merchantCode,
                representativeWallet,
                capturedTotal,
                pendingWaitTotal,
                settledPendingTotal,
                pendingWaitCount,
                differenceCount,
                reasons
        );
    }

    private boolean isSettlementTarget(MerchantFileRow merchant, LocalDate businessDate) {
        if ("D".equals(merchant.settleCycleKind)) {
            return true;
        }
        if ("W".equals(merchant.settleCycleKind)) {
            return businessDate.getDayOfWeek().getValue() == 7;
        }
        if ("M".equals(merchant.settleCycleKind)) {
            return businessDate.getDayOfMonth() == businessDate.lengthOfMonth();
        }
        return false;
    }

    private boolean isOldPending(TransactionFileRow txn, PendingFileRow pending) {
        LocalDate base = pending.captureDate != null ? pending.captureDate : txn.captureDate;
        return base != null && base.plusDays(2).isBefore(LocalDate.parse("2026-06-28"));
    }

    private String buildNoticeText(MerchantFileRow merchant, MerchantSettlement settlement) {
        StringBuilder b = new StringBuilder();
        b.append("加盟店=").append(merchant.merchantCode);
        b.append(",精算周期=").append(merchant.settleCycleKind);
        b.append(",確定売上=").append(settlement.capturedTotal.setScale(0, RoundingMode.UNNECESSARY).toPlainString());
        b.append(",未確定=").append(settlement.pendingWaitTotal.setScale(0, RoundingMode.UNNECESSARY).toPlainString());
        b.append(",差異件数=").append(settlement.differenceCount);
        b.append(",確認=").append(joinReasons(settlement.reasons));
        return b.toString();
    }

    private String joinReasons(List<String> reasons) {
        if (reasons.isEmpty()) {
            return "なし";
        }
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < reasons.size(); i++) {
            if (i > 0) {
                b.append("/");
            }
            b.append(reasons.get(i));
        }
        return b.toString();
    }

    private static BigDecimal money(String value) {
        return new BigDecimal(value).setScale(0, RoundingMode.UNNECESSARY);
    }

    private static final class MerchantFileRow {
        private final String merchantCode;
        private final String merchantStatus;
        private final String mcc;
        private final BigDecimal dailyLimitAmount;
        private final String riskRank;
        private final String settleCycleKind;

        private MerchantFileRow(String merchantCode, String merchantStatus, String mcc,
                                BigDecimal dailyLimitAmount, String riskRank, String settleCycleKind) {
            this.merchantCode = merchantCode;
            this.merchantStatus = merchantStatus;
            this.mcc = mcc;
            this.dailyLimitAmount = dailyLimitAmount;
            this.riskRank = riskRank;
            this.settleCycleKind = settleCycleKind;
        }
    }

    private static final class TransactionFileRow {
        private final String txnId;
        private final String requestId;
        private final String walletId;
        private final String merchantCode;
        private final BigDecimal requestAmount;
        private final String txnStatus;
        private final LocalDate authDate;
        private final LocalDate captureDate;

        private TransactionFileRow(String txnId, String requestId, String walletId, String merchantCode,
                                   BigDecimal requestAmount, String txnStatus,
                                   LocalDate authDate, LocalDate captureDate) {
            this.txnId = txnId;
            this.requestId = requestId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.requestAmount = requestAmount;
            this.txnStatus = txnStatus;
            this.authDate = authDate;
            this.captureDate = captureDate;
        }
    }

    private static final class PendingFileRow {
        private final String pendingId;
        private final String walletId;
        private final BigDecimal pendingAmount;
        private final String pendingStatus;
        private final LocalDate captureDate;

        private PendingFileRow(String pendingId, String walletId, BigDecimal pendingAmount,
                               String pendingStatus, LocalDate captureDate) {
            this.pendingId = pendingId;
            this.walletId = walletId;
            this.pendingAmount = pendingAmount;
            this.pendingStatus = pendingStatus;
            this.captureDate = captureDate;
        }
    }

    private static final class NoticeFileRow {
        private final String noticeId;
        private final String walletId;
        private final String noticeKind;
        private final String noticeText;
        private final String sendStatus;
        private final LocalDateTime createTs;

        private NoticeFileRow(String noticeId, String walletId, String noticeKind,
                              String noticeText, String sendStatus, LocalDateTime createTs) {
            this.noticeId = noticeId;
            this.walletId = walletId;
            this.noticeKind = noticeKind;
            this.noticeText = noticeText;
            this.sendStatus = sendStatus;
            this.createTs = createTs;
        }

        private String toSequentialRecord() {
            return noticeId + "," + walletId + "," + noticeKind + "," + noticeText + ","
                    + sendStatus + "," + createTs.format(TS_FMT);
        }
    }

    private static final class MerchantSettlement {
        private final String merchantCode;
        private final String representativeWalletId;
        private final BigDecimal capturedTotal;
        private final BigDecimal pendingWaitTotal;
        private final BigDecimal settledPendingTotal;
        private final int pendingWaitCount;
        private final int differenceCount;
        private final List<String> reasons;

        private MerchantSettlement(String merchantCode, String representativeWalletId,
                                   BigDecimal capturedTotal, BigDecimal pendingWaitTotal,
                                   BigDecimal settledPendingTotal, int pendingWaitCount,
                                   int differenceCount, List<String> reasons) {
            this.merchantCode = merchantCode;
            this.representativeWalletId = representativeWalletId;
            this.capturedTotal = capturedTotal;
            this.pendingWaitTotal = pendingWaitTotal;
            this.settledPendingTotal = settledPendingTotal;
            this.pendingWaitCount = pendingWaitCount;
            this.differenceCount = differenceCount;
            this.reasons = reasons;
        }

        private boolean requiresNotice() {
            return pendingWaitCount > 0 || differenceCount > 0 || !reasons.isEmpty();
        }

        private String noticeKind() {
            if (differenceCount > 0) {
                return "DIF";
            }
            if (pendingWaitCount > 0) {
                return "PND";
            }
            return "CHK";
        }

        private String representativeWalletId() {
            return representativeWalletId.isEmpty() ? "W0000000" : representativeWalletId;
        }
    }
}
