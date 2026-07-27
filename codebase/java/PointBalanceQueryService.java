/**
 * 変更履歴
 * 版数 / 年月日 / 担当 / 概要
 * 1.00 / 2021-04-01 / 業務開発部 / 初版作成。ポイント残高照会サービスを作成。
 * 1.10 / 2022-10-01 / 業務開発部 / ポイント名称を「取引ポイント」から「証券ポイント」へ変更。
 * 1.20 / 2024-03-15 / 業務開発部 / 延滞停止会員の交換不可表示を追加。
 */
public class PointBalanceQueryService {

    private static final String PROGRAM_ID = "CDPNTBAL";
    private static final String SETL_CONFIRMED = "D";
    private static final String TXN_SHOP_DOMESTIC = "P1";
    private static final String TXN_SHOP_OVERSEAS = "P2";
    private static final String TXN_CASH_DOMESTIC = "C1";
    private static final String TXN_CASH_OVERSEAS = "C2";
    private static final String DISP_SHOPPING = "S";
    private static final String DISP_CASHING = "K";

    private final java.util.List<Cdpntfc> pointRecords;
    private final java.util.List<Cdovsfc> salesRecords;
    private final java.util.List<Cdmvwfc> displayRecords = new java.util.ArrayList<>();
    private final java.util.List<Cdlogfc> logRecords = new java.util.ArrayList<>();

    public PointBalanceQueryService(java.util.List<Cdpntfc> pointRecords, java.util.List<Cdovsfc> salesRecords) {
        this.pointRecords = pointRecords;
        this.salesRecords = salesRecords;
    }

    public java.util.List<Cdmvwfc> displayRecords() {
        return java.util.Collections.unmodifiableList(displayRecords);
    }

    public java.util.List<Cdlogfc> logRecords() {
        return java.util.Collections.unmodifiableList(logRecords);
    }

    public void inquire() {
        java.util.Map<String, Long> confirmedEarnMap = new java.util.LinkedHashMap<>();
        java.util.Map<String, String> latestTxnMap = new java.util.LinkedHashMap<>();
        java.util.Map<String, String> dispKbnMap = new java.util.LinkedHashMap<>();

        for (Cdovsfc sales : salesRecords) {
            if (!SETL_CONFIRMED.equals(sales.ovSetlKbn())) {
                continue;
            }
            if (!isPointTargetTxn(sales.ovTxnKbn())) {
                continue;
            }
            long scheduledPoint = calcScheduledPoint(sales);
            if (scheduledPoint <= 0L) {
                continue;
            }
            Long current = confirmedEarnMap.get(sales.ovCardNo());
            confirmedEarnMap.put(sales.ovCardNo(), (current == null ? 0L : current) + scheduledPoint);
            latestTxnMap.put(sales.ovCardNo(), sales.ovTxnId());
            dispKbnMap.put(sales.ovCardNo(), toDispKbn(sales.ovTxnKbn()));
        }

        for (Cdpntfc point : pointRecords) {
            long scheduled = confirmedEarnMap.containsKey(point.pnCardNo()) ? confirmedEarnMap.get(point.pnCardNo()) : 0L;
            long usableBalance = point.pnPointBal() + parseAmount(point.pnPointEarned()) + parseAmount(point.pnPointAdj()) + scheduled;
            String txnId = latestTxnMap.getOrDefault(point.pnCardNo(), "");
            String dispKbn = dispKbnMap.getOrDefault(point.pnCardNo(), DISP_SHOPPING);

            String label;
            if (isExchangeStopped(point)) {
                label = "証券ポイント残高 " + usableBalance + "P 交換不可";
                logRecords.add(new Cdlogfc(nextLogId(), PROGRAM_ID, point.pnCardNo(), "S", today(), "延滞停止"));
            } else {
                label = "証券ポイント残高 " + usableBalance + "P 最終付与日 " + point.pnLastEarnDt();
                logRecords.add(new Cdlogfc(nextLogId(), PROGRAM_ID, point.pnCardNo(), "N", today(), "残高照会"));
            }

            displayRecords.add(new Cdmvwfc(point.pnCardNo(), txnId, dispKbn, usableBalance, label));
        }
    }

    private static boolean isPointTargetTxn(String txnKbn) {
        return TXN_SHOP_DOMESTIC.equals(txnKbn)
                || TXN_SHOP_OVERSEAS.equals(txnKbn)
                || TXN_CASH_DOMESTIC.equals(txnKbn)
                || TXN_CASH_OVERSEAS.equals(txnKbn);
    }

    private static String toDispKbn(String txnKbn) {
        if (TXN_CASH_DOMESTIC.equals(txnKbn) || TXN_CASH_OVERSEAS.equals(txnKbn)) {
            return DISP_CASHING;
        }
        return DISP_SHOPPING;
    }

    private static long calcScheduledPoint(Cdovsfc sales) {
        long baseAmount = sales.ovSetlAmt();
        if ("FA".equals(sales.ovFeeKbn()) || "FB".equals(sales.ovFeeKbn())) {
            baseAmount = baseAmount + sales.ovFeeAmt();
        }
        if (baseAmount <= 0L) {
            return 0L;
        }
        if (TXN_SHOP_DOMESTIC.equals(sales.ovTxnKbn())) {
            return baseAmount / 100L;
        }
        if (TXN_SHOP_OVERSEAS.equals(sales.ovTxnKbn())) {
            return baseAmount / 200L;
        }
        if (TXN_CASH_DOMESTIC.equals(sales.ovTxnKbn()) || TXN_CASH_OVERSEAS.equals(sales.ovTxnKbn())) {
            return baseAmount / 1000L;
        }
        return 0L;
    }

    private static boolean isExchangeStopped(Cdpntfc point) {
        return parseAmount(point.pnPointAdj()) <= -999999L;
    }

    private static long parseAmount(String value) {
        if (value == null || value.trim().isEmpty()) {
            return 0L;
        }
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException ex) {
            return 0L;
        }
    }

    private static int today() {
        return Integer.parseInt(java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE));
    }

    private String nextLogId() {
        return "L" + String.format("%011d", logRecords.size() + 1);
    }
}
