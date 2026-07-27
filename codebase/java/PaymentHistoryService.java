/**
 * 変更履歴
 * 版数  年月日    担当       概要
 * 1.00  20230712  会員系開発  入金履歴照会サービス新規作成
 */
public class PaymentHistoryService {
    private static final String STS_ALLOCATED = "\u5145\u5f53\u6e08";
    private static final String STS_UNALLOCATED = "\u672a\u914d\u8ce6";
    private static final String STS_OVERPAID = "\u904e\u5165\u91d1";

    private static final String BILL_CONFIRMED = "C";
    private static final String BILL_SKIP = "S";

    private static final String[][] CDPAYF = {
            {"P20260401001", "4111110000001001", "2026-04-01", "120000", "\u632f\u8fbc", STS_ALLOCATED, "BK260401001"},
            {"P20260412002", "4111110000001001", "2026-04-12", "15000", "\u53e3\u5ea7", STS_UNALLOCATED, "BK260412002"},
            {"P20260501003", "4111110000001001", "2026-05-01", "97000", "\u632f\u8fbc", STS_ALLOCATED, "BK260501003"},
            {"P20260520004", "4111110000001001", "2026-05-20", "10000", "\u53e3\u5ea7", STS_OVERPAID, "BK260520004"},
            {"P20260403005", "4111110000002002", "2026-04-03", "64000", "\u632f\u8fbc", STS_ALLOCATED, "BK260403005"},
            {"P20260505006", "4111110000002002", "2026-05-05", "18000", "\u53e3\u5ea7", STS_UNALLOCATED, "BK260505006"},
            {"P20260407007", "4111110000003003", "2026-04-07", "45000", "\u632f\u8fbc", STS_ALLOCATED, "BK260407007"},
            {"P20260509008", "4111110000009009", "2026-05-09", "30000", "\u632f\u8fbc", STS_ALLOCATED, "BK260509008"},
            {"P20260515009", "4111110000009009", "2026-05-15", "8500", "\u53e3\u5ea7", STS_OVERPAID, "BK260515009"}
    };

    private static final String[][] CDBILLF = {
            {"4111110000001001", "2026-03-31", "120000", "30000", "2026-04-10", "C", "StatementInquiryService"},
            {"4111110000001001", "2026-04-30", "97000", "25000", "2026-05-10", "C", "StatementInquiryService"},
            {"4111110000001001", "2026-05-31", "88000", "22000", "2026-06-10", "H", "StatementInquiryService"},
            {"4111110000002002", "2026-03-31", "64000", "16000", "2026-04-10", "C", "StatementInquiryService"},
            {"4111110000002002", "2026-04-30", "76000", "19000", "2026-05-10", "S", "StatementInquiryService"},
            {"4111110000003003", "2026-03-31", "45000", "12000", "2026-04-10", "S", "StatementInquiryService"},
            {"4111110000009009", "2026-04-30", "30000", "10000", "2026-05-10", "C", "StatementInquiryService"}
    };

    public static void main(String[] args) {
        String memberId = args.length == 0 ? "M0001" : args[0];
        String cardNo = cardNoByMember(memberId);

        if (cardNo == null) {
            System.out.println("\u4f1a\u54e1ID=" + memberId + " \u306f\u767b\u9332\u306a\u3057");
            return;
        }

        String[][] payments = selectPayments(cardNo);
        sortPayments(payments);

        String[][] bills = selectBills(cardNo);
        String[][] lines = allocate(payments, bills);

        long allocatedTotal = 0L;
        long unallocatedTotal = 0L;
        long overpaidTotal = 0L;

        for (int i = 0; i < lines.length; i++) {
            long appliedAmount = Long.parseLong(lines[i][9]);
            if (STS_ALLOCATED.equals(lines[i][8])) {
                allocatedTotal += appliedAmount;
            } else if (STS_OVERPAID.equals(lines[i][8])) {
                overpaidTotal += appliedAmount;
            } else {
                unallocatedTotal += appliedAmount;
            }
        }

        System.out.println("\u5165\u91d1\u5c65\u6b74\u7167\u4f1a\u30b5\u30fc\u30d3\u30b9");
        System.out.println("\u4f1a\u54e1ID=" + memberId + " \u30ab\u30fc\u30c9\u756a\u53f7=" + maskCardNo(cardNo));
        System.out.println("\u53d7\u4ed8\u65e5       \u5165\u91d1ID        \u65b9\u6cd5  \u5165\u91d1\u984d  \u8868\u793a\u533a\u5206  \u8acb\u6c42\u30b5\u30a4\u30af\u30eb  \u671f\u65e5        \u8acb\u6c42\u984d  \u53c2\u7167\u756a\u53f7");

        for (int i = 0; i < lines.length; i++) {
            System.out.println(lines[i][1] + " "
                    + lines[i][0] + " "
                    + lines[i][3] + " "
                    + lines[i][2] + " "
                    + lines[i][8] + " "
                    + lines[i][5] + " "
                    + lines[i][6] + " "
                    + lines[i][7] + " "
                    + lines[i][4]);
        }

        System.out.println("\u5145\u5f53\u6e08\u5408\u8a08=" + allocatedTotal
                + " \u672a\u914d\u8ce6\u5408\u8a08=" + unallocatedTotal
                + " \u904e\u5165\u91d1\u5408\u8a08=" + overpaidTotal);
    }

    private static String[][] allocate(String[][] payments, String[][] bills) {
        String[][] lines = new String[payments.length][];

        for (int i = 0; i < payments.length; i++) {
            String[] payment = payments[i];
            int exactBill = findBillByDueDateAndAmount(payment, bills);

            if (exactBill >= 0) {
                lines[i] = toLine(payment, bills[exactBill], STS_ALLOCATED, payment[3]);
                continue;
            }

            int nearestBill = findNearestConfirmedBill(payment, bills);
            if (nearestBill >= 0 && Long.parseLong(payment[3]) > Long.parseLong(bills[nearestBill][2])) {
                long overpaid = Long.parseLong(payment[3]) - Long.parseLong(bills[nearestBill][2]);
                lines[i] = toLine(payment, bills[nearestBill], STS_OVERPAID, Long.toString(overpaid));
            } else {
                lines[i] = new String[] {
                        payment[0],
                        payment[2],
                        payment[3],
                        payment[4],
                        payment[6],
                        "-",
                        "-",
                        "0",
                        STS_UNALLOCATED,
                        payment[3],
                        "-"
                };
            }
        }

        return lines;
    }

    private static String[] toLine(String[] payment, String[] bill, String status, String appliedAmount) {
        return new String[] {
                payment[0],
                payment[2],
                payment[3],
                payment[4],
                payment[6],
                bill[1],
                bill[4],
                bill[2],
                status,
                appliedAmount,
                bill[6]
        };
    }

    private static int findBillByDueDateAndAmount(String[] payment, String[][] bills) {
        for (int i = 0; i < bills.length; i++) {
            int daysFromDue = compareDate(payment[2], bills[i][4]);
            if (BILL_CONFIRMED.equals(bills[i][5])
                    && Long.parseLong(payment[3]) == Long.parseLong(bills[i][2])
                    && daysFromDue >= -10
                    && daysFromDue <= 3) {
                return i;
            }
        }
        return -1;
    }

    private static int findNearestConfirmedBill(String[] payment, String[][] bills) {
        int nearest = -1;
        int nearestDistance = Integer.MAX_VALUE;

        for (int i = 0; i < bills.length; i++) {
            if (!BILL_CONFIRMED.equals(bills[i][5])) {
                continue;
            }

            int distance = Math.abs(compareDate(payment[2], bills[i][4]));
            if (distance < nearestDistance) {
                nearest = i;
                nearestDistance = distance;
            }
        }

        return nearest;
    }

    private static String[][] selectPayments(String cardNo) {
        int count = 0;
        for (int i = 0; i < CDPAYF.length; i++) {
            if (CDPAYF[i][1].equals(cardNo)) {
                count++;
            }
        }

        String[][] selected = new String[count][];
        int pos = 0;
        for (int i = 0; i < CDPAYF.length; i++) {
            if (CDPAYF[i][1].equals(cardNo)) {
                selected[pos++] = CDPAYF[i];
            }
        }

        return selected;
    }

    private static String[][] selectBills(String cardNo) {
        int count = 0;
        for (int i = 0; i < CDBILLF.length; i++) {
            if (CDBILLF[i][0].equals(cardNo) && !BILL_SKIP.equals(CDBILLF[i][5])) {
                count++;
            }
        }

        String[][] selected = new String[count][];
        int pos = 0;
        for (int i = 0; i < CDBILLF.length; i++) {
            if (CDBILLF[i][0].equals(cardNo) && !BILL_SKIP.equals(CDBILLF[i][5])) {
                selected[pos++] = CDBILLF[i];
            }
        }

        return selected;
    }

    private static void sortPayments(String[][] payments) {
        for (int i = 1; i < payments.length; i++) {
            String[] target = payments[i];
            int j = i - 1;
            while (j >= 0 && compareDate(payments[j][2], target[2]) > 0) {
                payments[j + 1] = payments[j];
                j--;
            }
            payments[j + 1] = target;
        }
    }

    private static int compareDate(String left, String right) {
        return toSerialDays(left) - toSerialDays(right);
    }

    private static int toSerialDays(String ymd) {
        int year = Integer.parseInt(ymd.substring(0, 4));
        int month = Integer.parseInt(ymd.substring(5, 7));
        int day = Integer.parseInt(ymd.substring(8, 10));

        int total = year * 365 + day + leapDaysBefore(year);
        for (int monthIndex = 1; monthIndex < month; monthIndex++) {
            total += daysInMonth(year, monthIndex);
        }

        return total;
    }

    private static int leapDaysBefore(int year) {
        int y = year - 1;
        return y / 4 - y / 100 + y / 400;
    }

    private static int daysInMonth(int year, int month) {
        switch (month) {
            case 2:
                return isLeapYear(year) ? 29 : 28;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            default:
                return 31;
        }
    }

    private static boolean isLeapYear(int year) {
        return year % 400 == 0 || (year % 4 == 0 && year % 100 != 0);
    }

    private static String cardNoByMember(String memberId) {
        if ("M0001".equals(memberId)) {
            return "4111110000001001";
        }
        if ("M0002".equals(memberId)) {
            return "4111110000002002";
        }
        if ("M0003".equals(memberId)) {
            return "4111110000003003";
        }
        if ("M0009".equals(memberId)) {
            return "4111110000009009";
        }
        return null;
    }

    private static String maskCardNo(String cardNo) {
        return cardNo.substring(0, 6) + "******" + cardNo.substring(12);
    }
}
