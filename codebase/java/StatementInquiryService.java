/**
 * 変更履歴
 * 版数  年月日    担当       概要
 * 1.00  20240603  会員系開発  請求明細照会サービス新規作成
 */
public class StatementInquiryService {
    private static final String CARD_ACTIVE = "01";
    private static final String CARD_STOPPED = "02";
    private static final String CARD_CLOSED = "03";
    private static final String BILL_CONFIRMED = "C";
    private static final String BILL_SKIPPED = "S";
    private static final String PUBLISHED = "1";
    private static final String POSTED = "1";
    private static final String CAPTURED = "1";

    public static void main(String[] args) {
        String cardNo = args.length > 0 ? args[0] : "4980123412340001";
        String cycleDt = args.length > 1 ? args[1] : "20260625";
        String memberId = args.length > 2 ? args[2] : "M000001";
        boolean showAvailableCredit = args.length > 3 && "Y".equalsIgnoreCase(args[3]);

        inquire(cardNo, cycleDt, memberId, showAvailableCredit);
    }

    private static void inquire(String cardNo, String cycleDt, String memberId, boolean showAvailableCredit) {
        Result result = new Result(cardNo, cycleDt, memberId);

        if (isBlank(cardNo) || isBlank(cycleDt) || isBlank(memberId)) {
            result.message = "INPUT_ERROR";
            printResult(result);
            return;
        }

        String[] card = findCard(cardNo);
        if (card == null) {
            result.message = "CARD_NOT_FOUND";
            printResult(result);
            return;
        }

        if (!memberId.equals(card[1])) {
            result.message = "MEMBER_MISMATCH";
            printResult(result);
            return;
        }

        result.memberNameKana = card[5];
        result.cardStatus = card[2];
        result.creditLimit = parseInt(card[3]);

        if (CARD_STOPPED.equals(result.cardStatus) || CARD_CLOSED.equals(result.cardStatus)) {
            result.billStatus = BILL_SKIPPED;
            result.message = "BILL_SKIPPED_BY_CARD_STATUS";
            printResult(result);
            return;
        }

        if (!CARD_ACTIVE.equals(result.cardStatus)) {
            result.message = "INVALID_CARD_STATUS";
            printResult(result);
            return;
        }

        String[] statement = findPublishedStatement(cardNo, cycleDt);
        if (statement == null) {
            result.message = "STATEMENT_NOT_FOUND";
            printResult(result);
            return;
        }

        String[] bill = findBill(cardNo, cycleDt);
        if (bill == null) {
            result.message = "BILL_NOT_FOUND";
            printResult(result);
            return;
        }

        result.statementId = statement[2];
        result.billAmount = parseInt(bill[2]);
        result.minPayAmount = parseInt(bill[3]);
        result.dueDt = bill[4];
        result.billStatus = BILL_CONFIRMED;

        collectSales(result, cardNo, cycleDt);
        collectFees(result, cardNo, card[4], cycleDt);

        result.computedAmount = result.salesTotal + result.taxTotal + result.feeTotal;

        if (showAvailableCredit) {
            result.availableCredit = calculateAvailableCredit(cardNo, result.creditLimit, cycleDt);
            result.availableCreditShown = true;
        }

        int statementAmount = parseInt(statement[3]);
        if (statementAmount != result.billAmount) {
            result.message = "STATEMENT_BILL_AMOUNT_MISMATCH";
        } else if (result.computedAmount != result.billAmount) {
            result.message = "DETAIL_AMOUNT_MISMATCH";
        } else {
            result.message = "OK";
        }

        printResult(result);
    }

    private static void collectSales(Result result, String cardNo, String cycleDt) {
        for (int i = 0; i < SALES_FILE.length; i++) {
            String[] sale = SALES_FILE[i];
            if (cardNo.equals(sale[2]) && cycleDt.equals(sale[5]) && CAPTURED.equals(sale[8])) {
                result.salesIndexes[result.salesCount++] = i;
                result.salesTotal += parseInt(sale[6]);
                result.taxTotal += parseInt(sale[7]);
            }
        }
    }

    private static void collectFees(Result result, String cardNo, String cycleDay, String cycleDt) {
        for (int i = 0; i < FEE_FILE.length; i++) {
            String[] fee = FEE_FILE[i];
            if (cardNo.equals(fee[1])
                    && cycleDt.equals(fee[2])
                    && cycleDay.equals(fee[5])
                    && POSTED.equals(fee[6])) {
                result.feeIndexes[result.feeCount++] = i;
                result.feeTotal += parseInt(fee[3]);
            }
        }
    }

    private static void printResult(Result result) {
        System.out.println("SERVICE=BILL_INQUIRY");
        System.out.println("CARD_NO=" + maskCardNo(result.cardNo));
        System.out.println("CYCLE_DT=" + result.cycleDt);
        System.out.println("MEMBER_ID=" + result.memberId);
        System.out.println("RESULT=" + result.message);

        if (result.statementId == null) {
            return;
        }

        System.out.println("STATEMENT_ID=" + result.statementId);
        System.out.println("MEMBER_NAME_KANA=" + result.memberNameKana);
        System.out.println("CARD_STATUS=" + result.cardStatus);
        System.out.println("BILL_STATUS=" + result.billStatus);
        System.out.println("BILL_AMOUNT=" + result.billAmount);
        System.out.println("MIN_PAY_AMOUNT=" + result.minPayAmount);
        System.out.println("DUE_DT=" + result.dueDt);

        System.out.println("SALES_COUNT=" + result.salesCount);
        for (int i = 0; i < result.salesCount; i++) {
            String[] sale = SALES_FILE[result.salesIndexes[i]];
            System.out.println("SALE_ID=" + sale[0]
                    + ", AUTH_ID=" + sale[1]
                    + ", MERCHANT_ID=" + sale[3]
                    + ", SALE_DT=" + sale[4]
                    + ", SALE_AMOUNT=" + sale[6]
                    + ", TAX_AMOUNT=" + sale[7]);
        }

        System.out.println("FEE_COUNT=" + result.feeCount);
        for (int i = 0; i < result.feeCount; i++) {
            String[] fee = FEE_FILE[result.feeIndexes[i]];
            System.out.println("FEE_ID=" + fee[0]
                    + ", FEE_DT=" + fee[2]
                    + ", FEE_AMOUNT=" + fee[3]
                    + ", FEE_TYPE=" + fee[4]);
        }

        System.out.println("SALES_TOTAL=" + result.salesTotal);
        System.out.println("TAX_TOTAL=" + result.taxTotal);
        System.out.println("FEE_TOTAL=" + result.feeTotal);
        System.out.println("DETAIL_TOTAL=" + result.computedAmount);

        if (result.availableCreditShown) {
            System.out.println("AVAILABLE_CREDIT=" + result.availableCredit);
        }
    }

    private static String[] findCard(String cardNo) {
        for (int i = 0; i < CARD_FILE.length; i++) {
            if (cardNo.equals(CARD_FILE[i][0])) {
                return CARD_FILE[i];
            }
        }
        return null;
    }

    private static String[] findPublishedStatement(String cardNo, String cycleDt) {
        for (int i = 0; i < STATEMENT_INDEX_FILE.length; i++) {
            String[] record = STATEMENT_INDEX_FILE[i];
            if (cardNo.equals(record[0]) && cycleDt.equals(record[1]) && PUBLISHED.equals(record[5])) {
                return record;
            }
        }
        return null;
    }

    private static String[] findBill(String cardNo, String cycleDt) {
        for (int i = 0; i < BILL_FILE.length; i++) {
            String[] record = BILL_FILE[i];
            if (cardNo.equals(record[0]) && cycleDt.equals(record[1]) && BILL_CONFIRMED.equals(record[5])) {
                return record;
            }
        }
        return null;
    }

    private static int calculateAvailableCredit(String cardNo, int creditLimit, String cycleDt) {
        int unbilled = 0;

        for (int i = 0; i < SALES_FILE.length; i++) {
            String[] sale = SALES_FILE[i];
            if (cardNo.equals(sale[2]) && CAPTURED.equals(sale[8]) && sale[5].compareTo(cycleDt) > 0) {
                unbilled += parseInt(sale[6]) + parseInt(sale[7]);
            }
        }

        return Math.max(0, creditLimit - unbilled);
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().length() == 0;
    }

    private static int parseInt(String value) {
        return Integer.parseInt(value);
    }

    private static String maskCardNo(String cardNo) {
        if (cardNo == null || cardNo.length() < 10) {
            return "********";
        }
        return cardNo.substring(0, 6) + "******" + cardNo.substring(cardNo.length() - 4);
    }

    private static final String[][] CARD_FILE = {
            {"4980123412340001", "M000001", "01", "800000", "25", "YAMADA TARO", "20210115"},
            {"4980123412340002", "M000002", "01", "500000", "25", "SATO HANAKO", "20200308"},
            {"4980123412340003", "M000003", "02", "300000", "10", "SUZUKI ICHIRO", "20220520"},
            {"4980123412340004", "M000004", "03", "200000", "10", "TANAKA MIDORI", "20191201"}
    };

    private static final String[][] STATEMENT_INDEX_FILE = {
            {"4980123412340001", "20260625", "ST202606250001", "88000", "20260710", "1"},
            {"4980123412340002", "20260625", "ST202606250002", "33550", "20260710", "1"},
            {"4980123412340003", "20260610", "ST202606100003", "0", "20260625", "1"},
            {"4980123412340001", "20260525", "ST202605250001", "44200", "20260610", "0"}
    };

    private static final String[][] BILL_FILE = {
            {"4980123412340001", "20260625", "88000", "15000", "20260710", "C", "BIL001"},
            {"4980123412340002", "20260625", "33550", "10000", "20260710", "C", "BIL002"},
            {"4980123412340003", "20260610", "0", "0", "20260625", "S", "BIL003"},
            {"4980123412340004", "20260610", "0", "0", "20260625", "S", "BIL004"}
    };

    private static final String[][] SALES_FILE = {
            {"SL000001", "AU900001", "4980123412340001", "JP-TSE-001", "20260603", "20260625", "42000", "4200", "1"},
            {"SL000002", "AU900002", "4980123412340001", "JP-OSA-018", "20260611", "20260625", "18000", "1800", "1"},
            {"SL000003", "AU900003", "4980123412340001", "JP-NAG-052", "20260618", "20260625", "15000", "1500", "1"},
            {"SL000004", "AU900004", "4980123412340001", "JP-FUK-007", "20260620", "20260625", "5000", "500", "1"},
            {"SL000005", "AU900005", "4980123412340001", "JP-TSE-011", "20260626", "20260725", "22000", "2200", "1"},
            {"SL000006", "AU900006", "4980123412340002", "JP-SAP-003", "20260609", "20260625", "28500", "2850", "1"},
            {"SL000007", "AU900007", "4980123412340002", "JP-TSE-021", "20260622", "20260625", "2000", "200", "1"},
            {"SL000008", "AU900008", "4980123412340003", "JP-KYO-014", "20260604", "20260610", "12000", "1200", "1"}
    };

    private static final String[][] FEE_FILE = {
            {"FE000001", "4980123412340001", "20260625", "0", "NENKAIHI", "25", "1"},
            {"FE000002", "4980123412340002", "20260625", "0", "NENKAIHI", "25", "1"},
            {"FE000003", "4980123412340003", "20260610", "11000", "NENKAIHI", "10", "0"}
    };

    private static final class Result {
        private final String cardNo;
        private final String cycleDt;
        private final String memberId;
        private final int[] salesIndexes = new int[SALES_FILE.length];
        private final int[] feeIndexes = new int[FEE_FILE.length];

        private String statementId;
        private String memberNameKana;
        private String cardStatus;
        private String billStatus;
        private String dueDt;
        private String message;

        private int salesCount;
        private int feeCount;
        private int creditLimit;
        private int billAmount;
        private int minPayAmount;
        private int salesTotal;
        private int taxTotal;
        private int feeTotal;
        private int computedAmount;
        private int availableCredit;
        private boolean availableCreditShown;

        private Result(String cardNo, String cycleDt, String memberId) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.memberId = memberId;
        }
    }
}
