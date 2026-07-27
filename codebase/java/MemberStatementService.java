public class MemberStatementService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2024/04/02  開発一課  会員明細連携サービス初版作成
     * 1.01  2024/10/08  保守二課  返品承認分の明細連携を追加
     */

    private static final String CARD_STATUS_VALID = "01";
    private static final String CAP_STATUS_CONFIRMED = "C";
    private static final String RETURN_APPROVED = "承認";
    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] a) {
        java.time.LocalDate postingDate = java.time.LocalDate.now();
        java.util.List<Cdcapf> captures = createCaptureData();
        java.util.Map<String, Cdcardf> cardIndex = createCardIndex(createCardData());
        java.util.Map<String, java.util.List<Cdrtnf>> returnsBySaleId = createReturnsBySaleId(createReturnData());

        java.util.List<Cdstmtf> statements = createStatements(captures, cardIndex, returnsBySaleId, postingDate);

        for (Cdstmtf statement : statements) {
            System.out.println(statement.toLine());
        }
        System.out.println("明細連携件数=" + statements.size());
    }

    private static java.util.List<Cdstmtf> createStatements(
            java.util.List<Cdcapf> captures,
            java.util.Map<String, Cdcardf> cardIndex,
            java.util.Map<String, java.util.List<Cdrtnf>> returnsBySaleId,
            java.time.LocalDate postingDate) {

        java.util.List<Cdstmtf> statements = new java.util.ArrayList<Cdstmtf>();
        java.util.Map<String, Long> memberTotals = new java.util.LinkedHashMap<String, Long>();
        int sequence = 1;

        for (Cdcapf capture : captures) {
            if (!CAP_STATUS_CONFIRMED.equals(capture.capStatus)) {
                continue;
            }

            Cdcardf card = cardIndex.get(capture.cardNo);
            if (card == null) {
                System.err.println("カード未登録のため明細連携を保留: SALE-ID=" + capture.saleId + ", CARD-NO=" + capture.cardNo);
                continue;
            }

            if (!CARD_STATUS_VALID.equals(card.cardStatus)) {
                System.err.println("カード状態が有効でないため明細連携を除外: SALE-ID=" + capture.saleId + ", CARD-NO=" + capture.cardNo
                        + ", CF-CARD-STATUS=" + card.cardStatus);
                continue;
            }

            if (capture.billedAmt <= 0) {
                System.err.println("請求金額不正のため明細連携を除外: SALE-ID=" + capture.saleId + ", BILLED-AMT=" + capture.billedAmt);
                continue;
            }

            if (BASE_CURRENCY.equals(capture.currencyCd) && capture.feeAmt != 0) {
                System.err.println("円貨取引の手数料金額不整合を検出: SALE-ID=" + capture.saleId + ", FEE-AMT=" + capture.feeAmt);
            }

            long postingAmount = capture.billedAmt + capture.feeAmt;
            long previousTotal = getLong(memberTotals, card.memberId);
            if (previousTotal + postingAmount > card.creditLimit) {
                System.err.println("与信限度額超過見込み: MEMBER-ID=" + card.memberId + ", SALE-ID=" + capture.saleId
                        + ", 累計=" + (previousTotal + postingAmount) + ", 限度額=" + card.creditLimit);
            }
            memberTotals.put(card.memberId, previousTotal + postingAmount);

            statements.add(new Cdstmtf(
                    createStatementId(postingDate, sequence++),
                    card.memberId,
                    capture.cardNo,
                    capture.saleId,
                    capture.billedAmt,
                    capture.feeAmt,
                    postingDate));

            java.util.List<Cdrtnf> saleReturns = returnsBySaleId.get(capture.saleId);
            if (saleReturns == null) {
                continue;
            }

            long approvedReturnTotal = 0;
            for (Cdrtnf returned : saleReturns) {
                if (!RETURN_APPROVED.equals(returned.approvalStatus)) {
                    continue;
                }
                if (!capture.cardNo.equals(returned.cardNo)) {
                    System.err.println("返品カード番号不一致のため除外: RETURN-ID=" + returned.returnId + ", SALE-ID=" + returned.saleId);
                    continue;
                }
                if (returned.returnAmt <= 0) {
                    System.err.println("返品金額不正のため除外: RETURN-ID=" + returned.returnId + ", RETURN-AMT=" + returned.returnAmt);
                    continue;
                }
                approvedReturnTotal += returned.returnAmt;
                if (approvedReturnTotal > capture.billedAmt) {
                    System.err.println("返品累計が売上金額を超過: SALE-ID=" + capture.saleId + ", RETURN-ID=" + returned.returnId
                            + ", 返品累計=" + approvedReturnTotal + ", 売上=" + capture.billedAmt);
                }

                statements.add(new Cdstmtf(
                        createStatementId(postingDate, sequence++),
                        card.memberId,
                        returned.cardNo,
                        returned.saleId,
                        -returned.returnAmt,
                        0,
                        postingDate));
                memberTotals.put(card.memberId, getLong(memberTotals, card.memberId) - returned.returnAmt);
            }
        }

        return statements;
    }

    private static java.util.Map<String, Cdcardf> createCardIndex(java.util.List<Cdcardf> cards) {
        java.util.Map<String, Cdcardf> index = new java.util.LinkedHashMap<String, Cdcardf>();
        for (Cdcardf card : cards) {
            index.put(card.cardNo, card);
        }
        return index;
    }

    private static java.util.Map<String, java.util.List<Cdrtnf>> createReturnsBySaleId(java.util.List<Cdrtnf> returns) {
        java.util.Map<String, java.util.List<Cdrtnf>> index = new java.util.LinkedHashMap<String, java.util.List<Cdrtnf>>();
        for (Cdrtnf returned : returns) {
            java.util.List<Cdrtnf> list = index.get(returned.saleId);
            if (list == null) {
                list = new java.util.ArrayList<Cdrtnf>();
                index.put(returned.saleId, list);
            }
            list.add(returned);
        }
        return index;
    }

    private static long getLong(java.util.Map<String, Long> map, String key) {
        Long value = map.get(key);
        return value == null ? 0L : value.longValue();
    }

    private static String createStatementId(java.time.LocalDate postingDate, int sequence) {
        return "ST" + postingDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
                + String.format("%06d", Integer.valueOf(sequence));
    }

    private static java.util.List<Cdcapf> createCaptureData() {
        java.util.List<Cdcapf> list = new java.util.ArrayList<Cdcapf>();
        list.add(new Cdcapf("S202606280001", "4111110000001001", 128000, 0, "JPY", "C", "CDCAP01"));
        list.add(new Cdcapf("S202606280002", "4111110000001002", 53000, 1590, "USD", "C", "CDCAP01"));
        list.add(new Cdcapf("S202606280003", "4111110000001003", 76000, 0, "JPY", "S", "CDCAP01"));
        list.add(new Cdcapf("S202606280004", "4111110000001004", 420000, 12600, "EUR", "C", "CDCAP01"));
        list.add(new Cdcapf("S202606280005", "4111110000001999", 9800, 0, "JPY", "C", "CDCAP02"));
        list.add(new Cdcapf("S202606280006", "4111110000001005", 250000, 7500, "HKD", "H", "CDCAP02"));
        return list;
    }

    private static java.util.List<Cdcardf> createCardData() {
        java.util.List<Cdcardf> list = new java.util.ArrayList<Cdcardf>();
        list.add(new Cdcardf("4111110000001001", "M0000001", "01", 1000000, "ヤマダタロウ"));
        list.add(new Cdcardf("4111110000001002", "M0000002", "01", 300000, "サトウハナコ"));
        list.add(new Cdcardf("4111110000001003", "M0000003", "01", 500000, "タナカイチロウ"));
        list.add(new Cdcardf("4111110000001004", "M0000004", "09", 600000, "スズキジロウ"));
        list.add(new Cdcardf("4111110000001005", "M0000005", "01", 200000, "イトウユウコ"));
        return list;
    }

    private static java.util.List<Cdrtnf> createReturnData() {
        java.util.List<Cdrtnf> list = new java.util.ArrayList<Cdrtnf>();
        list.add(new Cdrtnf("R202606280001", "S202606280001", "4111110000001001", 28000,
                java.time.LocalDate.of(2026, 6, 28), "顧客都合", "承認"));
        list.add(new Cdrtnf("R202606280002", "S202606280002", "4111110000001002", 12000,
                java.time.LocalDate.of(2026, 6, 28), "約定取消", "承認"));
        list.add(new Cdrtnf("R202606280003", "S202606280002", "4111110000001002", 5000,
                java.time.LocalDate.of(2026, 6, 28), "入力訂正", "審査中"));
        return list;
    }

    private static final class Cdcapf {
        private final String saleId;
        private final String cardNo;
        private final long billedAmt;
        private final long feeAmt;
        private final String currencyCd;
        private final String capStatus;
        private final String programId;

        private Cdcapf(String saleId, String cardNo, long billedAmt, long feeAmt,
                       String currencyCd, String capStatus, String programId) {
            this.saleId = saleId;
            this.cardNo = cardNo;
            this.billedAmt = billedAmt;
            this.feeAmt = feeAmt;
            this.currencyCd = currencyCd;
            this.capStatus = capStatus;
            this.programId = programId;
        }
    }

    private static final class Cdcardf {
        private final String cardNo;
        private final String memberId;
        private final String cardStatus;
        private final long creditLimit;
        private final String memberNameKana;

        private Cdcardf(String cardNo, String memberId, String cardStatus, long creditLimit, String memberNameKana) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
            this.creditLimit = creditLimit;
            this.memberNameKana = memberNameKana;
        }
    }

    private static final class Cdrtnf {
        private final String returnId;
        private final String saleId;
        private final String cardNo;
        private final long returnAmt;
        private final java.time.LocalDate returnDt;
        private final String returnReason;
        private final String approvalStatus;

        private Cdrtnf(String returnId, String saleId, String cardNo, long returnAmt,
                       java.time.LocalDate returnDt, String returnReason, String approvalStatus) {
            this.returnId = returnId;
            this.saleId = saleId;
            this.cardNo = cardNo;
            this.returnAmt = returnAmt;
            this.returnDt = returnDt;
            this.returnReason = returnReason;
            this.approvalStatus = approvalStatus;
        }
    }

    private static final class Cdstmtf {
        private final String statementId;
        private final String memberId;
        private final String cardNo;
        private final String saleId;
        private final long billedAmt;
        private final long feeAmt;
        private final java.time.LocalDate postingDt;

        private Cdstmtf(String statementId, String memberId, String cardNo, String saleId,
                        long billedAmt, long feeAmt, java.time.LocalDate postingDt) {
            this.statementId = statementId;
            this.memberId = memberId;
            this.cardNo = cardNo;
            this.saleId = saleId;
            this.billedAmt = billedAmt;
            this.feeAmt = feeAmt;
            this.postingDt = postingDt;
        }

        private String toLine() {
            return statementId + ","
                    + memberId + ","
                    + cardNo + ","
                    + saleId + ","
                    + billedAmt + ","
                    + feeAmt + ","
                    + postingDt;
        }
    }
}
