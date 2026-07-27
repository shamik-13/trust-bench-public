/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2022/05/16  開発標準  与信ゲートウェイサービス初版
 */
public class AuthGatewayService {
    private static final String CARD_STATUS_ACTIVE = "01";
    private static final String BASE_CURRENCY = "JPY";

    private static final String HOLD_APPROVED = "00";
    private static final String HOLD_CANCELLED = "20";
    private static final String HOLD_CAPTURED = "30";

    private static final String DECISION_APPROVE = "A";
    private static final String DECISION_DECLINE = "D";

    private static final String REASON_LIMIT = "LIM";
    private static final String REASON_STATUS = "STS";
    private static final String REASON_CURRENCY = "CUR";

    public static void main(String[] a) {
        DataStore store = DataStore.synthetic();

        for (AuthRecord auth : store.authRecords) {
            CardRecord card = store.findCard(auth.cardNo);
            MerchantRecord merchant = store.findMerchant(auth.merchantCode);
            ResponseRecord response = decide(auth, card, merchant, store);
            store.writeResponse(response);
        }

        for (ResponseRecord response : store.responseRecords) {
            System.out.println(response.toRecordLine());
        }
    }

    private static ResponseRecord decide(AuthRecord auth, CardRecord card, MerchantRecord merchant, DataStore store) {
        long available = card == null ? 0L : publishedLimit(card);
        String declineReason = null;

        if (card == null || !CARD_STATUS_ACTIVE.equals(card.cardStatus)) {
            declineReason = REASON_STATUS;
        } else if (!BASE_CURRENCY.equals(auth.currencyCd)) {
            declineReason = REASON_CURRENCY;
        } else if (merchant == null || !"01".equals(merchant.status)) {
            declineReason = REASON_STATUS;
        } else {
            RiskSignal signal = aggregateRisk(auth, merchant, store);
            long requiredAmount = auth.authAmt + signal.reserveAmount;
            if (available < requiredAmount) {
                declineReason = REASON_LIMIT;
            }
        }

        String decision = declineReason == null ? DECISION_APPROVE : DECISION_DECLINE;
        return new ResponseRecord(auth.authId, auth.cardNo, decision, available, auth.authAmt, declineReason == null ? "" : declineReason);
    }

    private static long publishedLimit(CardRecord card) {
        return Math.max(0L, card.creditLimit);
    }

    private static RiskSignal aggregateRisk(AuthRecord auth, MerchantRecord merchant, DataStore store) {
        int sameCardCount = 0;
        long sameCardAmount = 0L;
        int sameMerchantCount = 0;

        for (AuthRecord prior : store.authRecords) {
            if (prior == auth) {
                break;
            }
            if (auth.cardNo.equals(prior.cardNo) && minutesBetween(prior.authTs, auth.authTs) <= 10) {
                sameCardCount++;
                sameCardAmount += prior.authAmt;
            }
            if (auth.merchantCode.equals(prior.merchantCode) && minutesBetween(prior.authTs, auth.authTs) <= 5) {
                sameMerchantCount++;
            }
        }

        int score = 0;
        if ("H".equals(merchant.riskRank)) {
            score += 45;
        } else if ("M".equals(merchant.riskRank)) {
            score += 20;
        }
        if (!"JP".equals(merchant.countryCd)) {
            score += 25;
        }
        if (sameCardCount >= 3 || sameCardAmount >= 300000L) {
            score += 30;
        }
        if (sameMerchantCount >= 5) {
            score += 15;
        }
        if (auth.authAmt >= 500000L) {
            score += 25;
        }

        long reserve = score >= 70 ? Math.max(10000L, auth.authAmt / 10L) : 0L;
        return new RiskSignal(score, reserve);
    }

    private static int minutesBetween(String from, String to) {
        int fromMinutes = parseMinutes(from);
        int toMinutes = parseMinutes(to);
        return Math.max(0, toMinutes - fromMinutes);
    }

    private static int parseMinutes(String ts) {
        int hour = Integer.parseInt(ts.substring(8, 10));
        int minute = Integer.parseInt(ts.substring(10, 12));
        return hour * 60 + minute;
    }

    private static final class DataStore {
        private final AuthRecord[] authRecords;
        private final CardRecord[] cardRecords;
        private final MerchantRecord[] merchantRecords;
        private final ResponseRecord[] responseRecords;
        private int responseCount;

        private DataStore(AuthRecord[] authRecords, CardRecord[] cardRecords, MerchantRecord[] merchantRecords) {
            this.authRecords = authRecords;
            this.cardRecords = cardRecords;
            this.merchantRecords = merchantRecords;
            this.responseRecords = new ResponseRecord[authRecords.length];
        }

        private static DataStore synthetic() {
            AuthRecord[] auths = {
                new AuthRecord("A260628001", "4980123411110001", 12000L, HOLD_APPROVED, "M0001001", "JPY", "20260628090200", 20260630),
                new AuthRecord("A260628002", "4980123411110001", 98000L, HOLD_APPROVED, "M0002002", "JPY", "20260628090600", 20260630),
                new AuthRecord("A260628003", "4980123411110002", 35000L, HOLD_CANCELLED, "M0003003", "JPY", "20260628091200", 20260629),
                new AuthRecord("A260628004", "4980123411110003", 7200L, HOLD_APPROVED, "M0004004", "JPY", "20260628091900", 20260630),
                new AuthRecord("A260628005", "4980123411110004", 410000L, HOLD_APPROVED, "M0005005", "USD", "20260628092400", 20260630),
                new AuthRecord("A260628006", "4980123411110001", 430000L, HOLD_APPROVED, "M0006006", "JPY", "20260628092800", 20260630),
                new AuthRecord("A260628007", "4980123411110005", 21000L, HOLD_CAPTURED, "M0007007", "JPY", "20260628093100", 20260628),
                new AuthRecord("A260628008", "4980123411110006", 66000L, HOLD_APPROVED, "M0008008", "JPY", "20260628093600", 20260630)
            };

            CardRecord[] cards = {
                new CardRecord("4980123411110001", "MBR000001", CARD_STATUS_ACTIVE, 500000L, "ヤマダ タロウ"),
                new CardRecord("4980123411110002", "MBR000002", "02", 300000L, "サトウ ハナコ"),
                new CardRecord("4980123411110003", "MBR000003", CARD_STATUS_ACTIVE, 100000L, "タナカ イチロウ"),
                new CardRecord("4980123411110004", "MBR000004", CARD_STATUS_ACTIVE, 800000L, "スズキ ミカ"),
                new CardRecord("4980123411110005", "MBR000005", "09", 150000L, "イトウ ケン"),
                new CardRecord("4980123411110006", "MBR000006", CARD_STATUS_ACTIVE, 70000L, "ナカムラ アキ")
            };

            MerchantRecord[] merchants = {
                new MerchantRecord("M0001001", "トウキョウエキナカ", "5812", "L", "01", "JP"),
                new MerchantRecord("M0002002", "シンジュクデンキ", "5732", "M", "01", "JP"),
                new MerchantRecord("M0003003", "ギンザブティック", "5691", "M", "01", "JP"),
                new MerchantRecord("M0004004", "カンサイチケット", "4722", "H", "09", "JP"),
                new MerchantRecord("M0005005", "カイガイホテル", "7011", "H", "01", "US"),
                new MerchantRecord("M0006006", "ネットカデン", "5732", "H", "01", "JP"),
                new MerchantRecord("M0007007", "ホッカイドウリョカン", "7011", "L", "01", "JP"),
                new MerchantRecord("M0008008", "ナゴヤホビー", "5945", "M", "01", "JP")
            };

            return new DataStore(auths, cards, merchants);
        }

        private CardRecord findCard(String cardNo) {
            for (CardRecord card : cardRecords) {
                if (card.cardNo.equals(cardNo)) {
                    return card;
                }
            }
            return null;
        }

        private MerchantRecord findMerchant(String merchantCode) {
            for (MerchantRecord merchant : merchantRecords) {
                if (merchant.merchantCode.equals(merchantCode)) {
                    return merchant;
                }
            }
            return null;
        }

        private void writeResponse(ResponseRecord response) {
            responseRecords[responseCount++] = response;
        }
    }

    private static final class AuthRecord {
        private final String authId;
        private final String cardNo;
        private final long authAmt;
        private final String authResult;
        private final String merchantCode;
        private final String currencyCd;
        private final String authTs;
        private final int holdExpDt;

        private AuthRecord(String authId, String cardNo, long authAmt, String authResult, String merchantCode,
                           String currencyCd, String authTs, int holdExpDt) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.authAmt = authAmt;
            this.authResult = authResult;
            this.merchantCode = merchantCode;
            this.currencyCd = currencyCd;
            this.authTs = authTs;
            this.holdExpDt = holdExpDt;
        }
    }

    private static final class CardRecord {
        private final String cardNo;
        private final String memberId;
        private final String cardStatus;
        private final long creditLimit;
        private final String memberNameKana;

        private CardRecord(String cardNo, String memberId, String cardStatus, long creditLimit, String memberNameKana) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
            this.creditLimit = creditLimit;
            this.memberNameKana = memberNameKana;
        }
    }

    private static final class MerchantRecord {
        private final String merchantCode;
        private final String merchantNameKana;
        private final String mcc;
        private final String riskRank;
        private final String status;
        private final String countryCd;

        private MerchantRecord(String merchantCode, String merchantNameKana, String mcc, String riskRank,
                               String status, String countryCd) {
            this.merchantCode = merchantCode;
            this.merchantNameKana = merchantNameKana;
            this.mcc = mcc;
            this.riskRank = riskRank;
            this.status = status;
            this.countryCd = countryCd;
        }
    }

    private static final class ResponseRecord {
        private final String authId;
        private final String cardNo;
        private final String decisionKbn;
        private final long availAmt;
        private final long authAmt;
        private final String declineReason;

        private ResponseRecord(String authId, String cardNo, String decisionKbn, long availAmt, long authAmt,
                               String declineReason) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.decisionKbn = decisionKbn;
            this.availAmt = availAmt;
            this.authAmt = authAmt;
            this.declineReason = declineReason;
        }

        private String toRecordLine() {
            return authId + "," + cardNo + "," + decisionKbn + "," + availAmt + "," + authAmt + "," + declineReason;
        }
    }

    private static final class RiskSignal {
        private final int score;
        private final long reserveAmount;

        private RiskSignal(int score, long reserveAmount) {
            this.score = score;
            this.reserveAmount = reserveAmount;
        }
    }
}
