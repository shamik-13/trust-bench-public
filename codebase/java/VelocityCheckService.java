public class VelocityCheckService {
    /**
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2023-03-13  開発担当  速度違反検知サービス初版作成
     */

    private static final String BASE_CURRENCY = "JPY";
    private static final String AUTH_APPROVED = "00";

    private static final String FLAG_NONE = "0";
    private static final String FLAG_SHORT_BURST = "1";
    private static final String FLAG_MERCHANT_REPEAT = "2";
    private static final String FLAG_FOREIGN_REPEAT = "3";

    private static final String LEVEL_NONE = "0";
    private static final String LEVEL_NOTICE = "1";
    private static final String LEVEL_WARNING = "2";
    private static final String LEVEL_CRITICAL = "3";

    public static void main(String[] a) {
        VelocityCheckService service = new VelocityCheckService();
        service.runBenchmark();
    }

    private void runBenchmark() {
        java.util.Map<String, CdvelRecord> cdvelf = new java.util.LinkedHashMap<String, CdvelRecord>();
        cdvelf.put("4980000000000001", new CdvelRecord("4980000000000001", ts("2026-06-28T09:00:00"), 2, 32000L, ts("2026-06-28T09:07:00"), FLAG_NONE));
        cdvelf.put("4980000000000002", new CdvelRecord("4980000000000002", ts("2026-06-28T09:00:00"), 4, 58000L, ts("2026-06-28T09:08:30"), FLAG_NONE));
        cdvelf.put("4980000000000003", new CdvelRecord("4980000000000003", ts("2026-06-28T09:00:00"), 1, 12000L, ts("2026-06-28T08:50:00"), FLAG_NONE));

        java.util.Map<String, CdmerRecord> cdmerf = new java.util.LinkedHashMap<String, CdmerRecord>();
        cdmerf.put("M10001", new CdmerRecord("M10001", "トウキヨウエキマエテン", "5411", "B", "1", "JP"));
        cdmerf.put("M20002", new CdmerRecord("M20002", "シブヤデンキ", "5732", "A", "1", "JP"));
        cdmerf.put("M90001", new CdmerRecord("M90001", "ソウルメンゼイテン", "5947", "C", "1", "KR"));
        cdmerf.put("M90002", new CdmerRecord("M90002", "タイペイリヨコウ", "4722", "C", "1", "TW"));

        java.util.List<CdauthRecord> cdauthf = new java.util.ArrayList<CdauthRecord>();
        cdauthf.add(new CdauthRecord("A000001", "4980000000000001", 12000L, AUTH_APPROVED, "M10001", BASE_CURRENCY, ts("2026-06-28T09:01:10"), date("2026-07-05")));
        cdauthf.add(new CdauthRecord("A000002", "4980000000000001", 18000L, AUTH_APPROVED, "M20002", BASE_CURRENCY, ts("2026-06-28T09:05:40"), date("2026-07-05")));
        cdauthf.add(new CdauthRecord("A000003", "4980000000000002", 9000L, AUTH_APPROVED, "M10001", BASE_CURRENCY, ts("2026-06-28T09:01:00"), date("2026-07-05")));
        cdauthf.add(new CdauthRecord("A000004", "4980000000000002", 9500L, AUTH_APPROVED, "M10001", BASE_CURRENCY, ts("2026-06-28T09:02:20"), date("2026-07-05")));
        cdauthf.add(new CdauthRecord("A000005", "4980000000000002", 9800L, AUTH_APPROVED, "M10001", BASE_CURRENCY, ts("2026-06-28T09:03:10"), date("2026-07-05")));
        cdauthf.add(new CdauthRecord("A000006", "4980000000000003", 21000L, AUTH_APPROVED, "M90001", "KRW", ts("2026-06-28T09:04:00"), date("2026-07-05")));
        cdauthf.add(new CdauthRecord("A000007", "4980000000000003", 33000L, AUTH_APPROVED, "M90002", "TWD", ts("2026-06-28T09:08:00"), date("2026-07-05")));
        cdauthf.add(new CdauthRecord("A000008", "4980000000000003", 15000L, "20", "M90002", "TWD", ts("2026-06-28T09:09:00"), date("2026-07-05")));

        java.time.LocalDateTime onlineTs = ts("2026-06-28T09:10:00");
        java.util.List<OnlineResult> results = detect(cdvelf, cdauthf, cdmerf, onlineTs);

        for (OnlineResult result : results) {
            System.out.println(result.toLine());
        }

        for (CdvelRecord updated : cdvelf.values()) {
            System.out.println(updated.toLine());
        }
    }

    private java.util.List<OnlineResult> detect(java.util.Map<String, CdvelRecord> cdvelf,
                                                java.util.List<CdauthRecord> cdauthf,
                                                java.util.Map<String, CdmerRecord> cdmerf,
                                                java.time.LocalDateTime onlineTs) {
        java.util.Map<String, java.util.List<CdauthRecord>> byCard = new java.util.LinkedHashMap<String, java.util.List<CdauthRecord>>();

        for (CdauthRecord auth : cdauthf) {
            if (!AUTH_APPROVED.equals(auth.authResult)) {
                continue;
            }
            java.util.List<CdauthRecord> list = byCard.get(auth.cardNo);
            if (list == null) {
                list = new java.util.ArrayList<CdauthRecord>();
                byCard.put(auth.cardNo, list);
            }
            list.add(auth);
        }

        java.util.List<OnlineResult> results = new java.util.ArrayList<OnlineResult>();

        for (java.util.Map.Entry<String, java.util.List<CdauthRecord>> entry : byCard.entrySet()) {
            String cardNo = entry.getKey();
            java.util.List<CdauthRecord> approvals = entry.getValue();
            java.util.Collections.sort(approvals, new java.util.Comparator<CdauthRecord>() {
                public int compare(CdauthRecord left, CdauthRecord right) {
                    return left.authTs.compareTo(right.authTs);
                }
            });

            CdvelRecord current = cdvelf.get(cardNo);
            if (current == null) {
                current = new CdvelRecord(cardNo, onlineTs.minusMinutes(10), 0, 0L, null, FLAG_NONE);
                cdvelf.put(cardNo, current);
            }

            int count10m = 0;
            long amount1h = 0L;
            java.time.LocalDateTime latestTs = current.lastAuthTs;
            java.util.Map<String, Integer> merchantCount10m = new java.util.HashMap<String, Integer>();
            int foreignCount10m = 0;

            for (CdauthRecord auth : approvals) {
                long minutes = java.time.Duration.between(auth.authTs, onlineTs).toMinutes();

                if (minutes >= 0 && minutes <= 10) {
                    count10m++;
                    Integer merchantCount = merchantCount10m.get(auth.merchantCode);
                    merchantCount10m.put(auth.merchantCode, merchantCount == null ? 1 : merchantCount + 1);

                    CdmerRecord merchant = cdmerf.get(auth.merchantCode);
                    if (merchant != null && !"JP".equals(merchant.countryCd)) {
                        foreignCount10m++;
                    }
                }

                if (minutes >= 0 && minutes <= 60) {
                    amount1h += auth.authAmt;
                }

                if (latestTs == null || auth.authTs.isAfter(latestTs)) {
                    latestTs = auth.authTs;
                }
            }

            String flag = FLAG_NONE;
            String ruleId = "V000";
            String level = LEVEL_NONE;

            if (count10m >= 6) {
                flag = FLAG_SHORT_BURST;
                ruleId = "V101";
                level = LEVEL_CRITICAL;
            } else if (hasRepeatedMerchant(merchantCount10m, 3)) {
                flag = FLAG_MERCHANT_REPEAT;
                ruleId = "V201";
                level = LEVEL_WARNING;
            } else if (foreignCount10m >= 2) {
                flag = FLAG_FOREIGN_REPEAT;
                ruleId = "V301";
                level = LEVEL_WARNING;
            } else if (count10m >= 4 || amount1h >= 100000L) {
                flag = FLAG_SHORT_BURST;
                ruleId = "V102";
                level = LEVEL_NOTICE;
            }

            CdvelRecord updated = new CdvelRecord(cardNo, onlineTs.minusMinutes(10), count10m, amount1h, latestTs, flag);
            cdvelf.put(cardNo, updated);
            results.add(new OnlineResult(cardNo, level, ruleId, flag));
        }

        return results;
    }

    private boolean hasRepeatedMerchant(java.util.Map<String, Integer> merchantCount, int threshold) {
        for (Integer count : merchantCount.values()) {
            if (count != null && count >= threshold) {
                return true;
            }
        }
        return false;
    }

    private static java.time.LocalDateTime ts(String value) {
        return java.time.LocalDateTime.parse(value);
    }

    private static java.time.LocalDate date(String value) {
        return java.time.LocalDate.parse(value);
    }

    private static final class CdvelRecord {
        private final String cardNo;
        private final java.time.LocalDateTime windowStartTs;
        private final int authCount10m;
        private final long authAmt1h;
        private final java.time.LocalDateTime lastAuthTs;
        private final String velocityFlag;

        private CdvelRecord(String cardNo,
                            java.time.LocalDateTime windowStartTs,
                            int authCount10m,
                            long authAmt1h,
                            java.time.LocalDateTime lastAuthTs,
                            String velocityFlag) {
            this.cardNo = cardNo;
            this.windowStartTs = windowStartTs;
            this.authCount10m = authCount10m;
            this.authAmt1h = authAmt1h;
            this.lastAuthTs = lastAuthTs;
            this.velocityFlag = velocityFlag;
        }

        private String toLine() {
            return "CDVELF 書込 CARD-NO=" + cardNo
                    + " WINDOW-START-TS=" + windowStartTs
                    + " AUTH-COUNT-10M=" + authCount10m
                    + " AUTH-AMT-1H=" + authAmt1h
                    + " LAST-AUTH-TS=" + lastAuthTs
                    + " VELOCITY-FLAG=" + velocityFlag;
        }
    }

    private static final class CdauthRecord {
        private final String authId;
        private final String cardNo;
        private final long authAmt;
        private final String authResult;
        private final String merchantCode;
        private final String currencyCd;
        private final java.time.LocalDateTime authTs;
        private final java.time.LocalDate holdExpDt;

        private CdauthRecord(String authId,
                             String cardNo,
                             long authAmt,
                             String authResult,
                             String merchantCode,
                             String currencyCd,
                             java.time.LocalDateTime authTs,
                             java.time.LocalDate holdExpDt) {
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

    private static final class CdmerRecord {
        private final String merchantCode;
        private final String merchantNameKana;
        private final String mcc;
        private final String riskRank;
        private final String status;
        private final String countryCd;

        private CdmerRecord(String merchantCode,
                            String merchantNameKana,
                            String mcc,
                            String riskRank,
                            String status,
                            String countryCd) {
            this.merchantCode = merchantCode;
            this.merchantNameKana = merchantNameKana;
            this.mcc = mcc;
            this.riskRank = riskRank;
            this.status = status;
            this.countryCd = countryCd;
        }
    }

    private static final class OnlineResult {
        private final String cardNo;
        private final String warningLevel;
        private final String ruleId;
        private final String velocityFlag;

        private OnlineResult(String cardNo, String warningLevel, String ruleId, String velocityFlag) {
            this.cardNo = cardNo;
            this.warningLevel = warningLevel;
            this.ruleId = ruleId;
            this.velocityFlag = velocityFlag;
        }

        private String toLine() {
            return "オンライン応答 CARD-NO=" + cardNo
                    + " 警告レベル=" + warningLevel
                    + " ルールID=" + ruleId
                    + " 速度フラグ=" + velocityFlag;
        }
    }
}
