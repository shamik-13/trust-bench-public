package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2024/07/30  みらいペイ システム部 加盟店・手数料チーム  加盟店審査サービス初版
 */
public class MerchantApplicationScreeningService {
    private static final String STATUS_UNSCREENED = "00";
    private static final String STATUS_APPROVED = "10";
    private static final String STATUS_REVIEW = "20";
    private static final String STATUS_REJECTED = "90";

    private static final String MERCHANT_ACTIVE = "01";
    private static final String MERCHANT_STOPPED = "02";
    private static final String MERCHANT_CANCELLED = "09";

    private static final String CATEGORY_GENERAL = "C1";
    private static final String CATEGORY_RESTAURANT = "C2";
    private static final String CATEGORY_PUBLIC = "C3";
    private static final String CATEGORY_EC = "C4";
    private static final String CATEGORY_HIGH_RISK = "C5";

    private MerchantApplicationScreeningService() {
    }

    public static void main(String[] a) {
        VsamStore store = VsamStore.synthetic();
        ScreeningBatch batch = new ScreeningBatch(store);
        batch.execute();
        store.printResult();
    }

    private static final class ScreeningBatch {
        private final VsamStore store;

        private ScreeningBatch(VsamStore store) {
            this.store = store;
        }

        private void execute() {
            for (Pmaplf application : store.applications) {
                if (!STATUS_UNSCREENED.equals(application.screeningStatus)) {
                    continue;
                }

                ScreeningDecision decision = screen(application);
                application.riskScore = decision.riskScore;
                application.screeningStatus = decision.status;
                application.reviewerId = decision.reviewerId;

                if (STATUS_APPROVED.equals(decision.status)) {
                    Pmerf existing = store.findMerchant(application.merchantCode);
                    if (existing == null) {
                        Pmerf provisional = new Pmerf(
                                application.merchantCode,
                                decision.merchantName,
                                decision.categoryCode,
                                MERCHANT_ACTIVE
                        );
                        store.merchants.add(provisional);
                    }
                }
            }
        }

        private ScreeningDecision screen(Pmaplf application) {
            int score = 0;
            String reviewer = "AUTO01";

            if (isBlank(application.applicationId) || isBlank(application.merchantCode) || isBlank(application.applyDt)) {
                return new ScreeningDecision(STATUS_REJECTED, 100, "SYSERR", CATEGORY_HIGH_RISK, "入力不備");
            }

            String categoryCode = decideCategory(application);
            Pmcatf category = store.findCategory(categoryCode);
            if (category == null || !"1".equals(category.activeFlag)) {
                return new ScreeningDecision(STATUS_REJECTED, 95, "CATNG", categoryCode, "業種未登録");
            }

            score += categoryRiskPoint(category.riskRank);
            score += applyDateRisk(application.applyDt);

            MerchantHistory history = store.collectHistory(application.merchantCode);
            if (history.activeCount > 0) {
                score += 35;
            }
            if (history.stoppedCount > 0) {
                score += 45;
                reviewer = "RISK01";
            }
            if (history.cancelledCount > 0) {
                score += 25;
            }
            if (CATEGORY_HIGH_RISK.equals(category.categoryCode)) {
                score += 30;
                reviewer = "RISK02";
            }
            if (CATEGORY_EC.equals(category.categoryCode)) {
                score += 12;
            }

            if (score >= 85) {
                return new ScreeningDecision(STATUS_REJECTED, Math.min(score, 100), reviewer, category.categoryCode, "要否決");
            }
            if (score >= 55) {
                return new ScreeningDecision(STATUS_REVIEW, score, reviewer, category.categoryCode, "有人確認");
            }
            return new ScreeningDecision(STATUS_APPROVED, score, reviewer, category.categoryCode,
                    merchantNameOf(application.merchantCode));
        }

        private String decideCategory(Pmaplf application) {
            int n = numericTail(application.merchantCode);
            if (n % 17 == 0) {
                return CATEGORY_HIGH_RISK;
            }
            if (n % 11 == 0) {
                return CATEGORY_PUBLIC;
            }
            if (n % 7 == 0) {
                return CATEGORY_EC;
            }
            if (n % 3 == 0) {
                return CATEGORY_RESTAURANT;
            }
            return CATEGORY_GENERAL;
        }

        private int categoryRiskPoint(String riskRank) {
            if ("A".equals(riskRank)) {
                return 5;
            }
            if ("B".equals(riskRank)) {
                return 18;
            }
            if ("C".equals(riskRank)) {
                return 35;
            }
            return 60;
        }

        private int applyDateRisk(String yyyymmdd) {
            if (yyyymmdd.length() != 8) {
                return 20;
            }
            int day;
            try {
                day = Integer.parseInt(yyyymmdd.substring(6, 8));
            } catch (NumberFormatException e) {
                return 20;
            }
            return day >= 28 ? 8 : 0;
        }

        private String merchantNameOf(String merchantCode) {
            return "仮加盟店" + merchantCode;
        }

        private int numericTail(String value) {
            int result = 0;
            for (int i = 0; i < value.length(); i++) {
                char c = value.charAt(i);
                if (c >= '0' && c <= '9') {
                    result = (result * 10 + (c - '0')) % 100000;
                }
            }
            return result;
        }
    }

    private static final class VsamStore {
        private final java.util.List<Pmaplf> applications = new java.util.ArrayList<Pmaplf>();
        private final java.util.List<Pmerf> merchants = new java.util.ArrayList<Pmerf>();
        private final java.util.List<Pmcatf> categories = new java.util.ArrayList<Pmcatf>();

        private static VsamStore synthetic() {
            VsamStore store = new VsamStore();

            store.categories.add(new Pmcatf(CATEGORY_GENERAL, "一般物販", "A", "1", "1", "20260401"));
            store.categories.add(new Pmcatf(CATEGORY_RESTAURANT, "飲食", "B", "1", "1", "20260401"));
            store.categories.add(new Pmcatf(CATEGORY_PUBLIC, "公共・公金", "A", "0", "1", "20260401"));
            store.categories.add(new Pmcatf(CATEGORY_EC, "EC・通信販売", "B", "1", "1", "20260401"));
            store.categories.add(new Pmcatf(CATEGORY_HIGH_RISK, "高リスク業種", "D", "1", "1", "20260401"));

            store.merchants.add(new Pmerf("M0001001", "東京中央商店", CATEGORY_GENERAL, MERCHANT_ACTIVE));
            store.merchants.add(new Pmerf("M0001014", "北浜通販", CATEGORY_EC, MERCHANT_STOPPED));
            store.merchants.add(new Pmerf("M0001022", "旧銀座食堂", CATEGORY_RESTAURANT, MERCHANT_CANCELLED));
            store.merchants.add(new Pmerf("M0001034", "新宿娯楽企画", CATEGORY_HIGH_RISK, MERCHANT_STOPPED));

            store.applications.add(new Pmaplf("A202606280001", "M0001002", "20260628", STATUS_UNSCREENED, 0, ""));
            store.applications.add(new Pmaplf("A202606280002", "M0001014", "20260628", STATUS_UNSCREENED, 0, ""));
            store.applications.add(new Pmaplf("A202606280003", "M0001022", "20260627", STATUS_UNSCREENED, 0, ""));
            store.applications.add(new Pmaplf("A202606280004", "M0001034", "20260628", STATUS_UNSCREENED, 0, ""));
            store.applications.add(new Pmaplf("A202606280005", "M0001042", "20260625", STATUS_UNSCREENED, 0, ""));

            return store;
        }

        private Pmerf findMerchant(String merchantCode) {
            for (Pmerf merchant : merchants) {
                if (merchant.merchantCode.equals(merchantCode)) {
                    return merchant;
                }
            }
            return null;
        }

        private Pmcatf findCategory(String categoryCode) {
            for (Pmcatf category : categories) {
                if (category.categoryCode.equals(categoryCode)) {
                    return category;
                }
            }
            return null;
        }

        private MerchantHistory collectHistory(String merchantCode) {
            MerchantHistory history = new MerchantHistory();
            for (Pmerf merchant : merchants) {
                if (!merchant.merchantCode.equals(merchantCode)) {
                    continue;
                }
                if (MERCHANT_ACTIVE.equals(merchant.merStatus)) {
                    history.activeCount++;
                } else if (MERCHANT_STOPPED.equals(merchant.merStatus)) {
                    history.stoppedCount++;
                } else if (MERCHANT_CANCELLED.equals(merchant.merStatus)) {
                    history.cancelledCount++;
                }
            }
            return history;
        }

        private void printResult() {
            for (Pmaplf application : applications) {
                System.out.println(application.applicationId + "," + application.merchantCode + ","
                        + application.applyDt + "," + application.screeningStatus + ","
                        + application.riskScore + "," + application.reviewerId);
            }
        }
    }

    private static final class MerchantHistory {
        private int activeCount;
        private int stoppedCount;
        private int cancelledCount;
    }

    private static final class ScreeningDecision {
        private final String status;
        private final int riskScore;
        private final String reviewerId;
        private final String categoryCode;
        private final String merchantName;

        private ScreeningDecision(String status, int riskScore, String reviewerId, String categoryCode, String merchantName) {
            this.status = status;
            this.riskScore = riskScore;
            this.reviewerId = reviewerId;
            this.categoryCode = categoryCode;
            this.merchantName = merchantName;
        }
    }

    private static final class Pmaplf {
        private final String applicationId;
        private final String merchantCode;
        private final String applyDt;
        private String screeningStatus;
        private int riskScore;
        private String reviewerId;

        private Pmaplf(String applicationId, String merchantCode, String applyDt,
                       String screeningStatus, int riskScore, String reviewerId) {
            this.applicationId = applicationId;
            this.merchantCode = merchantCode;
            this.applyDt = applyDt;
            this.screeningStatus = screeningStatus;
            this.riskScore = riskScore;
            this.reviewerId = reviewerId;
        }
    }

    private static final class Pmerf {
        private final String merchantCode;
        private final String merchantName;
        private final String merCategory;
        private final String merStatus;

        private Pmerf(String merchantCode, String merchantName, String merCategory, String merStatus) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merCategory = merCategory;
            this.merStatus = merStatus;
        }
    }

    private static final class Pmcatf {
        private final String categoryCode;
        private final String categoryName;
        private final String riskRank;
        private final String taxableFlag;
        private final String activeFlag;
        private final String lastUpdateDt;

        private Pmcatf(String categoryCode, String categoryName, String riskRank,
                       String taxableFlag, String activeFlag, String lastUpdateDt) {
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDt = lastUpdateDt;
        }
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }
}
