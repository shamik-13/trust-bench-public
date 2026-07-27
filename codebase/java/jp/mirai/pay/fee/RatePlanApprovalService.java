package jp.mirai.pay.fee;

/*
 * 変更履歴
 * 版数    年月日      担当      概要
 * 1.00    2024/12/16  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class RatePlanApprovalService {
    private static final java.time.format.DateTimeFormatter 日付形式 =
            java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd");

    public static void main(String[] a) {
        java.util.List<Pmcatf> カテゴリ表 = java.util.Arrays.asList(
                new Pmcatf("C1", "一般物販", 2, true, true, "20260601"),
                new Pmcatf("C3", "公共・公金", 1, false, true, "20260603"),
                new Pmcatf("C4", "EC・通信販売", 4, true, true, "20260605"),
                new Pmcatf("C9", "旧業種区分", 5, false, false, "20251231")
        );

        java.util.List<Pmratf> pmratf = new java.util.ArrayList<>();
        pmratf.add(new Pmratf("RP-202607-0001", "C1", "20260701", "NT-778201", "承認済", "6ed9b8b1d6fb2e9c"));
        pmratf.add(new Pmratf("RP-202607-0002", "C3", "20260701", "NT-778202", "申請中", ""));

        java.util.List<ApprovalRequest> 申請一覧 = java.util.Arrays.asList(
                new ApprovalRequest("RP-202607-0101", "C1", "20260701", "NT-778301"),
                new ApprovalRequest("RP-202607-0102", "C3", "20260701", "NT-778302"),
                new ApprovalRequest("RP-202607-0103", "C4", "20260801", "NT-778303"),
                new ApprovalRequest("RP-202607-0104", "C9", "20260801", "NT-778304"),
                new ApprovalRequest("RP-202607-0105", "ZZZ999", "20260801", "NT-778305")
        );

        FeeScheduleLoader 規程ローダ = new FeeScheduleLoader();
        ApprovalEngine engine = new ApprovalEngine(カテゴリ表, pmratf, 規程ローダ);

        for (ApprovalRequest 申請 : 申請一覧) {
            ApprovalResult 結果 = engine.approve(申請);
            System.out.println(結果.message);
        }

        System.out.println("処理結果：ＰＭＲＡＴＦ件数＝" + pmratf.size());
        for (Pmratf r : pmratf) {
            System.out.println(r.ratePlanId + "," + r.categoryCode + "," + r.effectiveDt + ","
                    + r.noticeId + "," + r.approvalStatus + "," + r.ruleHash);
        }
    }

    private static final class ApprovalEngine {
        private final java.util.Map<String, Pmcatf> categoryByCode;
        private final java.util.List<Pmratf> ratePlanTable;
        private final FeeScheduleLoader feeScheduleLoader;

        ApprovalEngine(java.util.List<Pmcatf> categories, java.util.List<Pmratf> ratePlanTable,
                       FeeScheduleLoader feeScheduleLoader) {
            this.categoryByCode = new java.util.HashMap<>();
            for (Pmcatf category : categories) {
                categoryByCode.put(category.categoryCode, category);
            }
            this.ratePlanTable = ratePlanTable;
            this.feeScheduleLoader = feeScheduleLoader;
        }

        ApprovalResult approve(ApprovalRequest request) {
            java.util.List<String> errors = validateBasic(request);
            Pmcatf category = categoryByCode.get(request.categoryCode);

            if (category == null) {
                errors.add("カテゴリ未登録");
            } else {
                if (!category.activeFlag) {
                    errors.add("カテゴリ停止中");
                }
                if (category.riskRank >= 4 && !request.noticeId.startsWith("NT-")) {
                    errors.add("高リスクカテゴリ通知番号不正");
                }
            }

            if (hasApprovedDuplicate(request.categoryCode, request.effectiveDt)) {
                errors.add("同一カテゴリ同一有効日の承認済レートあり");
            }

            FeeRule rule = feeScheduleLoader.load(request.categoryCode, request.effectiveDt);
            if (rule == null) {
                errors.add("規程側レート未登録");
            } else if (category != null && rule.taxableFlag != category.taxableFlag) {
                errors.add("課税区分不一致");
            }

            if (!errors.isEmpty()) {
                return new ApprovalResult(false, "否認：" + request.ratePlanId + "：" + String.join("／", errors));
            }

            String hash = shortHash(rule.materialForHash());
            ratePlanTable.add(new Pmratf(request.ratePlanId, request.categoryCode, request.effectiveDt,
                    request.noticeId, "承認済", hash));

            return new ApprovalResult(true, "承認：" + request.ratePlanId + "：規程ハッシュ＝" + hash);
        }

        private java.util.List<String> validateBasic(ApprovalRequest request) {
            java.util.List<String> errors = new java.util.ArrayList<>();
            if (isBlank(request.ratePlanId)) {
                errors.add("レート計画ＩＤ未設定");
            }
            if (isBlank(request.categoryCode)) {
                errors.add("カテゴリコード未設定");
            }
            if (isBlank(request.effectiveDt)) {
                errors.add("有効日未設定");
            } else {
                try {
                    java.time.LocalDate effectiveDate = java.time.LocalDate.parse(request.effectiveDt, 日付形式);
                    java.time.LocalDate lowerLimit = java.time.LocalDate.of(2026, 7, 1);
                    if (effectiveDate.isBefore(lowerLimit)) {
                        errors.add("有効日が受付下限日以前");
                    }
                } catch (java.time.format.DateTimeParseException ex) {
                    errors.add("有効日形式不正");
                }
            }
            if (isBlank(request.noticeId)) {
                errors.add("通知番号未設定");
            }
            return errors;
        }

        private boolean hasApprovedDuplicate(String categoryCode, String effectiveDt) {
            for (Pmratf row : ratePlanTable) {
                if (row.categoryCode.equals(categoryCode)
                        && row.effectiveDt.equals(effectiveDt)
                        && "承認済".equals(row.approvalStatus)) {
                    return true;
                }
            }
            return false;
        }
    }

    private static final class FeeScheduleLoader {
        private final java.util.Map<String, FeeRule> rules = new java.util.HashMap<>();

        FeeScheduleLoader() {
            add(new FeeRule("C1", "20260701", "FEE-C1-2026H2", true, 0, 0, "業種区分別MDR"));
            add(new FeeRule("C3", "20260701", "FEE-C3-2026H2", false, 0, 0, "業種区分別MDR"));
            add(new FeeRule("C4", "20260801", "FEE-C4-2026H2", true, 0, 0, "業種区分別MDR"));
        }

        FeeRule load(String categoryCode, String effectiveDt) {
            return rules.get(categoryCode + "|" + effectiveDt);
        }

        private void add(FeeRule rule) {
            rules.put(rule.categoryCode + "|" + rule.effectiveDt, rule);
        }
    }

    private static String shortHash(String material) {
        try {
            java.security.MessageDigest digest = java.security.MessageDigest.getInstance("SHA-256");
            byte[] bytes = digest.digest(material.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 8; i++) {
                sb.append(String.format("%02x", bytes[i]));
            }
            return sb.toString();
        } catch (java.security.NoSuchAlgorithmException ex) {
            throw new IllegalStateException("ハッシュ方式利用不可", ex);
        }
    }

    private static boolean isBlank(String v) {
        return v == null || v.trim().isEmpty();
    }

    private static final class ApprovalRequest {
        final String ratePlanId;
        final String categoryCode;
        final String effectiveDt;
        final String noticeId;

        ApprovalRequest(String ratePlanId, String categoryCode, String effectiveDt, String noticeId) {
            this.ratePlanId = ratePlanId;
            this.categoryCode = categoryCode;
            this.effectiveDt = effectiveDt;
            this.noticeId = noticeId;
        }
    }

    private static final class ApprovalResult {
        final boolean approved;
        final String message;

        ApprovalResult(boolean approved, String message) {
            this.approved = approved;
            this.message = message;
        }
    }

    private static final class Pmratf {
        final String ratePlanId;
        final String categoryCode;
        final String effectiveDt;
        final String noticeId;
        final String approvalStatus;
        final String ruleHash;

        Pmratf(String ratePlanId, String categoryCode, String effectiveDt, String noticeId,
               String approvalStatus, String ruleHash) {
            this.ratePlanId = ratePlanId;
            this.categoryCode = categoryCode;
            this.effectiveDt = effectiveDt;
            this.noticeId = noticeId;
            this.approvalStatus = approvalStatus;
            this.ruleHash = ruleHash;
        }
    }

    private static final class Pmcatf {
        final String categoryCode;
        final String categoryName;
        final int riskRank;
        final boolean taxableFlag;
        final boolean activeFlag;
        final String lastUpdateDt;

        Pmcatf(String categoryCode, String categoryName, int riskRank, boolean taxableFlag,
               boolean activeFlag, String lastUpdateDt) {
            this.categoryCode = categoryCode;
            this.categoryName = categoryName;
            this.riskRank = riskRank;
            this.taxableFlag = taxableFlag;
            this.activeFlag = activeFlag;
            this.lastUpdateDt = lastUpdateDt;
        }
    }

    private static final class FeeRule {
        final String categoryCode;
        final String effectiveDt;
        final String ruleId;
        final boolean taxableFlag;
        final int minimumYen;
        final int maximumYen;
        final String calculationType;

        FeeRule(String categoryCode, String effectiveDt, String ruleId, boolean taxableFlag,
                int minimumYen, int maximumYen, String calculationType) {
            this.categoryCode = categoryCode;
            this.effectiveDt = effectiveDt;
            this.ruleId = ruleId;
            this.taxableFlag = taxableFlag;
            this.minimumYen = minimumYen;
            this.maximumYen = maximumYen;
            this.calculationType = calculationType;
        }

        String materialForHash() {
            return ruleId + "|" + categoryCode + "|" + effectiveDt + "|" + taxableFlag + "|"
                    + minimumYen + "|" + maximumYen + "|" + calculationType;
        }
    }
}
