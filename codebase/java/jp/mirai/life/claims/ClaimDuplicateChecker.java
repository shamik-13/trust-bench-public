package jp.mirai.life.claims;

import java.util.List;

/**
 * 変更履歴
 * 版数  年月日      担当            概要
 * 1.0   20240318    保険金システムG  初版 - 請求重複チェッカー実装
 *
 * 同一証券番号で支払事由発生日が近接する請求を検出する重複チェッカー。
 * 支払割合・支払削減割合には一切関与しない（割合は支払エンジン/約款の所管）。
 */
public class ClaimDuplicateChecker {

    private static final String CLAIM_STATUS_PAYABLE = "01";      // 支払対象
    private static final String CLAIM_STATUS_EVALUATING = "05";   // 査定中
    private static final String CLAIM_STATUS_DENIED = "09";       // 否認

    private static final int EVENT_DATE_WINDOW_DAYS = 90;

    /**
     * 請求重複をチェック。
     * 証券番号一致かつ支払事由発生日が±90日以内の請求を検索し、
     * 最初にマッチした請求IDと状態を返す。日付は YYYYMMDD 整数。
     */
    public static DuplicateCheckResult checkDuplicate(
            String candidatePolNo,
            int candidateEventDt,
            List<ClaimModel.Claim> lfclmfRecords) {

        if (candidatePolNo == null || lfclmfRecords == null) {
            return null;
        }

        for (ClaimModel.Claim record : lfclmfRecords) {
            if (!candidatePolNo.equals(record.polNo())) {
                continue;
            }

            long daysDiff = Math.abs(daysBetween(candidateEventDt, record.eventDt()));
            if (daysDiff > EVENT_DATE_WINDOW_DAYS) {
                continue;
            }

            return new DuplicateCheckResult(record.claimId(), record.status());
        }

        return null;
    }

    /** YYYYMMDD 整数の概算経過日数（30日/月、365日/年の概算）。 */
    private static long daysBetween(int yyyymmddA, int yyyymmddB) {
        return ordinal(yyyymmddA) - ordinal(yyyymmddB);
    }

    private static long ordinal(int yyyymmdd) {
        int year = yyyymmdd / 10000;
        int month = yyyymmdd / 100 % 100;
        int day = yyyymmdd % 100;
        return (long) year * 365 + (long) month * 30 + day;
    }

    /** 請求重複チェック結果。 */
    public static class DuplicateCheckResult {
        private final String claimId;
        private final String claimStatusKbn;

        public DuplicateCheckResult(String claimId, String claimStatusKbn) {
            this.claimId = claimId;
            this.claimStatusKbn = claimStatusKbn;
        }

        public String getClaimId() {
            return claimId;
        }

        public String getClaimStatusKbn() {
            return claimStatusKbn;
        }

        public boolean isPayable() {
            return CLAIM_STATUS_PAYABLE.equals(claimStatusKbn);
        }

        public boolean isEvaluating() {
            return CLAIM_STATUS_EVALUATING.equals(claimStatusKbn);
        }

        public boolean isDenied() {
            return CLAIM_STATUS_DENIED.equals(claimStatusKbn);
        }
    }
}
