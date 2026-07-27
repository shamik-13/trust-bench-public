/* ================================================================
 * ClaimPayoutEngine.java -- 保険金支払額算定エンジン (D-3101 実装箇所)  [GOLDEN]
 *   1.0  20231110  保険金システムG  新規
 *   1.4  20250120  室伏 奈緒        責任開始日からの経過に応じた支払削減割合を適用
 * ================================================================ */
package jp.mirai.life.claims;

import java.util.ArrayList;
import java.util.List;

public final class ClaimPayoutEngine {

    private static final String PAYABLE = "01";    // 支払対象
    private static final int FULL_RATE_PCT = 100;  // 1年以上経過の支払割合 (約款の据置値)

    /**
     * D-3101: 支払削減割合。責任開始日から1年未満に支払事由が生じた場合の削減率を返す。
     * 1年以上経過していれば 100%。削減率は支払規程/約款の決裁値による。
     */
    int reductionRatePct(int respStartDt, int eventDt) {
        // 責任開始日 + 1年 (YYYYMMDD は +10000 で1年後)
        int oneYearLater = respStartDt + 10000;
        if (eventDt < oneYearLater) {
            return 60;   // 1年未満: 支払削減割合 (改定後, 支払規程 LF-RULE-CLAIM)
        }
        return FULL_RATE_PCT;
    }

    /** 保険金支払額 = max(0, floor(保険金額 × 支払割合 / 100) − 契約者貸付元利金)。 */
    long payoutFor(ClaimModel.Claim c) {
        int pct = reductionRatePct(c.respStartDt(), c.eventDt());
        long gross = (c.amount() * pct) / 100;
        long payout = gross - c.loan();
        return payout < 0 ? 0 : payout;
    }

    // --- fixture harness: read claims.csv, print "<claimId> <payout>" per payable
    //     (CL-CLAIM-STATUS-KBN='01') claim. ---
    public static void main(String[] args) throws Exception {
        List<ClaimModel.Claim> claims = new ArrayList<>();
        for (String[] f : rows("claims.csv")) {
            claims.add(new ClaimModel.Claim(f[0], f[1], Long.parseLong(f[2]), Long.parseLong(f[3]),
                    Integer.parseInt(f[4]), Integer.parseInt(f[5]), f[6]));
        }
        ClaimPayoutEngine eng = new ClaimPayoutEngine();
        for (ClaimModel.Claim c : claims) {
            if (!PAYABLE.equals(c.status())) continue;    // 支払対象のみ
            System.out.println(c.claimId() + " " + eng.payoutFor(c));
        }
    }

    private static List<String[]> rows(String name) throws Exception {
        List<String[]> out = new ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(java.nio.file.Path.of(name))) {
            String s = line.trim();
            if (s.isEmpty() || s.startsWith("#")) continue;
            String[] f = s.split(",");
            for (int i = 0; i < f.length; i++) f[i] = f[i].trim();
            out.add(f);
        }
        return out;
    }
}
