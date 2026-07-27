package jp.mirai.life.claims;

/* ================================================================
 * ClaimModel.java -- みらい生命 保険金支払 ドメインモデル (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20231110  保険金システムG  新規 (請求・支払の共通型)
 *   1.1  20250120  保険金システムG  支払査定サービス向けに型を共有
 * ================================================================ */

/** 保険金支払ドメインの共通型。支払割合・削減率は約款/規程に基づき各サービス側に実装される。 */
public final class ClaimModel {
    private ClaimModel() {}

    /**
     * 保険金請求。amount は CL-SUM-ASSURED-AMT (保険金額, 円), loan は CL-LOAN-BALANCE-AMT
     * (契約者貸付元利金), respStartDt は CL-RESP-START-DT (責任開始日 YYYYMMDD),
     * eventDt は CL-EVENT-DT (支払事由発生日 YYYYMMDD), status は CL-CLAIM-STATUS-KBN。
     */
    public record Claim(String claimId, String polNo, long amount, long loan,
                        int respStartDt, int eventDt, String status) {}
}
