package jp.mirai.pay.refund;

/* ================================================================
 * RefundModel.java -- みらいペイ 返金受付 ドメインモデル (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20240305  ペイ返金基盤  新規 (原取引・返金申請・判定結果の共通型)
 *   1.1  20250418  ペイ返金基盤  返金受付判定サービス向けに型を共有
 * ================================================================ */

/** 返金受付ドメインの共通型。受付可能期間は規程に基づき判定サービス側に実装される。 */
public final class RefundModel {
    private RefundModel() {}

    /** 原取引。amount は ORIG-TXN-AMT (円)、txnDt は ORIG-TXN-DT (YYYYMMDD)。 */
    public record OrigTxn(String origTxnId, String walletId, long amount, int txnDt) {}

    /** 返金申請。refundAmt は REFUND-AMT (円)、reqDt は REQ-DT (YYYYMMDD)、reason は REQ-REASON。 */
    public record Request(String reqId, String origTxnId, long refundAmt, int reqDt, String reason) {}
}
