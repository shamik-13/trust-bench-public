package jp.mirai.pay.fee;

/* ================================================================
 * FeeModel.java -- みらいペイ 加盟店手数料 ドメインモデル (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20240220  ペイ加盟店基盤  新規 (加盟店・決済明細・手数料の共通型)
 *   1.1  20250410  ペイ加盟店基盤  MDR 手数料算定サービス向けに型を共有
 * ================================================================ */

/** 加盟店手数料ドメインの共通型。レート表は規程に基づき各サービス側に実装される。 */
public final class FeeModel {
    private FeeModel() {}

    /** 加盟店。category は MR-MER-CATEGORY (業種区分)、status は MR-MER-STATUS。 */
    public record Merchant(String merchantCode, String category, String status) {}

    /** 決済明細。amount は TX-TXN-AMT (円, integer minor units)。 */
    public record Txn(String txnId, String merchantCode, long amount, int txnDt) {}
}
