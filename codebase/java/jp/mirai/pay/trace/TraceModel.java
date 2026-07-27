package jp.mirai.pay.trace;

/* ================================================================
 * TraceModel.java -- みらいペイ 与信→精算 追跡 ドメインモデル (shared/pinned)
 * 変更履歴:
 *   1.0  20240412  ペイ精算基盤  新規 (ホールド・売上確定リンクの共通型)
 * ================================================================ */

/** 与信→精算 追跡ドメインの共通型。連携のキー導出/精算区分の付与はサービス側に実装される。 */
public final class TraceModel {
    private TraceModel() {}

    /** オーソリ・ホールド。status は HD-HOLD-STATUS。 */
    public record Hold(String holdId, String walletId, String merchantCode, long amount, String status) {}

    /** 売上確定リンク。settleTxnId は精算側との連携キー、settleKbn は精算区分。 */
    public record CaptureLink(String capId, String holdId, String settleTxnId,
                              String merchantCode, String settleKbn, long amount) {}
}
