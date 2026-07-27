package jp.mirai.sec.matching;

/* ================================================================
 * RiskModel.java -- みらい証券 グループ与信 ドメインモデル (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20240220  証券IT基盤  新規 (建玉・エクスポージャ算定の共通型)
 *   1.1  20250620  証券IT基盤  グループ与信集計サービス向けに型を共有
 * ================================================================ */

/** グループ与信ドメインの共通型。集計ロジックは各サービス側に実装される。 */
public final class RiskModel {
    private RiskModel() {}

    /** 一建玉。netQty は符号付き建玉 (買い + / 売り −)、price は評価単価 (×100 minor units)。 */
    public record Position(String cifNo, String instrCode, long netQty, long price) {}
}
