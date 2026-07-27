package jp.mirai.pay.authorization;

/* ================================================================
 * PayModel.java -- みらいペイ ウォレット与信 ドメインモデル (shared/pinned, high fan-in)
 * 変更履歴:
 *   1.0  20240310  ペイ基盤  新規 (ウォレット残高・ホールド・未確定決済の共通型)
 *   1.1  20250620  ペイ基盤  オーソリ判定サービス向けに型を共有
 * ================================================================ */

/** ウォレット与信ドメインの共通型。判定ロジックは AuthEngine 側に実装される。 */
public final class PayModel {
    private PayModel() {}

    /** ウォレット。status は WALLET-STATUS、ledgerBal は残高元帳の確定残高 (円, integer minor units)。 */
    public record Wallet(String walletId, String status, long ledgerBal) {}

    /** オーソリ・ホールド。result は HOLD-RESULT、expDt は HOLD-EXP-DT (YYYYMMDD)、ccy は通貨。 */
    public record Hold(String walletId, long amt, String result, int expDt, String ccy) {}

    /** 未確定決済 (清算待ち)。status は PEND-STATUS。 */
    public record Pending(String walletId, long amt, String status) {}

    /** オーソリ要求。 */
    public record Request(String reqId, String walletId, long amt, String ccy) {}
}
