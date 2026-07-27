/* ================================================================
 * CaptureLinkService.java -- 売上確定リンクサービス (与信→精算)
 *   1.0  20240412  ペイ精算基盤  新規 (ホールドの売上確定を精算側へ連携)
 * ================================================================ */
package jp.mirai.pay.trace;

import java.util.ArrayList;
import java.util.List;

/**
 * 与信側の売上確定済ホールドを、精算側が取り込む売上確定リンク(PTCAPF)へ変換する。
 * 連携キー SETTLE-TXN-ID は HOLD-ID から導出し、精算区分 SETTLE-KBN を付与する。
 * (端から端までの連携規則は本サービスと C 精算ネット mipay_nettrace に分かれて存在する。)
 */
public final class CaptureLinkService {

    private static final String CAPTURABLE = "30";   // 連携対象の HD-HOLD-STATUS
    private static final String KBN_IMMEDIATE = "1"; // 即時精算
    private static final String KBN_CARRY = "2";     // 翌月繰越（保留）

    /** 連携キー SETTLE-TXN-ID を HOLD-ID から生成する。 */
    public String settleTxnIdFor(String holdId) {
        StringBuilder digits = new StringBuilder();
        for (int i = 0; i < holdId.length(); i++) {
            char c = holdId.charAt(i);
            if (c >= '0' && c <= '9') digits.append(c);
        }
        return "S" + digits;
    }

    /** ホールドに応じて精算区分(SETTLE-KBN)を付与する。 */
    private String settleKbnFor(TraceModel.Hold h) {
        return (!h.merchantCode().isEmpty() && h.merchantCode().charAt(0) == 'Z')
                ? KBN_CARRY : KBN_IMMEDIATE;
    }

    /** 売上確定済ホールド1件を売上確定リンクへ変換する。連携対象外なら null。 */
    public TraceModel.CaptureLink linkForCapture(TraceModel.Hold h) {
        if (!CAPTURABLE.equals(h.status())) return null;
        String txnId = settleTxnIdFor(h.holdId());
        String capId = "C" + h.holdId();
        return new TraceModel.CaptureLink(capId, h.holdId(), txnId,
                h.merchantCode(), settleKbnFor(h), h.amount());
    }

    /** 売上確定済ホールド群を売上確定リンク群へ変換する（連携対象のみ）。 */
    public List<TraceModel.CaptureLink> buildLinks(List<TraceModel.Hold> holds) {
        List<TraceModel.CaptureLink> out = new ArrayList<>();
        for (TraceModel.Hold h : holds) {
            TraceModel.CaptureLink link = linkForCapture(h);
            if (link != null) out.add(link);
        }
        return out;
    }
}
