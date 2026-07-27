/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20220222  福田 亮太 (E-211)  注文管理サービス(コントロールプレーン薄層)初版作成
 */

package jp.mirai.sec.matching;


import java.util.ArrayList;
import java.util.List;

/** 約定連携を担うコントロールプレーンの薄いファサード。mihft_match を呼び出して注文ライフサイクルを
 *  仲介するのみで、約定・リスク等の業務判定そのものは保持しない(判定は各エンジン本体に従う)。 */
public class OmsService {
    private final String inDataset;   // SCORDF
    private final String outDataset;  // SCEXEC
    private final List<String> accepted = new ArrayList<>();

    public OmsService(String inDataset, String outDataset) {
        this.inDataset = inDataset;
        this.outDataset = outDataset;
    }

    /** 受領した注文を正規化し、mihft_match へ引き渡す(業務判定は委譲)。 */
    public void route(String orderId) {
        if (orderId == null || orderId.isEmpty()) {
            throw new IllegalArgumentException("ORDER-ID 未設定");
        }
        accepted.add(orderId);
    }

    public int acceptedCount() {
        return accepted.size();
    }

    public String inDataset()  { return inDataset; }
    public String outDataset() { return outDataset; }
}
