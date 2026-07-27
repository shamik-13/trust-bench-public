/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  西村 亮 (E-204)  注文管理サービス(コントロールプレーン薄層)初版作成
 */

package jp.mirai.sec.pretrade;


import java.util.ArrayList;
import java.util.List;

/** 発注ゲートウェイ連携を担うコントロールプレーンの薄いファサード。mihft_gateway / mihft_risk を呼び出して注文ライフサイクルを
 *  仲介するのみで、約定・リスク等の業務判定そのものは保持しない(判定は各エンジン本体に従う)。 */
public class OmsService {
    private final String inDataset;   // SCORDF
    private final String outDataset;  // SCCUST
    private final List<String> accepted = new ArrayList<>();

    public OmsService(String inDataset, String outDataset) {
        this.inDataset = inDataset;
        this.outDataset = outDataset;
    }

    /** 受領した注文を正規化し、mihft_gateway / mihft_risk へ引き渡す(業務判定は委譲)。 */
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
