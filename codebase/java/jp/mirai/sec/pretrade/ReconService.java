/*
 * 変更履歴
 * 版数  年月日    担当      概要
 * 1.00  20210715  西村 亮 (E-204)  日次照合サービス(コントロールプレーン薄層)初版作成
 */

package jp.mirai.sec.pretrade;


import java.util.HashMap;
import java.util.Map;

/** リスク判定結果(SCREJT)と発注(SCORDF)を日次で突合する薄いファサード。差分の検知のみを行い、金額・数量の算定規則は
 *  各エンジン本体に従う(本クラスは規則を再実装しない)。 */
public class ReconService {
    private final Map<String, Long> left = new HashMap<>();
    private final Map<String, Long> right = new HashMap<>();

    public void putLeft(String key, long qty)  { left.merge(key, qty, Long::sum); }
    public void putRight(String key, long qty) { right.merge(key, qty, Long::sum); }

    /** 左右の件数差を突合し、不一致キーの一覧を返す。 */
    public java.util.List<String> breaks() {
        java.util.List<String> out = new java.util.ArrayList<>();
        java.util.Set<String> keys = new java.util.HashSet<>(left.keySet());
        keys.addAll(right.keySet());
        for (String k : keys) {
            if (!left.getOrDefault(k, 0L).equals(right.getOrDefault(k, 0L))) {
                out.add(k);
            }
        }
        return out;
    }
}
