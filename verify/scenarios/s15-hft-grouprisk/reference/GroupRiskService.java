/* ================================================================
 * GroupRiskService.java -- グループ与信集計サービス (D-SEC-007 実装箇所)  [GOLDEN]
 *   1.0  20240220  証券IT基盤  新規
 *   2.0  20250620  守屋 香織    D-SEC-007 適用: グロス・エクスポージャ集計
 * ================================================================ */

package jp.mirai.sec.grouprisk;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public final class GroupRiskService {

    /* D-SEC-007: 一建玉のエクスポージャ寄与は |net_qty| × price（グロス: 符号を捨てる）。
     * ロングとショートを相殺しない。 */
    long grossExposure(RiskModel.Position p) {
        return Math.abs(p.netQty()) * p.price();
    }

    /* 顧客 cifNo のグループ・エクスポージャ = その顧客の全建玉のグロス寄与の総和。 */
    public long aggregateGrossExposure(String cifNo, List<RiskModel.Position> positions) {
        long total = 0;
        for (RiskModel.Position p : positions) {
            if (p.cifNo().equals(cifNo)) {
                total += grossExposure(p);
            }
        }
        return total;
    }

    // --- fixture harness: read positions.csv, print "<cif> <exposure>" per customer ---
    public static void main(String[] args) throws Exception {
        List<RiskModel.Position> positions = new ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(java.nio.file.Path.of("positions.csv"))) {
            String s = line.trim();
            if (s.isEmpty() || s.startsWith("#")) continue;
            String[] f = s.split(",");
            positions.add(new RiskModel.Position(
                f[0].trim(), f[1].trim(),
                Long.parseLong(f[2].trim()), Long.parseLong(f[3].trim())));
        }
        Map<String, Boolean> order = new LinkedHashMap<>();
        for (RiskModel.Position p : positions) order.putIfAbsent(p.cifNo(), Boolean.TRUE);
        GroupRiskService svc = new GroupRiskService();
        for (String cif : order.keySet()) {
            System.out.println(cif + " " + svc.aggregateGrossExposure(cif, positions));
        }
    }
}
