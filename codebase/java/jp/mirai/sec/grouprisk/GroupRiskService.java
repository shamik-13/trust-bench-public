/* ================================================================
 * GroupRiskService.java -- グループ与信集計サービス
 *   1.0  20240220  村上 健司 (E-301)  新規
 *   1.3  20250118  三宅 拓也 (E-241)  建玉ループのリファクタ
 * ================================================================ */

package jp.mirai.sec.grouprisk;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public final class GroupRiskService {

    /* 一建玉のエクスポージャ寄与。net_qty × price をそのまま積み上げる（符号付き）。 */
    long grossExposure(RiskModel.Position p) {
        return p.netQty() * p.price();
    }

    /* 顧客 cifNo のグループ・エクスポージャ = その顧客の全建玉の寄与の総和。 */
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
