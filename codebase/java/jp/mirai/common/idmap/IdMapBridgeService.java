package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2025/06/29  共通基盤部  IdMapModel連携サービスの初版作成
 */
public class IdMapBridgeService {
    private static final String STATUS_LINKED = "1";
    private static final String STATUS_UNLINKED = "0";
    private static final String STATUS_CONFLICT = "9";
    private static final Class<?> PINNED_MODEL_TYPE = IdMapModel.class;

    private IdMapBridgeService() {
    }

    public static void main(String[] a) {
        CxidmfRecord[] cxidmf = loadCxidmf();
        BridgeResult result = verifyAndNormalize(cxidmf);

        for (int i = 0; i < cxidmf.length; i++) {
            writeCxidmf(cxidmf[i]);
        }

        System.out.println("処理件数=" + result.totalCount
                + ", 正常リンク=" + result.linkedCount
                + ", 未リンク=" + result.unlinkedCount
                + ", 不整合=" + result.conflictCount
                + ", 共有型=" + PINNED_MODEL_TYPE.getSimpleName());
    }

    private static BridgeResult verifyAndNormalize(CxidmfRecord[] rows) {
        int linked = 0;
        int unlinked = 0;
        int conflict = 0;

        for (int i = 0; i < rows.length; i++) {
            CxidmfRecord current = rows[i];
            String expectedKey = buildPersistentKey(current.companyCode, current.localTxnNo, current.customerAliasId);
            boolean keyMatched = expectedKey.equals(current.idmapKey);
            boolean duplicated = hasDuplicateBusinessKey(rows, i);

            if (!isUsable(current.companyCode) || !isUsable(current.localTxnNo) || !isUsable(current.customerAliasId)) {
                current.linkStatusKbn = STATUS_UNLINKED;
                unlinked++;
            } else if (!keyMatched || duplicated) {
                current.linkStatusKbn = STATUS_CONFLICT;
                conflict++;
            } else {
                current.linkStatusKbn = STATUS_LINKED;
                linked++;
            }
        }

        return new BridgeResult(rows.length, linked, unlinked, conflict);
    }

    private static boolean hasDuplicateBusinessKey(CxidmfRecord[] rows, int targetIndex) {
        CxidmfRecord target = rows[targetIndex];
        for (int i = 0; i < rows.length; i++) {
            if (i == targetIndex) {
                continue;
            }
            CxidmfRecord other = rows[i];
            if (same(target.companyCode, other.companyCode)
                    && same(target.localTxnNo, other.localTxnNo)
                    && same(target.customerAliasId, other.customerAliasId)
                    && !same(target.idmapKey, other.idmapKey)) {
                return true;
            }
        }
        return false;
    }

    private static String buildPersistentKey(String companyCode, String localTxnNo, String customerAliasId) {
        return normalize(companyCode) + "-" + normalize(localTxnNo) + "-" + normalize(customerAliasId);
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim();
    }

    private static boolean isUsable(String value) {
        return normalize(value).length() > 0;
    }

    private static boolean same(String left, String right) {
        return normalize(left).equals(normalize(right));
    }

    private static CxidmfRecord[] loadCxidmf() {
        return new CxidmfRecord[]{
                new CxidmfRecord("CM-10000001-A000001", "CM", "10000001", "A000001", " "),
                new CxidmfRecord("CM-10000002-A000002", "CM", "10000002", "A000002", " "),
                new CxidmfRecord("CM-10000003-A000003", "CM", "10000003", "A000003", " "),
                new CxidmfRecord("BK-20000001-B000001", "BK", "20000001", "B000001", " "),
                new CxidmfRecord("BK-20000002-B000002", "BK", "20000002", "B000002", " "),
                new CxidmfRecord("SC-30000001-C000001", "SC", "30000001", "C000001", " "),
                new CxidmfRecord("SC-30000002-C000002", "SC", "30000002", "C000002", " "),
                new CxidmfRecord("CM-10000008-A000008", "CM", "10000008", "", " "),
                new CxidmfRecord("BK-20000009-B000009", "BK", "20000009", "B000009", " "),
                new CxidmfRecord("BK-別キー-B000009", "BK", "20000009", "B000009", " ")
        };
    }

    private static void writeCxidmf(CxidmfRecord row) {
        System.out.println("CXIDMF更新 IDMAP-KEY=" + row.idmapKey
                + ", 会社コード=" + row.companyCode
                + ", LOCAL-TXN-NO=" + row.localTxnNo
                + ", 顧客別名ID=" + row.customerAliasId
                + ", LINK-STATUS-KBN=" + row.linkStatusKbn);
    }

    private static final class CxidmfRecord {
        private final String idmapKey;
        private final String companyCode;
        private final String localTxnNo;
        private final String customerAliasId;
        private String linkStatusKbn;

        private CxidmfRecord(String idmapKey, String companyCode, String localTxnNo,
                             String customerAliasId, String linkStatusKbn) {
            this.idmapKey = idmapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
        }
    }

    private static final class BridgeResult {
        private final int totalCount;
        private final int linkedCount;
        private final int unlinkedCount;
        private final int conflictCount;

        private BridgeResult(int totalCount, int linkedCount, int unlinkedCount, int conflictCount) {
            this.totalCount = totalCount;
            this.linkedCount = linkedCount;
            this.unlinkedCount = unlinkedCount;
            this.conflictCount = conflictCount;
        }
    }
}
