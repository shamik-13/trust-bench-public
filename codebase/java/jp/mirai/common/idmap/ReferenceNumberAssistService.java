package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.0   2024-09-05  共通基盤    初版作成
 */
public class ReferenceNumberAssistService {
    private static final String IMPORT_BATCH_ID = "B20240905001";
    private static final String STATUS_UNTAIO = "0";
    private static final String ERR_COMPANY = "E101";
    private static final String ERR_LOCAL_NO = "E102";
    private static final String ERR_IDMAP = "E201";
    private static final String ERR_REF_MISMATCH = "E301";

    public static void main(String[] a) {
        CmtxnfRecord[] cmtxnf = new CmtxnfRecord[] {
                new CmtxnfRecord("TXN000001", "BK", 100000000101L, 250000L, "01"),
                new CmtxnfRecord("TXN000002", "SC", 200000000202L, 180000L, "01"),
                new CmtxnfRecord("TXN000003", "CD", 300000000303L, 12800L, "09"),
                new CmtxnfRecord("TXN000004", "PY", 400000000404L, 980L, "01"),
                new CmtxnfRecord("TXN000005", "LF", 500000000505L, 1200000L, "01"),
                new CmtxnfRecord("TXN000006", "BK", 0L, 42000L, "01"),
                new CmtxnfRecord("TXN000007", "", 700000000707L, 76000L, "01"),
                new CmtxnfRecord("TXN000008", "CM", 800000000808L, 0L, "01"),
                new CmtxnfRecord("TXN000009", "ZZ", 900000000909L, 33000L, "01"),
                new CmtxnfRecord("TXN000010", "SC", 200000000210L, 510000L, "01")
        };

        CxidmfRecord[] cxidmf = new CxidmfRecord[] {
                new CxidmfRecord("BK-100000000101", "BK", 100000000101L, "CUST-BK-0001", "1"),
                new CxidmfRecord("SC-200000000202", "SC", 200000000202L, "CUST-SC-0002", "1"),
                new CxidmfRecord("CD-300000000303", "CD", 300000000303L, "CUST-CD-0003", "0"),
                new CxidmfRecord("PY-400000000404", "PY", 400000000404L, "CUST-PY-0004", "1"),
                new CxidmfRecord("LF-500000000505", "LF", 500000000505L, "CUST-LF-0005", "1"),
                new CxidmfRecord("CM-800000000808", "CM", 800000000808L, "CUST-CM-0008", "1")
        };

        CmerrfWriter writer = new CmerrfWriter(32);
        CxidmfIndex index = new CxidmfIndex(cxidmf);
        GroupRefService groupRefService = new GroupRefService();

        int saibanKouhoCount = 0;
        long kingakuGoukei = 0L;

        for (int i = 0; i < cmtxnf.length; i++) {
            CmtxnfRecord txn = cmtxnf[i];
            boolean valid = true;

            if (!isCompanyCode(txn.companyCode)) {
                writer.write(error(i, txn, ERR_COMPANY));
                valid = false;
            }
            if (txn.localTxnNo <= 0L) {
                writer.write(error(i, txn, ERR_LOCAL_NO));
                valid = false;
            }

            CxidmfRecord link = null;
            if (valid) {
                link = index.find(txn.companyCode, txn.localTxnNo);
                if (link == null || !"1".equals(link.linkStatusKbn)) {
                    writer.write(error(i, txn, ERR_IDMAP));
                    valid = false;
                }
            }

            if (valid && "01".equals(txn.txnStatusKbn)) {
                long groupRef;
                try {
                    groupRef = groupRefService.toGroupRef(txn.companyCode, txn.localTxnNo);
                } catch (RuntimeException ex) {
                    writer.write(error(i, txn, ERR_REF_MISMATCH));
                    continue;
                }
                if (groupRef <= 0L) {
                    writer.write(error(i, txn, ERR_REF_MISMATCH));
                    continue;
                }
                saibanKouhoCount++;
                kingakuGoukei += txn.txnAmt;
            }
        }

        for (int i = 0; i < writer.size; i++) {
            CmerrfRecord e = writer.records[i];
            System.out.println(e.errorId + "," + e.importBatchId + "," + e.companyCode + ","
                    + e.localTxnNo + "," + e.errorCode + "," + e.errorStatusKbn);
        }
        System.out.println("採番候補件数=" + saibanKouhoCount + ",採番候補金額合計=" + kingakuGoukei);
    }

    private static CmerrfRecord error(int row, CmtxnfRecord txn, String code) {
        return new CmerrfRecord(
                "ER" + IMPORT_BATCH_ID.substring(1) + String.format("%04d", row + 1),
                IMPORT_BATCH_ID,
                blankToCm(txn.companyCode),
                txn.localTxnNo,
                code,
                STATUS_UNTAIO);
    }

    private static boolean isCompanyCode(String companyCode) {
        return "BK".equals(companyCode)
                || "SC".equals(companyCode)
                || "CD".equals(companyCode)
                || "PY".equals(companyCode)
                || "LF".equals(companyCode)
                || "CM".equals(companyCode);
    }

    private static String blankToCm(String companyCode) {
        return companyCode == null || companyCode.length() == 0 ? "CM" : companyCode;
    }

    private static final class CmtxnfRecord {
        final String txnId;
        final String companyCode;
        final long localTxnNo;
        final long txnAmt;
        final String txnStatusKbn;

        CmtxnfRecord(String txnId, String companyCode, long localTxnNo, long txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class CxidmfRecord {
        final String idmapKey;
        final String companyCode;
        final long localTxnNo;
        final String customerAliasId;
        final String linkStatusKbn;

        CxidmfRecord(String idmapKey, String companyCode, long localTxnNo, String customerAliasId, String linkStatusKbn) {
            this.idmapKey = idmapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
        }
    }

    private static final class CmerrfRecord {
        final String errorId;
        final String importBatchId;
        final String companyCode;
        final long localTxnNo;
        final String errorCode;
        final String errorStatusKbn;

        CmerrfRecord(String errorId, String importBatchId, String companyCode, long localTxnNo,
                     String errorCode, String errorStatusKbn) {
            this.errorId = errorId;
            this.importBatchId = importBatchId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.errorCode = errorCode;
            this.errorStatusKbn = errorStatusKbn;
        }
    }

    private static final class CxidmfIndex {
        private final CxidmfRecord[] records;

        CxidmfIndex(CxidmfRecord[] records) {
            this.records = records;
        }

        CxidmfRecord find(String companyCode, long localTxnNo) {
            for (int i = 0; i < records.length; i++) {
                CxidmfRecord r = records[i];
                if (r.companyCode.equals(companyCode) && r.localTxnNo == localTxnNo) {
                    return r;
                }
            }
            return null;
        }
    }

    private static final class CmerrfWriter {
        private final CmerrfRecord[] records;
        private int size;

        CmerrfWriter(int capacity) {
            this.records = new CmerrfRecord[capacity];
        }

        void write(CmerrfRecord record) {
            if (size >= records.length) {
                throw new IllegalStateException("CMERRF書込上限超過");
            }
            records[size++] = record;
        }
    }
}
