package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2025-06-29  共通基盤G   CXIDMFリンク切れ補修の事前判定版を作成
 */
public class IdMapRepairService {
    private static final String TXN_STATUS_CONFIRMED = "01";
    private static final String AUDIT_STATUS_EFFECTIVE = "01";
    private static final String LINK_STATUS_BROKEN = "9";
    private static final String LINK_STATUS_REPAIRED = "1";
    private static final String EVENT_TYPE_REPAIR = "IDMAP補修";
    private static final String JOURNAL_STATUS_NORMAL = "01";

    public static void main(String[] a) {
        java.util.List<Cxidmf> cxidmf = cxidmf入力();
        java.util.List<Cmtxnf> cmtxnf = cmtxnf入力();
        java.util.List<Cmaudf> cmaudf = cmaudf入力();

        java.util.Map<TxnKey, Cmtxnf> 取引索引 = 確定取引索引(cmtxnf);
        java.util.Map<TxnKey, java.util.List<Cmaudf>> 監査索引 = 有効監査索引(cmaudf);
        AuditLinkService auditLinkService = new AuditLinkService(cmaudf);

        java.util.List<Cxidmf> 補修後Cxidmf = new java.util.ArrayList<Cxidmf>();
        java.util.List<Cajrnf> cajrnf = new java.util.ArrayList<Cajrnf>();
        long journalSeq = 71000001L;

        for (Cxidmf idmap : cxidmf) {
            if (!LINK_STATUS_BROKEN.equals(idmap.linkStatusKbn)) {
                補修後Cxidmf.add(idmap);
                continue;
            }

            TxnKey key = new TxnKey(idmap.companyCode, idmap.localTxnNo);
            Cmtxnf txn = 取引索引.get(key);
            java.util.List<Cmaudf> audits = 監査索引.getOrDefault(key, java.util.Collections.<Cmaudf>emptyList());

            if (txn == null || audits.isEmpty()) {
                補修後Cxidmf.add(idmap);
                continue;
            }

            Cmaudf audit = audits.get(0);
            java.util.Optional<Cmaudf> reverse = auditLinkService.監査逆引き(audit.groupRefNo);
            if (!reverse.isPresent() || !key.equals(new TxnKey(reverse.get().companyCode, reverse.get().localTxnNo))) {
                補修後Cxidmf.add(idmap);
                continue;
            }

            補修後Cxidmf.add(new Cxidmf(
                    idmap.idmapKey,
                    idmap.companyCode,
                    idmap.localTxnNo,
                    idmap.customerAliasId,
                    LINK_STATUS_REPAIRED));

            cajrnf.add(new Cajrnf(
                    journalSeq++,
                    audit.auditId,
                    audit.groupRefNo,
                    EVENT_TYPE_REPAIR,
                    JOURNAL_STATUS_NORMAL));
        }

        for (Cxidmf out : 補修後Cxidmf) {
            System.out.println("CXIDMF出力 "
                    + out.idmapKey + ","
                    + out.companyCode + ","
                    + out.localTxnNo + ","
                    + out.customerAliasId + ","
                    + out.linkStatusKbn);
        }

        for (Cajrnf out : cajrnf) {
            System.out.println("CAJRNF出力 "
                    + out.journalSeq + ","
                    + out.auditId + ","
                    + out.groupRefNo + ","
                    + out.eventTypeKbn + ","
                    + out.journalStatusKbn);
        }
    }

    private static java.util.Map<TxnKey, Cmtxnf> 確定取引索引(java.util.List<Cmtxnf> records) {
        java.util.Map<TxnKey, Cmtxnf> index = new java.util.LinkedHashMap<TxnKey, Cmtxnf>();
        for (Cmtxnf record : records) {
            if (TXN_STATUS_CONFIRMED.equals(record.txnStatusKbn)) {
                index.put(new TxnKey(record.companyCode, record.localTxnNo), record);
            }
        }
        return index;
    }

    private static java.util.Map<TxnKey, java.util.List<Cmaudf>> 有効監査索引(java.util.List<Cmaudf> records) {
        java.util.Map<TxnKey, java.util.List<Cmaudf>> index =
                new java.util.LinkedHashMap<TxnKey, java.util.List<Cmaudf>>();
        for (Cmaudf record : records) {
            if (!AUDIT_STATUS_EFFECTIVE.equals(record.auditStatusKbn)) {
                continue;
            }
            TxnKey key = new TxnKey(record.companyCode, record.localTxnNo);
            java.util.List<Cmaudf> audits = index.get(key);
            if (audits == null) {
                audits = new java.util.ArrayList<Cmaudf>();
                index.put(key, audits);
            }
            audits.add(record);
        }
        return index;
    }

    private static java.util.List<Cxidmf> cxidmf入力() {
        java.util.List<Cxidmf> records = new java.util.ArrayList<Cxidmf>();
        records.add(new Cxidmf("BK-IDM-202506-0001", "BK", 240600010001L, "AL-BK-880001", LINK_STATUS_BROKEN));
        records.add(new Cxidmf("SC-IDM-202506-0002", "SC", 240600020041L, "AL-SC-771204", LINK_STATUS_BROKEN));
        records.add(new Cxidmf("CD-IDM-202506-0003", "CD", 240600030112L, "AL-CD-445512", LINK_STATUS_BROKEN));
        records.add(new Cxidmf("PY-IDM-202506-0004", "PY", 240600040018L, "AL-PY-210087", "1"));
        records.add(new Cxidmf("LF-IDM-202506-0005", "LF", 240600050006L, "AL-LF-300912", LINK_STATUS_BROKEN));
        return records;
    }

    private static java.util.List<Cmtxnf> cmtxnf入力() {
        java.util.List<Cmtxnf> records = new java.util.ArrayList<Cmtxnf>();
        records.add(new Cmtxnf("TX-BK-0001", "BK", 240600010001L, new java.math.BigDecimal("1250000"), "01"));
        records.add(new Cmtxnf("TX-SC-0041", "SC", 240600020041L, new java.math.BigDecimal("873500"), "01"));
        records.add(new Cmtxnf("TX-CD-0112", "CD", 240600030112L, new java.math.BigDecimal("45000"), "09"));
        records.add(new Cmtxnf("TX-PY-0018", "PY", 240600040018L, new java.math.BigDecimal("3180"), "01"));
        records.add(new Cmtxnf("TX-LF-0006", "LF", 240600050006L, new java.math.BigDecimal("198000"), "01"));
        return records;
    }

    private static java.util.List<Cmaudf> cmaudf入力() {
        java.util.List<Cmaudf> records = new java.util.ArrayList<Cmaudf>();
        records.add(new Cmaudf("AU-BK-900001", 1000240600010001L, "BK", 240600010001L, "01"));
        records.add(new Cmaudf("AU-SC-900041", 2000240600020041L, "SC", 240600020041L, "01"));
        records.add(new Cmaudf("AU-CD-900112", 3000240600030112L, "CD", 240600030112L, "01"));
        records.add(new Cmaudf("AU-PY-900018", 4000240600040018L, "PY", 240600040018L, "01"));
        records.add(new Cmaudf("AU-LF-900006", 5000240600050006L, "LF", 240600050006L, "09"));
        return records;
    }

    private static final class AuditLinkService {
        private final java.util.Map<Long, Cmaudf> groupRefIndex;

        private AuditLinkService(java.util.List<Cmaudf> audits) {
            this.groupRefIndex = new java.util.LinkedHashMap<Long, Cmaudf>();
            for (Cmaudf audit : audits) {
                this.groupRefIndex.put(audit.groupRefNo, audit);
            }
        }

        private java.util.Optional<Cmaudf> 監査逆引き(long groupRefNo) {
            Cmaudf audit = groupRefIndex.get(groupRefNo);
            if (audit == null) {
                return java.util.Optional.empty();
            }
            return java.util.Optional.of(audit);
        }
    }

    private static final class TxnKey {
        private final String companyCode;
        private final long localTxnNo;

        private TxnKey(String companyCode, long localTxnNo) {
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof TxnKey)) {
                return false;
            }
            TxnKey that = (TxnKey) other;
            return localTxnNo == that.localTxnNo && companyCode.equals(that.companyCode);
        }

        @Override
        public int hashCode() {
            return java.util.Objects.hash(companyCode, Long.valueOf(localTxnNo));
        }
    }

    private static final class Cxidmf {
        private final String idmapKey;
        private final String companyCode;
        private final long localTxnNo;
        private final String customerAliasId;
        private final String linkStatusKbn;

        private Cxidmf(String idmapKey, String companyCode, long localTxnNo, String customerAliasId, String linkStatusKbn) {
            this.idmapKey = idmapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
        }
    }

    private static final class Cmtxnf {
        private final String txnId;
        private final String companyCode;
        private final long localTxnNo;
        private final java.math.BigDecimal txnAmt;
        private final String txnStatusKbn;

        private Cmtxnf(String txnId, String companyCode, long localTxnNo, java.math.BigDecimal txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }
    }

    private static final class Cmaudf {
        private final String auditId;
        private final long groupRefNo;
        private final String companyCode;
        private final long localTxnNo;
        private final String auditStatusKbn;

        private Cmaudf(String auditId, long groupRefNo, String companyCode, long localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }
    }

    private static final class Cajrnf {
        private final long journalSeq;
        private final String auditId;
        private final long groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private Cajrnf(long journalSeq, String auditId, long groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }
    }
}
