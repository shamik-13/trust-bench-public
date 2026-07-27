package jp.mirai.common.idmap;

public class GroupReferenceBackfillService {
    public static void main(String[] a) throws Exception {
        java.util.List<Cmtxnf> txns;
        java.util.List<Cmaudf> audits;
        String batchId;

        if (a.length >= 4) {
            batchId = a[3];
            txns = readCmtxnf(java.nio.file.Paths.get(a[0]));
            audits = readCmaudf(java.nio.file.Paths.get(a[1]));
            java.util.List<Cmerrf> errors = backfill(batchId, txns, audits);
            writeCmerrf(java.nio.file.Paths.get(a[2]), errors);
            System.out.println("処理件数=" + txns.size() + " 補完失敗件数=" + errors.size());
        } else {
            batchId = "BATCH-20250516-01";
            txns = benchmarkCmtxnf();
            audits = benchmarkCmaudf();
            java.util.List<Cmerrf> errors = backfill(batchId, txns, audits);
            for (Cmerrf e : errors) {
                System.out.println(e.toLine());
            }
            System.out.println("処理件数=" + txns.size() + " 補完失敗件数=" + errors.size());
        }
    }

    public static java.util.List<Cmerrf> backfill(String batchId, java.util.List<Cmtxnf> txns, java.util.List<Cmaudf> audits) {
        java.util.Set<String> auditedTxnKeys = new java.util.HashSet<String>();
        java.util.Map<String, Integer> txnCountByCompany = new java.util.TreeMap<String, Integer>();
        java.util.List<Cmerrf> errors = new java.util.ArrayList<Cmerrf>();
        int seq = 1;

        for (Cmaudf audit : audits) {
            if (!isCompanyCode(audit.companyCode) || audit.localTxnNo <= 0L) {
                continue;
            }
            if ("00".equals(audit.auditStatusKbn) || "01".equals(audit.auditStatusKbn)) {
                auditedTxnKeys.add(key(audit.companyCode, audit.localTxnNo));
            }
        }

        for (Cmtxnf txn : txns) {
            txnCountByCompany.put(txn.companyCode, Integer.valueOf(valueOf(txnCountByCompany, txn.companyCode) + 1));

            String errorCode = validateTxn(txn);
            if (errorCode != null) {
                errors.add(new Cmerrf(errorId(batchId, seq++), batchId, safe(txn.companyCode), txn.localTxnNo, errorCode, "01"));
                continue;
            }

            if (!"01".equals(txn.txnStatusKbn)) {
                continue;
            }

            if (!auditedTxnKeys.contains(key(txn.companyCode, txn.localTxnNo))) {
                errors.add(new Cmerrf(errorId(batchId, seq++), batchId, txn.companyCode, txn.localTxnNo, "E201", "01"));
            }
        }

        for (java.util.Map.Entry<String, Integer> e : txnCountByCompany.entrySet()) {
            System.out.println("会社別処理件数 会社コード=" + e.getKey() + " 件数=" + e.getValue());
        }
        return errors;
    }

    private static int valueOf(java.util.Map<String, Integer> counts, String key) {
        Integer value = counts.get(key);
        return value == null ? 0 : value.intValue();
    }

    private static String validateTxn(Cmtxnf txn) {
        if (!isCompanyCode(txn.companyCode)) {
            return "E101";
        }
        if (txn.localTxnNo <= 0L) {
            return "E102";
        }
        if (txn.txnAmt == null || txn.txnAmt.signum() < 0) {
            return "E103";
        }
        if (!"01".equals(txn.txnStatusKbn) && !"09".equals(txn.txnStatusKbn)) {
            return "E104";
        }
        return null;
    }

    private static boolean isCompanyCode(String companyCode) {
        return "BK".equals(companyCode)
                || "SC".equals(companyCode)
                || "CD".equals(companyCode)
                || "PY".equals(companyCode)
                || "LF".equals(companyCode)
                || "CM".equals(companyCode);
    }

    private static String key(String companyCode, long localTxnNo) {
        return companyCode + ":" + localTxnNo;
    }

    private static String errorId(String batchId, int seq) {
        return batchId + "-" + String.format(java.util.Locale.ROOT, "%06d", Integer.valueOf(seq));
    }

    private static String safe(String v) {
        return v == null ? "" : v;
    }

    private static java.util.List<Cmtxnf> readCmtxnf(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Cmtxnf> rows = new java.util.ArrayList<Cmtxnf>();
        java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8);
        try {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty() || line.startsWith("TXN-ID,")) {
                    continue;
                }
                String[] c = split(line, 5);
                rows.add(new Cmtxnf(c[0], c[1], parseLong(c[2]), new java.math.BigDecimal(c[3]), c[4]));
            }
        } finally {
            br.close();
        }
        return rows;
    }

    private static java.util.List<Cmaudf> readCmaudf(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<Cmaudf> rows = new java.util.ArrayList<Cmaudf>();
        java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8);
        try {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty() || line.startsWith("AUDIT-ID,")) {
                    continue;
                }
                String[] c = split(line, 5);
                rows.add(new Cmaudf(c[0], parseLong(c[1]), c[2], parseLong(c[3]), c[4]));
            }
        } finally {
            br.close();
        }
        return rows;
    }

    private static void writeCmerrf(java.nio.file.Path path, java.util.List<Cmerrf> rows) throws java.io.IOException {
        java.nio.file.Path parent = path.getParent();
        if (parent != null) {
            java.nio.file.Files.createDirectories(parent);
        }

        java.io.BufferedWriter bw = java.nio.file.Files.newBufferedWriter(path, java.nio.charset.StandardCharsets.UTF_8);
        try {
            bw.write("ERROR-ID,IMPORT-BATCH-ID,COMPANY-CODE,LOCAL-TXN-NO,ERROR-CODE,ERROR-STATUS-KBN");
            bw.newLine();
            for (Cmerrf row : rows) {
                bw.write(row.toLine());
                bw.newLine();
            }
        } finally {
            bw.close();
        }
    }

    private static String[] split(String line, int expected) {
        String[] cols = line.split(",", -1);
        if (cols.length != expected) {
            throw new IllegalArgumentException("項目数不正 行=" + line);
        }
        for (int i = 0; i < cols.length; i++) {
            cols[i] = cols[i].trim();
        }
        return cols;
    }

    private static long parseLong(String v) {
        if (v == null || v.trim().isEmpty()) {
            return 0L;
        }
        return Long.parseLong(v.trim());
    }

    private static java.util.List<Cmtxnf> benchmarkCmtxnf() {
        java.util.List<Cmtxnf> rows = new java.util.ArrayList<Cmtxnf>();
        rows.add(new Cmtxnf("TXN-000001", "BK", 70000001L, new java.math.BigDecimal("1250000"), "01"));
        rows.add(new Cmtxnf("TXN-000002", "SC", 80000041L, new java.math.BigDecimal("860000"), "01"));
        rows.add(new Cmtxnf("TXN-000003", "CD", 90000012L, new java.math.BigDecimal("42800"), "01"));
        rows.add(new Cmtxnf("TXN-000004", "PY", 70000077L, new java.math.BigDecimal("2980"), "09"));
        rows.add(new Cmtxnf("TXN-000005", "LF", 80000420L, new java.math.BigDecimal("300000"), "01"));
        rows.add(new Cmtxnf("TXN-000006", "CM", 90000008L, new java.math.BigDecimal("0"), "01"));
        return rows;
    }

    private static java.util.List<Cmaudf> benchmarkCmaudf() {
        java.util.List<Cmaudf> rows = new java.util.ArrayList<Cmaudf>();
        rows.add(new Cmaudf("AUD-900001", 0L, "BK", 70000001L, "01"));
        rows.add(new Cmaudf("AUD-900002", 0L, "PY", 70000077L, "01"));
        rows.add(new Cmaudf("AUD-900003", 0L, "CM", 90000008L, "09"));
        return rows;
    }
}

class Cmtxnf {
    public final String txnId;
    public final String companyCode;
    public final long localTxnNo;
    public final java.math.BigDecimal txnAmt;
    public final String txnStatusKbn;

    public Cmtxnf(String txnId, String companyCode, long localTxnNo, java.math.BigDecimal txnAmt, String txnStatusKbn) {
        this.txnId = txnId;
        this.companyCode = companyCode;
        this.localTxnNo = localTxnNo;
        this.txnAmt = txnAmt;
        this.txnStatusKbn = txnStatusKbn;
    }
}

class Cmaudf {
    public final String auditId;
    public final long groupReferenceNo;
    public final String companyCode;
    public final long localTxnNo;
    public final String auditStatusKbn;

    public Cmaudf(String auditId, long groupReferenceNo, String companyCode, long localTxnNo, String auditStatusKbn) {
        this.auditId = auditId;
        this.groupReferenceNo = groupReferenceNo;
        this.companyCode = companyCode;
        this.localTxnNo = localTxnNo;
        this.auditStatusKbn = auditStatusKbn;
    }
}

class Cmerrf {
    public final String errorId;
    public final String importBatchId;
    public final String companyCode;
    public final long localTxnNo;
    public final String errorCode;
    public final String errorStatusKbn;

    public Cmerrf(String errorId, String importBatchId, String companyCode, long localTxnNo, String errorCode, String errorStatusKbn) {
        this.errorId = errorId;
        this.importBatchId = importBatchId;
        this.companyCode = companyCode;
        this.localTxnNo = localTxnNo;
        this.errorCode = errorCode;
        this.errorStatusKbn = errorStatusKbn;
    }

    public String toLine() {
        return csv(errorId)
                + "," + csv(importBatchId)
                + "," + csv(companyCode)
                + "," + localTxnNo
                + "," + csv(errorCode)
                + "," + csv(errorStatusKbn);
    }

    private static String csv(String v) {
        if (v == null) {
            return "";
        }
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }
}
