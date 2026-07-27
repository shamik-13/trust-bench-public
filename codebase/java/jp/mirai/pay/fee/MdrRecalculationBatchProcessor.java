package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2024-08-05  みらいペイ システム部 加盟店・手数料チーム  MDR再計算バッチの初版作成
 */
public class MdrRecalculationBatchProcessor {
    private static final String STATUS_CHARGEABLE = "01";
    private static final java.nio.charset.Charset CHARSET = java.nio.charset.StandardCharsets.UTF_8;

    public static void main(String[] a) throws Exception {
        if (a.length != 6) {
            System.err.println("使用方法: java jp.mirai.pay.fee.MdrRecalculationBatchProcessor PFTXNF PFMERF PFFEEF PMRATF 開始日 終了日");
            System.exit(2);
        }

        java.nio.file.Path pftxnf = java.nio.file.Paths.get(a[0]);
        java.nio.file.Path pfmerf = java.nio.file.Paths.get(a[1]);
        java.nio.file.Path pffeef = java.nio.file.Paths.get(a[2]);
        java.nio.file.Path pmratf = java.nio.file.Paths.get(a[3]);
        java.time.LocalDate from = parseDate(a[4], "開始日");
        java.time.LocalDate to = parseDate(a[5], "終了日");
        if (to.isBefore(from)) {
            throw new IllegalArgumentException("終了日が開始日より前です");
        }

        java.util.Map<String, Merchant> merchants = readMerchants(pfmerf);
        java.util.Map<String, OldFee> oldFees = readOldFees(pffeef);
        java.util.Map<String, RateRule> activeRules = readRateRules(pmratf, to);
        BatchResult result = processTransactions(pftxnf, merchants, oldFees, activeRules, from, to);

        java.nio.file.Path revised = pffeef.resolveSibling(pffeef.getFileName().toString() + ".rev");
        writeRevisedFees(revised, result.revisedFees);

        System.out.println("処理件数=" + result.totalCount);
        System.out.println("改訂対象件数=" + result.revisedFees.size());
        System.out.println("対象外件数=" + result.skippedCount);
        System.out.println("差額合計=" + result.totalDifference);
        System.out.println("出力ファイル=" + revised);
    }

    private static BatchResult processTransactions(
            java.nio.file.Path pftxnf,
            java.util.Map<String, Merchant> merchants,
            java.util.Map<String, OldFee> oldFees,
            java.util.Map<String, RateRule> activeRules,
            java.time.LocalDate from,
            java.time.LocalDate to) throws Exception {
        BatchResult result = new BatchResult();

        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(pftxnf, CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isHeaderOrBlank(line)) {
                    continue;
                }

                TxnRow row = parseTxn(line, lineNo);
                result.totalCount++;

                if (row.txnDt.isBefore(from) || row.txnDt.isAfter(to)) {
                    result.skippedCount++;
                    continue;
                }

                Merchant merchant = merchants.get(row.merchantCode);
                if (merchant == null) {
                    throw new IllegalStateException("加盟店未登録: 行=" + lineNo + ", 加盟店=" + row.merchantCode);
                }
                if (!STATUS_CHARGEABLE.equals(merchant.status)) {
                    result.skippedCount++;
                    continue;
                }

                RateRule rule = activeRules.get(merchant.category);
                if (rule == null) {
                    throw new IllegalStateException("有効な料率規程なし: 行=" + lineNo + ", 業種=" + merchant.category);
                }

                FeeModel.Txn txn = newFeeModelTxn(row.txnId, row.merchantCode, row.txnAmt, row.txnDt);
                long newFee = invokeFeeFor(txn, merchant.category);
                OldFee oldFee = oldFees.get(row.txnId);
                if (oldFee == null) {
                    throw new IllegalStateException("旧明細なし: 行=" + lineNo + ", 取引=" + row.txnId);
                }

                boolean categoryChanged = !merchant.category.equals(oldFee.categorySnapshot);
                boolean ruleChanged = !rule.ruleHash.equals(oldFee.ruleHashSnapshot);
                if (!categoryChanged && !ruleChanged) {
                    result.skippedCount++;
                    continue;
                }

                long difference = newFee - oldFee.feeAmt;
                if (difference == 0L && oldFee.txnAmt == row.txnAmt) {
                    result.skippedCount++;
                    continue;
                }

                result.totalDifference += difference;
                result.revisedFees.add(new RevisedFee(
                        "R" + row.txnId,
                        row.merchantCode,
                        row.txnAmt,
                        "規程:" + merchant.category + ":" + rule.noticeId + ":MdrFeeEngine",
                        newFee,
                        difference));
            }
        }

        return result;
    }

    private static java.util.Map<String, Merchant> readMerchants(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, Merchant> merchants = new java.util.HashMap<>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isHeaderOrBlank(line)) {
                    continue;
                }
                String[] c = split(line, 4, lineNo, "PFMERF");
                validateCategory(c[2], lineNo);
                merchants.put(c[0], new Merchant(c[0], c[1], c[2], c[3]));
            }
        }
        return merchants;
    }

    private static java.util.Map<String, OldFee> readOldFees(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, OldFee> fees = new java.util.HashMap<>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isHeaderOrBlank(line)) {
                    continue;
                }
                String[] c = split(line, 5, lineNo, "PFFEEF");
                long txnAmt = parseAmount(c[2], lineNo, "TXN-AMT");
                long feeAmt = parseAmount(c[4], lineNo, "FEE-AMT");
                String[] snapshot = c[3].split(":", -1);
                String category = snapshot.length > 1 ? snapshot[1] : "";
                String ruleHash = snapshot.length > 2 ? snapshot[2] : "";
                fees.put(c[0], new OldFee(c[0], c[1], txnAmt, feeAmt, category, ruleHash));
            }
        }
        return fees;
    }

    private static java.util.Map<String, RateRule> readRateRules(java.nio.file.Path path, java.time.LocalDate asOf) throws java.io.IOException {
        java.util.Map<String, RateRule> selected = new java.util.HashMap<>();
        try (java.io.BufferedReader reader = java.nio.file.Files.newBufferedReader(path, CHARSET)) {
            String line;
            int lineNo = 0;
            while ((line = reader.readLine()) != null) {
                lineNo++;
                if (isHeaderOrBlank(line)) {
                    continue;
                }
                String[] c = split(line, 6, lineNo, "PMRATF");
                validateCategory(c[1], lineNo);
                java.time.LocalDate effectiveDt = parseDate(c[2], "EFFECTIVE-DT");
                if (effectiveDt.isAfter(asOf) || !"承認済".equals(c[4])) {
                    continue;
                }

                RateRule current = selected.get(c[1]);
                if (current == null || effectiveDt.isAfter(current.effectiveDt)) {
                    selected.put(c[1], new RateRule(c[0], c[1], effectiveDt, c[3], c[4], c[5]));
                }
            }
        }
        return selected;
    }

    private static void writeRevisedFees(java.nio.file.Path path, java.util.List<RevisedFee> revisedFees) throws java.io.IOException {
        try (java.io.BufferedWriter writer = java.nio.file.Files.newBufferedWriter(path, CHARSET)) {
            writer.write("FEE-ID,MERCHANT-CODE,TXN-AMT,MDR-RATE,FEE-AMT");
            writer.newLine();
            for (RevisedFee fee : revisedFees) {
                writer.write(String.join(",",
                        fee.feeId,
                        fee.merchantCode,
                        String.valueOf(fee.txnAmt),
                        fee.mdrRateToken,
                        String.valueOf(fee.feeAmt)));
                writer.newLine();
            }
        }
    }

    private static TxnRow parseTxn(String line, int lineNo) {
        String[] c = split(line, 4, lineNo, "PFTXNF");
        return new TxnRow(c[0], c[1], parseAmount(c[2], lineNo, "TXN-AMT"), parseDate(c[3], "TXN-DT"));
    }

    private static FeeModel.Txn newFeeModelTxn(String txnId, String merchantCode, long amount, java.time.LocalDate txnDt) throws Exception {
        for (java.lang.reflect.Constructor<?> ctor : FeeModel.Txn.class.getDeclaredConstructors()) {
            Class<?>[] p = ctor.getParameterTypes();
            if (p.length == 4 && p[0] == String.class && p[1] == String.class && isLongType(p[2]) && p[3] == java.time.LocalDate.class) {
                ctor.setAccessible(true);
                return FeeModel.Txn.class.cast(ctor.newInstance(txnId, merchantCode, amount, txnDt));
            }
        }
        throw new IllegalStateException("FeeModel.Txnのコンストラクタ形式が想定外です");
    }

    // 料率値は当バッチで保持しない。手数料額は規程に基づき MdrFeeEngine が業種区分から算定する。
    private static long invokeFeeFor(FeeModel.Txn txn, String category) throws Exception {
        java.lang.reflect.Method m = MdrFeeEngine.class.getDeclaredMethod("feeFor", FeeModel.Txn.class, String.class);
        m.setAccessible(true);
        Object target = java.lang.reflect.Modifier.isStatic(m.getModifiers()) ? null : MdrFeeEngine.class.getDeclaredConstructor().newInstance();
        return ((Number) m.invoke(target, txn, category)).longValue();
    }

    private static boolean isLongType(Class<?> type) {
        return type == long.class || type == Long.class;
    }

    private static String[] split(String line, int minColumns, int lineNo, String fileName) {
        String[] raw = line.split(",", -1);
        if (raw.length < minColumns) {
            throw new IllegalArgumentException(fileName + "の項目数不足: 行=" + lineNo);
        }
        String[] trimmed = new String[raw.length];
        for (int i = 0; i < raw.length; i++) {
            trimmed[i] = raw[i].trim();
        }
        return trimmed;
    }

    private static java.time.LocalDate parseDate(String value, String name) {
        try {
            return java.time.LocalDate.parse(value);
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException(name + "の日付形式不正: " + value, e);
        }
    }

    private static long parseAmount(String value, int lineNo, String name) {
        try {
            long amount = Long.parseLong(value);
            if (amount < 0L) {
                throw new IllegalArgumentException(name + "が負数です: 行=" + lineNo);
            }
            return amount;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(name + "の数値形式不正: 行=" + lineNo + ", 値=" + value, e);
        }
    }

    private static void validateCategory(String category, int lineNo) {
        if (!"C1".equals(category) && !"C2".equals(category) && !"C3".equals(category)
                && !"C4".equals(category) && !"C5".equals(category)) {
            throw new IllegalArgumentException("業種区分不正: 行=" + lineNo + ", 値=" + category);
        }
    }

    private static boolean isHeaderOrBlank(String line) {
        String s = line.trim();
        return s.isEmpty() || s.startsWith("#") || s.startsWith("TXN-ID") || s.startsWith("FEE-ID")
                || s.startsWith("MERCHANT-CODE") || s.startsWith("RATE-PLAN-ID");
    }

    private static final class TxnRow {
        final String txnId;
        final String merchantCode;
        final long txnAmt;
        final java.time.LocalDate txnDt;

        TxnRow(String txnId, String merchantCode, long txnAmt, java.time.LocalDate txnDt) {
            this.txnId = txnId;
            this.merchantCode = merchantCode;
            this.txnAmt = txnAmt;
            this.txnDt = txnDt;
        }
    }

    private static final class Merchant {
        final String merchantCode;
        final String merchantName;
        final String category;
        final String status;

        Merchant(String merchantCode, String merchantName, String category, String status) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.category = category;
            this.status = status;
        }
    }

    private static final class OldFee {
        final String feeId;
        final String merchantCode;
        final long txnAmt;
        final long feeAmt;
        final String categorySnapshot;
        final String ruleHashSnapshot;

        OldFee(String feeId, String merchantCode, long txnAmt, long feeAmt, String categorySnapshot, String ruleHashSnapshot) {
            this.feeId = feeId;
            this.merchantCode = merchantCode;
            this.txnAmt = txnAmt;
            this.feeAmt = feeAmt;
            this.categorySnapshot = categorySnapshot;
            this.ruleHashSnapshot = ruleHashSnapshot;
        }
    }

    private static final class RateRule {
        final String ratePlanId;
        final String categoryCode;
        final java.time.LocalDate effectiveDt;
        final String noticeId;
        final String approvalStatus;
        final String ruleHash;

        RateRule(String ratePlanId, String categoryCode, java.time.LocalDate effectiveDt,
                 String noticeId, String approvalStatus, String ruleHash) {
            this.ratePlanId = ratePlanId;
            this.categoryCode = categoryCode;
            this.effectiveDt = effectiveDt;
            this.noticeId = noticeId;
            this.approvalStatus = approvalStatus;
            this.ruleHash = ruleHash;
        }
    }

    private static final class RevisedFee {
        final String feeId;
        final String merchantCode;
        final long txnAmt;
        final String mdrRateToken;
        final long feeAmt;
        final long difference;

        RevisedFee(String feeId, String merchantCode, long txnAmt, String mdrRateToken, long feeAmt, long difference) {
            this.feeId = feeId;
            this.merchantCode = merchantCode;
            this.txnAmt = txnAmt;
            this.mdrRateToken = mdrRateToken;
            this.feeAmt = feeAmt;
            this.difference = difference;
        }
    }

    private static final class BatchResult {
        long totalCount;
        long skippedCount;
        long totalDifference;
        final java.util.List<RevisedFee> revisedFees = new java.util.ArrayList<>();
    }
}
