package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数    年月日      担当            概要
 * 1.00    2024/03/15  保険金システムG  支払振込オーケストレータ初版作成
 */
public class PaymentTransferOrchestrator {
    private static final String STATUS_SHIHARAI_SHONIN_ZUMI = "40";
    private static final String STATUS_FURIKOMI_ZUMI = "90";
    private static final int ONE_YEAR_OR_MORE_PAYMENT_RATE_PERCENT = 100;

    private static java.util.List<PayRecord> readPayRecords(java.nio.file.Path path) throws java.io.IOException {
        java.util.List<PayRecord> records = new java.util.ArrayList<>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (isSkippable(line)) {
                continue;
            }
            String[] c = splitCsv(line, 5);
            records.add(new PayRecord(c[0], c[1], parseLong(c[2], "GROSS-AMT"), parseRate(c[3]), parseLong(c[4], "PAYOUT-AMT")));
        }
        return records;
    }

    private static java.util.Map<String, BeneficiaryRecord> readBeneficiaryByPolicy(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, BeneficiaryRecord> records = new java.util.LinkedHashMap<>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (isSkippable(line)) {
                continue;
            }
            String[] c = splitCsv(line, 8);
            BeneficiaryRecord record = new BeneficiaryRecord(c[0], c[1], c[2], c[3], c[4], c[5], c[6], parseInt(c[7], "PAYMENT-PRIORITY"));
            BeneficiaryRecord current = records.get(record.polNo);
            if (current == null || record.paymentPriority < current.paymentPriority) {
                records.put(record.polNo, record);
            }
        }
        return records;
    }

    private static java.util.Map<String, ClaimRecord> readClaimById(java.nio.file.Path path) throws java.io.IOException {
        java.util.Map<String, ClaimRecord> records = new java.util.LinkedHashMap<>();
        for (String line : java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8)) {
            if (isSkippable(line)) {
                continue;
            }
            String[] c = splitCsv(line, 7);
            records.put(c[0], new ClaimRecord(c[0], c[1], parseLong(c[2], "SUM-ASSURED-AMT"),
                    parseLong(c[3], "LOAN-BALANCE-AMT"), parseDate(c[4], "RESP-START-DT"),
                    parseDate(c[5], "EVENT-DT"), c[6]));
        }
        return records;
    }

    private static void validateTransferSource(PayRecord pay, BeneficiaryRecord beneficiary, ClaimRecord claim) {
        if (pay.payoutAmt <= 0) {
            throw new IllegalStateException("支払額が正ではありません PAY-ID=" + pay.payId);
        }
        if (claim.eventDt.isBefore(claim.respStartDt)) {
            throw new IllegalStateException("事故日が責任開始日より前です CLAIM-ID=" + claim.claimId);
        }
        if (isBlank(beneficiary.bankCd) || isBlank(beneficiary.branchCd) || isBlank(beneficiary.acctNo) || isBlank(beneficiary.nameKana)) {
            throw new IllegalStateException("振込先情報が不足しています POL-NO=" + beneficiary.polNo);
        }
        if (!beneficiary.bankCd.matches("\\d{4}") || !beneficiary.branchCd.matches("\\d{3}") || !beneficiary.acctNo.matches("\\d{7}")) {
            throw new IllegalStateException("振込先コード体系が不正です POL-NO=" + beneficiary.polNo);
        }
    }

    private static long calculateTransferAmount(PayRecord pay, ClaimRecord claim) {
        long netBase = Math.max(0L, Math.min(pay.grossAmt, claim.sumAssuredAmt) - claim.loanBalanceAmt);
        long regulatedAmount = netBase * ONE_YEAR_OR_MORE_PAYMENT_RATE_PERCENT / 100L;
        long engineAmount = pay.reductionRate == null ? pay.payoutAmt : Math.round(pay.grossAmt * pay.reductionRate);
        long amount = Math.min(pay.payoutAmt, Math.min(regulatedAmount, engineAmount));
        if (amount <= 0L) {
            throw new IllegalStateException("振込額が算出できません CLAIM-ID=" + claim.claimId + " PAY-ID=" + pay.payId);
        }
        return amount;
    }

    private static TransferRecord buildTransferRecord(PayRecord pay, BeneficiaryRecord beneficiary, long amount, java.time.LocalDate transferDate) {
        String transferId = "X" + transferDate.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE) + "-" + leftPad(pay.payId, 12);
        return new TransferRecord(transferId, pay.payId, beneficiary.bankCd, beneficiary.branchCd,
                beneficiary.acctNo, beneficiary.nameKana, amount, transferDate);
    }

    private static void writeTransfersAllOrNothing(java.nio.file.Path output, java.util.List<TransferRecord> records) throws java.io.IOException {
        java.nio.file.Path temp = output.resolveSibling(output.getFileName().toString() + ".tmp");
        java.util.List<String> lines = new java.util.ArrayList<>();
        for (TransferRecord record : records) {
            lines.add(record.toCsv());
        }
        java.nio.file.Files.write(temp, lines, java.nio.charset.StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING);
        try {
            java.nio.file.Files.move(temp, output, java.nio.file.StandardCopyOption.REPLACE_EXISTING,
                    java.nio.file.StandardCopyOption.ATOMIC_MOVE);
        } catch (java.nio.file.AtomicMoveNotSupportedException e) {
            java.nio.file.Files.move(temp, output, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        }
    }

    private static void writeUpdatedClaims(java.nio.file.Path output, java.util.Map<String, ClaimRecord> claimById,
            java.util.Set<String> completedClaimIds) throws java.io.IOException {
        java.util.List<String> lines = new java.util.ArrayList<>();
        for (ClaimRecord claim : claimById.values()) {
            ClaimRecord updated = completedClaimIds.contains(claim.claimId) ? claim.withStatus(STATUS_FURIKOMI_ZUMI) : claim;
            lines.add(updated.toCsv());
        }
        java.nio.file.Files.write(output, lines, java.nio.charset.StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING);
    }

    private static String[] splitCsv(String line, int expected) {
        String[] c = line.split(",", -1);
        if (c.length != expected) {
            throw new IllegalArgumentException("項目数が不正です 期待=" + expected + " 実際=" + c.length + " 行=" + line);
        }
        for (int i = 0; i < c.length; i++) {
            c[i] = c[i].trim();
        }
        return c;
    }

    private static boolean isSkippable(String line) {
        String s = line == null ? "" : line.trim();
        return s.isEmpty() || s.startsWith("#");
    }

    private static long parseLong(String value, String name) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値項目が不正です " + name + "=" + value, e);
        }
    }

    private static int parseInt(String value, String name) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("数値項目が不正です " + name + "=" + value, e);
        }
    }

    private static Double parseRate(String value) {
        if (isBlank(value)) {
            return null;
        }
        double rate = Double.parseDouble(value);
        if (rate < 0.0d || rate > 1.0d) {
            throw new IllegalArgumentException("削減率が範囲外です REDUCTION-RATE=" + value);
        }
        return rate;
    }

    private static java.time.LocalDate parseDate(String value, String name) {
        try {
            return java.time.LocalDate.parse(value, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        } catch (java.time.format.DateTimeParseException e) {
            throw new IllegalArgumentException("日付項目が不正です " + name + "=" + value, e);
        }
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static String leftPad(String value, int length) {
        String s = value == null ? "" : value;
        if (s.length() >= length) {
            return s.substring(s.length() - length);
        }
        StringBuilder b = new StringBuilder(length);
        for (int i = s.length(); i < length; i++) {
            b.append('0');
        }
        return b.append(s).toString();
    }

    private static final class PayRecord {
        private final String payId;
        private final String claimId;
        private final long grossAmt;
        private final Double reductionRate;
        private final long payoutAmt;

        private PayRecord(String payId, String claimId, long grossAmt, Double reductionRate, long payoutAmt) {
            this.payId = payId;
            this.claimId = claimId;
            this.grossAmt = grossAmt;
            this.reductionRate = reductionRate;
            this.payoutAmt = payoutAmt;
        }
    }

    private static final class BeneficiaryRecord {
        private final String polNo;
        private final String beneficiaryId;
        private final String nameKana;
        private final String relationshipKbn;
        private final String bankCd;
        private final String branchCd;
        private final String acctNo;
        private final int paymentPriority;

        private BeneficiaryRecord(String polNo, String beneficiaryId, String nameKana, String relationshipKbn,
                String bankCd, String branchCd, String acctNo, int paymentPriority) {
            this.polNo = polNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.paymentPriority = paymentPriority;
        }
    }

    private static final class ClaimRecord {
        private final String claimId;
        private final String polNo;
        private final long sumAssuredAmt;
        private final long loanBalanceAmt;
        private final java.time.LocalDate respStartDt;
        private final java.time.LocalDate eventDt;
        private final String claimStatusKbn;

        private ClaimRecord(String claimId, String polNo, long sumAssuredAmt, long loanBalanceAmt,
                java.time.LocalDate respStartDt, java.time.LocalDate eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }

        private ClaimRecord withStatus(String status) {
            return new ClaimRecord(claimId, polNo, sumAssuredAmt, loanBalanceAmt, respStartDt, eventDt, status);
        }

        private String toCsv() {
            return claimId + "," + polNo + "," + sumAssuredAmt + "," + loanBalanceAmt + ","
                    + respStartDt.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE) + ","
                    + eventDt.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE) + "," + claimStatusKbn;
        }
    }

    private static final class TransferRecord {
        private final String transferId;
        private final String payId;
        private final String bankCd;
        private final String branchCd;
        private final String acctNo;
        private final String acctHolderKna;
        private final long amount;
        private final java.time.LocalDate transferDt;

        private TransferRecord(String transferId, String payId, String bankCd, String branchCd,
                String acctNo, String acctHolderKna, long amount, java.time.LocalDate transferDt) {
            this.transferId = transferId;
            this.payId = payId;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.acctHolderKna = acctHolderKna;
            this.amount = amount;
            this.transferDt = transferDt;
        }

        private String toCsv() {
            return transferId + "," + payId + "," + bankCd + "," + branchCd + "," + acctNo + ","
                    + acctHolderKna + "," + amount + ","
                    + transferDt.format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
        }
    }
}
