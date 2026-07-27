package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当          概要
 * 1.00  2024/03/15  保険金システムG  初版作成
 */
public class BeneficiaryVerificationService {
    private static final String CLAIM_STATUS_PAYABLE = "01";
    private static final int PAY_RATIO_AFTER_ONE_YEAR_PERCENT = 100;

    private static final java.util.Map<String, java.util.Set<String>> RELATIONSHIP_BY_POLICY_TYPE =
            createRelationshipTable();

    private static java.util.List<RoutingBeneficiary> verify(
            String policyType, ClaimRecord claim, java.util.List<BeneficiaryRecord> beneficiaries) {
        java.util.Set<String> permitted = RELATIONSHIP_BY_POLICY_TYPE.get(policyType);
        if (permitted == null) {
            throw new IllegalArgumentException("保険種類の続柄許容表が未登録です。保険種類=" + policyType);
        }

        java.util.Map<Integer, BeneficiaryRecord> byPriority = new java.util.HashMap<>();
        for (BeneficiaryRecord b : beneficiaries) {
            if (!permitted.contains(b.relationshipKbn)) {
                throw new IllegalStateException("許容外の続柄区分です。受取人ID=" + b.beneficiaryId
                        + " 続柄区分=" + b.relationshipKbn + " 保険種類=" + policyType);
            }
            if (!isDigits(b.bankCd, 4)) {
                throw new IllegalStateException("金融機関コード形式不正です。受取人ID=" + b.beneficiaryId
                        + " 銀行コード=" + b.bankCd);
            }
            if (!isDigits(b.branchCd, 3)) {
                throw new IllegalStateException("支店コード形式不正です。受取人ID=" + b.beneficiaryId
                        + " 支店コード=" + b.branchCd);
            }
            if (b.paymentPriority < 1) {
                throw new IllegalStateException("支払順位は1以上である必要があります。受取人ID=" + b.beneficiaryId);
            }
            BeneficiaryRecord existing = byPriority.putIfAbsent(b.paymentPriority, b);
            if (existing != null) {
                throw new IllegalStateException("支払順位が重複しています。順位=" + b.paymentPriority
                        + " 受取人ID=" + existing.beneficiaryId + "," + b.beneficiaryId);
            }
        }

        for (int i = 1; i <= beneficiaries.size(); i++) {
            if (!byPriority.containsKey(i)) {
                throw new IllegalStateException("支払順位に欠番があります。欠番順位=" + i);
            }
        }

        int payRatio = isAfterOrEqualOneYear(claim.respStartDt, claim.eventDt)
                ? PAY_RATIO_AFTER_ONE_YEAR_PERCENT
                : 0;

        java.util.List<RoutingBeneficiary> result = new java.util.ArrayList<>();
        for (int i = 1; i <= beneficiaries.size(); i++) {
            BeneficiaryRecord b = byPriority.get(i);
            result.add(new RoutingBeneficiary(b.policyNo, b.beneficiaryId, b.nameKana, b.relationshipKbn,
                    b.bankCd, b.branchCd, b.acctNo, b.paymentPriority, payRatio));
        }
        return result;
    }

    private static ClaimRecord readClaim(java.nio.file.Path path, String policyNo) throws java.io.IOException {
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (first) {
                    first = false;
                    if (line.startsWith("CLAIM-ID,") || line.startsWith("CLAIM_ID,")) {
                        continue;
                    }
                }
                java.util.List<String> c = parseCsv(line);
                if (c.size() < 7) {
                    throw new IllegalArgumentException("請求CSVの項目数が不足しています。行=" + line);
                }
                ClaimRecord r = new ClaimRecord(c.get(0), c.get(1), parseLong(c.get(2), "保険金額"),
                        parseLong(c.get(3), "貸付残高"), parseDate(c.get(4), "責任開始日"),
                        parseDate(c.get(5), "事故日"), c.get(6));
                if (policyNo.equals(r.policyNo)) {
                    return r;
                }
            }
        }
        return null;
    }

    private static java.util.List<BeneficiaryRecord> readBeneficiaries(java.nio.file.Path path, String policyNo)
            throws java.io.IOException {
        java.util.List<BeneficiaryRecord> records = new java.util.ArrayList<>();
        try (java.io.BufferedReader br = java.nio.file.Files.newBufferedReader(path, java.nio.charset.StandardCharsets.UTF_8)) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (first) {
                    first = false;
                    if (line.startsWith("POL-NO,") || line.startsWith("POL_NO,")) {
                        continue;
                    }
                }
                java.util.List<String> c = parseCsv(line);
                if (c.size() < 8) {
                    throw new IllegalArgumentException("受取人CSVの項目数が不足しています。行=" + line);
                }
                BeneficiaryRecord r = new BeneficiaryRecord(c.get(0), c.get(1), c.get(2), c.get(3),
                        c.get(4), c.get(5), c.get(6), parseInt(c.get(7), "支払順位"));
                if (policyNo.equals(r.policyNo)) {
                    records.add(r);
                }
            }
        }
        return records;
    }

    private static boolean isAfterOrEqualOneYear(java.time.LocalDate start, java.time.LocalDate event) {
        return !event.isBefore(start.plusYears(1));
    }

    private static boolean isDigits(String value, int length) {
        if (value == null || value.length() != length) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            if (!Character.isDigit(value.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private static long parseLong(String value, String itemName) {
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(itemName + "が数値ではありません。値=" + value, e);
        }
    }

    private static int parseInt(String value, String itemName) {
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(itemName + "が数値ではありません。値=" + value, e);
        }
    }

    private static java.time.LocalDate parseDate(String value, String itemName) {
        String v = value.trim();
        try {
            if (v.length() == 8 && isDigits(v, 8)) {
                return java.time.LocalDate.parse(v, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
            }
            return java.time.LocalDate.parse(v);
        } catch (java.time.DateTimeException e) {
            throw new IllegalArgumentException(itemName + "が日付ではありません。値=" + value, e);
        }
    }

    private static java.util.List<String> parseCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    current.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                out.add(current.toString().trim());
                current.setLength(0);
            } else {
                current.append(ch);
            }
        }
        out.add(current.toString().trim());
        return out;
    }

    private static String csv(String... values) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                b.append(',');
            }
            String v = values[i] == null ? "" : values[i];
            if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
                b.append('"').append(v.replace("\"", "\"\"")).append('"');
            } else {
                b.append(v);
            }
        }
        return b.toString();
    }

    private static java.util.Map<String, java.util.Set<String>> createRelationshipTable() {
        java.util.Map<String, java.util.Set<String>> m = new java.util.HashMap<>();
        m.put("TERM", set("01", "02", "03", "04"));
        m.put("WHOLE", set("01", "02", "03", "04", "05"));
        m.put("MED", set("01", "02", "03"));
        m.put("ANNUITY", set("01", "02", "06"));
        return java.util.Collections.unmodifiableMap(m);
    }

    private static java.util.Set<String> set(String... values) {
        java.util.Set<String> s = new java.util.HashSet<>();
        java.util.Collections.addAll(s, values);
        return java.util.Collections.unmodifiableSet(s);
    }

    private static final class ClaimRecord {
        final String claimId;
        final String policyNo;
        final long sumAssuredAmt;
        final long loanBalanceAmt;
        final java.time.LocalDate respStartDt;
        final java.time.LocalDate eventDt;
        final String claimStatusKbn;

        ClaimRecord(String claimId, String policyNo, long sumAssuredAmt, long loanBalanceAmt,
                    java.time.LocalDate respStartDt, java.time.LocalDate eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.policyNo = policyNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }
    }

    private static final class BeneficiaryRecord {
        final String policyNo;
        final String beneficiaryId;
        final String nameKana;
        final String relationshipKbn;
        final String bankCd;
        final String branchCd;
        final String acctNo;
        final int paymentPriority;

        BeneficiaryRecord(String policyNo, String beneficiaryId, String nameKana, String relationshipKbn,
                          String bankCd, String branchCd, String acctNo, int paymentPriority) {
            this.policyNo = policyNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.paymentPriority = paymentPriority;
        }
    }

    private static final class RoutingBeneficiary {
        final String policyNo;
        final String beneficiaryId;
        final String nameKana;
        final String relationshipKbn;
        final String bankCd;
        final String branchCd;
        final String acctNo;
        final int paymentPriority;
        final int payRatioPercent;

        RoutingBeneficiary(String policyNo, String beneficiaryId, String nameKana, String relationshipKbn,
                           String bankCd, String branchCd, String acctNo, int paymentPriority, int payRatioPercent) {
            this.policyNo = policyNo;
            this.beneficiaryId = beneficiaryId;
            this.nameKana = nameKana;
            this.relationshipKbn = relationshipKbn;
            this.bankCd = bankCd;
            this.branchCd = branchCd;
            this.acctNo = acctNo;
            this.paymentPriority = paymentPriority;
            this.payRatioPercent = payRatioPercent;
        }
    }
}
