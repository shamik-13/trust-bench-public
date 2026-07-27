package jp.mirai.pay.fee;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class QualifiedInvoiceTaxService {
    private static final String CHARGEABLE_STATUS = "01";
    private static final String TAXABLE = "1";
    private static final BigDecimal TAX_RATE_10 = new BigDecimal("0.10");
    private static final BigDecimal TAX_RATE_0 = BigDecimal.ZERO;

    public static void main(String[] a) throws Exception {
        Path pfbilf = Path.of(a.length > 0 ? a[0] : "PFBILF.csv");
        Path pffeef = Path.of(a.length > 1 ? a[1] : "PFFEEF.csv");
        Path pmcatf = Path.of(a.length > 2 ? a[2] : "PMCATF.csv");
        Path pfinvf = Path.of(a.length > 3 ? a[3] : "PFINVF.csv");

        QualifiedInvoiceTaxService service = new QualifiedInvoiceTaxService();
        List<PFBILF> bills = service.readPfbilf(pfbilf);
        List<PFFEEF> fees = service.readPffeef(pffeef);
        Map<String, PMCATF> categories = service.readPmcatf(pmcatf);

        List<PFINVF> invoices = service.createInvoices(bills, fees, categories, LocalDate.now());
        service.writePfinvf(pfinvf, invoices);
        System.out.println("PFINVF出力件数=" + invoices.size());
    }

    List<PFINVF> createInvoices(
            List<PFBILF> bills,
            List<PFFEEF> fees,
            Map<String, PMCATF> categories,
            LocalDate issueDate) {
        Map<String, List<PFFEEF>> feesByMerchant = new LinkedHashMap<>();
        for (PFFEEF fee : fees) {
            if (fee.feeAmt() < 0) {
                throw new IllegalArgumentException("手数料金額が不正です: " + fee.feeId());
            }
            feesByMerchant.computeIfAbsent(fee.merchantCode(), k -> new ArrayList<>()).add(fee);
        }

        List<PFINVF> invoices = new ArrayList<>();
        for (PFBILF bill : bills) {
            if (!CHARGEABLE_STATUS.equals(bill.status())) {
                continue;
            }
            if (bill.feeTotalAmt() < 0 || bill.taxAmt() < 0) {
                throw new IllegalArgumentException("請求金額が不正です: " + bill.billId());
            }

            Map<String, TaxBucket> buckets = new LinkedHashMap<>();
            buckets.put("10", new TaxBucket(TAX_RATE_10));
            buckets.put("00", new TaxBucket(TAX_RATE_0));

            for (PFFEEF fee : feesByMerchant.getOrDefault(bill.merchantCode(), List.of())) {
                PMCATF category = categories.get(categoryCodeFromMerchant(fee.merchantCode()));
                if (category == null || !"1".equals(category.activeFlag())) {
                    continue;
                }

                String taxClass = TAXABLE.equals(category.taxableFlag()) ? "10" : "00";
                buckets.get(taxClass).add(fee.feeAmt());
            }

            long totalBase = buckets.get("10").baseAmount + buckets.get("00").baseAmount;
            if (totalBase != bill.feeTotalAmt()) {
                buckets.get("10").baseAmount = bill.feeTotalAmt();
                buckets.get("00").baseAmount = 0;
            }

            invoices.add(new PFINVF(
                    invoiceId(bill, issueDate),
                    bill.billId(),
                    bill.merchantCode(),
                    qualifiedInvoiceNo(bill.merchantCode()),
                    issueDate.toString(),
                    taxBreakdownText(buckets)));
        }
        return invoices;
    }

    private String taxBreakdownText(Map<String, TaxBucket> buckets) {
        List<String> parts = new ArrayList<>();
        for (Map.Entry<String, TaxBucket> entry : buckets.entrySet()) {
            TaxBucket bucket = entry.getValue();
            if (bucket.baseAmount == 0) {
                continue;
            }
            long tax = BigDecimal.valueOf(bucket.baseAmount)
                    .multiply(bucket.rate)
                    .setScale(0, RoundingMode.DOWN)
                    .longValueExact();
            parts.add(entry.getKey() + ":" + bucket.baseAmount + ":" + tax);
        }
        return String.join("|", parts);
    }

    private String invoiceId(PFBILF bill, LocalDate issueDate) {
        YearMonth ym = YearMonth.parse(bill.billingMonth());
        return "INV-" + ym.toString().replace("-", "") + "-" + bill.billId() + "-"
                + issueDate.toString().replace("-", "");
    }

    private String qualifiedInvoiceNo(String merchantCode) {
        long numeric = Math.abs((long) merchantCode.hashCode());
        return "T" + String.format("%013d", numeric % 10_000_000_000_000L);
    }

    private String categoryCodeFromMerchant(String merchantCode) {
        if (merchantCode == null || merchantCode.length() < 2) {
            return "C1";
        }
        String prefix = merchantCode.substring(0, 2);
        return switch (prefix) {
            case "C1", "C2", "C3", "C4", "C5" -> prefix;
            default -> "C1";
        };
    }

    private List<PFBILF> readPfbilf(Path path) throws Exception {
        List<PFBILF> rows = new ArrayList<>();
        for (String line : Files.readAllLines(path, StandardCharsets.UTF_8)) {
            String s = line.trim();
            if (s.isEmpty() || s.startsWith("#") || s.startsWith("BILL-ID")) {
                continue;
            }
            String[] f = s.split(",", -1);
            if (f.length < 7) {
                throw new IllegalArgumentException("PFBILF項目数不正: " + s);
            }
            rows.add(new PFBILF(
                    f[0].trim(),
                    f[1].trim(),
                    f[2].trim(),
                    Long.parseLong(f[3].trim()),
                    Long.parseLong(f[4].trim()),
                    f[5].trim(),
                    f[6].trim()));
        }
        return rows;
    }

    private List<PFFEEF> readPffeef(Path path) throws Exception {
        List<PFFEEF> rows = new ArrayList<>();
        for (String line : Files.readAllLines(path, StandardCharsets.UTF_8)) {
            String s = line.trim();
            if (s.isEmpty() || s.startsWith("#") || s.startsWith("FEE-ID")) {
                continue;
            }
            String[] f = s.split(",", -1);
            if (f.length < 5) {
                throw new IllegalArgumentException("PFFEEF項目数不正: " + s);
            }
            rows.add(new PFFEEF(
                    f[0].trim(),
                    f[1].trim(),
                    Long.parseLong(f[2].trim()),
                    new BigDecimal(f[3].trim()),
                    Long.parseLong(f[4].trim())));
        }
        return rows;
    }

    private Map<String, PMCATF> readPmcatf(Path path) throws Exception {
        Map<String, PMCATF> rows = new LinkedHashMap<>();
        for (String line : Files.readAllLines(path, StandardCharsets.UTF_8)) {
            String s = line.trim();
            if (s.isEmpty() || s.startsWith("#") || s.startsWith("CATEGORY-CODE")) {
                continue;
            }
            String[] f = s.split(",", -1);
            if (f.length < 6) {
                throw new IllegalArgumentException("PMCATF項目数不正: " + s);
            }
            PMCATF row = new PMCATF(
                    f[0].trim(),
                    f[1].trim(),
                    f[2].trim(),
                    f[3].trim(),
                    f[4].trim(),
                    f[5].trim());
            rows.put(row.categoryCode(), row);
        }
        return rows;
    }

    private void writePfinvf(Path path, List<PFINVF> rows) throws Exception {
        List<String> lines = new ArrayList<>();
        lines.add("INVOICE-ID,BILL-ID,MERCHANT-CODE,QUALIFIED-INVOICE-NO,ISSUE-DT,TAX-BREAKDOWN");
        for (PFINVF row : rows) {
            lines.add(String.join(",",
                    row.invoiceId(),
                    row.billId(),
                    row.merchantCode(),
                    row.qualifiedInvoiceNo(),
                    row.issueDt(),
                    row.taxBreakdown()));
        }
        Files.write(path, lines, StandardCharsets.UTF_8);
    }

    private static final class TaxBucket {
        private final BigDecimal rate;
        private long baseAmount;

        private TaxBucket(BigDecimal rate) {
            this.rate = rate;
        }

        private void add(long amount) {
            baseAmount += amount;
        }
    }
}

record PFBILF(
        String billId,
        String merchantCode,
        String billingMonth,
        long feeTotalAmt,
        long taxAmt,
        String status,
        String createdAt) {
}

record PFFEEF(
        String feeId,
        String merchantCode,
        long feeAmt,
        BigDecimal taxRate,
        long settledAt) {
}

record PMCATF(
        String categoryCode,
        String categoryName,
        String taxableFlag,
        String accountCode,
        String activeFlag,
        String updatedAt) {
}

record PFINVF(
        String invoiceId,
        String billId,
        String merchantCode,
        String qualifiedInvoiceNo,
        String issueDt,
        String taxBreakdown) {
}
