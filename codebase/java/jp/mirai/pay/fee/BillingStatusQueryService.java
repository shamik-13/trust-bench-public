package jp.mirai.pay.fee;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2024-07-22  みらいペイ システム部 加盟店・手数料チーム  初版作成
 */
public class BillingStatusQueryService {
    private static final String CHARGEABLE_STATUS = "01";
    private static final String STATUS_UNPAID = "01";
    private static final String STATUS_ISSUED = "02";
    private static final String STATUS_PAID = "03";
    private static final String STATUS_CANCELLED = "09";

    public static void main(String[] a) {
        Repository repo = Repository.synthetic();
        BillingStatusQueryService service = new BillingStatusQueryService(repo);

        try {
            QueryResult result;
            if (a.length == 1) {
                result = service.findByBillId(a[0]);
            } else if (a.length == 2) {
                result = service.findByMerchantMonth(a[0], a[1]);
            } else {
                throw new IllegalArgumentException("引数は請求ID、または加盟店コードと請求年月を指定してください。");
            }
            System.out.println(result.toOperatorLine());
        } catch (IllegalArgumentException | IllegalStateException e) {
            System.err.println("照会エラー: " + e.getMessage());
            System.exit(1);
        }
    }

    private final Repository repo;

    private BillingStatusQueryService(Repository repo) {
        this.repo = repo;
    }

    private QueryResult findByBillId(String billId) {
        String normalizedBillId = requireCode("請求ID", billId);
        Pfbilf bill = repo.findBill(normalizedBillId);
        if (bill == null) {
            throw new IllegalArgumentException("請求が存在しません。請求ID=" + normalizedBillId);
        }
        return buildResult(new Pfbilf[] { bill });
    }

    private QueryResult findByMerchantMonth(String merchantCode, String billingMonth) {
        String normalizedMerchantCode = requireCode("加盟店コード", merchantCode);
        String normalizedMonth = requireMonth(billingMonth);

        Pfmerf merchant = repo.findMerchant(normalizedMerchantCode);
        if (merchant == null) {
            throw new IllegalArgumentException("加盟店が存在しません。加盟店コード=" + normalizedMerchantCode);
        }
        if (!CHARGEABLE_STATUS.equals(merchant.merStatus)) {
            throw new IllegalStateException("課金対象外の加盟店です。加盟店コード=" + normalizedMerchantCode + " 状態=" + merchant.merStatus);
        }

        Pfbilf[] bills = repo.findBills(normalizedMerchantCode, normalizedMonth);
        if (bills.length == 0) {
            throw new IllegalArgumentException("対象年月の請求が存在しません。加盟店コード=" + normalizedMerchantCode + " 請求年月=" + normalizedMonth);
        }
        return buildResult(bills);
    }

    private QueryResult buildResult(Pfbilf[] bills) {
        long feeTotal = 0;
        long taxTotal = 0;
        String dueDate = null;
        boolean hasInvoice = false;
        boolean reissuable = true;
        String merchantCode = bills[0].merchantCode;
        String billingMonth = bills[0].billingMonth;

        for (Pfbilf bill : bills) {
            if (!merchantCode.equals(bill.merchantCode)) {
                throw new IllegalStateException("加盟店が混在しています。");
            }
            if (!billingMonth.equals(bill.billingMonth)) {
                throw new IllegalStateException("請求年月が混在しています。");
            }

            feeTotal = Math.addExact(feeTotal, bill.feeTotalAmt);
            taxTotal = Math.addExact(taxTotal, bill.taxAmt);
            dueDate = earliestDate(dueDate, bill.dueDt);
            hasInvoice |= repo.hasInvoice(bill.billId);
            reissuable &= isReissuable(bill.status);
        }

        Pfmerf merchant = repo.findMerchant(merchantCode);
        String merchantName = merchant == null ? "" : merchant.merchantName;
        String category = merchant == null ? "" : merchant.merCategory;

        return new QueryResult(merchantCode, merchantName, category, billingMonth, feeTotal, taxTotal, dueDate, hasInvoice, reissuable);
    }

    private static boolean isReissuable(String status) {
        return STATUS_UNPAID.equals(status) || STATUS_ISSUED.equals(status);
    }

    private static String earliestDate(String current, String candidate) {
        if (candidate == null || candidate.isBlank()) {
            return current;
        }
        if (current == null || candidate.compareTo(current) < 0) {
            return candidate;
        }
        return current;
    }

    private static String requireCode(String label, String value) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(label + "が未指定です。");
        }
        String trimmed = value.trim();
        if (!trimmed.matches("[0-9A-Z\\-]{4,20}")) {
            throw new IllegalArgumentException(label + "の形式が不正です。" + label + "=" + trimmed);
        }
        return trimmed;
    }

    private static String requireMonth(String value) {
        if (value == null || !value.matches("[0-9]{6}")) {
            throw new IllegalArgumentException("請求年月の形式が不正です。請求年月=" + value);
        }
        int month = Integer.parseInt(value.substring(4, 6));
        if (month < 1 || month > 12) {
            throw new IllegalArgumentException("請求年月の月が不正です。請求年月=" + value);
        }
        return value;
    }

    private static final class Repository {
        private final Pfbilf[] bills;
        private final Pfinvf[] invoices;
        private final Pfmerf[] merchants;

        private Repository(Pfbilf[] bills, Pfinvf[] invoices, Pfmerf[] merchants) {
            this.bills = bills;
            this.invoices = invoices;
            this.merchants = merchants;
        }

        private static Repository synthetic() {
            return new Repository(
                new Pfbilf[] {
                    new Pfbilf("BIL-202604-0001", "MRC10001", "202604", 128000L, 12800L, STATUS_ISSUED, "2026-05-31"),
                    new Pfbilf("BIL-202604-0002", "MRC10002", "202604", 78500L, 7850L, STATUS_PAID, "2026-05-31"),
                    new Pfbilf("BIL-202605-0001", "MRC10001", "202605", 141200L, 14120L, STATUS_UNPAID, "2026-06-30"),
                    new Pfbilf("BIL-202605-0003", "MRC30001", "202605", 19200L, 1920L, STATUS_CANCELLED, "2026-06-30")
                },
                new Pfinvf[] {
                    new Pfinvf("INV-202604-0001", "BIL-202604-0001", "MRC10001", "T7010001999999", "2026-05-02", "10%=12800"),
                    new Pfinvf("INV-202604-0002", "BIL-202604-0002", "MRC10002", "T7010001888888", "2026-05-02", "10%=7850")
                },
                new Pfmerf[] {
                    new Pfmerf("MRC10001", "未来堂銀座店", "C1", "01"),
                    new Pfmerf("MRC10002", "青葉食堂", "C2", "01"),
                    new Pfmerf("MRC30001", "北都公金収納", "C3", "02"),
                    new Pfmerf("MRC90001", "湾岸通販", "C4", "09")
                }
            );
        }

        private Pfbilf findBill(String billId) {
            for (Pfbilf bill : bills) {
                if (bill.billId.equals(billId)) {
                    return bill;
                }
            }
            return null;
        }

        private Pfbilf[] findBills(String merchantCode, String billingMonth) {
            int count = 0;
            for (Pfbilf bill : bills) {
                if (bill.merchantCode.equals(merchantCode) && bill.billingMonth.equals(billingMonth)) {
                    count++;
                }
            }
            Pfbilf[] found = new Pfbilf[count];
            int index = 0;
            for (Pfbilf bill : bills) {
                if (bill.merchantCode.equals(merchantCode) && bill.billingMonth.equals(billingMonth)) {
                    found[index++] = bill;
                }
            }
            return found;
        }

        private boolean hasInvoice(String billId) {
            for (Pfinvf invoice : invoices) {
                if (invoice.billId.equals(billId)) {
                    return true;
                }
            }
            return false;
        }

        private Pfmerf findMerchant(String merchantCode) {
            for (Pfmerf merchant : merchants) {
                if (merchant.merchantCode.equals(merchantCode)) {
                    return merchant;
                }
            }
            return null;
        }
    }

    private static final class QueryResult {
        private final String merchantCode;
        private final String merchantName;
        private final String category;
        private final String billingMonth;
        private final long feeTotalAmt;
        private final long taxAmt;
        private final String dueDate;
        private final boolean invoiceIssued;
        private final boolean reissuable;

        private QueryResult(String merchantCode, String merchantName, String category, String billingMonth, long feeTotalAmt, long taxAmt, String dueDate, boolean invoiceIssued, boolean reissuable) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.category = category;
            this.billingMonth = billingMonth;
            this.feeTotalAmt = feeTotalAmt;
            this.taxAmt = taxAmt;
            this.dueDate = dueDate;
            this.invoiceIssued = invoiceIssued;
            this.reissuable = reissuable;
        }

        private String toOperatorLine() {
            return "加盟店=" + merchantCode
                + " 名称=" + merchantName
                + " 業種=" + category
                + " 請求年月=" + billingMonth
                + " 請求額=" + feeTotalAmt
                + " 税額=" + taxAmt
                + " 期日=" + dueDate
                + " インボイス発行済=" + (invoiceIssued ? "有" : "無")
                + " 再発行可否=" + (reissuable ? "可" : "不可");
        }
    }

    private static final class Pfbilf {
        private final String billId;
        private final String merchantCode;
        private final String billingMonth;
        private final long feeTotalAmt;
        private final long taxAmt;
        private final String status;
        private final String dueDt;

        private Pfbilf(String billId, String merchantCode, String billingMonth, long feeTotalAmt, long taxAmt, String status, String dueDt) {
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.billingMonth = billingMonth;
            this.feeTotalAmt = feeTotalAmt;
            this.taxAmt = taxAmt;
            this.status = status;
            this.dueDt = dueDt;
        }
    }

    private static final class Pfinvf {
        private final String invoiceId;
        private final String billId;
        private final String merchantCode;
        private final String qualifiedInvoiceNo;
        private final String issueDt;
        private final String taxBreakdown;

        private Pfinvf(String invoiceId, String billId, String merchantCode, String qualifiedInvoiceNo, String issueDt, String taxBreakdown) {
            this.invoiceId = invoiceId;
            this.billId = billId;
            this.merchantCode = merchantCode;
            this.qualifiedInvoiceNo = qualifiedInvoiceNo;
            this.issueDt = issueDt;
            this.taxBreakdown = taxBreakdown;
        }
    }

    private static final class Pfmerf {
        private final String merchantCode;
        private final String merchantName;
        private final String merCategory;
        private final String merStatus;

        private Pfmerf(String merchantCode, String merchantName, String merCategory, String merStatus) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merCategory = merCategory;
            this.merStatus = merStatus;
        }
    }
}
