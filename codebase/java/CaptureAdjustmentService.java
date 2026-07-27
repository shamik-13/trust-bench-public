/**
 * 変更履歴
 * 版数  年月日    担当       概要
 * 1.00  20220819  会員系開発  売上調整受付サービス新規作成
 */
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CaptureAdjustmentService {
    private static final String BILL_FIXED = "C";
    private static final String BILL_HOLD = "H";
    private static final String BILL_EXCLUDED = "S";
    private static final String AUTH_APPROVED = "00";
    private static final String CAPTURE_POSTED = "0";
    private static final String CAPTURE_ADJUSTING = "7";

    public static void main(String[] args) {
        List<SalesRecord> salesFile = new ArrayList<SalesRecord>();
        salesFile.add(new SalesRecord("S000001", "A000001", "4111110000000001", "M10001", "20260601", "20260602", 12000, 1200, CAPTURE_POSTED));
        salesFile.add(new SalesRecord("S000002", "A000002", "4111110000000002", "M10002", "20260603", "20260604", 30000, 3000, CAPTURE_POSTED));
        salesFile.add(new SalesRecord("S000003", "A000003", "4111110000000003", "M10001", "20260605", "20260606", 8000, 800, CAPTURE_ADJUSTING));
        salesFile.add(new SalesRecord("S000004", "A000004", "4111110000000004", "M10003", "20260607", "20260608", 45000, 4500, CAPTURE_POSTED));

        List<AuthRecord> authFile = new ArrayList<AuthRecord>();
        authFile.add(new AuthRecord("A000001", "4111110000000001", "M10001", "20260601", "101530", 13200, "JPY", AUTH_APPROVED, "123456", "20260701"));
        authFile.add(new AuthRecord("A000002", "4111110000000002", "M10002", "20260603", "121000", 33000, "JPY", AUTH_APPROVED, "223344", "20260703"));
        authFile.add(new AuthRecord("A000003", "4111110000000003", "M10001", "20260605", "174500", 8800, "JPY", AUTH_APPROVED, "345678", "20260705"));
        authFile.add(new AuthRecord("A000004", "4111110000000004", "M10003", "20260607", "090010", 49500, "JPY", "05", "", "20260707"));

        List<BillRecord> billFile = new ArrayList<BillRecord>();
        billFile.add(new BillRecord("4111110000000001", "20260620", 13200, 3000, "20260710", BILL_HOLD, "BILL020"));
        billFile.add(new BillRecord("4111110000000002", "20260620", 33000, 6000, "20260710", BILL_FIXED, "BILL020"));
        billFile.add(new BillRecord("4111110000000004", "20260620", 49500, 9000, "20260710", BILL_EXCLUDED, "BILL020"));

        List<AdjustmentRequest> requests = new ArrayList<AdjustmentRequest>();
        requests.add(new AdjustmentRequest("R0001", "MERCHANT", "S000001", "M10001", "4111110000000001", 13200));
        requests.add(new AdjustmentRequest("R0002", "CARDHOLDER", "S000002", "M10002", "4111110000000002", 33000));
        requests.add(new AdjustmentRequest("R0003", "MERCHANT", "S000003", "M10001", "4111110000000003", 8800));
        requests.add(new AdjustmentRequest("R0004", "CARDHOLDER", "S000004", "M10003", "4111110000000004", 49500));

        List<AdjustmentResult> results = processAdjustments(requests, salesFile, authFile, billFile);

        int acceptedCount = 0;
        int offsetCandidateCount = 0;
        int rejectedCount = 0;
        int targetAmount = 0;

        for (AdjustmentResult result : results) {
            System.out.println(result.toDisplayLine());

            if ("ACCEPTED".equals(result.decision)) {
                acceptedCount++;
                targetAmount += result.targetAmount;
            } else if ("OFFSET_CANDIDATE".equals(result.decision)) {
                offsetCandidateCount++;
                targetAmount += result.targetAmount;
            } else {
                rejectedCount++;
            }
        }

        System.out.println("SUMMARY accepted=" + acceptedCount
                + " offsetCandidate=" + offsetCandidateCount
                + " rejected=" + rejectedCount
                + " targetAmount=" + targetAmount);
    }

    public static List<AdjustmentResult> processAdjustments(
            List<AdjustmentRequest> requests,
            List<SalesRecord> salesFile,
            List<AuthRecord> authFile,
            List<BillRecord> billFile) {

        Map<String, AuthRecord> authIndex = createAuthIndex(authFile);
        Map<String, BillRecord> latestBillIndex = createLatestBillIndex(billFile);
        List<AdjustmentResult> results = new ArrayList<AdjustmentResult>();

        for (AdjustmentRequest request : requests) {
            results.add(judgeAndApply(request, salesFile, authIndex, latestBillIndex));
        }

        return results;
    }

    private static AdjustmentResult judgeAndApply(
            AdjustmentRequest request,
            List<SalesRecord> salesFile,
            Map<String, AuthRecord> authIndex,
            Map<String, BillRecord> latestBillIndex) {

        SalesRecord sales = findSales(salesFile, request.salesId);
        if (sales == null) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "NO_ORIGINAL_SALES");
        }

        if (!sales.merchantId.equals(request.merchantId) || !sales.cardNo.equals(request.cardNo)) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "SALES_MISMATCH");
        }

        if (!CAPTURE_POSTED.equals(sales.captureStatus)) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "ALREADY_ACCEPTED_OR_NOT_TARGET");
        }

        AuthRecord auth = authIndex.get(sales.authId);
        if (auth == null) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "NO_AUTH_HISTORY");
        }

        if (!auth.cardNo.equals(sales.cardNo) || !auth.merchantId.equals(sales.merchantId)) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "AUTH_MISMATCH");
        }

        if (!AUTH_APPROVED.equals(auth.authResult)) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "AUTH_DECLINED");
        }

        int grossSalesAmount = sales.salesAmount + sales.taxAmount;
        if (request.claimAmount <= 0 || request.claimAmount > grossSalesAmount) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "INVALID_CLAIM_AMOUNT");
        }

        if (auth.authAmount < grossSalesAmount) {
            return AdjustmentResult.rejected(request.requestId, request.salesId, "AUTH_AMOUNT_SHORTAGE");
        }

        BillRecord bill = latestBillIndex.get(sales.cardNo);
        if (bill != null && BILL_EXCLUDED.equals(bill.billStatus)) {
            return AdjustmentResult.rejected(request.requestId, sales.salesId, "BILL_EXCLUDED");
        }

        if (bill != null && BILL_FIXED.equals(bill.billStatus)) {
            return AdjustmentResult.offsetCandidate(request.requestId, sales.salesId, request.claimAmount, bill);
        }

        sales.captureStatus = CAPTURE_ADJUSTING;
        return AdjustmentResult.accepted(request.requestId, sales.salesId, request.claimAmount);
    }

    private static SalesRecord findSales(List<SalesRecord> salesFile, String salesId) {
        for (SalesRecord sales : salesFile) {
            if (sales.salesId.equals(salesId)) {
                return sales;
            }
        }
        return null;
    }

    private static Map<String, AuthRecord> createAuthIndex(List<AuthRecord> authFile) {
        Map<String, AuthRecord> index = new HashMap<String, AuthRecord>();
        for (AuthRecord auth : authFile) {
            index.put(auth.authId, auth);
        }
        return index;
    }

    private static Map<String, BillRecord> createLatestBillIndex(List<BillRecord> billFile) {
        Map<String, BillRecord> index = new HashMap<String, BillRecord>();
        for (BillRecord bill : billFile) {
            BillRecord current = index.get(bill.cardNo);
            if (current == null || bill.cycleDate.compareTo(current.cycleDate) > 0) {
                index.put(bill.cardNo, bill);
            }
        }
        return index;
    }

    public static final class AdjustmentRequest {
        public final String requestId;
        public final String sourceType;
        public final String salesId;
        public final String merchantId;
        public final String cardNo;
        public final int claimAmount;

        public AdjustmentRequest(String requestId, String sourceType, String salesId, String merchantId, String cardNo, int claimAmount) {
            this.requestId = requestId;
            this.sourceType = sourceType;
            this.salesId = salesId;
            this.merchantId = merchantId;
            this.cardNo = cardNo;
            this.claimAmount = claimAmount;
        }
    }

    public static final class SalesRecord {
        public final String salesId;
        public final String authId;
        public final String cardNo;
        public final String merchantId;
        public final String salesDate;
        public final String postingDate;
        public final int salesAmount;
        public final int taxAmount;
        public String captureStatus;

        public SalesRecord(String salesId, String authId, String cardNo, String merchantId, String salesDate,
                           String postingDate, int salesAmount, int taxAmount, String captureStatus) {
            this.salesId = salesId;
            this.authId = authId;
            this.cardNo = cardNo;
            this.merchantId = merchantId;
            this.salesDate = salesDate;
            this.postingDate = postingDate;
            this.salesAmount = salesAmount;
            this.taxAmount = taxAmount;
            this.captureStatus = captureStatus;
        }
    }

    public static final class AuthRecord {
        public final String authId;
        public final String cardNo;
        public final String merchantId;
        public final String authDate;
        public final String authTime;
        public final int authAmount;
        public final String currencyCode;
        public final String authResult;
        public final String approvalCode;
        public final String holdExpireDate;

        public AuthRecord(String authId, String cardNo, String merchantId, String authDate, String authTime,
                          int authAmount, String currencyCode, String authResult, String approvalCode, String holdExpireDate) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.merchantId = merchantId;
            this.authDate = authDate;
            this.authTime = authTime;
            this.authAmount = authAmount;
            this.currencyCode = currencyCode;
            this.authResult = authResult;
            this.approvalCode = approvalCode;
            this.holdExpireDate = holdExpireDate;
        }
    }

    public static final class BillRecord {
        public final String cardNo;
        public final String cycleDate;
        public final int billAmount;
        public final int minPayAmount;
        public final String dueDate;
        public final String billStatus;
        public final String programId;

        public BillRecord(String cardNo, String cycleDate, int billAmount, int minPayAmount, String dueDate,
                          String billStatus, String programId) {
            this.cardNo = cardNo;
            this.cycleDate = cycleDate;
            this.billAmount = billAmount;
            this.minPayAmount = minPayAmount;
            this.dueDate = dueDate;
            this.billStatus = billStatus;
            this.programId = programId;
        }
    }

    public static final class AdjustmentResult {
        public final String requestId;
        public final String salesId;
        public final String decision;
        public final int targetAmount;
        public final String reason;
        public final String cycleDate;
        public final String dueDate;
        public final String programId;

        private AdjustmentResult(String requestId, String salesId, String decision, int targetAmount,
                                 String reason, String cycleDate, String dueDate, String programId) {
            this.requestId = requestId;
            this.salesId = salesId;
            this.decision = decision;
            this.targetAmount = targetAmount;
            this.reason = reason;
            this.cycleDate = cycleDate;
            this.dueDate = dueDate;
            this.programId = programId;
        }

        public static AdjustmentResult accepted(String requestId, String salesId, int targetAmount) {
            return new AdjustmentResult(requestId, salesId, "ACCEPTED", targetAmount,
                    "UNBILLED_DETAIL_UPDATED", "", "", "");
        }

        public static AdjustmentResult offsetCandidate(String requestId, String salesId, int targetAmount, BillRecord bill) {
            return new AdjustmentResult(requestId, salesId, "OFFSET_CANDIDATE", targetAmount,
                    "BILL_ALREADY_FIXED", bill.cycleDate, bill.dueDate, bill.programId);
        }

        public static AdjustmentResult rejected(String requestId, String salesId, String reason) {
            return new AdjustmentResult(requestId, salesId, "REJECTED", 0, reason, "", "", "");
        }

        public String toDisplayLine() {
            String billReference = cycleDate.length() == 0
                    ? ""
                    : " cycleDate=" + cycleDate + " dueDate=" + dueDate + " program=" + programId;

            return "request=" + requestId
                    + " sales=" + salesId
                    + " decision=" + decision
                    + " amount=" + targetAmount
                    + " reason=" + reason
                    + billReference;
        }
    }
}
