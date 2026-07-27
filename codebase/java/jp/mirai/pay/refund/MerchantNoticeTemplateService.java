package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2024-04-01  業務基盤     初版作成
 * 1.01  2024-09-18  返金運用     自動審査区分とリスク重みによる文面分岐を追加
 * 1.02  2025-02-07  加盟店精算   取引IDマスクと金額表示規則を追加
 */
public class MerchantNoticeTemplateService {

    private static final String GROUP_CUSTOMER = "利用者都合";
    private static final String GROUP_STORE = "加盟店都合";
    private static final String GROUP_SYSTEM = "システム都合";
    private static final String GROUP_RISK = "リスク審査";
    private static final String GROUP_OTHER = "その他";

    public NoticeResult decide(String reasonCode, String hanteiKbn, long amountYen, String transactionId) {
        String code = normalize(reasonCode);
        String kbn = normalize(hanteiKbn);

        if (code.isEmpty()) {
            throw new IllegalArgumentException("理由コードが未設定です。");
        }
        if (amountYen < 0) {
            throw new IllegalArgumentException("金額が不正です。");
        }
        if (transactionId == null || transactionId.trim().isEmpty()) {
            throw new IllegalArgumentException("取引IDが未設定です。");
        }

        PrRsnf reason = readPrRsnf(code);
        String templateId = selectTemplate(reason, kbn, amountYen);

        return new NoticeResult(
                templateId,
                reason.reasonGroup,
                formatAmount(amountYen),
                maskTransactionId(transactionId)
        );
    }

    public static void main(String[] a) {
        MerchantNoticeTemplateService service = new MerchantNoticeTemplateService();
        NoticeResult result = service.decide("C101", "承認", 12800L, "TRX2025062800012345");
        System.out.println(result.toLine());
    }

    private PrRsnf readPrRsnf(String reasonCode) {
        switch (reasonCode) {
            case "C101":
            case "C102":
            case "C199":
                return new PrRsnf(reasonCode, GROUP_CUSTOMER, 10, "可");
            case "M201":
            case "M202":
                return new PrRsnf(reasonCode, GROUP_STORE, 25, "可");
            case "S301":
            case "S302":
                return new PrRsnf(reasonCode, GROUP_SYSTEM, 35, "否");
            case "R401":
            case "R402":
            case "R499":
                return new PrRsnf(reasonCode, GROUP_RISK, 90, "否");
            default:
                return new PrRsnf(reasonCode, GROUP_OTHER, 50, "否");
        }
    }

    private String selectTemplate(PrRsnf reason, String hanteiKbn, long amountYen) {
        if ("否認".equals(hanteiKbn) || reason.riskWeight >= 80) {
            return "MN-RISK-HOLD-01";
        }
        if ("差戻".equals(hanteiKbn)) {
            return "MN-REVIEW-BACK-01";
        }
        if ("否".equals(reason.autoReviewKbn) && amountYen >= 100000L) {
            return "MN-MANUAL-HIGH-01";
        }
        if (GROUP_SYSTEM.equals(reason.reasonGroup)) {
            return "MN-SYS-REFUND-01";
        }
        if (GROUP_STORE.equals(reason.reasonGroup)) {
            return "MN-MERCHANT-REFUND-01";
        }
        if (GROUP_CUSTOMER.equals(reason.reasonGroup)) {
            return "MN-CUSTOMER-REFUND-01";
        }
        return "MN-GENERAL-REFUND-01";
    }

    private String formatAmount(long amountYen) {
        String source = Long.toString(amountYen);
        StringBuilder builder = new StringBuilder();
        int count = 0;
        for (int i = source.length() - 1; i >= 0; i--) {
            if (count == 3) {
                builder.append(',');
                count = 0;
            }
            builder.append(source.charAt(i));
            count++;
        }
        return builder.reverse().insert(0, "¥").toString();
    }

    private String maskTransactionId(String transactionId) {
        String value = transactionId.trim();
        if (value.length() <= 8) {
            return repeat("*", value.length());
        }
        String head = value.substring(0, 4);
        String tail = value.substring(value.length() - 4);
        return head + repeat("*", value.length() - 8) + tail;
    }

    private String normalize(String value) {
        return value == null ? "" : value.trim();
    }

    private String repeat(String value, int count) {
        StringBuilder builder = new StringBuilder(value.length() * count);
        for (int i = 0; i < count; i++) {
            builder.append(value);
        }
        return builder.toString();
    }

    private static final class PrRsnf {
        private final String reasonCode;
        private final String reasonGroup;
        private final int riskWeight;
        private final String autoReviewKbn;

        private PrRsnf(String reasonCode, String reasonGroup, int riskWeight, String autoReviewKbn) {
            this.reasonCode = reasonCode;
            this.reasonGroup = reasonGroup;
            this.riskWeight = riskWeight;
            this.autoReviewKbn = autoReviewKbn;
        }
    }

    public static final class NoticeResult {
        private final String templateId;
        private final String reasonGroup;
        private final String amountText;
        private final String maskedTransactionId;

        private NoticeResult(String templateId, String reasonGroup, String amountText, String maskedTransactionId) {
            this.templateId = templateId;
            this.reasonGroup = reasonGroup;
            this.amountText = amountText;
            this.maskedTransactionId = maskedTransactionId;
        }

        public String templateId() {
            return templateId;
        }

        public String reasonGroup() {
            return reasonGroup;
        }

        public String amountText() {
            return amountText;
        }

        public String maskedTransactionId() {
            return maskedTransactionId;
        }

        private String toLine() {
            return templateId + "," + reasonGroup + "," + amountText + "," + maskedTransactionId;
        }
    }
}
