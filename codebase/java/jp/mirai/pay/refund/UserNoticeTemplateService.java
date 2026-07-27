package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数  年月日      担当                                概要
 * 1.00  2025-04-08  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class UserNoticeTemplateService {

    private static final java.util.Locale LOCALE_JA = java.util.Locale.JAPAN;
    private static final java.time.format.DateTimeFormatter DATE_FORMAT =
            java.time.format.DateTimeFormatter.ofPattern("yyyy年M月d日", LOCALE_JA);

    private static final java.util.Map<String, ReasonRule> PRRSNF = createReasonRules();

    public static void main(String[] a) {
        System.out.println("利用者通知文面サービス 起動確認");
    }

    public String buildNotice(RefundModel refundModel, String reasonCode) {
        if (refundModel == null) {
            throw new IllegalArgumentException("返金モデルが未設定です");
        }
        String normalizedReasonCode = normalizeRequired(reasonCode, "返金理由コード");
        ReasonRule rule = PRRSNF.get(normalizedReasonCode);
        if (rule == null) {
            rule = new ReasonRule(normalizedReasonCode, "その他", 50, "1");
        }

        DisplayValues displayValues = toDisplayValues(refundModel);
        NoticeKind noticeKind = judgeNoticeKind(rule);

        String headline;
        String body;
        switch (noticeKind) {
            case ACCEPTED:
                headline = "返金受付のお知らせ";
                body = "お申し出の内容を確認し、返金手続きを受け付けました。";
                break;
            case REVIEW:
                headline = "返金確認中のお知らせ";
                body = "お申し出の内容について確認を進めています。確認完了後、あらためて結果をご案内します。";
                break;
            case MANUAL_REVIEW:
                headline = "返金確認に関するお知らせ";
                body = "返金手続きに必要な確認を行っています。確認完了までしばらくお待ちください。";
                break;
            default:
                throw new IllegalStateException("通知区分が不正です");
        }

        return headline + "\n"
                + body + "\n"
                + "対象取引日：" + displayValues.transactionDateText + "\n"
                + "対象金額：" + displayValues.amountText;
    }

    private static NoticeKind judgeNoticeKind(ReasonRule rule) {
        if ("9".equals(rule.autoReviewKbn)) {
            return NoticeKind.MANUAL_REVIEW;
        }
        if (rule.riskWeight >= 70) {
            return NoticeKind.REVIEW;
        }
        if ("2".equals(rule.autoReviewKbn)) {
            return NoticeKind.REVIEW;
        }
        return NoticeKind.ACCEPTED;
    }

    private static DisplayValues toDisplayValues(RefundModel refundModel) {
        Object amountValue = readProperty(refundModel, "amount", "getAmount", "refundAmount", "getRefundAmount");
        Object transactionDateValue = readProperty(refundModel,
                "transactionDate", "getTransactionDate", "txDate", "getTxDate", "tradeDate", "getTradeDate");

        java.math.BigDecimal amount = toBigDecimal(amountValue);
        java.time.LocalDate transactionDate = toLocalDate(transactionDateValue);

        java.text.NumberFormat currencyFormat = java.text.NumberFormat.getCurrencyInstance(LOCALE_JA);
        currencyFormat.setMaximumFractionDigits(0);
        currencyFormat.setMinimumFractionDigits(0);

        return new DisplayValues(currencyFormat.format(amount), DATE_FORMAT.format(transactionDate));
    }

    private static Object readProperty(Object target, String... methodNames) {
        for (String methodName : methodNames) {
            try {
                java.lang.reflect.Method method = target.getClass().getMethod(methodName);
                return method.invoke(target);
            } catch (NoSuchMethodException e) {
                continue;
            } catch (IllegalAccessException e) {
                throw new IllegalArgumentException("返金モデルの参照権限が不足しています", e);
            } catch (java.lang.reflect.InvocationTargetException e) {
                throw new IllegalArgumentException("返金モデルの参照中にエラーが発生しました", e);
            }
        }
        throw new IllegalArgumentException("返金モデルの表示項目が取得できません");
    }

    private static java.math.BigDecimal toBigDecimal(Object value) {
        if (value == null) {
            throw new IllegalArgumentException("返金金額が未設定です");
        }
        if (value instanceof java.math.BigDecimal) {
            return (java.math.BigDecimal) value;
        }
        if (value instanceof java.math.BigInteger) {
            return new java.math.BigDecimal((java.math.BigInteger) value);
        }
        if (value instanceof Number) {
            return java.math.BigDecimal.valueOf(((Number) value).doubleValue());
        }
        if (value instanceof CharSequence) {
            String text = value.toString().trim();
            if (text.isEmpty()) {
                throw new IllegalArgumentException("返金金額が空です");
            }
            return new java.math.BigDecimal(text);
        }
        throw new IllegalArgumentException("返金金額の型が不正です");
    }

    private static java.time.LocalDate toLocalDate(Object value) {
        if (value == null) {
            throw new IllegalArgumentException("取引日が未設定です");
        }
        if (value instanceof java.time.LocalDate) {
            return (java.time.LocalDate) value;
        }
        if (value instanceof java.time.LocalDateTime) {
            return ((java.time.LocalDateTime) value).toLocalDate();
        }
        if (value instanceof java.util.Date) {
            return java.time.Instant.ofEpochMilli(((java.util.Date) value).getTime())
                    .atZone(java.time.ZoneId.of("Asia/Tokyo"))
                    .toLocalDate();
        }
        if (value instanceof CharSequence) {
            String text = value.toString().trim();
            if (text.matches("\\d{8}")) {
                return java.time.LocalDate.parse(text, java.time.format.DateTimeFormatter.BASIC_ISO_DATE);
            }
            return java.time.LocalDate.parse(text);
        }
        throw new IllegalArgumentException("取引日の型が不正です");
    }

    private static String normalizeRequired(String value, String itemName) {
        if (value == null) {
            throw new IllegalArgumentException(itemName + "が未設定です");
        }
        String normalized = value.trim().toUpperCase(LOCALE_JA);
        if (normalized.isEmpty()) {
            throw new IllegalArgumentException(itemName + "が空です");
        }
        return normalized;
    }

    private static java.util.Map<String, ReasonRule> createReasonRules() {
        java.util.Map<String, ReasonRule> rules = new java.util.LinkedHashMap<String, ReasonRule>();
        rules.put("DUP_PAY", new ReasonRule("DUP_PAY", "二重決済", 20, "1"));
        rules.put("CAN_REV", new ReasonRule("CAN_REV", "取消反映遅延", 35, "1"));
        rules.put("AMT_ERR", new ReasonRule("AMT_ERR", "金額相違", 55, "2"));
        rules.put("MER_RET", new ReasonRule("MER_RET", "加盟店返品", 25, "1"));
        rules.put("SYS_LAT", new ReasonRule("SYS_LAT", "システム遅延", 30, "1"));
        rules.put("CARD_LOST", new ReasonRule("CARD_LOST", "カード紛失申告", 80, "9"));
        rules.put("UNAUTH", new ReasonRule("UNAUTH", "利用覚えなし", 95, "9"));
        return java.util.Collections.unmodifiableMap(rules);
    }

    private enum NoticeKind {
        ACCEPTED,
        REVIEW,
        MANUAL_REVIEW
    }

    private static final class ReasonRule {
        private final String reasonCode;
        private final String reasonGroup;
        private final int riskWeight;
        private final String autoReviewKbn;

        private ReasonRule(String reasonCode, String reasonGroup, int riskWeight, String autoReviewKbn) {
            this.reasonCode = reasonCode;
            this.reasonGroup = reasonGroup;
            this.riskWeight = riskWeight;
            this.autoReviewKbn = autoReviewKbn;
        }
    }

    private static final class DisplayValues {
        private final String amountText;
        private final String transactionDateText;

        private DisplayValues(String amountText, String transactionDateText) {
            this.amountText = amountText;
            this.transactionDateText = transactionDateText;
        }
    }
}
