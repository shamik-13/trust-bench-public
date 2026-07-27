package jp.mirai.life.claims;

public class AssessmentCaseRouter {
    private static final String AUTH_LEVEL_TANTOSHA = "01";
    private static final String AUTH_LEVEL_KACHO = "02";
    private static final String AUTH_LEVEL_YAKUIN = "03";
    private static final String CLAIM_STATUS_PAYABLE = "01";

    private static final long KACHO_THRESHOLD_YEN = 5_000_000L;
    private static final long YAKUIN_THRESHOLD_YEN = 50_000_000L;

    private static final int PAYMENT_RATIO_AFTER_ONE_YEAR_PERCENT = 100;

    public int routePendingAssessments(
            Iterable<?> lfrasfRecords,
            Iterable<?> lfclmfRecords,
            Object lfrasfWriter) {

        if (lfrasfRecords == null) {
            throw new IllegalArgumentException("LFRASF入力が未指定です。");
        }
        if (lfclmfRecords == null) {
            throw new IllegalArgumentException("LFCLMF入力が未指定です。");
        }
        if (lfrasfWriter == null) {
            throw new IllegalArgumentException("LFRASF更新先が未指定です。");
        }

        java.util.Map<String, Object> claimById = new java.util.HashMap<String, Object>();
        for (Object claim : lfclmfRecords) {
            validateClaimMaster(claim);
            claimById.put(textValue(claim, "claimId"), claim);
        }

        int updatedCount = 0;
        for (Object assessment : lfrasfRecords) {
            validateAssessment(assessment);

            String currentAuthLevel = textValue(assessment, "authLevelKbn");
            if (!AUTH_LEVEL_TANTOSHA.equals(currentAuthLevel)) {
                continue;
            }

            String claimId = textValue(assessment, "claimId");
            Object claim = claimById.get(claimId);
            if (claim == null) {
                throw new IllegalStateException("請求IDに対応するLFCLMFが存在しません。請求ID=" + claimId);
            }

            String nextAuthLevel = decideAuthLevel(claim);
            Object recordToWrite = assessment;
            if (!nextAuthLevel.equals(currentAuthLevel)) {
                recordToWrite = withAuthLevelKbn(assessment, nextAuthLevel);
                updatedCount++;
            }
            writeRecord(lfrasfWriter, recordToWrite);
        }

        return updatedCount;
    }

    private static String decideAuthLevel(Object claim) {
        if (!CLAIM_STATUS_PAYABLE.equals(textValue(claim, "claimStatusKbn"))) {
            throw new IllegalStateException("支払対象外の請求は査定案件ルータで昇格判定できません。請求ID="
                    + textValue(claim, "claimId"));
        }

        long sumAssuredAmount = longValue(claim, "sumAssuredAmt");
        if (sumAssuredAmount < KACHO_THRESHOLD_YEN) {
            return AUTH_LEVEL_TANTOSHA;
        }
        if (sumAssuredAmount <= YAKUIN_THRESHOLD_YEN) {
            return AUTH_LEVEL_KACHO;
        }
        return AUTH_LEVEL_YAKUIN;
    }

    private static void validateAssessment(Object assessment) {
        if (assessment == null) {
            throw new IllegalArgumentException("LFRASFレコードがnullです。");
        }

        requireText(textValue(assessment, "assessId"), "査定ID");
        requireText(textValue(assessment, "claimId"), "請求ID");
        requireText(textValue(assessment, "assessDt"), "査定日");
        requireText(textValue(assessment, "categoryKbn"), "分類区分");

        String authLevel = textValue(assessment, "authLevelKbn");
        requireText(authLevel, "決裁権限区分");
        requireText(textValue(assessment, "resultKbn"), "査定結果区分");
        requireText(textValue(assessment, "assessorId"), "査定担当者ID");

        if (!AUTH_LEVEL_TANTOSHA.equals(authLevel)
                && !AUTH_LEVEL_KACHO.equals(authLevel)
                && !AUTH_LEVEL_YAKUIN.equals(authLevel)) {
            throw new IllegalArgumentException("決裁権限区分が不正です。査定ID="
                    + textValue(assessment, "assessId"));
        }
    }

    private static void validateClaimMaster(Object claim) {
        if (claim == null) {
            throw new IllegalArgumentException("LFCLMFレコードがnullです。");
        }

        requireText(textValue(claim, "claimId"), "請求ID");
        requireText(textValue(claim, "polNo"), "証券番号");
        requireText(textValue(claim, "respStartDt"), "責任開始日");
        requireText(textValue(claim, "eventDt"), "事故日");

        String status = textValue(claim, "claimStatusKbn");
        requireText(status, "請求状態区分");

        if (longValue(claim, "sumAssuredAmt") < 0L) {
            throw new IllegalArgumentException("保険金額が不正です。請求ID=" + textValue(claim, "claimId"));
        }
        if (longValue(claim, "loanBalanceAmt") < 0L) {
            throw new IllegalArgumentException("貸付残高が不正です。請求ID=" + textValue(claim, "claimId"));
        }
        if (!CLAIM_STATUS_PAYABLE.equals(status) && !"05".equals(status) && !"09".equals(status)) {
            throw new IllegalArgumentException("請求状態区分が不正です。請求ID=" + textValue(claim, "claimId"));
        }
    }

    private static void requireText(String value, String itemName) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(itemName + "が未設定です。");
        }
    }

    private static Object withAuthLevelKbn(Object record, String authLevelKbn) {
        try {
            java.lang.reflect.Method method = record.getClass().getMethod("withAuthLevelKbn", String.class);
            return method.invoke(record, authLevelKbn);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("LFRASFレコードの決裁権限区分を更新できません。", e);
        }
    }

    private static void writeRecord(Object writer, Object record) {
        try {
            java.lang.reflect.Method method = findSingleArgumentMethod(writer.getClass(), "write", record);
            method.invoke(writer, record);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("LFRASFレコードを書き込めません。", e);
        }
    }

    private static java.lang.reflect.Method findSingleArgumentMethod(
            Class<?> type,
            String methodName,
            Object argument) throws NoSuchMethodException {

        Class<?> argumentType = argument == null ? Object.class : argument.getClass();
        for (java.lang.reflect.Method method : type.getMethods()) {
            Class<?>[] parameterTypes = method.getParameterTypes();
            if (methodName.equals(method.getName())
                    && parameterTypes.length == 1
                    && parameterTypes[0].isAssignableFrom(argumentType)) {
                return method;
            }
        }
        throw new NoSuchMethodException(type.getName() + "." + methodName);
    }

    private static String textValue(Object target, String propertyName) {
        Object value = propertyValue(target, propertyName);
        return value == null ? null : String.valueOf(value);
    }

    private static long longValue(Object target, String propertyName) {
        Object value = propertyValue(target, propertyName);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        if (value instanceof String) {
            return Long.parseLong((String) value);
        }
        throw new IllegalArgumentException(propertyName + "が数値ではありません。");
    }

    private static Object propertyValue(Object target, String propertyName) {
        try {
            java.lang.reflect.Method method = target.getClass().getMethod(propertyName);
            return method.invoke(target);
        } catch (ReflectiveOperationException firstFailure) {
            try {
                java.lang.reflect.Field field = target.getClass().getField(propertyName);
                return field.get(target);
            } catch (ReflectiveOperationException secondFailure) {
                throw new IllegalArgumentException("必要な項目が存在しません。" + propertyName, secondFailure);
            }
        }
    }

    public static int paymentRatioAfterOneYearPercent() {
        return PAYMENT_RATIO_AFTER_ONE_YEAR_PERCENT;
    }
}
