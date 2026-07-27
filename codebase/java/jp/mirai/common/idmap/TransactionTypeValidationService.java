package jp.mirai.common.idmap;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2025-02-14  共通基盤  初版作成
 * 1.01  2025-04-03  共通基盤  停止中コード判定を追加
 */
public class TransactionTypeValidationService {
    private static final String TYPE_TORIHIKI_STATUS = "TRXSTS";
    private static final String TYPE_CHOHYO_SHUBETSU = "RPTTYP";
    private static final String STATUS_YUKO = "1";
    private static final String STATUS_TEISHI = "9";

    public static void main(String[] a) {
        CmcodfStore cmcodf = new CmcodfStore();
        cmcodf.add(new CmcodfRecord("TRXSTS:01", TYPE_TORIHIKI_STATUS, "01", 20240101, 99991231, STATUS_YUKO));
        cmcodf.add(new CmcodfRecord("TRXSTS:02", TYPE_TORIHIKI_STATUS, "02", 20240101, 99991231, STATUS_YUKO));
        cmcodf.add(new CmcodfRecord("TRXSTS:90", TYPE_TORIHIKI_STATUS, "90", 20230101, 20251231, STATUS_YUKO));
        cmcodf.add(new CmcodfRecord("TRXSTS:99", TYPE_TORIHIKI_STATUS, "99", 20240101, 99991231, STATUS_TEISHI));
        cmcodf.add(new CmcodfRecord("RPTTYP:100", TYPE_CHOHYO_SHUBETSU, "100", 20240101, 99991231, STATUS_YUKO));
        cmcodf.add(new CmcodfRecord("RPTTYP:210", TYPE_CHOHYO_SHUBETSU, "210", 20240401, 99991231, STATUS_YUKO));
        cmcodf.add(new CmcodfRecord("RPTTYP:300", TYPE_CHOHYO_SHUBETSU, "300", 20230101, 20250331, STATUS_YUKO));
        cmcodf.add(new CmcodfRecord("RPTTYP:900", TYPE_CHOHYO_SHUBETSU, "900", 20240101, 99991231, STATUS_TEISHI));

        ValidationBatch batch = new ValidationBatch(cmcodf, 20250630);
        batch.accept(TYPE_TORIHIKI_STATUS, " 01 ");
        batch.accept(TYPE_TORIHIKI_STATUS, "90");
        batch.accept(TYPE_TORIHIKI_STATUS, "99");
        batch.accept(TYPE_TORIHIKI_STATUS, "A1");
        batch.accept(TYPE_CHOHYO_SHUBETSU, "100");
        batch.accept(TYPE_CHOHYO_SHUBETSU, "300");
        batch.accept(TYPE_CHOHYO_SHUBETSU, "900");
        batch.accept(TYPE_CHOHYO_SHUBETSU, "0210");

        BatchSummary summary = batch.finish();
        System.out.println("取引区分検証サービス");
        System.out.println("処理件数=" + summary.total + " 正常=" + summary.accepted + " 否認=" + summary.rejected);
        for (ValidationResult result : summary.results) {
            System.out.println(result.type + " 入力=" + result.inputCode + " 正規化=" + result.normalizedCode
                    + " 判定=" + result.decision + " 理由=" + result.reason);
        }
    }

    private static final class ValidationBatch {
        private final CmcodfStore cmcodf;
        private final int businessDate;
        private final java.util.List<ValidationResult> results = new java.util.ArrayList<ValidationResult>();
        private int accepted;
        private int rejected;

        ValidationBatch(CmcodfStore cmcodf, int businessDate) {
            this.cmcodf = cmcodf;
            this.businessDate = businessDate;
        }

        void accept(String codeType, String rawCode) {
            ValidationResult result = validate(codeType, rawCode);
            results.add(result);
            if ("許可".equals(result.decision)) {
                accepted++;
            } else {
                rejected++;
            }
        }

        BatchSummary finish() {
            return new BatchSummary(results.size(), accepted, rejected,
                    java.util.Collections.unmodifiableList(new java.util.ArrayList<ValidationResult>(results)));
        }

        private ValidationResult validate(String codeType, String rawCode) {
            String normalized = normalize(rawCode);
            if (normalized.length() == 0) {
                return new ValidationResult(codeType, rawCode, normalized, "否認", "コード未設定");
            }
            if (!isSupportedType(codeType)) {
                return new ValidationResult(codeType, rawCode, normalized, "否認", "対象外コード種別");
            }
            if (!isDigits(normalized)) {
                return new ValidationResult(codeType, rawCode, normalized, "否認", "数字以外を含む");
            }

            CmcodfRecord record = cmcodf.find(codeType, normalized);
            if (record == null) {
                return new ValidationResult(codeType, rawCode, normalized, "否認", "標準コード未登録");
            }
            if (!STATUS_YUKO.equals(record.codeStatusKbn)) {
                return new ValidationResult(codeType, rawCode, normalized, "否認", "コード停止中");
            }
            if (businessDate < record.validFrom) {
                return new ValidationResult(codeType, rawCode, normalized, "否認", "適用開始前");
            }
            if (businessDate > record.validTo) {
                return new ValidationResult(codeType, rawCode, normalized, "否認", "適用期限切れ");
            }
            return new ValidationResult(codeType, rawCode, normalized, "許可", "標準コード有効");
        }

        private boolean isSupportedType(String codeType) {
            return TYPE_TORIHIKI_STATUS.equals(codeType) || TYPE_CHOHYO_SHUBETSU.equals(codeType);
        }

        private String normalize(String rawCode) {
            if (rawCode == null) {
                return "";
            }
            String trimmed = rawCode.trim();
            int firstNonZero = 0;
            while (firstNonZero < trimmed.length() - 1 && trimmed.charAt(firstNonZero) == '0') {
                firstNonZero++;
            }
            return trimmed.substring(firstNonZero);
        }

        private boolean isDigits(String value) {
            for (int i = 0; i < value.length(); i++) {
                char c = value.charAt(i);
                if (c < '0' || c > '9') {
                    return false;
                }
            }
            return true;
        }
    }

    private static final class CmcodfStore {
        private final java.util.Map<String, CmcodfRecord> records = new java.util.LinkedHashMap<String, CmcodfRecord>();

        void add(CmcodfRecord record) {
            records.put(record.codeKey, record);
        }

        CmcodfRecord find(String codeType, String codeValue) {
            return records.get(codeType + ":" + codeValue);
        }
    }

    private static final class CmcodfRecord {
        final String codeKey;
        final String codeType;
        final String codeValue;
        final int validFrom;
        final int validTo;
        final String codeStatusKbn;

        CmcodfRecord(String codeKey, String codeType, String codeValue, int validFrom, int validTo, String codeStatusKbn) {
            this.codeKey = codeKey;
            this.codeType = codeType;
            this.codeValue = codeValue;
            this.validFrom = validFrom;
            this.validTo = validTo;
            this.codeStatusKbn = codeStatusKbn;
        }
    }

    private static final class ValidationResult {
        final String type;
        final String inputCode;
        final String normalizedCode;
        final String decision;
        final String reason;

        ValidationResult(String type, String inputCode, String normalizedCode, String decision, String reason) {
            this.type = type;
            this.inputCode = inputCode;
            this.normalizedCode = normalizedCode;
            this.decision = decision;
            this.reason = reason;
        }
    }

    private static final class BatchSummary {
        final int total;
        final int accepted;
        final int rejected;
        final java.util.List<ValidationResult> results;

        BatchSummary(int total, int accepted, int rejected, java.util.List<ValidationResult> results) {
            this.total = total;
            this.accepted = accepted;
            this.rejected = rejected;
            this.results = results;
        }
    }
}
