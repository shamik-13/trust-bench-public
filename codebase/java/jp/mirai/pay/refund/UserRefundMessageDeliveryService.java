package jp.mirai.pay.refund;

/**
 * 変更履歴
 * 版数    年月日      担当                                概要
 * 1.00    2025-04-29  みらいペイ システム部 返金・チャージバックチーム  初版作成
 */
public class UserRefundMessageDeliveryService {

    private static final String DELIVERY_MIHAISHIN = "0";
    private static final String DELIVERY_SEIKO = "1";
    private static final String DELIVERY_SAISOU = "2";
    private static final String DELIVERY_KOTEI_SHIPPAI = "9";

    private static final int SMS_MAX_LENGTH = 660;
    private static final int MAIL_MAX_LENGTH = 4000;
    private static final int APP_MAX_LENGTH = 1800;

    public static void main(String[] a) {
        new UserRefundMessageDeliveryService().execute();
    }

    private void execute() {
        Class<?> kyotsuKata = RefundModel.class;
        if (kyotsuKata == null) {
            throw new IllegalStateException("共通型定義を確認できません");
        }

        PnmsgfFile pnmsgf = new PnmsgfFile();
        pnmsgf.add(new Pnmsgf("MSG000001", "REQ24060001", "WLT000000001", "SMS",
                "返金受付のお知らせ。受付番号:${REQ_ID} 金額:${AMOUNT}", DELIVERY_MIHAISHIN));
        pnmsgf.add(new Pnmsgf("MSG000002", "REQ24060002", "WLT000000002", "MAIL",
                "ご利用者様\n返金手続を開始しました。\n受付番号:${REQ_ID}\n振込予定日:${REFUND_DATE}", DELIVERY_MIHAISHIN));
        pnmsgf.add(new Pnmsgf("MSG000003", "REQ24060003", "WLT000000003", "APP",
                "返金が完了しました。受付番号:${REQ_ID}", DELIVERY_MIHAISHIN));
        pnmsgf.add(new Pnmsgf("MSG000004", "REQ24060004", "WLT000000004", "SMS",
                "返金不能のお知らせ。受付番号:${REQ_ID} 理由:${REASON}", DELIVERY_MIHAISHIN));
        pnmsgf.add(new Pnmsgf("MSG000005", "REQ24060005", "WLT000000005", "MAIL",
                "返金受付のお知らせ。受付番号:${REQ_ID}", DELIVERY_SEIKO));

        DeliverySummary summary = new DeliverySummary();

        for (Pnmsgf record : pnmsgf.records) {
            if (!DELIVERY_MIHAISHIN.equals(record.deliveryKbn)) {
                continue;
            }

            ValidationResult validation = validate(record);
            if (!validation.ok) {
                record.deliveryKbn = DELIVERY_KOTEI_SHIPPAI;
                summary.permanentFailure++;
                log("恒久失敗 " + record.messageId + " " + validation.reason);
                continue;
            }

            DeliveryResult result = deliver(record);
            record.deliveryKbn = result.deliveryKbn;

            if (DELIVERY_SEIKO.equals(result.deliveryKbn)) {
                summary.success++;
            } else if (DELIVERY_SAISOU.equals(result.deliveryKbn)) {
                summary.temporaryFailure++;
            } else {
                summary.permanentFailure++;
            }

            log(result.message);
        }

        for (Pnmsgf record : pnmsgf.records) {
            pnmsgf.write(record);
        }

        log("配信件数=" + summary.success
                + " 一時失敗=" + summary.temporaryFailure
                + " 恒久失敗=" + summary.permanentFailure);
    }

    private ValidationResult validate(Pnmsgf record) {
        if (isBlank(record.messageId) || isBlank(record.reqId) || isBlank(record.walletId)) {
            return ValidationResult.ng("主キー項目不足");
        }
        if (!"SMS".equals(record.channelKbn) && !"MAIL".equals(record.channelKbn) && !"APP".equals(record.channelKbn)) {
            return ValidationResult.ng("チャネル区分不正");
        }
        if (isBlank(record.messageBody)) {
            return ValidationResult.ng("本文未設定");
        }
        if (record.messageBody.contains("${REQ_ID}") || record.messageBody.contains("${AMOUNT}")
                || record.messageBody.contains("${REFUND_DATE}") || record.messageBody.contains("${REASON}")) {
            return ValidationResult.ng("必須差込値未解決");
        }
        if ("SMS".equals(record.channelKbn) && record.messageBody.length() > SMS_MAX_LENGTH) {
            return ValidationResult.ng("本文長超過");
        }
        if ("MAIL".equals(record.channelKbn) && record.messageBody.length() > MAIL_MAX_LENGTH) {
            return ValidationResult.ng("本文長超過");
        }
        if ("APP".equals(record.channelKbn) && record.messageBody.length() > APP_MAX_LENGTH) {
            return ValidationResult.ng("本文長超過");
        }
        if (isStopped(record.walletId, record.channelKbn)) {
            return ValidationResult.ng("配信停止中");
        }
        return ValidationResult.ok();
    }

    private DeliveryResult deliver(Pnmsgf record) {
        int hash = Math.abs((record.messageId + record.walletId + record.channelKbn).hashCode());
        if (hash % 17 == 0) {
            return new DeliveryResult(DELIVERY_KOTEI_SHIPPAI, "恒久失敗 " + record.messageId + " 宛先無効");
        }
        if (hash % 5 == 0) {
            return new DeliveryResult(DELIVERY_SAISOU, "一時失敗 " + record.messageId + " 再送対象");
        }
        return new DeliveryResult(DELIVERY_SEIKO, "配信成功 " + record.messageId);
    }

    private boolean isStopped(String walletId, String channelKbn) {
        return ("WLT000000004".equals(walletId) && "SMS".equals(channelKbn))
                || ("WLT000000008".equals(walletId) && "MAIL".equals(channelKbn));
    }

    private boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private void log(String message) {
        System.out.println(message);
    }

    private static final class PnmsgfFile {
        private final java.util.List<Pnmsgf> records = new java.util.ArrayList<>();

        private void add(Pnmsgf record) {
            records.add(record);
        }

        private void write(Pnmsgf record) {
            System.out.println(record.messageId + ","
                    + record.reqId + ","
                    + record.walletId + ","
                    + record.channelKbn + ","
                    + record.messageBody.replace('\n', ' ') + ","
                    + record.deliveryKbn);
        }
    }

    private static final class Pnmsgf {
        private final String messageId;
        private final String reqId;
        private final String walletId;
        private final String channelKbn;
        private final String messageBody;
        private String deliveryKbn;

        private Pnmsgf(String messageId, String reqId, String walletId,
                       String channelKbn, String messageBody, String deliveryKbn) {
            this.messageId = messageId;
            this.reqId = reqId;
            this.walletId = walletId;
            this.channelKbn = channelKbn;
            this.messageBody = messageBody;
            this.deliveryKbn = deliveryKbn;
        }
    }

    private static final class ValidationResult {
        private final boolean ok;
        private final String reason;

        private ValidationResult(boolean ok, String reason) {
            this.ok = ok;
            this.reason = reason;
        }

        private static ValidationResult ok() {
            return new ValidationResult(true, "");
        }

        private static ValidationResult ng(String reason) {
            return new ValidationResult(false, reason);
        }
    }

    private static final class DeliveryResult {
        private final String deliveryKbn;
        private final String message;

        private DeliveryResult(String deliveryKbn, String message) {
            this.deliveryKbn = deliveryKbn;
            this.message = message;
        }
    }

    private static final class DeliverySummary {
        private int success;
        private int permanentFailure;
        private int temporaryFailure;
    }
}
