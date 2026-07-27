package jp.mirai.pay.settlement;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/06/24  加盟店精算チーム  初版作成
 * 1.01  2025/02/05  加盟店精算チーム  再送上限到達時の失敗判定を追加
 */
public class MerchantNotificationService {
    private static final String STATUS_UNSENT = "0";
    private static final String STATUS_SENT = "1";
    private static final String STATUS_PENDING = "2";
    private static final String STATUS_FAILED = "9";
    private static final String MERCHANT_SETTLEABLE = "01";
    private static final int MAX_RETRY = 3;

    public static void main(String[] a) {
        PsntffStore noticeStore = new PsntffStore();
        PsmerfStore merchantStore = new PsmerfStore();
        ContactDirectory contactDirectory = new ContactDirectory();
        TransportGateway gateway = new TransportGateway();

        DeliveryBatchResult result = new MerchantNotificationService()
                .executeBatch(noticeStore, merchantStore, contactDirectory, gateway);

        System.out.println("加盟店通知配信サービス 処理結果");
        System.out.println("対象件数=" + result.targetCount);
        System.out.println("送信済件数=" + result.sentCount);
        System.out.println("保留件数=" + result.pendingCount);
        System.out.println("失敗件数=" + result.failedCount);
        System.out.println("未送信残件数=" + noticeStore.countByStatus(STATUS_UNSENT));
    }

    private DeliveryBatchResult executeBatch(PsntffStore noticeStore,
                                             PsmerfStore merchantStore,
                                             ContactDirectory contactDirectory,
                                             TransportGateway gateway) {
        DeliveryBatchResult result = new DeliveryBatchResult();
        long now = System.currentTimeMillis();

        for (NoticeRecord notice : noticeStore.findUnsent()) {
            result.targetCount++;

            MerchantRecord merchant = merchantStore.findByCode(notice.merchantCode);
            if (merchant == null) {
                notice.sendStatus = STATUS_FAILED;
                notice.httpStatus = 0;
                notice.retryCount++;
                notice.lastMessage = "加盟店マスタ未登録";
                result.failedCount++;
                continue;
            }

            if (!MERCHANT_SETTLEABLE.equals(merchant.merchantStatus)) {
                notice.sendStatus = STATUS_PENDING;
                notice.sendAt = 0L;
                notice.lastMessage = "加盟店状態により送信保留";
                result.pendingCount++;
                continue;
            }

            ContactSetting contact = contactDirectory.findByMerchantCode(notice.merchantCode);
            if (contact == null || !contact.isUsable()) {
                notice.sendStatus = STATUS_FAILED;
                notice.httpStatus = 0;
                notice.retryCount++;
                notice.lastMessage = "連絡先設定不備";
                result.failedCount++;
                continue;
            }

            DeliveryRequest request = buildRequest(notice, merchant, contact);
            DeliveryResponse response = gateway.send(request);

            notice.httpStatus = response.httpStatus;
            notice.retryCount++;
            notice.lastMessage = response.message;

            if (response.success) {
                notice.sendStatus = STATUS_SENT;
                notice.sendAt = now;
                result.sentCount++;
            } else if (notice.retryCount >= MAX_RETRY) {
                notice.sendStatus = STATUS_FAILED;
                result.failedCount++;
            } else {
                notice.sendStatus = STATUS_UNSENT;
                result.failedCount++;
            }
        }

        noticeStore.rewriteAll();
        return result;
    }

    private DeliveryRequest buildRequest(NoticeRecord notice, MerchantRecord merchant, ContactSetting contact) {
        String subject = "精算通知 " + notice.settleId;
        String body = "加盟店=" + merchant.merchantName
                + ", 通知区分=" + notice.noticeKind
                + ", 精算番号=" + notice.settleId
                + ", 加盟店番号=" + notice.merchantCode;
        return new DeliveryRequest(notice.noticeId, contact.channel, contact.destination, subject, body);
    }

    private static final class PsntffStore {
        private final NoticeRecord[] records = {
                new NoticeRecord("N202505200001", "M000001", "C", "S202505190001", STATUS_UNSENT, 0L),
                new NoticeRecord("N202505200002", "M000002", "R", "S202505190002", STATUS_UNSENT, 0L),
                new NoticeRecord("N202505200003", "M000003", "C", "S202505190003", STATUS_UNSENT, 0L),
                new NoticeRecord("N202505200004", "M000004", "C", "S202505190004", STATUS_SENT, 1747000000000L),
                new NoticeRecord("N202505200005", "M000005", "R", "S202505190005", STATUS_UNSENT, 0L)
        };

        private NoticeRecord[] findUnsent() {
            int count = 0;
            for (NoticeRecord record : records) {
                if (STATUS_UNSENT.equals(record.sendStatus)) {
                    count++;
                }
            }

            NoticeRecord[] found = new NoticeRecord[count];
            int pos = 0;
            for (NoticeRecord record : records) {
                if (STATUS_UNSENT.equals(record.sendStatus)) {
                    found[pos++] = record;
                }
            }
            return found;
        }

        private int countByStatus(String status) {
            int count = 0;
            for (NoticeRecord record : records) {
                if (status.equals(record.sendStatus)) {
                    count++;
                }
            }
            return count;
        }

        private void rewriteAll() {
            for (NoticeRecord record : records) {
                if (record.noticeId == null || record.noticeId.trim().isEmpty()) {
                    throw new IllegalStateException("通知番号が空です");
                }
                if (record.merchantCode == null || record.merchantCode.trim().isEmpty()) {
                    throw new IllegalStateException("加盟店番号が空です");
                }
                if (!"C".equals(record.noticeKind) && !"R".equals(record.noticeKind)) {
                    throw new IllegalStateException("通知区分が不正です: " + record.noticeKind);
                }
            }
        }
    }

    private static final class PsmerfStore {
        private final MerchantRecord[] records = {
                new MerchantRecord("M000001", "みらい珈琲銀座店", "01", "0001234567"),
                new MerchantRecord("M000002", "東都文具商会", "02", "0002234567"),
                new MerchantRecord("M000003", "北浜薬局", "01", "0003234567"),
                new MerchantRecord("M000004", "青葉電子", "09", "0004234567"),
                new MerchantRecord("M000005", "港北ベーカリー", "01", "0005234567")
        };

        private MerchantRecord findByCode(String merchantCode) {
            for (MerchantRecord record : records) {
                if (record.merchantCode.equals(merchantCode)) {
                    return record;
                }
            }
            return null;
        }
    }

    private static final class ContactDirectory {
        private final ContactSetting[] settings = {
                new ContactSetting("M000001", "MAIL", "keiri@miraicoffee.example.jp", true),
                new ContactSetting("M000003", "WEBHOOK", "https://api.kitahama.example.jp/settlement", true),
                new ContactSetting("M000005", "WEBHOOK", "https://api.kohoku.example.jp/fail", true)
        };

        private ContactSetting findByMerchantCode(String merchantCode) {
            for (ContactSetting setting : settings) {
                if (setting.merchantCode.equals(merchantCode)) {
                    return setting;
                }
            }
            return null;
        }
    }

    private static final class TransportGateway {
        private DeliveryResponse send(DeliveryRequest request) {
            if ("MAIL".equals(request.channel)) {
                boolean validMail = request.destination.indexOf('@') > 0
                        && request.destination.endsWith(".jp");
                return validMail
                        ? new DeliveryResponse(true, 250, "メール送信完了")
                        : new DeliveryResponse(false, 553, "メール宛先不正");
            }

            if ("WEBHOOK".equals(request.channel)) {
                if (!request.destination.startsWith("https://")) {
                    return new DeliveryResponse(false, 400, "Webhook URL不正");
                }
                if (request.destination.contains("/fail")) {
                    return new DeliveryResponse(false, 503, "Webhook応答異常");
                }
                return new DeliveryResponse(true, 200, "Webhook送信完了");
            }

            return new DeliveryResponse(false, 0, "配信方式不正");
        }
    }

    private static final class NoticeRecord {
        private final String noticeId;
        private final String merchantCode;
        private final String noticeKind;
        private final String settleId;
        private String sendStatus;
        private long sendAt;
        private int httpStatus;
        private int retryCount;
        private String lastMessage;

        private NoticeRecord(String noticeId,
                             String merchantCode,
                             String noticeKind,
                             String settleId,
                             String sendStatus,
                             long sendAt) {
            this.noticeId = noticeId;
            this.merchantCode = merchantCode;
            this.noticeKind = noticeKind;
            this.settleId = settleId;
            this.sendStatus = sendStatus;
            this.sendAt = sendAt;
            this.httpStatus = 0;
            this.retryCount = 0;
            this.lastMessage = "";
        }
    }

    private static final class MerchantRecord {
        private final String merchantCode;
        private final String merchantName;
        private final String merchantStatus;
        private final String bankAccountNo;

        private MerchantRecord(String merchantCode, String merchantName, String merchantStatus, String bankAccountNo) {
            this.merchantCode = merchantCode;
            this.merchantName = merchantName;
            this.merchantStatus = merchantStatus;
            this.bankAccountNo = bankAccountNo;
        }
    }

    private static final class ContactSetting {
        private final String merchantCode;
        private final String channel;
        private final String destination;
        private final boolean enabled;

        private ContactSetting(String merchantCode, String channel, String destination, boolean enabled) {
            this.merchantCode = merchantCode;
            this.channel = channel;
            this.destination = destination;
            this.enabled = enabled;
        }

        private boolean isUsable() {
            return enabled
                    && destination != null
                    && !destination.trim().isEmpty()
                    && ("MAIL".equals(channel) || "WEBHOOK".equals(channel));
        }
    }

    private static final class DeliveryRequest {
        private final String noticeId;
        private final String channel;
        private final String destination;
        private final String subject;
        private final String body;

        private DeliveryRequest(String noticeId, String channel, String destination, String subject, String body) {
            this.noticeId = noticeId;
            this.channel = channel;
            this.destination = destination;
            this.subject = subject;
            this.body = body;
        }
    }

    private static final class DeliveryResponse {
        private final boolean success;
        private final int httpStatus;
        private final String message;

        private DeliveryResponse(boolean success, int httpStatus, String message) {
            this.success = success;
            this.httpStatus = httpStatus;
            this.message = message;
        }
    }

    private static final class DeliveryBatchResult {
        private int targetCount;
        private int sentCount;
        private int pendingCount;
        private int failedCount;
    }
}
