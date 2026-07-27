/**
 * 変更履歴
 * 版数  年月日    担当       概要
 * 1.00  20230127  会員系保守  会員ステータス管理サービス新規作成
 */
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

public class MemberStatusService {
    public static void main(String[] args) {
        Repository repo = new Repository();
        repo.seed();

        Service service = new Service(repo);

        System.out.println(service.inquire("M0001002").message);
        System.out.println(service.apply("M0001002", "02", "OP停止").message);
        System.out.println(service.apply("M0001003", "01", "OP復帰").message);
        System.out.println(service.inquire("M0001002").message);
    }

    public static final class Service {
        private final Repository repo;

        public Service(Repository repo) {
            this.repo = repo;
        }

        public Result inquire(String memberId) {
            MemberStatus status = repo.findMemberStatus(memberId);
            if (status == null) {
                return Result.ng("該当会員なし 会員ID=" + memberId);
            }

            Card card = repo.findCardByMember(memberId);
            if (card == null) {
                return Result.ng("カード情報なし 会員ID=" + memberId);
            }

            Delinquency delinquency = repo.findLatestDelinquency(card.cardNo);
            String delinquencyText;
            if (delinquency == null || delinquency.daysPastDue <= 0 || delinquency.pastDueAmt <= 0) {
                delinquencyText = "延滞なし";
            } else {
                delinquencyText = "延滞あり " + delinquency.daysPastDue + "日 "
                        + delinquency.pastDueAmt + "円 督促段階=" + delinquency.dunningStage;
            }

            return Result.ok(
                    "照会正常 会員ID=" + memberId
                            + " 会員状態=" + status.statusCd
                            + " 理由=" + status.statusReason
                            + " カード状態=" + card.cardStatus
                            + " 請求判定=" + billStatus(card.cardStatus)
                            + " " + delinquencyText);
        }

        public Result apply(String memberId, String requestedStatus, String reason) {
            if (isBlank(memberId)) {
                return Result.ng("会員ID未指定");
            }
            if (!"01".equals(requestedStatus) && !"02".equals(requestedStatus) && !"03".equals(requestedStatus)) {
                return Result.ng("会員状態コード不正 会員ID=" + memberId + " 要求=" + requestedStatus);
            }
            if (isBlank(reason)) {
                return Result.ng("状態理由未指定 会員ID=" + memberId);
            }

            MemberStatus current = repo.findMemberStatus(memberId);
            if (current == null) {
                return Result.ng("更新対象なし 会員ID=" + memberId);
            }

            Card card = repo.findCardByMember(memberId);
            if (card == null) {
                return Result.ng("カード情報なし 会員ID=" + memberId);
            }

            if ("03".equals(card.cardStatus)) {
                return Result.ng("解約カードの状態更新不可 会員ID=" + memberId);
            }
            if ("03".equals(current.statusCd) && !"03".equals(requestedStatus)) {
                return Result.ng("退会済会員の復帰不可 会員ID=" + memberId);
            }
            if (current.statusCd.equals(requestedStatus)) {
                return Result.ng("同一状態への更新不可 会員ID=" + memberId + " 状態=" + requestedStatus);
            }

            if ("01".equals(requestedStatus)) {
                Delinquency latest = repo.findLatestDelinquency(card.cardNo);
                if (latest != null && latest.daysPastDue > 0 && latest.pastDueAmt > 0) {
                    return Result.ng("未払い請求あり復帰不可 会員ID=" + memberId
                            + " 延滞日数=" + latest.daysPastDue
                            + " 未払額=" + latest.pastDueAmt);
                }
            }

            String normalizedReason = reason.trim();
            if (normalizedReason.length() > 20) {
                normalizedReason = normalizedReason.substring(0, 20);
            }

            repo.writeMemberStatus(new MemberStatus(
                    memberId,
                    requestedStatus,
                    normalizedReason,
                    today(),
                    now()));

            return Result.ok("更新正常 会員ID=" + memberId + " 変更後状態=" + requestedStatus);
        }

        private String billStatus(String cardStatus) {
            if ("01".equals(cardStatus) || "09".equals(cardStatus)) {
                return "C";
            }
            return "S";
        }
    }

    public static final class Repository {
        private final Map<String, MemberStatus> cdmemstatf = new LinkedHashMap<String, MemberStatus>();
        private final Map<String, Card> cdcardf = new LinkedHashMap<String, Card>();
        private final List<Delinquency> cddelinqf = new ArrayList<Delinquency>();

        public void seed() {
            cdmemstatf.put("M0001001", new MemberStatus("M0001001", "01", "通常", "20260601", "20260601090000"));
            cdmemstatf.put("M0001002", new MemberStatus("M0001002", "01", "通常", "20260603", "20260603101530"));
            cdmemstatf.put("M0001003", new MemberStatus("M0001003", "02", "延滞停止", "20260610", "20260610112205"));
            cdmemstatf.put("M0001004", new MemberStatus("M0001004", "03", "退会", "20260520", "20260520170000"));

            cdcardf.put("4100000000001001", new Card("4100000000001001", "M0001001", "01", 800000, "15", "ヤマダ タロウ", "20240112"));
            cdcardf.put("4100000000001002", new Card("4100000000001002", "M0001002", "01", 1200000, "20", "サトウ ハナコ", "20240218"));
            cdcardf.put("4100000000001003", new Card("4100000000001003", "M0001003", "09", 500000, "25", "スズキ イチロウ", "20231205"));
            cdcardf.put("4100000000001004", new Card("4100000000001004", "M0001004", "03", 300000, "10", "タナカ ジロウ", "20221130"));

            cddelinqf.add(new Delinquency("4100000000001003", "20260525", 31, 48000, "2", "20260626"));
            cddelinqf.add(new Delinquency("4100000000001003", "20260625", 62, 97000, "3", "20260627"));
            cddelinqf.add(new Delinquency("4100000000001002", "20260620", 0, 0, "0", "20260627"));
        }

        public MemberStatus findMemberStatus(String memberId) {
            return cdmemstatf.get(memberId);
        }

        public Card findCardByMember(String memberId) {
            for (Card card : cdcardf.values()) {
                if (card.memberId.equals(memberId)) {
                    return card;
                }
            }
            return null;
        }

        public Delinquency findLatestDelinquency(String cardNo) {
            Delinquency latest = null;
            for (Delinquency row : cddelinqf) {
                if (!row.cardNo.equals(cardNo)) {
                    continue;
                }
                if (latest == null || row.extractDt.compareTo(latest.extractDt) > 0) {
                    latest = row;
                }
            }
            return latest;
        }

        public void writeMemberStatus(MemberStatus row) {
            cdmemstatf.put(row.memberId, row);
        }
    }

    public static final class MemberStatus {
        public final String memberId;
        public final String statusCd;
        public final String statusReason;
        public final String effectiveDt;
        public final String lastUpdatedTs;

        public MemberStatus(String memberId, String statusCd, String statusReason, String effectiveDt, String lastUpdatedTs) {
            this.memberId = memberId;
            this.statusCd = statusCd;
            this.statusReason = statusReason;
            this.effectiveDt = effectiveDt;
            this.lastUpdatedTs = lastUpdatedTs;
        }
    }

    public static final class Card {
        public final String cardNo;
        public final String memberId;
        public final String cardStatus;
        public final int creditLimit;
        public final String billCycleCd;
        public final String memberNameKana;
        public final String openDt;

        public Card(String cardNo, String memberId, String cardStatus, int creditLimit, String billCycleCd, String memberNameKana, String openDt) {
            this.cardNo = cardNo;
            this.memberId = memberId;
            this.cardStatus = cardStatus;
            this.creditLimit = creditLimit;
            this.billCycleCd = billCycleCd;
            this.memberNameKana = memberNameKana;
            this.openDt = openDt;
        }
    }

    public static final class Delinquency {
        public final String cardNo;
        public final String cycleDt;
        public final int daysPastDue;
        public final int pastDueAmt;
        public final String dunningStage;
        public final String extractDt;

        public Delinquency(String cardNo, String cycleDt, int daysPastDue, int pastDueAmt, String dunningStage, String extractDt) {
            this.cardNo = cardNo;
            this.cycleDt = cycleDt;
            this.daysPastDue = daysPastDue;
            this.pastDueAmt = pastDueAmt;
            this.dunningStage = dunningStage;
            this.extractDt = extractDt;
        }
    }

    public static final class Result {
        public final boolean ok;
        public final String message;

        private Result(boolean ok, String message) {
            this.ok = ok;
            this.message = message;
        }

        public static Result ok(String message) {
            return new Result(true, message);
        }

        public static Result ng(String message) {
            return new Result(false, message);
        }
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().length() == 0;
    }

    private static String today() {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd");
        formatter.setTimeZone(TimeZone.getTimeZone("Asia/Tokyo"));
        return formatter.format(new Date());
    }

    private static String now() {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHHmmss");
        formatter.setTimeZone(TimeZone.getTimeZone("Asia/Tokyo"));
        return formatter.format(new Date());
    }
}
