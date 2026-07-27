/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.00  2024/02/15  業務一課  初版作成。明細照会と表示区分変換を追加。
 * 1.01  2024/08/09  業務一課  明細表示順変更対応。
 * 1.02  2025/01/20  業務二課  カード番号マスキング改修。
 * 1.03  2025/11/12  業務二課  CDOVSF確定済み請求額の表示反映に対応。海外固有計算は対象外。
 */
public class OverseasStatementInquiryService {
    private static final String PROGRAM_ID = "CDSTMINQ";

    private final java.util.List<Cdstmfc> statements;
    private final java.util.List<Cdovsfc> overseas;
    private final java.util.List<Cdmvwfc> existing;

    public OverseasStatementInquiryService(java.util.List<Cdstmfc> statements,
                                           java.util.List<Cdovsfc> overseas,
                                           java.util.List<Cdmvwfc> existing) {
        this.statements = statements;
        this.overseas = overseas;
        this.existing = existing;
    }

    public Result execute(int from, int to) {
        if (from > to) {
            throw new IllegalArgumentException("照会期間が不正です");
        }

        java.util.Map<String, Cdovsfc> settledByTxn = new java.util.HashMap<>();
        for (Cdovsfc r : overseas) {
            if (isBlank(r.ovTxnId()) || isBlank(r.ovCardNo())) {
                continue;
            }
            if (!"D".equals(r.ovSetlKbn())) {
                continue;
            }
            settledByTxn.put(r.ovTxnId(), r);
        }

        java.util.Set<String> writtenKeys = new java.util.HashSet<>();
        for (Cdmvwfc r : existing) {
            writtenKeys.add(r.mvCardNo() + "|" + r.mvTxnId() + "|" + r.mvDispKbn());
        }

        java.util.List<WorkLine> work = new java.util.ArrayList<>();
        java.util.List<Cdlogfc> logs = new java.util.ArrayList<>();

        for (Cdstmfc s : statements) {
            if (!validStatement(s, logs)) {
                continue;
            }
            if (!inRange(s.stStatementId(), from, to)) {
                continue;
            }

            WorkLine line = buildLine(s, settledByTxn.get(s.stTxnId()), logs);
            if (line != null) {
                work.add(line);
            }
        }

        java.util.Map<String, WorkLine> aggregated = new java.util.LinkedHashMap<>();
        for (WorkLine line : work) {
            String key = line.cardNo + "|" + line.txnId + "|" + line.dispKbn + "|" + line.dispLabel;
            WorkLine base = aggregated.get(key);
            if (base == null) {
                aggregated.put(key, line);
            } else {
                base.dispAmt = base.dispAmt + line.dispAmt;
            }
        }

        java.util.List<WorkLine> sorted = new java.util.ArrayList<>(aggregated.values());
        sorted.sort((x, y) -> {
            int c = x.cardNo.compareTo(y.cardNo);
            if (c != 0) {
                return c;
            }
            c = orderOf(x.dispKbn) - orderOf(y.dispKbn);
            if (c != 0) {
                return c;
            }
            return x.txnId.compareTo(y.txnId);
        });

        java.util.List<Cdmvwfc> writes = new java.util.ArrayList<>();
        for (WorkLine line : sorted) {
            String writeKey = line.cardNo + "|" + line.txnId + "|" + line.dispKbn;
            if (writtenKeys.contains(writeKey)) {
                logs.add(log(line.cardNo, "重複", "CDMVWF"));
                continue;
            }
            writes.add(new Cdmvwfc(
                    maskCardNo(line.cardNo),
                    line.txnId,
                    line.dispKbn,
                    line.dispAmt,
                    line.dispLabel));
            writtenKeys.add(writeKey);
        }

        return new Result(writes, logs);
    }

    /**
     * 明細1件分の表示行を組み立てる。CDOVSFに確定済みレコードがあれば、
     * 既に確定している請求額（ovSetlAmt）を表示金額として採用する。
     * 取引区分ごとの固有計算は当サービスでは行わず、確定済みの値をそのまま反映する。
     */
    private WorkLine buildLine(Cdstmfc s, Cdovsfc settled, java.util.List<Cdlogfc> logs) {
        String dispKbn = toDispKbn(s.stLineKbn());
        if (dispKbn == null) {
            logs.add(log(s.stCardNo(), "明細区分不正", s.stLineKbn()));
            return null;
        }

        long amount = s.stLineAmt();
        if (settled != null) {
            if (!s.stCardNo().equals(settled.ovCardNo())) {
                logs.add(log(s.stCardNo(), "明細カード不一致", settled.ovTxnId()));
                return null;
            }
            if (settled.ovSetlAmt() < 0L) {
                logs.add(log(s.stCardNo(), "確定請求額不正", settled.ovTxnId()));
                return null;
            }
            amount = settled.ovSetlAmt();
            logs.add(log(s.stCardNo(), "確定額反映", settled.ovTxnId()));
        }

        String label = normalizeLabel(s.stLineLabel(), s.stLineKbn());
        return new WorkLine(s.stCardNo(), s.stTxnId(), dispKbn, amount, label);
    }

    private static String toDispKbn(String lineKbn) {
        if ("P1".equals(lineKbn) || "P2".equals(lineKbn) || "A1".equals(lineKbn)) {
            return "S";
        }
        if ("C1".equals(lineKbn) || "C2".equals(lineKbn)) {
            return "K";
        }
        return null;
    }

    private boolean validStatement(Cdstmfc s, java.util.List<Cdlogfc> logs) {
        if (s == null || isBlank(s.stCardNo()) || isBlank(s.stStatementId()) || isBlank(s.stTxnId())) {
            logs.add(log(s == null ? "" : s.stCardNo(), "明細キー不正", "CDSTMF"));
            return false;
        }
        if (s.stLineAmt() < 0L) {
            logs.add(log(s.stCardNo(), "明細金額不正", s.stTxnId()));
            return false;
        }
        return true;
    }

    private boolean inRange(String statementId, int from, int to) {
        if (statementId.length() < 8) {
            return false;
        }
        try {
            int d = Integer.parseInt(statementId.substring(0, 8));
            return d >= from && d <= to;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private static int orderOf(String dispKbn) {
        if ("S".equals(dispKbn)) {
            return 1;
        }
        if ("K".equals(dispKbn)) {
            return 2;
        }
        return 9;
    }

    private String normalizeLabel(String label, String lineKbn) {
        String base = trimToEmpty(label);
        if (base.length() > 0) {
            return base;
        }
        if ("P1".equals(lineKbn)) {
            return "国内ショッピング";
        }
        if ("P2".equals(lineKbn)) {
            return "海外ショッピング";
        }
        if ("C1".equals(lineKbn)) {
            return "国内キャッシング";
        }
        if ("C2".equals(lineKbn)) {
            return "海外キャッシング";
        }
        if ("A1".equals(lineKbn)) {
            return "年会費";
        }
        return "明細";
    }

    private String maskCardNo(String cardNo) {
        String n = trimToEmpty(cardNo).replace("-", "");
        if (n.length() <= 8) {
            return n;
        }
        return n.substring(0, 6) + "******" + n.substring(n.length() - 4);
    }

    private Cdlogfc log(String cardNo, String eventKbn, String detailCd) {
        return new Cdlogfc(
                "L" + System.nanoTime(),
                PROGRAM_ID,
                maskCardNo(cardNo),
                eventKbn,
                Integer.parseInt(java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)),
                trimToEmpty(detailCd));
    }

    private static boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }

    private static String trimToEmpty(String s) {
        return s == null ? "" : s.trim();
    }

    public static final class Result {
        public final java.util.List<Cdmvwfc> cdmvwfWrites;
        public final java.util.List<Cdlogfc> cdlogfWrites;

        Result(java.util.List<Cdmvwfc> cdmvwfWrites, java.util.List<Cdlogfc> cdlogfWrites) {
            this.cdmvwfWrites = java.util.Collections.unmodifiableList(cdmvwfWrites);
            this.cdlogfWrites = java.util.Collections.unmodifiableList(cdlogfWrites);
        }
    }

    private static final class WorkLine {
        final String cardNo;
        final String txnId;
        final String dispKbn;
        long dispAmt;
        final String dispLabel;

        WorkLine(String cardNo, String txnId, String dispKbn, long dispAmt, String dispLabel) {
            this.cardNo = cardNo;
            this.txnId = txnId;
            this.dispKbn = dispKbn;
            this.dispAmt = dispAmt;
            this.dispLabel = dispLabel;
        }
    }
}
