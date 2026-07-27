package jp.mirai.pay.trace;

/**
 * 変更履歴
 * 版数  年月日      担当                              概要
 * 1.0   2024/10/03  みらいペイ システム部 精算・連携チーム  初版作成。連携キーごとの存在、加盟店、金額符号、区分値検証を実装。
 */
public class TraceKeyValidationService {
    private static final String OK = "00";
    private static final String HOLD_NASHI = "11";
    private static final String CAP_NASHI = "12";
    private static final String SETTLE_NASHI = "13";
    private static final String KAMEITEN_FUICHI = "21";
    private static final String KINGAKU_FUGO_FUSEI = "31";
    private static final String KUBUN_FUSEI = "41";

    /** PTHOLDF/PTCAPF/PTSETF を読み込み、連携キーごとの検査結果(PTKEYF)を生成して書き出す。 */
    public void run() {
        java.util.List<HoldRow> holds = loadPtholdf();
        java.util.List<CapRow> caps = loadPtcapf();
        java.util.List<SettleRow> settles = loadPtsetf();

        java.util.Map<String, HoldRow> holdById = new java.util.LinkedHashMap<>();
        for (HoldRow h : holds) {
            holdById.put(h.holdId, h);
        }

        java.util.Map<String, CapRow> capById = new java.util.LinkedHashMap<>();
        for (CapRow c : caps) {
            capById.put(c.capId, c);
        }

        java.util.Map<String, SettleRow> settleById = new java.util.LinkedHashMap<>();
        for (SettleRow s : settles) {
            settleById.put(s.settleTxnId, s);
        }

        java.util.List<KeyResultRow> ptkeyf = new java.util.ArrayList<>();

        for (CapRow cap : caps) {
            HoldRow hold = holdById.get(cap.holdId);
            SettleRow settle = settleById.get(cap.settleTxnId);

            String holdResult = checkHold(cap, hold);
            ptkeyf.add(new KeyResultRow(
                    traceKey("H", cap.holdId),
                    cap.holdId,
                    cap.capId,
                    cap.settleTxnId,
                    cap.merchantCode,
                    holdResult));

            String capResult = checkCap(cap, hold, settle);
            ptkeyf.add(new KeyResultRow(
                    traceKey("C", cap.capId),
                    cap.holdId,
                    cap.capId,
                    cap.settleTxnId,
                    cap.merchantCode,
                    capResult));

            String settleResult = checkSettle(cap, settle);
            ptkeyf.add(new KeyResultRow(
                    traceKey("S", cap.settleTxnId),
                    cap.holdId,
                    cap.capId,
                    cap.settleTxnId,
                    cap.merchantCode,
                    settleResult));
        }

        for (HoldRow hold : holds) {
            if (!referencedByCap(hold.holdId, caps)) {
                ptkeyf.add(new KeyResultRow(
                        traceKey("H", hold.holdId),
                        hold.holdId,
                        "",
                        "",
                        hold.merchantCode,
                        CAP_NASHI));
            }
        }

        writePtkeyf(ptkeyf);
    }

    private static String checkHold(CapRow cap, HoldRow hold) {
        if (hold == null) {
            return HOLD_NASHI;
        }
        if (!isValidHoldStatus(hold.holdStatus)) {
            return KUBUN_FUSEI;
        }
        if (!hold.merchantCode.equals(cap.merchantCode)) {
            return KAMEITEN_FUICHI;
        }
        if (hold.holdAmt.signum() <= 0) {
            return KINGAKU_FUGO_FUSEI;
        }
        return OK;
    }

    private static String checkCap(CapRow cap, HoldRow hold, SettleRow settle) {
        if (!isValidSettleKbn(cap.settleKbn)) {
            return KUBUN_FUSEI;
        }
        if (cap.capAmt.signum() <= 0) {
            return KINGAKU_FUGO_FUSEI;
        }
        if (hold == null) {
            return HOLD_NASHI;
        }
        if (settle == null) {
            return SETTLE_NASHI;
        }
        if (!cap.merchantCode.equals(hold.merchantCode) || !cap.merchantCode.equals(settle.merchantCode)) {
            return KAMEITEN_FUICHI;
        }
        return OK;
    }

    private static String checkSettle(CapRow cap, SettleRow settle) {
        if (settle == null) {
            return SETTLE_NASHI;
        }
        if (!isValidSettleKbn(settle.settleKbn)) {
            return KUBUN_FUSEI;
        }
        if (!cap.settleKbn.equals(settle.settleKbn)) {
            return KUBUN_FUSEI;
        }
        if (!cap.merchantCode.equals(settle.merchantCode)) {
            return KAMEITEN_FUICHI;
        }
        if (settle.txnAmt.signum() <= 0) {
            return KINGAKU_FUGO_FUSEI;
        }
        return OK;
    }

    private static boolean isValidHoldStatus(String value) {
        return "00".equals(value) || "30".equals(value) || "20".equals(value);
    }

    private static boolean isValidSettleKbn(String value) {
        return "1".equals(value) || "2".equals(value) || "9".equals(value);
    }

    private static boolean referencedByCap(String holdId, java.util.List<CapRow> caps) {
        for (CapRow cap : caps) {
            if (holdId.equals(cap.holdId)) {
                return true;
            }
        }
        return false;
    }

    private static String traceKey(String kind, String id) {
        return kind + "-" + id;
    }

    private static void writePtkeyf(java.util.List<KeyResultRow> rows) {
        System.out.println("TRACE-KEY,HOLD-ID,CAP-ID,SETTLE-TXN-ID,MERCHANT-CODE,CHECK-RESULT");
        for (KeyResultRow r : rows) {
            System.out.println(csv(r.traceKey) + ","
                    + csv(r.holdId) + ","
                    + csv(r.capId) + ","
                    + csv(r.settleTxnId) + ","
                    + csv(r.merchantCode) + ","
                    + csv(r.checkResult));
        }
    }

    private static String csv(String value) {
        if (value == null) {
            return "";
        }
        if (value.indexOf(',') < 0 && value.indexOf('"') < 0 && value.indexOf('\n') < 0) {
            return value;
        }
        return "\"" + value.replace("\"", "\"\"") + "\"";
    }

    private static java.util.List<HoldRow> loadPtholdf() {
        java.util.List<HoldRow> rows = new java.util.ArrayList<>();
        rows.add(new HoldRow("HD202501150001", "WL100001", "M00012001", amount("12000"), "00"));
        rows.add(new HoldRow("HD202501150002", "WL100002", "M00012002", amount("4800"), "30"));
        rows.add(new HoldRow("HD202501150003", "WL100003", "M00012003", amount("0"), "00"));
        rows.add(new HoldRow("HD202501150004", "WL100004", "M00012004", amount("9200"), "70"));
        rows.add(new HoldRow("HD202501150005", "WL100005", "M00012005", amount("3150"), "20"));
        return rows;
    }

    private static java.util.List<CapRow> loadPtcapf() {
        java.util.List<CapRow> rows = new java.util.ArrayList<>();
        rows.add(new CapRow("CP202501150001", "HD202501150001", "ST202501150001", "M00012001", "1", amount("12000")));
        rows.add(new CapRow("CP202501150002", "HD202501150002", "ST202501150002", "M00012999", "2", amount("4800")));
        rows.add(new CapRow("CP202501150003", "HD202501150003", "ST202501150003", "M00012003", "1", amount("0")));
        rows.add(new CapRow("CP202501150004", "HD202501159999", "ST202501150004", "M00012004", "9", amount("9200")));
        rows.add(new CapRow("CP202501150005", "HD202501150004", "ST202501159999", "M00012004", "7", amount("9200")));
        return rows;
    }

    private static java.util.List<SettleRow> loadPtsetf() {
        java.util.List<SettleRow> rows = new java.util.ArrayList<>();
        rows.add(new SettleRow("ST202501150001", "M00012001", amount("12000"), "1"));
        rows.add(new SettleRow("ST202501150002", "M00012002", amount("4800"), "2"));
        rows.add(new SettleRow("ST202501150003", "M00012003", amount("-1000"), "1"));
        rows.add(new SettleRow("ST202501150004", "M00012004", amount("9200"), "8"));
        return rows;
    }

    private static java.math.BigDecimal amount(String value) {
        return new java.math.BigDecimal(value);
    }

    private static final class HoldRow {
        private final String holdId;
        private final String walletId;
        private final String merchantCode;
        private final java.math.BigDecimal holdAmt;
        private final String holdStatus;

        private HoldRow(String holdId, String walletId, String merchantCode, java.math.BigDecimal holdAmt, String holdStatus) {
            this.holdId = holdId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.holdAmt = holdAmt;
            this.holdStatus = holdStatus;
        }
    }

    private static final class CapRow {
        private final String capId;
        private final String holdId;
        private final String settleTxnId;
        private final String merchantCode;
        private final String settleKbn;
        private final java.math.BigDecimal capAmt;

        private CapRow(String capId, String holdId, String settleTxnId, String merchantCode, String settleKbn, java.math.BigDecimal capAmt) {
            this.capId = capId;
            this.holdId = holdId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.settleKbn = settleKbn;
            this.capAmt = capAmt;
        }
    }

    private static final class SettleRow {
        private final String settleTxnId;
        private final String merchantCode;
        private final java.math.BigDecimal txnAmt;
        private final String settleKbn;

        private SettleRow(String settleTxnId, String merchantCode, java.math.BigDecimal txnAmt, String settleKbn) {
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.txnAmt = txnAmt;
            this.settleKbn = settleKbn;
        }
    }

    private static final class KeyResultRow {
        private final String traceKey;
        private final String holdId;
        private final String capId;
        private final String settleTxnId;
        private final String merchantCode;
        private final String checkResult;

        private KeyResultRow(String traceKey, String holdId, String capId, String settleTxnId, String merchantCode, String checkResult) {
            this.traceKey = traceKey;
            this.holdId = holdId;
            this.capId = capId;
            this.settleTxnId = settleTxnId;
            this.merchantCode = merchantCode;
            this.checkResult = checkResult;
        }
    }
}
