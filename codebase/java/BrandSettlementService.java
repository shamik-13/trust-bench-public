public class BrandSettlementService {
    /**
     * 変更履歴
     * 版数  年月日      担当    概要
     * 1.00  2024/03/18  精算    初版作成。国際ブランド精算結果を取引単位に照会出力。
     * 1.01  2025/02/07  精算    レート表示桁数を小数第4位から小数第6位へ変更。
     * 1.02  2025/11/12  精算    CDOVSF確定済み手数料の表示を追加。海外ATM固有計算は対象外。
     */

    private static final String PROGRAM_ID = "BRDSETL";
    private static final java.math.BigDecimal ONE_HUNDRED = new java.math.BigDecimal("100");
    private static final java.math.RoundingMode ROUND = java.math.RoundingMode.HALF_UP;

    public void execute(java.util.List<Cdbrdf> brandRows,
                 java.util.List<Cdfxrf> rateRows,
                 java.util.List<Cdovsf> overseaRows,
                 java.util.List<Cdmvwf> outRows,
                 java.util.List<Cdlogf> logRows) {
        java.util.Map<String, Cdfxrf> rateIndex = new java.util.HashMap<String, Cdfxrf>();
        for (Cdfxrf r : rateRows) {
            if (isBlank(r.rateDt) || isBlank(r.brandKbn) || isBlank(r.ccyCd) || r.fxRate == null || r.markupRate == null) {
                addLog(logRows, "", "E", "RATE-FMT");
                continue;
            }
            rateIndex.put(rateKey(r.rateDt, r.brandKbn, r.ccyCd), r);
        }

        java.util.Map<String, Cdovsf> fixedIndex = new java.util.HashMap<String, Cdovsf>();
        for (Cdovsf r : overseaRows) {
            if (!PROGRAM_ID.equals(r.programId)) {
                continue;
            }
            if ("D".equals(r.setlKbn)) {
                fixedIndex.put(r.txnId, r);
            }
        }

        java.util.Set<String> processed = new java.util.HashSet<String>();
        for (Cdbrdf b : brandRows) {
            if (!validateBrandRow(b, logRows)) {
                continue;
            }
            if (!processed.add(b.txnId)) {
                addLog(logRows, b.cardNo, "W", "DUP-TXN");
                continue;
            }

            Cdovsf o = fixedIndex.get(b.txnId);
            if (o == null) {
                addLog(logRows, b.cardNo, "W", "NO-OVS");
                continue;
            }
            if (!sameCard(b.cardNo, o.cardNo)) {
                addLog(logRows, b.cardNo, "E", "CARD-NG");
                continue;
            }

            Cdfxrf rate = rateIndex.get(rateKey(b.setlDt, b.brandKbn, b.ccyCd));
            if (rate == null) {
                addLog(logRows, b.cardNo, "E", "NO-RATE");
                continue;
            }

            java.math.BigDecimal markupMultiplier = java.math.BigDecimal.ONE.add(
                    rate.markupRate.divide(ONE_HUNDRED, 10, ROUND));
            java.math.BigDecimal referenceJpy = b.brandAmt.multiply(rate.fxRate)
                    .multiply(markupMultiplier).setScale(0, ROUND);
            java.math.BigDecimal brandDiff = b.jpyAmt.subtract(referenceJpy);
            java.math.BigDecimal fixedDiff = o.setlAmt.subtract(b.jpyAmt);
            String dispKbn = toDispKbn(o.txnKbn);

            outRows.add(new Cdmvwf(b.cardNo, b.txnId, dispKbn, b.brandAmt,
                    "ブランド精算額 " + b.ccyCd + " " + money(b.brandAmt)
                            + " レート " + rate.fxRate.setScale(6, ROUND).toPlainString()));
            outRows.add(new Cdmvwf(b.cardNo, b.txnId, dispKbn, b.jpyAmt,
                    "円換算額 " + yen(b.jpyAmt) + " 差分 " + signedYen(brandDiff)));
            outRows.add(new Cdmvwf(b.cardNo, b.txnId, dispKbn, o.setlAmt,
                    "請求確定額 " + yen(o.setlAmt) + " 差分 " + signedYen(fixedDiff)));

            if (!"00".equals(o.feeKbn)) {
                outRows.add(new Cdmvwf(b.cardNo, b.txnId, dispKbn, o.feeAmt,
                        "確定済手数料 " + o.feeKbn + " " + yen(o.feeAmt)));
            }
            if (!isBlank(o.intStartDt)) {
                addLog(logRows, b.cardNo, "I", "INT-DISP");
            }
            addLog(logRows, b.cardNo, "I", "SETL-OK");
        }
    }

    private static boolean validateBrandRow(Cdbrdf b, java.util.List<Cdlogf> logRows) {
        if (b == null || isBlank(b.brandSetlId) || isBlank(b.txnId) || isBlank(b.cardNo)
                || isBlank(b.brandKbn) || isBlank(b.ccyCd) || b.brandAmt == null
                || b.jpyAmt == null || isBlank(b.setlDt)) {
            addLog(logRows, b == null ? "" : b.cardNo, "E", "BRD-FMT");
            return false;
        }
        if (b.brandAmt.signum() < 0 || b.jpyAmt.signum() < 0) {
            addLog(logRows, b.cardNo, "E", "AMT-NG");
            return false;
        }
        return true;
    }

    private static String toDispKbn(String txnKbn) {
        if ("P1".equals(txnKbn) || "P2".equals(txnKbn)) {
            return "S";
        }
        if ("C1".equals(txnKbn) || "C2".equals(txnKbn)) {
            return "K";
        }
        return "S";
    }

    private static boolean sameCard(String left, String right) {
        return left != null && left.equals(right);
    }

    private static String rateKey(String rateDt, String brandKbn, String ccyCd) {
        return rateDt + "|" + brandKbn + "|" + ccyCd;
    }

    private static boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }

    private static String money(java.math.BigDecimal v) {
        return v.setScale(2, ROUND).toPlainString();
    }

    private static String yen(java.math.BigDecimal v) {
        return v.setScale(0, ROUND).toPlainString() + "円";
    }

    private static String signedYen(java.math.BigDecimal v) {
        java.math.BigDecimal rounded = v.setScale(0, ROUND);
        return (rounded.signum() >= 0 ? "+" : "") + rounded.toPlainString() + "円";
    }

    private static void addLog(java.util.List<Cdlogf> logRows, String cardNo, String eventKbn, String detailCd) {
        String logId = PROGRAM_ID + "-" + String.format("%05d", logRows.size() + 1);
        logRows.add(new Cdlogf(logId, PROGRAM_ID, cardNo, eventKbn,
                java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE), detailCd));
    }

    private static final class Cdbrdf {
        final String brandSetlId;
        final String txnId;
        final String cardNo;
        final String brandKbn;
        final String ccyCd;
        final java.math.BigDecimal brandAmt;
        final java.math.BigDecimal jpyAmt;
        final String setlDt;

        Cdbrdf(String brandSetlId, String txnId, String cardNo, String brandKbn, String ccyCd,
               java.math.BigDecimal brandAmt, java.math.BigDecimal jpyAmt, String setlDt) {
            this.brandSetlId = brandSetlId;
            this.txnId = txnId;
            this.cardNo = cardNo;
            this.brandKbn = brandKbn;
            this.ccyCd = ccyCd;
            this.brandAmt = brandAmt;
            this.jpyAmt = jpyAmt;
            this.setlDt = setlDt;
        }
    }

    private static final class Cdfxrf {
        final String rateDt;
        final String brandKbn;
        final String ccyCd;
        final java.math.BigDecimal fxRate;
        final java.math.BigDecimal markupRate;
        final String sourceKbn;

        Cdfxrf(String rateDt, String brandKbn, String ccyCd, java.math.BigDecimal fxRate,
               java.math.BigDecimal markupRate, String sourceKbn) {
            this.rateDt = rateDt;
            this.brandKbn = brandKbn;
            this.ccyCd = ccyCd;
            this.fxRate = fxRate;
            this.markupRate = markupRate;
            this.sourceKbn = sourceKbn;
        }
    }

    private static final class Cdovsf {
        final String txnId;
        final String cardNo;
        final String txnKbn;
        final String feeKbn;
        final java.math.BigDecimal feeAmt;
        final String intStartDt;
        final java.math.BigDecimal setlAmt;
        final String setlKbn;
        final String programId;

        Cdovsf(String txnId, String cardNo, String txnKbn, String feeKbn, java.math.BigDecimal feeAmt,
               String intStartDt, java.math.BigDecimal setlAmt, String setlKbn, String programId) {
            this.txnId = txnId;
            this.cardNo = cardNo;
            this.txnKbn = txnKbn;
            this.feeKbn = feeKbn;
            this.feeAmt = feeAmt;
            this.intStartDt = intStartDt;
            this.setlAmt = setlAmt;
            this.setlKbn = setlKbn;
            this.programId = programId;
        }
    }

    private static final class Cdmvwf {
        final String cardNo;
        final String txnId;
        final String dispKbn;
        final java.math.BigDecimal dispAmt;
        final String dispLabel;

        Cdmvwf(String cardNo, String txnId, String dispKbn, java.math.BigDecimal dispAmt, String dispLabel) {
            this.cardNo = cardNo;
            this.txnId = txnId;
            this.dispKbn = dispKbn;
            this.dispAmt = dispAmt;
            this.dispLabel = dispLabel;
        }
    }

    private static final class Cdlogf {
        final String logId;
        final String programId;
        final String cardNo;
        final String eventKbn;
        final String eventDt;
        final String detailCd;

        Cdlogf(String logId, String programId, String cardNo, String eventKbn, String eventDt, String detailCd) {
            this.logId = logId;
            this.programId = programId;
            this.cardNo = cardNo;
            this.eventKbn = eventKbn;
            this.eventDt = eventDt;
            this.detailCd = detailCd;
        }
    }
}
