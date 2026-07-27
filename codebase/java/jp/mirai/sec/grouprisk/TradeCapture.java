package jp.mirai.sec.grouprisk;

public class TradeCapture {
    /*
     * 変更履歴
     * 版数  年月日      担当      概要
     * 1.00  2024-02-13  中川 美和 (E-283)  初版作成
     */

    private static final long MIHFT_MAX_NOTIONAL = 500000000L;

    private static final int RC_ACCEPT = 0;
    private static final int RC_REJECT_MARGIN = 4;
    private static final int RC_REJECT_NOTIONAL = 8;
    private static final int RC_REJECT_TICK = 12;

    private static final java.math.BigDecimal BP_DENOMINATOR = new java.math.BigDecimal("10000");

    public static void main(String[] a) throws Exception {
        java.io.BufferedReader reader;
        if (a != null && a.length > 0) {
            reader = java.nio.file.Files.newBufferedReader(java.nio.file.Paths.get(a[0]), java.nio.charset.StandardCharsets.UTF_8);
        } else {
            reader = new java.io.BufferedReader(new java.io.InputStreamReader(System.in, java.nio.charset.StandardCharsets.UTF_8));
        }

        java.util.Map<String, OrderState> oms = new java.util.LinkedHashMap<String, OrderState>();
        java.util.Set<String> execIds = new java.util.HashSet<String>();

        System.out.println("OUT-KBN,EXEC-ID,ORDER-ID,INSTR-CODE,SIDE-KBN,FILL-QTY,FILL-AMT,EXEC-TS,INSTR-TIER,RATE-BP,TICK-SIZE,MARGIN-AMT,DECISION-CODE,OMS-FILL-QTY,OMS-FILL-AMT,STATUS-KBN");

        String line;
        int lineNo = 0;
        boolean headerChecked = false;
        while ((line = reader.readLine()) != null) {
            lineNo++;
            if (line.trim().isEmpty()) {
                continue;
            }

            java.util.List<String> cols = splitCsv(line);
            if (!headerChecked) {
                headerChecked = true;
                if (isHeader(cols)) {
                    continue;
                }
            }

            ExecFill fill;
            try {
                fill = parseExec(cols);
            } catch (RuntimeException ex) {
                emitReject(lineNo, cols, "12", "形式不正");
                continue;
            }

            TierRule rule = tierRule(fill.instrCode);
            int decision = decide(fill, rule, execIds);
            if (decision == RC_ACCEPT) {
                execIds.add(fill.execId);
                OrderState state = oms.get(fill.orderId);
                if (state == null) {
                    state = new OrderState(fill.orderId, fill.instrCode, fill.sideKbn);
                    oms.put(fill.orderId, state);
                }
                state.apply(fill);
                emitDropCopy(fill, rule, decision, state);
            } else {
                OrderState state = oms.get(fill.orderId);
                emitDropCopy(fill, rule, decision, state);
            }
        }

        for (OrderState state : oms.values()) {
            emitReconcile(state);
        }
    }

    private static ExecFill parseExec(java.util.List<String> cols) {
        if (cols.size() != 7) {
            throw new IllegalArgumentException();
        }
        String execId = required(cols.get(0));
        String orderId = required(cols.get(1));
        String instrCode = required(cols.get(2));
        String sideKbn = required(cols.get(3));
        long fillQty = Long.parseLong(required(cols.get(4)));
        java.math.BigDecimal fillAmt = new java.math.BigDecimal(required(cols.get(5)));
        String execTs = required(cols.get(6));
        java.time.OffsetDateTime.parse(execTs);
        return new ExecFill(execId, orderId, instrCode, sideKbn, fillQty, fillAmt, execTs);
    }

    private static int decide(ExecFill fill, TierRule rule, java.util.Set<String> execIds) {
        if (execIds.contains(fill.execId)) {
            return RC_REJECT_NOTIONAL;
        }
        if (!"B".equals(fill.sideKbn) && !"S".equals(fill.sideKbn)) {
            return RC_REJECT_TICK;
        }
        if (fill.fillQty <= 0 || fill.fillAmt.signum() <= 0) {
            return RC_REJECT_NOTIONAL;
        }
        if (fill.fillAmt.compareTo(new java.math.BigDecimal(MIHFT_MAX_NOTIONAL)) > 0) {
            return RC_REJECT_NOTIONAL;
        }
        java.math.BigDecimal[] div = fill.fillAmt.divideAndRemainder(new java.math.BigDecimal(fill.fillQty));
        if (div[1].signum() != 0) {
            return RC_REJECT_TICK;
        }
        long price = div[0].longValueExact();
        if (price % rule.tickSize != 0) {
            return RC_REJECT_TICK;
        }
        java.math.BigDecimal margin = marginAmount(fill.fillAmt, rule.rateBp);
        if (margin.compareTo(fill.fillAmt) > 0) {
            return RC_REJECT_MARGIN;
        }
        return RC_ACCEPT;
    }

    private static TierRule tierRule(String instrCode) {
        TierRule external = riskModelTierRule(instrCode);
        if (external != null) {
            return external;
        }

        int tier = 3;
        String digits = instrCode.replaceAll("[^0-9]", "");
        if (digits.length() >= 4) {
            int code = Integer.parseInt(digits.substring(0, 4));
            if (code < 4000) {
                tier = 1;
            } else if (code < 8000) {
                tier = 2;
            }
        } else if (instrCode.startsWith("ETF")) {
            tier = 1;
        }

        if (tier == 1) {
            return new TierRule(1, 1000, 100);
        }
        if (tier == 2) {
            return new TierRule(2, 2000, 500);
        }
        return new TierRule(3, 4000, 1000);
    }

    private static TierRule riskModelTierRule(String instrCode) {
        try {
            Class<?> riskModel = Class.forName("RiskModel");
            java.lang.reflect.Method method = riskModel.getMethod("tierOf", String.class);
            Object tierObject = method.invoke(null, instrCode);
            if (tierObject instanceof Number) {
                int tier = ((Number) tierObject).intValue();
                if (tier == 1) {
                    return new TierRule(1, 1000, 100);
                }
                if (tier == 2) {
                    return new TierRule(2, 2000, 500);
                }
                if (tier == 3) {
                    return new TierRule(3, 4000, 1000);
                }
            }
        } catch (ReflectiveOperationException ex) {
            return null;
        } catch (RuntimeException ex) {
            return null;
        }
        return null;
    }

    private static java.math.BigDecimal marginAmount(java.math.BigDecimal notional, int rateBp) {
        return notional.multiply(new java.math.BigDecimal(rateBp)).divide(BP_DENOMINATOR, 0, java.math.RoundingMode.UP);
    }

    private static void emitDropCopy(ExecFill fill, TierRule rule, int decision, OrderState state) {
        java.math.BigDecimal margin = marginAmount(fill.fillAmt, rule.rateBp);
        long omsQty = state == null ? 0L : state.fillQty;
        java.math.BigDecimal omsAmt = state == null ? java.math.BigDecimal.ZERO : state.fillAmt;
        String status = decision == RC_ACCEPT ? "約定取込" : "取込拒否";
        System.out.println(joinCsv(
                "DROP",
                fill.execId,
                fill.orderId,
                fill.instrCode,
                fill.sideKbn,
                String.valueOf(fill.fillQty),
                fill.fillAmt.toPlainString(),
                fill.execTs,
                String.valueOf(rule.tier),
                String.valueOf(rule.rateBp),
                String.valueOf(rule.tickSize),
                margin.toPlainString(),
                String.valueOf(decision),
                String.valueOf(omsQty),
                omsAmt.toPlainString(),
                status));
    }

    private static void emitReconcile(OrderState state) {
        System.out.println(joinCsv(
                "RECON",
                "",
                state.orderId,
                state.instrCode,
                state.sideKbn,
                "",
                "",
                state.lastExecTs,
                "",
                "",
                "",
                "",
                "0",
                String.valueOf(state.fillQty),
                state.fillAmt.toPlainString(),
                state.statusKbn()));
    }

    private static void emitReject(int lineNo, java.util.List<String> cols, String code, String status) {
        String execId = cols.size() > 0 ? cols.get(0) : "";
        String orderId = cols.size() > 1 ? cols.get(1) : "";
        String instrCode = cols.size() > 2 ? cols.get(2) : "";
        String sideKbn = cols.size() > 3 ? cols.get(3) : "";
        System.out.println(joinCsv(
                "DROP",
                execId,
                orderId,
                instrCode,
                sideKbn,
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                code,
                "0",
                "0",
                status + ":" + lineNo));
    }

    private static boolean isHeader(java.util.List<String> cols) {
        return cols.size() == 7
                && "EXEC-ID".equals(cols.get(0))
                && "ORDER-ID".equals(cols.get(1))
                && "INSTR-CODE".equals(cols.get(2))
                && "SIDE-KBN".equals(cols.get(3))
                && "FILL-QTY".equals(cols.get(4))
                && "FILL-AMT".equals(cols.get(5))
                && "EXEC-TS".equals(cols.get(6));
    }

    private static String required(String value) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException();
        }
        return value.trim();
    }

    private static java.util.List<String> splitCsv(String line) {
        java.util.List<String> out = new java.util.ArrayList<String>();
        StringBuilder cur = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (quoted) {
                if (ch == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        cur.append('"');
                        i++;
                    } else {
                        quoted = false;
                    }
                } else {
                    cur.append(ch);
                }
            } else if (ch == '"') {
                quoted = true;
            } else if (ch == ',') {
                out.add(cur.toString());
                cur.setLength(0);
            } else {
                cur.append(ch);
            }
        }
        out.add(cur.toString());
        return out;
    }

    private static String joinCsv(String... values) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                sb.append(',');
            }
            String v = values[i] == null ? "" : values[i];
            boolean quote = v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0;
            if (quote) {
                sb.append('"');
                for (int j = 0; j < v.length(); j++) {
                    char ch = v.charAt(j);
                    if (ch == '"') {
                        sb.append("\"\"");
                    } else {
                        sb.append(ch);
                    }
                }
                sb.append('"');
            } else {
                sb.append(v);
            }
        }
        return sb.toString();
    }

    private static final class ExecFill {
        final String execId;
        final String orderId;
        final String instrCode;
        final String sideKbn;
        final long fillQty;
        final java.math.BigDecimal fillAmt;
        final String execTs;

        ExecFill(String execId, String orderId, String instrCode, String sideKbn, long fillQty, java.math.BigDecimal fillAmt, String execTs) {
            this.execId = execId;
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
            this.fillQty = fillQty;
            this.fillAmt = fillAmt;
            this.execTs = execTs;
        }
    }

    private static final class TierRule {
        final int tier;
        final int rateBp;
        final int tickSize;

        TierRule(int tier, int rateBp, int tickSize) {
            this.tier = tier;
            this.rateBp = rateBp;
            this.tickSize = tickSize;
        }
    }

    private static final class OrderState {
        final String orderId;
        final String instrCode;
        final String sideKbn;
        long fillQty;
        java.math.BigDecimal fillAmt = java.math.BigDecimal.ZERO;
        String lastExecTs = "";

        OrderState(String orderId, String instrCode, String sideKbn) {
            this.orderId = orderId;
            this.instrCode = instrCode;
            this.sideKbn = sideKbn;
        }

        void apply(ExecFill fill) {
            fillQty += fill.fillQty;
            fillAmt = fillAmt.add(fill.fillAmt);
            if (lastExecTs.isEmpty() || fill.execTs.compareTo(lastExecTs) > 0) {
                lastExecTs = fill.execTs;
            }
        }

        String statusKbn() {
            return fillQty > 0 ? "照合済" : "未約定";
        }
    }
}
