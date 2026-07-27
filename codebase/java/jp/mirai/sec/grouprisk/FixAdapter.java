package jp.mirai.sec.grouprisk;

public class FixAdapter {
    /**
     * 変更履歴
     * 版数  年月日      担当    概要
     * 1.0   2019-04-16  運用部  初版作成
     */

    private static final char SOH = '\u0001';

    public static void main(String[] a) {
        java.util.List<String> fixMessages = new java.util.ArrayList<String>();
        if (a != null && a.length > 0) {
            for (String s : a) {
                fixMessages.add(s);
            }
        } else {
            fixMessages.add("8=FIX.4.4|9=120|35=D|49=BUY01|56=GATE01|34=17|52=20250115-03:20:00.000|11=JP-0001|55=7203.T|54=1|38=1000|40=2|44=2800|59=0|60=20250115-03:20:00.000|10=000|");
            fixMessages.add("8=FIX.4.4|9=150|35=8|49=GATE01|56=BUY01|34=18|52=20250115-03:20:01.000|37=ORD-9001|11=JP-0001|17=EX-1|150=0|39=0|55=7203.T|54=1|38=1000|14=0|151=1000|6=0|10=000|");
            fixMessages.add("8=FIX.4.4|9=160|35=8|49=GATE01|56=BUY01|34=19|52=20250115-03:20:04.000|37=ORD-9001|11=JP-0001|17=EX-2|150=F|39=1|55=7203.T|54=1|38=1000|32=300|31=2801|14=300|151=700|6=2801|10=000|");
        }

        java.util.Map<String, InternalOrder> orders = new java.util.LinkedHashMap<String, InternalOrder>();
        java.util.Map<String, FillSummary> fills = new java.util.LinkedHashMap<String, FillSummary>();

        for (String raw : fixMessages) {
            try {
                FixMessage message = FixMessage.parse(raw);
                String type = message.required("35");
                if ("D".equals(type)) {
                    InternalOrder order = toInternalOrder(message);
                    orders.put(order.clientOrderId, order);
                    System.out.println("注文受付: " + order.clientOrderId + " " + order.symbol + " " + order.side + " " + order.quantity + "株");
                } else if ("8".equals(type)) {
                    Fill fill = toFill(message);
                    FillSummary summary = fills.get(fill.clientOrderId);
                    if (summary == null) {
                        summary = new FillSummary(fill.clientOrderId, fill.symbol, fill.side, fill.orderQuantity);
                        fills.put(fill.clientOrderId, summary);
                    }
                    summary.apply(fill);
                    System.out.println("約定通知: " + fill.clientOrderId + " 状態=" + fill.orderStatus + " 累計=" + summary.cumQuantity + " 残=" + summary.leavesQuantity);
                } else {
                    System.out.println("対象外電文: 種別=" + type);
                }
            } catch (RuntimeException ex) {
                System.out.println("電文不正: " + ex.getMessage());
            }
        }

        for (FillSummary s : fills.values()) {
            System.out.println("集計結果: " + s.clientOrderId + " " + s.symbol + " 累計数量=" + s.cumQuantity + " 平均単価=" + s.averagePrice());
        }
    }

    private static InternalOrder toInternalOrder(FixMessage m) {
        String clientOrderId = m.required("11");
        String symbol = m.required("55");
        String side = decodeSide(m.required("54"));
        int quantity = positiveInt(m.required("38"), "注文数量");
        String orderType = decodeOrderType(m.required("40"));
        java.math.BigDecimal price = null;
        if ("指値".equals(orderType)) {
            price = positiveDecimal(m.required("44"), "指値単価");
        }
        String timeInForce = decodeTimeInForce(m.optional("59", "0"));
        String transactTime = m.optional("60", m.optional("52", ""));

        validateSymbol(symbol);
        validateOrderScale(symbol, quantity, price);

        return new InternalOrder(clientOrderId, symbol, side, quantity, orderType, price, timeInForce, transactTime);
    }

    private static Fill toFill(FixMessage m) {
        String clientOrderId = m.required("11");
        String symbol = m.required("55");
        String side = decodeSide(m.required("54"));
        int orderQuantity = nonNegativeInt(m.required("38"), "注文数量");
        int lastQuantity = nonNegativeInt(m.optional("32", "0"), "直近約定数量");
        java.math.BigDecimal lastPrice = lastQuantity == 0
                ? java.math.BigDecimal.ZERO
                : positiveDecimal(m.required("31"), "直近約定単価");
        int cumQuantity = nonNegativeInt(m.required("14"), "累計約定数量");
        int leavesQuantity = nonNegativeInt(m.required("151"), "残数量");
        java.math.BigDecimal averagePrice = positiveOrZeroDecimal(m.required("6"), "平均単価");
        String execType = decodeExecType(m.required("150"));
        String orderStatus = decodeOrderStatus(m.required("39"));

        if (cumQuantity + leavesQuantity > orderQuantity) {
            throw new IllegalArgumentException("数量整合性不正");
        }
        if (lastQuantity == 0 && lastPrice.signum() != 0) {
            throw new IllegalArgumentException("直近約定単価不正");
        }
        if (cumQuantity == 0 && averagePrice.signum() != 0) {
            throw new IllegalArgumentException("平均単価不正");
        }

        return new Fill(clientOrderId, symbol, side, orderQuantity, lastQuantity, lastPrice, cumQuantity, leavesQuantity, averagePrice, execType, orderStatus);
    }

    private static void validateSymbol(String symbol) {
        if (!symbol.matches("[0-9A-Z]{4}(\\.T)?")) {
            throw new IllegalArgumentException("銘柄形式不正");
        }
    }

    private static void validateOrderScale(String symbol, int quantity, java.math.BigDecimal price) {
        if (quantity % 100 != 0) {
            throw new IllegalArgumentException("売買単位不正");
        }
        if (quantity > 1000000) {
            throw new IllegalArgumentException("大口上限超過");
        }
        if (price != null && price.scale() > 1) {
            throw new IllegalArgumentException("呼値単位不正");
        }
        reflectRiskModel(symbol, quantity, price);
    }

    private static void reflectRiskModel(String symbol, int quantity, java.math.BigDecimal price) {
        try {
            Class<?> c = Class.forName("RiskModel");
            java.lang.reflect.Method[] methods = c.getMethods();
            for (java.lang.reflect.Method method : methods) {
                if ("validateOrder".equals(method.getName()) && method.getParameterTypes().length == 3) {
                    method.invoke(null, symbol, Integer.valueOf(quantity), price);
                    return;
                }
            }
        } catch (ClassNotFoundException ex) {
            return;
        } catch (ReflectiveOperationException ex) {
            Throwable cause = ex.getCause();
            if (cause instanceof RuntimeException) {
                throw (RuntimeException) cause;
            }
            throw new IllegalArgumentException("リスク判定失敗");
        }
    }

    private static String decodeSide(String v) {
        if ("1".equals(v)) {
            return "買";
        }
        if ("2".equals(v)) {
            return "売";
        }
        throw new IllegalArgumentException("売買区分不正");
    }

    private static String decodeOrderType(String v) {
        if ("1".equals(v)) {
            return "成行";
        }
        if ("2".equals(v)) {
            return "指値";
        }
        throw new IllegalArgumentException("注文種別不正");
    }

    private static String decodeTimeInForce(String v) {
        if ("0".equals(v)) {
            return "当日";
        }
        if ("3".equals(v)) {
            return "引け";
        }
        if ("4".equals(v)) {
            return "寄付";
        }
        throw new IllegalArgumentException("有効条件不正");
    }

    private static String decodeExecType(String v) {
        if ("0".equals(v)) {
            return "新規受付";
        }
        if ("4".equals(v)) {
            return "取消";
        }
        if ("5".equals(v)) {
            return "訂正";
        }
        if ("8".equals(v)) {
            return "拒否";
        }
        if ("F".equals(v)) {
            return "約定";
        }
        throw new IllegalArgumentException("執行種別不正");
    }

    private static String decodeOrderStatus(String v) {
        if ("0".equals(v)) {
            return "受付済";
        }
        if ("1".equals(v)) {
            return "一部約定";
        }
        if ("2".equals(v)) {
            return "全約定";
        }
        if ("4".equals(v)) {
            return "取消済";
        }
        if ("8".equals(v)) {
            return "拒否";
        }
        throw new IllegalArgumentException("注文状態不正");
    }

    private static int positiveInt(String v, String name) {
        int n = nonNegativeInt(v, name);
        if (n <= 0) {
            throw new IllegalArgumentException(name + "不正");
        }
        return n;
    }

    private static int nonNegativeInt(String v, String name) {
        try {
            int n = Integer.parseInt(v);
            if (n < 0) {
                throw new IllegalArgumentException(name + "不正");
            }
            return n;
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(name + "不正");
        }
    }

    private static java.math.BigDecimal positiveDecimal(String v, String name) {
        java.math.BigDecimal d = positiveOrZeroDecimal(v, name);
        if (d.signum() <= 0) {
            throw new IllegalArgumentException(name + "不正");
        }
        return d;
    }

    private static java.math.BigDecimal positiveOrZeroDecimal(String v, String name) {
        try {
            java.math.BigDecimal d = new java.math.BigDecimal(v);
            if (d.signum() < 0) {
                throw new IllegalArgumentException(name + "不正");
            }
            return d;
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(name + "不正");
        }
    }

    private static final class FixMessage {
        private final java.util.Map<String, String> tags;

        private FixMessage(java.util.Map<String, String> tags) {
            this.tags = tags;
        }

        static FixMessage parse(String raw) {
            if (raw == null || raw.trim().isEmpty()) {
                throw new IllegalArgumentException("空電文");
            }
            String normalized = raw.indexOf(SOH) >= 0 ? raw : raw.replace('|', SOH);
            String[] fields = normalized.split(String.valueOf(SOH));
            java.util.Map<String, String> tags = new java.util.LinkedHashMap<String, String>();
            for (String field : fields) {
                if (field.isEmpty()) {
                    continue;
                }
                int p = field.indexOf('=');
                if (p <= 0 || p == field.length() - 1) {
                    throw new IllegalArgumentException("項目形式不正");
                }
                tags.put(field.substring(0, p), field.substring(p + 1));
            }
            if (!tags.containsKey("8") || !tags.containsKey("35")) {
                throw new IllegalArgumentException("必須項目不足");
            }
            return new FixMessage(tags);
        }

        String required(String tag) {
            String v = tags.get(tag);
            if (v == null || v.isEmpty()) {
                throw new IllegalArgumentException("必須タグ不足:" + tag);
            }
            return v;
        }

        String optional(String tag, String defaultValue) {
            String v = tags.get(tag);
            return v == null || v.isEmpty() ? defaultValue : v;
        }
    }

    private static final class InternalOrder {
        final String clientOrderId;
        final String symbol;
        final String side;
        final int quantity;
        final String orderType;
        final java.math.BigDecimal price;
        final String timeInForce;
        final String transactTime;

        InternalOrder(String clientOrderId, String symbol, String side, int quantity, String orderType, java.math.BigDecimal price, String timeInForce, String transactTime) {
            this.clientOrderId = clientOrderId;
            this.symbol = symbol;
            this.side = side;
            this.quantity = quantity;
            this.orderType = orderType;
            this.price = price;
            this.timeInForce = timeInForce;
            this.transactTime = transactTime;
        }
    }

    private static final class Fill {
        final String clientOrderId;
        final String symbol;
        final String side;
        final int orderQuantity;
        final int lastQuantity;
        final java.math.BigDecimal lastPrice;
        final int cumQuantity;
        final int leavesQuantity;
        final java.math.BigDecimal averagePrice;
        final String execType;
        final String orderStatus;

        Fill(String clientOrderId, String symbol, String side, int orderQuantity, int lastQuantity, java.math.BigDecimal lastPrice, int cumQuantity, int leavesQuantity, java.math.BigDecimal averagePrice, String execType, String orderStatus) {
            this.clientOrderId = clientOrderId;
            this.symbol = symbol;
            this.side = side;
            this.orderQuantity = orderQuantity;
            this.lastQuantity = lastQuantity;
            this.lastPrice = lastPrice;
            this.cumQuantity = cumQuantity;
            this.leavesQuantity = leavesQuantity;
            this.averagePrice = averagePrice;
            this.execType = execType;
            this.orderStatus = orderStatus;
        }
    }

    private static final class FillSummary {
        final String clientOrderId;
        final String symbol;
        final String side;
        final int orderQuantity;
        int cumQuantity;
        int leavesQuantity;
        java.math.BigDecimal notional;

        FillSummary(String clientOrderId, String symbol, String side, int orderQuantity) {
            this.clientOrderId = clientOrderId;
            this.symbol = symbol;
            this.side = side;
            this.orderQuantity = orderQuantity;
            this.leavesQuantity = orderQuantity;
            this.notional = java.math.BigDecimal.ZERO;
        }

        void apply(Fill fill) {
            if (!clientOrderId.equals(fill.clientOrderId) || !symbol.equals(fill.symbol) || !side.equals(fill.side)) {
                throw new IllegalArgumentException("約定紐付け不正");
            }
            if (fill.cumQuantity < cumQuantity) {
                throw new IllegalArgumentException("累計数量逆転");
            }
            int delta = fill.cumQuantity - cumQuantity;
            if (delta > 0) {
                java.math.BigDecimal price = fill.lastQuantity == delta ? fill.lastPrice : fill.averagePrice;
                notional = notional.add(price.multiply(java.math.BigDecimal.valueOf(delta)));
            }
            cumQuantity = fill.cumQuantity;
            leavesQuantity = fill.leavesQuantity;
        }

        java.math.BigDecimal averagePrice() {
            if (cumQuantity == 0) {
                return java.math.BigDecimal.ZERO;
            }
            return notional.divide(java.math.BigDecimal.valueOf(cumQuantity), 4, java.math.RoundingMode.HALF_UP);
        }
    }
}
