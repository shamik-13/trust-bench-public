package jp.mirai.sec.pretrade;

public class IntradayRiskService {
    private final java.util.Map<String, Position> positions = new java.util.HashMap<>();
    private final java.util.Map<String, RiskLimit> limits = new java.util.HashMap<>();

    public IntradayRiskService() {
    }

    public void setLimit(String symbol, long maxLongPosition, long maxShortPosition, double maxNotional) {
        requireSymbol(symbol);
        if (maxLongPosition < 0 || maxShortPosition < 0 || maxNotional < 0.0 || Double.isNaN(maxNotional)) {
            throw new IllegalArgumentException("Limits must be non-negative");
        }
        limits.put(symbol, new RiskLimit(maxLongPosition, maxShortPosition, maxNotional));
    }

    public boolean canAcceptOrder(String symbol, Side side, long quantity, double price) {
        requireOrder(symbol, side, quantity, price);

        Position current = positions.getOrDefault(symbol, new Position());
        Position projected = current.copy();
        projected.apply(side, quantity, price);

        RiskLimit limit = limits.get(symbol);
        if (limit == null) {
            return true;
        }

        return projected.netQuantity <= limit.maxLongPosition
                && projected.netQuantity >= -limit.maxShortPosition
                && projected.grossNotional <= limit.maxNotional;
    }

    public void recordTrade(String symbol, Side side, long quantity, double price) {
        if (!canAcceptOrder(symbol, side, quantity, price)) {
            throw new IllegalStateException("Trade would breach intraday risk limits");
        }
        positions.computeIfAbsent(symbol, ignored -> new Position()).apply(side, quantity, price);
    }

    public long getNetPosition(String symbol) {
        requireSymbol(symbol);
        return positions.getOrDefault(symbol, new Position()).netQuantity;
    }

    public double getGrossNotional(String symbol) {
        requireSymbol(symbol);
        return positions.getOrDefault(symbol, new Position()).grossNotional;
    }

    public double getRealizedPnl(String symbol) {
        requireSymbol(symbol);
        return positions.getOrDefault(symbol, new Position()).realizedPnl;
    }

    public void resetIntradayState() {
        positions.clear();
    }

    private static void requireSymbol(String symbol) {
        if (symbol == null || symbol.trim().isEmpty()) {
            throw new IllegalArgumentException("Symbol is required");
        }
    }

    private static void requireOrder(String symbol, Side side, long quantity, double price) {
        requireSymbol(symbol);
        if (side == null) {
            throw new IllegalArgumentException("Side is required");
        }
        if (quantity <= 0) {
            throw new IllegalArgumentException("Quantity must be positive");
        }
        if (price <= 0.0 || Double.isNaN(price) || Double.isInfinite(price)) {
            throw new IllegalArgumentException("Price must be a finite positive value");
        }
    }

    public enum Side {
        BUY,
        SELL
    }

    private static final class RiskLimit {
        private final long maxLongPosition;
        private final long maxShortPosition;
        private final double maxNotional;

        private RiskLimit(long maxLongPosition, long maxShortPosition, double maxNotional) {
            this.maxLongPosition = maxLongPosition;
            this.maxShortPosition = maxShortPosition;
            this.maxNotional = maxNotional;
        }
    }

    private static final class Position {
        private long netQuantity;
        private double averageCost;
        private double grossNotional;
        private double realizedPnl;

        private Position copy() {
            Position copy = new Position();
            copy.netQuantity = netQuantity;
            copy.averageCost = averageCost;
            copy.grossNotional = grossNotional;
            copy.realizedPnl = realizedPnl;
            return copy;
        }

        private void apply(Side side, long quantity, double price) {
            grossNotional += quantity * price;
            long signedQuantity = side == Side.BUY ? quantity : -quantity;

            if (netQuantity == 0 || sameDirection(netQuantity, signedQuantity)) {
                long newNetQuantity = netQuantity + signedQuantity;
                averageCost = ((Math.abs(netQuantity) * averageCost) + (quantity * price))
                        / Math.abs(newNetQuantity);
                netQuantity = newNetQuantity;
                return;
            }

            long closingQuantity = Math.min(Math.abs(netQuantity), quantity);
            if (netQuantity > 0) {
                realizedPnl += closingQuantity * (price - averageCost);
            } else {
                realizedPnl += closingQuantity * (averageCost - price);
            }

            netQuantity += signedQuantity;
            if (netQuantity == 0) {
                averageCost = 0.0;
            } else if (Math.abs(signedQuantity) > closingQuantity) {
                averageCost = price;
            }
        }

        private static boolean sameDirection(long a, long b) {
            return (a > 0 && b > 0) || (a < 0 && b < 0);
        }
    }
}
