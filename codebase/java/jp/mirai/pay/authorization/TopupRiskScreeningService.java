package jp.mirai.pay.authorization;

/**
 * 変更履歴
 * 版数  年月日      担当      概要
 * 1.0   2025-01-23  みらいペイ システム部  トップアップリスク判定サービス初版作成
 */
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class TopupRiskScreeningService {
    private static final Charset 入出力文字コード = StandardCharsets.UTF_8;
    private static final String 店舗コード = "JP-TOPUP";
    private static final BigDecimal 高額チャージ境界 = new BigDecimal("100000");
    private static final BigDecimal 中額チャージ境界 = new BigDecimal("30000");
    private static final BigDecimal 速度金額境界 = new BigDecimal("200000");
    private static final int 追加認証境界 = 70;

    public static void main(String[] a) throws Exception {
        Path topupPath = Paths.get(a.length > 0 ? a[0] : "PYTOPF.csv");
        Path velocityPath = Paths.get(a.length > 1 ? a[1] : "PYVELF.csv");
        Path scorePath = Paths.get(a.length > 2 ? a[2] : "PYSCOF.csv");

        List<TopupLine> topups = readTopups(topupPath);
        Map<String, VelocityLine> velocities = readVelocities(velocityPath);
        Map<String, ScoreLine> latestScores = readLatestScores(scorePath);

        List<ScoreLine> appended = new ArrayList<>();
        Instant 判定時刻 = Instant.now();

        topups.stream()
                .filter(t -> "受付中".equals(t.status) || "AUTH_REQUESTED".equals(t.status))
                .sorted(Comparator.comparing(t -> t.requestTs))
                .forEach(topup -> {
                    VelocityLine velocity = velocities.get(topup.walletId);
                    ScoreLine previous = latestScores.get(topup.walletId);
                    ScreeningResult result = screen(topup, velocity, previous, 判定時刻);
                    ScoreLine score = new ScoreLine(
                            "SCR-" + topup.topupId,
                            topup.walletId,
                            店舗コード,
                            result.score,
                            result.reason,
                            判定時刻
                    );
                    appended.add(score);
                    latestScores.put(topup.walletId, score);
                });

        if (!appended.isEmpty()) {
            appendScores(scorePath, appended);
        }

        System.out.println("判定件数=" + appended.size());
    }

    private static ScreeningResult screen(TopupLine topup, VelocityLine velocity, ScoreLine previous, Instant now) {
        int score = 0;
        List<String> reasons = new ArrayList<>();

        if (topup.amount.compareTo(BigDecimal.ZERO) <= 0) {
            score += 100;
            reasons.add("金額不正");
        }

        if (topup.amount.compareTo(高額チャージ境界) >= 0) {
            score += 35;
            reasons.add("高額チャージ");
        } else if (topup.amount.compareTo(中額チャージ境界) >= 0) {
            score += 15;
            reasons.add("中額チャージ");
        }

        if (!("銀行振込".equals(topup.paymentMethod)
                || "クレジット".equals(topup.paymentMethod)
                || "デビット".equals(topup.paymentMethod)
                || "コンビニ".equals(topup.paymentMethod))) {
            score += 20;
            reasons.add("支払手段未確認");
        }

        long requestAgeMinutes = Math.abs(Duration.between(topup.requestTs, now).toMinutes());
        if (requestAgeMinutes > 24L * 60L) {
            score += 10;
            reasons.add("要求時刻乖離");
        }

        if (velocity == null) {
            score += 18;
            reasons.add("速度情報なし");
        } else {
            if (velocity.authCount >= 5) {
                score += 22;
                reasons.add("認証回数過多");
            }
            if (velocity.denyCount >= 2) {
                score += 28;
                reasons.add("否認履歴あり");
            }
            if (velocity.authSumAmt.add(topup.amount).compareTo(速度金額境界) >= 0) {
                score += 25;
                reasons.add("累計金額過多");
            }
            long intervalSeconds = Math.abs(Duration.between(velocity.lastReqTs, topup.requestTs).getSeconds());
            if (intervalSeconds <= 180) {
                score += 16;
                reasons.add("短時間再要求");
            }
        }

        if (previous != null) {
            if (previous.riskScore >= 80) {
                score += 30;
                reasons.add("既存高リスク");
            } else if (previous.riskScore >= 50) {
                score += 12;
                reasons.add("既存中リスク");
            }
        }

        int normalized = Math.min(100, Math.max(0, score));
        String decision = normalized >= 追加認証境界 ? "追加認証要" : "通常受付可";
        if (reasons.isEmpty()) {
            reasons.add("通常範囲");
        }
        return new ScreeningResult(normalized, decision + ":" + String.join("/", reasons));
    }

    private static List<TopupLine> readTopups(Path path) throws IOException {
        List<TopupLine> rows = new ArrayList<>();
        if (!Files.exists(path)) {
            return rows;
        }
        try (BufferedReader reader = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            boolean first = true;
            while ((line = reader.readLine()) != null) {
                if (first && line.toUpperCase(Locale.ROOT).contains("TOPUP-ID")) {
                    first = false;
                    continue;
                }
                first = false;
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> cols = splitCsv(line);
                if (cols.size() < 6) {
                    throw new IllegalArgumentException("PYTOPF項目数不正:" + line);
                }
                rows.add(new TopupLine(
                        require(cols.get(0), "TOPUP-ID"),
                        require(cols.get(1), "WALLET-ID"),
                        money(cols.get(2), "TOPUP-AMT"),
                        require(cols.get(3), "PAYMENT-METHOD"),
                        require(cols.get(4), "TOPUP-STATUS"),
                        instant(cols.get(5), "REQUEST-TS")
                ));
            }
        }
        return rows;
    }

    private static Map<String, VelocityLine> readVelocities(Path path) throws IOException {
        Map<String, VelocityLine> rows = new HashMap<>();
        if (!Files.exists(path)) {
            return rows;
        }
        try (BufferedReader reader = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            boolean first = true;
            while ((line = reader.readLine()) != null) {
                if (first && line.toUpperCase(Locale.ROOT).contains("WALLET-ID")) {
                    first = false;
                    continue;
                }
                first = false;
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> cols = splitCsv(line);
                if (cols.size() < 6) {
                    throw new IllegalArgumentException("PYVELF項目数不正:" + line);
                }
                VelocityLine row = new VelocityLine(
                        require(cols.get(0), "WALLET-ID"),
                        instant(cols.get(1), "WINDOW-START-TS"),
                        integer(cols.get(2), "AUTH-COUNT"),
                        money(cols.get(3), "AUTH-SUM-AMT"),
                        integer(cols.get(4), "DENY-COUNT"),
                        instant(cols.get(5), "LAST-REQ-TS")
                );
                rows.put(row.walletId, row);
            }
        }
        return rows;
    }

    private static Map<String, ScoreLine> readLatestScores(Path path) throws IOException {
        Map<String, ScoreLine> rows = new HashMap<>();
        if (!Files.exists(path)) {
            return rows;
        }
        try (BufferedReader reader = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            boolean first = true;
            while ((line = reader.readLine()) != null) {
                if (first && line.toUpperCase(Locale.ROOT).contains("SCORE-ID")) {
                    first = false;
                    continue;
                }
                first = false;
                if (line.trim().isEmpty()) {
                    continue;
                }
                List<String> cols = splitCsv(line);
                if (cols.size() < 6) {
                    throw new IllegalArgumentException("PYSCOF項目数不正:" + line);
                }
                ScoreLine row = new ScoreLine(
                        require(cols.get(0), "SCORE-ID"),
                        require(cols.get(1), "WALLET-ID"),
                        require(cols.get(2), "MERCHANT-CODE"),
                        integer(cols.get(3), "RISK-SCORE"),
                        require(cols.get(4), "SCORE-REASON"),
                        instant(cols.get(5), "SCORE-AS-OF-TS")
                );
                ScoreLine current = rows.get(row.walletId);
                if (current == null || row.asOfTs.isAfter(current.asOfTs)) {
                    rows.put(row.walletId, row);
                }
            }
        }
        return rows;
    }

    private static void appendScores(Path path, List<ScoreLine> rows) throws IOException {
        boolean writeHeader = !Files.exists(path) || Files.size(path) == 0;
        try (BufferedWriter writer = Files.newBufferedWriter(
                path,
                入出力文字コード,
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.APPEND)) {
            if (writeHeader) {
                writer.write("SCORE-ID,WALLET-ID,MERCHANT-CODE,RISK-SCORE,SCORE-REASON,SCORE-AS-OF-TS");
                writer.newLine();
            }
            for (ScoreLine row : rows) {
                writer.write(csv(row.scoreId));
                writer.write(',');
                writer.write(csv(row.walletId));
                writer.write(',');
                writer.write(csv(row.merchantCode));
                writer.write(',');
                writer.write(Integer.toString(row.riskScore));
                writer.write(',');
                writer.write(csv(row.reason));
                writer.write(',');
                writer.write(csv(row.asOfTs.toString()));
                writer.newLine();
            }
        }
    }

    private static List<String> splitCsv(String line) {
        List<String> cols = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quoted && c == '"' && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                current.append('"');
                i++;
            } else if (c == '"') {
                quoted = !quoted;
            } else if (c == ',' && !quoted) {
                cols.add(current.toString().trim());
                current.setLength(0);
            } else {
                current.append(c);
            }
        }
        cols.add(current.toString().trim());
        return cols;
    }

    private static String csv(String value) {
        String v = value == null ? "" : value;
        if (v.indexOf(',') >= 0 || v.indexOf('"') >= 0 || v.indexOf('\n') >= 0 || v.indexOf('\r') >= 0) {
            return '"' + v.replace("\"", "\"\"") + '"';
        }
        return v;
    }

    private static String require(String value, String name) {
        if (value == null || value.trim().isEmpty()) {
            throw new IllegalArgumentException(name + "未設定");
        }
        return value.trim();
    }

    private static BigDecimal money(String value, String name) {
        try {
            return new BigDecimal(require(value, name)).setScale(0, RoundingMode.DOWN);
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(name + "数値不正:" + value, ex);
        }
    }

    private static int integer(String value, String name) {
        try {
            int parsed = Integer.parseInt(require(value, name));
            if (parsed < 0) {
                throw new IllegalArgumentException(name + "負数不正:" + value);
            }
            return parsed;
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(name + "整数不正:" + value, ex);
        }
    }

    private static Instant instant(String value, String name) {
        try {
            return Instant.parse(require(value, name));
        } catch (DateTimeParseException ex) {
            throw new IllegalArgumentException(name + "時刻不正:" + value, ex);
        }
    }

    private static final class TopupLine {
        final String topupId;
        final String walletId;
        final BigDecimal amount;
        final String paymentMethod;
        final String status;
        final Instant requestTs;

        TopupLine(String topupId, String walletId, BigDecimal amount, String paymentMethod, String status, Instant requestTs) {
            this.topupId = topupId;
            this.walletId = walletId;
            this.amount = amount;
            this.paymentMethod = paymentMethod;
            this.status = status;
            this.requestTs = requestTs;
        }
    }

    private static final class VelocityLine {
        final String walletId;
        final Instant windowStartTs;
        final int authCount;
        final BigDecimal authSumAmt;
        final int denyCount;
        final Instant lastReqTs;

        VelocityLine(String walletId, Instant windowStartTs, int authCount, BigDecimal authSumAmt, int denyCount, Instant lastReqTs) {
            this.walletId = walletId;
            this.windowStartTs = windowStartTs;
            this.authCount = authCount;
            this.authSumAmt = authSumAmt;
            this.denyCount = denyCount;
            this.lastReqTs = lastReqTs;
        }
    }

    private static final class ScoreLine {
        final String scoreId;
        final String walletId;
        final String merchantCode;
        final int riskScore;
        final String reason;
        final Instant asOfTs;

        ScoreLine(String scoreId, String walletId, String merchantCode, int riskScore, String reason, Instant asOfTs) {
            this.scoreId = scoreId;
            this.walletId = walletId;
            this.merchantCode = merchantCode;
            this.riskScore = riskScore;
            this.reason = reason;
            this.asOfTs = asOfTs;
        }
    }

    private static final class ScreeningResult {
        final int score;
        final String reason;

        ScreeningResult(int score, String reason) {
            this.score = score;
            this.reason = reason;
        }
    }
}
