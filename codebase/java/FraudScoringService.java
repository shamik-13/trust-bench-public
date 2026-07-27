/*
 * 変更履歴
 * 版数    年月日      担当        概要
 * 1.00    2023-02-27  開発担当    初版作成。不正スコア算出、CDFRDF追記、ゲートウェイ応答出力を実装。
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
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class FraudScoringService {
    private static final Charset IO_CHARSET = StandardCharsets.UTF_8;
    private static final DateTimeFormatter DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
    private static final String MODEL_VERSION = "FS-20260628-01";
    private static final String BASE_CURRENCY = "JPY";

    public static void main(String[] args) {
        Path authFile = Path.of(args.length > 0 ? args[0] : "CDAUTHF.dat");
        Path merchantFile = Path.of(args.length > 1 ? args[1] : "CDMERF.dat");
        Path velocityFile = Path.of(args.length > 2 ? args[2] : "CDVELF.dat");
        Path fraudResultFile = Path.of(args.length > 3 ? args[3] : "CDFRDF.dat");

        try {
            FraudScoringService service = new FraudScoringService();
            GatewayReply reply = service.process(authFile, merchantFile, velocityFile, fraudResultFile);
            System.out.println("処理件数=" + reply.processedCount);
            System.out.println("最高スコア=" + reply.maxScore);
            System.out.println("主要ルール=" + reply.primaryRule);
            System.out.println("モデル版数=" + reply.modelVersion);
        } catch (IOException | IllegalArgumentException e) {
            System.err.println("不正スコアリング異常終了:" + e.getMessage());
            System.exit(8);
        }
    }

    public GatewayReply process(Path authFile, Path merchantFile, Path velocityFile, Path fraudResultFile) throws IOException {
        Map<String, MerchantRecord> merchantIndex = loadMerchants(merchantFile);
        Map<String, VelocityRecord> velocityIndex = loadVelocities(velocityFile);

        int processedCount = 0;
        int maxScore = -1;
        String primaryRule = "NONE";
        LocalDateTime scoreTime = LocalDateTime.now();

        try (BufferedReader authInput = Files.newBufferedReader(authFile, IO_CHARSET);
             BufferedWriter fraudOutput = Files.newBufferedWriter(fraudResultFile, IO_CHARSET)) {
            String line;
            while ((line = authInput.readLine()) != null) {
                if (line.isBlank() || line.startsWith("#")) {
                    continue;
                }

                AuthRecord auth = AuthRecord.parse(line);
                MerchantRecord merchant = merchantIndex.get(auth.merchantCode);
                VelocityRecord velocity = velocityIndex.get(auth.cardNo);

                ScoreResult score = score(auth, merchant, velocity, scoreTime);
                fraudOutput.write(score.toCdfRdfLine());
                fraudOutput.newLine();

                processedCount++;
                if (score.fraudScore > maxScore) {
                    maxScore = score.fraudScore;
                    primaryRule = score.ruleHitCode;
                }
            }
        }

        if (processedCount == 0) {
            return new GatewayReply(0, 0, "NONE", MODEL_VERSION);
        }
        return new GatewayReply(processedCount, maxScore, primaryRule, MODEL_VERSION);
    }

    private ScoreResult score(AuthRecord auth, MerchantRecord merchant, VelocityRecord velocity, LocalDateTime scoreTime) {
        int points = 0;
        String primaryRule = "BASE";

        if (!"00".equals(auth.authResult)) {
            points += 10;
            primaryRule = chooseRule(primaryRule, "AUTHSTS", points, 10);
        }

        if (!BASE_CURRENCY.equals(auth.currencyCode)) {
            points += 18;
            primaryRule = chooseRule(primaryRule, "CUR", points, 18);
        }

        int amountPoints = amountPoints(auth.authAmount);
        if (amountPoints > 0) {
            points += amountPoints;
            primaryRule = chooseRule(primaryRule, "AMT", points, amountPoints);
        }

        if (merchant == null) {
            points += 20;
            primaryRule = chooseRule(primaryRule, "MERUNKN", points, 20);
        } else {
            int merchantPoints = merchantPoints(merchant);
            if (merchantPoints > 0) {
                points += merchantPoints;
                primaryRule = chooseRule(primaryRule, "MER", points, merchantPoints);
            }
        }

        if (velocity == null) {
            points += 8;
            primaryRule = chooseRule(primaryRule, "VELMISS", points, 8);
        } else {
            int velocityPoints = velocityPoints(auth, velocity);
            if (velocityPoints > 0) {
                points += velocityPoints;
                primaryRule = chooseRule(primaryRule, "VEL", points, velocityPoints);
            }
        }

        int timePoints = timePoints(auth.authTimestamp.toLocalTime());
        if (timePoints > 0) {
            points += timePoints;
            primaryRule = chooseRule(primaryRule, "TIME", points, timePoints);
        }

        int finalScore = Math.max(0, Math.min(100, points));
        if (finalScore < 15) {
            primaryRule = "LOW";
        }

        return new ScoreResult(auth.authId, auth.cardNo, finalScore, primaryRule, MODEL_VERSION, scoreTime);
    }

    private static int amountPoints(BigDecimal amount) {
        if (amount.compareTo(new BigDecimal("1000000")) >= 0) {
            return 30;
        }
        if (amount.compareTo(new BigDecimal("300000")) >= 0) {
            return 20;
        }
        if (amount.compareTo(new BigDecimal("100000")) >= 0) {
            return 12;
        }
        if (amount.compareTo(new BigDecimal("1000")) < 0) {
            return 5;
        }
        return 0;
    }

    private static int merchantPoints(MerchantRecord merchant) {
        int points = 0;
        if (!"1".equals(merchant.status)) {
            points += 25;
        }
        if ("H".equals(merchant.riskRank)) {
            points += 20;
        } else if ("M".equals(merchant.riskRank)) {
            points += 10;
        }
        if ("7995".equals(merchant.mcc) || "5967".equals(merchant.mcc) || "6051".equals(merchant.mcc)) {
            points += 18;
        }
        if (!"JP".equals(merchant.countryCode)) {
            points += 12;
        }
        return points;
    }

    private static int velocityPoints(AuthRecord auth, VelocityRecord velocity) {
        int points = 0;
        if ("Y".equals(velocity.velocityFlag)) {
            points += 28;
        }
        if (velocity.authCount10m >= 8) {
            points += 22;
        } else if (velocity.authCount10m >= 4) {
            points += 12;
        }
        if (velocity.authAmount1h.add(auth.authAmount).compareTo(new BigDecimal("500000")) >= 0) {
            points += 16;
        }
        long seconds = Math.abs(Duration.between(velocity.lastAuthTimestamp, auth.authTimestamp).getSeconds());
        if (seconds <= 30) {
            points += 10;
        }
        return points;
    }

    private static int timePoints(LocalTime time) {
        if (!time.isBefore(LocalTime.of(0, 0)) && time.isBefore(LocalTime.of(5, 0))) {
            return 10;
        }
        if (!time.isBefore(LocalTime.of(23, 0))) {
            return 6;
        }
        return 0;
    }

    private static String chooseRule(String currentRule, String candidateRule, int totalPoints, int addedPoints) {
        if ("BASE".equals(currentRule) || "LOW".equals(currentRule)) {
            return candidateRule;
        }
        return addedPoints >= Math.max(10, totalPoints / 4) ? candidateRule : currentRule;
    }

    private static Map<String, MerchantRecord> loadMerchants(Path file) throws IOException {
        Map<String, MerchantRecord> index = new HashMap<>();
        try (BufferedReader input = Files.newBufferedReader(file, IO_CHARSET)) {
            String line;
            while ((line = input.readLine()) != null) {
                if (line.isBlank() || line.startsWith("#")) {
                    continue;
                }
                MerchantRecord record = MerchantRecord.parse(line);
                index.put(record.merchantCode, record);
            }
        }
        return index;
    }

    private static Map<String, VelocityRecord> loadVelocities(Path file) throws IOException {
        Map<String, VelocityRecord> index = new HashMap<>();
        try (BufferedReader input = Files.newBufferedReader(file, IO_CHARSET)) {
            String line;
            while ((line = input.readLine()) != null) {
                if (line.isBlank() || line.startsWith("#")) {
                    continue;
                }
                VelocityRecord record = VelocityRecord.parse(line);
                index.put(record.cardNo, record);
            }
        }
        return index;
    }

    private static final class AuthRecord {
        final String authId;
        final String cardNo;
        final BigDecimal authAmount;
        final String authResult;
        final String merchantCode;
        final String currencyCode;
        final LocalDateTime authTimestamp;
        final String holdExpireDate;

        AuthRecord(String authId, String cardNo, BigDecimal authAmount, String authResult, String merchantCode,
                   String currencyCode, LocalDateTime authTimestamp, String holdExpireDate) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.authAmount = authAmount;
            this.authResult = authResult;
            this.merchantCode = merchantCode;
            this.currencyCode = currencyCode;
            this.authTimestamp = authTimestamp;
            this.holdExpireDate = holdExpireDate;
        }

        static AuthRecord parse(String line) {
            String[] fields = split(line, 8, "CDAUTHF");
            return new AuthRecord(fields[0], fields[1], amount(fields[2], "AUTH-AMT"), fields[3], fields[4],
                    fields[5], dateTime(fields[6], "AUTH-TS"), fields[7]);
        }
    }

    private static final class MerchantRecord {
        final String merchantCode;
        final String merchantNameKana;
        final String mcc;
        final String riskRank;
        final String status;
        final String countryCode;

        MerchantRecord(String merchantCode, String merchantNameKana, String mcc, String riskRank, String status,
                       String countryCode) {
            this.merchantCode = merchantCode;
            this.merchantNameKana = merchantNameKana;
            this.mcc = mcc;
            this.riskRank = riskRank;
            this.status = status;
            this.countryCode = countryCode;
        }

        static MerchantRecord parse(String line) {
            String[] fields = split(line, 6, "CDMERF");
            return new MerchantRecord(fields[0], fields[1], fields[2], fields[3], fields[4], fields[5]);
        }
    }

    private static final class VelocityRecord {
        final String cardNo;
        final LocalDateTime windowStartTimestamp;
        final int authCount10m;
        final BigDecimal authAmount1h;
        final LocalDateTime lastAuthTimestamp;
        final String velocityFlag;

        VelocityRecord(String cardNo, LocalDateTime windowStartTimestamp, int authCount10m, BigDecimal authAmount1h,
                       LocalDateTime lastAuthTimestamp, String velocityFlag) {
            this.cardNo = cardNo;
            this.windowStartTimestamp = windowStartTimestamp;
            this.authCount10m = authCount10m;
            this.authAmount1h = authAmount1h;
            this.lastAuthTimestamp = lastAuthTimestamp;
            this.velocityFlag = velocityFlag;
        }

        static VelocityRecord parse(String line) {
            String[] fields = split(line, 6, "CDVELF");
            return new VelocityRecord(fields[0], dateTime(fields[1], "WINDOW-START-TS"),
                    integer(fields[2], "AUTH-COUNT-10M"), amount(fields[3], "AUTH-AMT-1H"),
                    dateTime(fields[4], "LAST-AUTH-TS"), fields[5]);
        }
    }

    public static final class GatewayReply {
        public final int processedCount;
        public final int maxScore;
        public final String primaryRule;
        public final String modelVersion;

        GatewayReply(int processedCount, int maxScore, String primaryRule, String modelVersion) {
            this.processedCount = processedCount;
            this.maxScore = maxScore;
            this.primaryRule = primaryRule;
            this.modelVersion = modelVersion;
        }
    }

    private static final class ScoreResult {
        final String authId;
        final String cardNo;
        final int fraudScore;
        final String ruleHitCode;
        final String modelVersion;
        final LocalDateTime scoreTimestamp;

        ScoreResult(String authId, String cardNo, int fraudScore, String ruleHitCode, String modelVersion,
                    LocalDateTime scoreTimestamp) {
            this.authId = authId;
            this.cardNo = cardNo;
            this.fraudScore = fraudScore;
            this.ruleHitCode = ruleHitCode;
            this.modelVersion = modelVersion;
            this.scoreTimestamp = scoreTimestamp;
        }

        String toCdfRdfLine() {
            return String.join(",",
                    authId,
                    cardNo,
                    String.format(Locale.ROOT, "%03d", fraudScore),
                    ruleHitCode,
                    modelVersion,
                    DATE_TIME_FORMAT.format(scoreTimestamp));
        }
    }

    private static String[] split(String line, int fieldCount, String fileName) {
        String[] fields = line.split(",", -1);
        if (fields.length != fieldCount) {
            throw new IllegalArgumentException(fileName + "項目数不正:" + line);
        }
        for (int i = 0; i < fields.length; i++) {
            fields[i] = fields[i].trim();
        }
        return fields;
    }

    private static BigDecimal amount(String value, String fieldName) {
        try {
            return new BigDecimal(value).setScale(0, RoundingMode.HALF_UP);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(fieldName + "数値不正:" + value);
        }
    }

    private static int integer(String value, String fieldName) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(fieldName + "整数不正:" + value);
        }
    }

    private static LocalDateTime dateTime(String value, String fieldName) {
        try {
            return LocalDateTime.parse(value, DATE_TIME_FORMAT);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(fieldName + "日時不正:" + value);
        }
    }
}
