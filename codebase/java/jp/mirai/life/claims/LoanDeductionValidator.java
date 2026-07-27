package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数  年月日      担当        概要
 * 1.00  2024-03-15  第一査定G    貸付控除上限チェック初版
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
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class LoanDeductionValidator {
    private static final Charset 入出力文字コード = StandardCharsets.UTF_8;
    private static final BigDecimal 貸付上限率 = new BigDecimal("0.90");
    private static final BigDecimal 一年以上経過支払割合 = new BigDecimal("1.00");

    private static final String 請求状態_支払対象 = "01";
    private static final String 査定結果_貸付超過 = "91";
    private static final String カテゴリ_貸付控除 = "LN";
    private static final String 承認権限_自動査定 = "A1";
    private static final String 査定者_バッチ = "BATCH-LN";

    private LoanDeductionValidator() {
    }

    private static List<Lfclmf行> lfclmf読込(Path path) throws IOException {
        List<Lfclmf行> rows = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            boolean first = true;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) {
                    continue;
                }
                List<String> c = csv分割(line);
                if (first && "CLAIM-ID".equals(c.get(0))) {
                    first = false;
                    continue;
                }
                first = false;
                rows.add(new Lfclmf行(
                        c.get(0),
                        c.get(1),
                        金額(c.get(2)),
                        金額(c.get(3)),
                        日付(c.get(4)),
                        日付(c.get(5)),
                        c.get(6)));
            }
        }
        return rows;
    }

    private static Map<String, Lflanf行> lflanf読込(Path path) throws IOException {
        Map<String, Lflanf行> rows = new LinkedHashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            boolean first = true;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) {
                    continue;
                }
                List<String> c = csv分割(line);
                if (first && "POL-NO".equals(c.get(0))) {
                    first = false;
                    continue;
                }
                first = false;
                Lflanf行 row = new Lflanf行(
                        c.get(0),
                        金額(c.get(1)),
                        金額(c.get(2)),
                        金額(c.get(3)),
                        日付(c.get(4)));
                rows.put(row.polNo, row);
            }
        }
        return rows;
    }

    private static List<Lfrasf行> lfrasf読込(Path path) throws IOException {
        List<Lfrasf行> rows = new ArrayList<>();
        if (!Files.exists(path)) {
            return rows;
        }
        try (BufferedReader reader = Files.newBufferedReader(path, 入出力文字コード)) {
            String line;
            boolean first = true;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) {
                    continue;
                }
                List<String> c = csv分割(line);
                if (first && "ASSESS-ID".equals(c.get(0))) {
                    first = false;
                    continue;
                }
                first = false;
                rows.add(new Lfrasf行(
                        c.get(0),
                        c.get(1),
                        日付(c.get(2)),
                        c.get(3),
                        c.get(4),
                        c.get(5),
                        c.get(6)));
            }
        }
        return rows;
    }

    private static void lfrasf書込(Path path, List<Lfrasf行> rows) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(path, 入出力文字コード)) {
            writer.write("ASSESS-ID,CLAIM-ID,ASSESS-DT,CATEGORY-KBN,AUTH-LEVEL-KBN,RESULT-KBN,ASSESSOR-ID");
            writer.newLine();
            for (Lfrasf行 row : rows) {
                writer.write(csv結合(
                        row.assessId,
                        row.claimId,
                        row.assessDt.toString(),
                        row.categoryKbn,
                        row.authLevelKbn,
                        row.resultKbn,
                        row.assessorId));
                writer.newLine();
            }
        }
    }

    private static String 採番(String claimId, LocalDate assessDt, int seq) {
        return "RA" + assessDt.toString().replace("-", "") + "-" + claimId + "-" + String.format("%05d", seq);
    }

    private static BigDecimal 金額(String value) {
        return new BigDecimal(value.trim()).setScale(0, RoundingMode.UNNECESSARY);
    }

    private static LocalDate 日付(String value) {
        return LocalDate.parse(value.trim());
    }

    private static List<String> csv分割(String line) {
        List<String> values = new ArrayList<>();
        StringBuilder field = new StringBuilder();
        boolean quoted = false;

        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    field.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (ch == ',' && !quoted) {
                values.add(field.toString().trim());
                field.setLength(0);
            } else {
                field.append(ch);
            }
        }
        values.add(field.toString().trim());
        return values;
    }

    private static String csv結合(String... values) {
        StringBuilder line = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                line.append(',');
            }
            line.append(csv値(values[i]));
        }
        return line.toString();
    }

    private static String csv値(String value) {
        if (value.indexOf(',') < 0 && value.indexOf('"') < 0 && value.indexOf('\n') < 0 && value.indexOf('\r') < 0) {
            return value;
        }
        return '"' + value.replace("\"", "\"\"") + '"';
    }

    private static final class Lfclmf行 {
        private final String claimId;
        private final String polNo;
        private final BigDecimal sumAssuredAmt;
        private final BigDecimal loanBalanceAmt;
        private final LocalDate respStartDt;
        private final LocalDate eventDt;
        private final String claimStatusKbn;

        private Lfclmf行(String claimId, String polNo, BigDecimal sumAssuredAmt, BigDecimal loanBalanceAmt,
                LocalDate respStartDt, LocalDate eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }
    }

    private static final class Lflanf行 {
        private final String polNo;
        private final BigDecimal loanAmt;
        private final BigDecimal interestAmt;
        private final BigDecimal totalBalance;
        private final LocalDate lastUpdateDt;

        private Lflanf行(String polNo, BigDecimal loanAmt, BigDecimal interestAmt, BigDecimal totalBalance,
                LocalDate lastUpdateDt) {
            this.polNo = polNo;
            this.loanAmt = loanAmt;
            this.interestAmt = interestAmt;
            this.totalBalance = totalBalance;
            this.lastUpdateDt = lastUpdateDt;
        }
    }

    private static final class Lfrasf行 {
        private final String assessId;
        private final String claimId;
        private final LocalDate assessDt;
        private final String categoryKbn;
        private final String authLevelKbn;
        private final String resultKbn;
        private final String assessorId;

        private Lfrasf行(String assessId, String claimId, LocalDate assessDt, String categoryKbn, String authLevelKbn,
                String resultKbn, String assessorId) {
            this.assessId = assessId;
            this.claimId = claimId;
            this.assessDt = assessDt;
            this.categoryKbn = categoryKbn;
            this.authLevelKbn = authLevelKbn;
            this.resultKbn = resultKbn;
            this.assessorId = assessorId;
        }
    }
}
