package jp.mirai.life.claims;

/**
 * 変更履歴
 * 版数    年月日      担当          概要
 * 1.00    2024-03-15  保険金システムG  請求ステータス管理サービス作成
 */
public class ClaimStatusManager {
    private static final String STATUS_PAYABLE = "01";
    private static final String STATUS_ASSESSING = "05";
    private static final String STATUS_DENIED = "09";
    private static final String PAYMENT_TARGET_STATUS = STATUS_PAYABLE;
    private static final int PAYMENT_RATIO_AFTER_ONE_YEAR_PERCENT = 100;

    private static final String STATUS_ABORTED = "99";
    private static final String ERROR_ILLEGAL_TRANSITION = "E_STATUS_TRANSITION_ILLEGAL";
    private static final String ERROR_CLAIM_NOT_FOUND = "E_CLAIM_NOT_FOUND";
    private static final String ERROR_RECORD_FORMAT = "E_RECORD_FORMAT";

    private static final class Service {
        private final java.nio.file.Path lfclmf;
        private final java.nio.file.Path lfpayh;

        Service(java.nio.file.Path lfclmf, java.nio.file.Path lfpayh) {
            this.lfclmf = lfclmf;
            this.lfpayh = lfpayh;
        }

        void changeStatus(String claimId, String statusTo, String operatorId) throws java.io.IOException {
            if (isBlank(claimId) || isBlank(statusTo) || isBlank(operatorId)) {
                throw new IllegalArgumentException("請求ID、変更後ステータス、オペレータIDは必須です");
            }

            java.util.List<String> sourceLines = readLinesIfExists(lfclmf);
            java.util.List<ClaimRecord> claims = new java.util.ArrayList<>();
            for (String line : sourceLines) {
                if (!isBlank(line)) {
                    claims.add(ClaimRecord.parse(line));
                }
            }

            ClaimRecord target = null;
            java.util.List<ClaimRecord> updatedClaims = new java.util.ArrayList<>();
            for (ClaimRecord claim : claims) {
                if (claim.claimId.equals(claimId)) {
                    if (target != null) {
                        throw new IllegalStateException(ERROR_RECORD_FORMAT + ":請求IDが重複しています");
                    }
                    target = claim;
                    validateTransition(claim.claimStatusKbn, statusTo);
                    updatedClaims.add(claim.withStatus(statusTo));
                } else {
                    updatedClaims.add(claim);
                }
            }

            if (target == null) {
                throw new IllegalArgumentException(ERROR_CLAIM_NOT_FOUND + ":請求が見つかりません");
            }

            java.time.LocalDateTime changedAt = java.time.LocalDateTime.now(java.time.Clock.systemDefaultZone());
            long sequence = nextSequence(readLinesIfExists(lfpayh));
            PayHistoryRecord history = new PayHistoryRecord(
                    sequence,
                    claimId,
                    target.claimStatusKbn,
                    statusTo,
                    changedAt.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss")),
                    operatorId
            );

            java.nio.file.Path directory = lfclmf.toAbsolutePath().getParent();
            if (directory == null) {
                directory = java.nio.file.Paths.get(".").toAbsolutePath().normalize();
            }
            java.nio.file.Files.createDirectories(directory);
            java.nio.file.Path tempClaim = java.nio.file.Files.createTempFile(directory, "LFCLMF", ".tmp");
            java.nio.file.Path tempPayh = java.nio.file.Files.createTempFile(directory, "LFPAYH", ".tmp");

            try {
                java.util.List<String> claimLines = new java.util.ArrayList<>();
                for (ClaimRecord claim : updatedClaims) {
                    claimLines.add(claim.format());
                }
                writeLines(tempClaim, claimLines);

                java.util.List<String> historyLines = readLinesIfExists(lfpayh);
                historyLines.add(history.format());
                writeLines(tempPayh, historyLines);

                moveAtomically(tempClaim, lfclmf);
                moveAtomically(tempPayh, lfpayh);
            } catch (java.io.IOException | RuntimeException e) {
                java.nio.file.Files.deleteIfExists(tempClaim);
                java.nio.file.Files.deleteIfExists(tempPayh);
                throw e;
            }
        }

        private void validateTransition(String statusFrom, String statusTo) {
            if (STATUS_ABORTED.equals(statusTo)) {
                return;
            }
            if ("10".equals(statusFrom) && "20".equals(statusTo)) {
                return;
            }
            if ("20".equals(statusFrom) && "25".equals(statusTo)) {
                return;
            }
            if ("25".equals(statusFrom) && "30".equals(statusTo)) {
                return;
            }
            if ("30".equals(statusFrom) && "40".equals(statusTo)) {
                return;
            }
            if ("40".equals(statusFrom) && "90".equals(statusTo)) {
                return;
            }
            throw new IllegalStateException(ERROR_ILLEGAL_TRANSITION + ":" + statusFrom + "から" + statusTo + "へは変更できません");
        }

        private long nextSequence(java.util.List<String> lines) {
            long max = 0L;
            for (String line : lines) {
                if (isBlank(line)) {
                    continue;
                }
                String[] columns = splitCsv(line);
                if (columns.length < 1) {
                    throw new IllegalStateException(ERROR_RECORD_FORMAT + ":履歴レコードの形式が不正です");
                }
                try {
                    max = Math.max(max, Long.parseLong(columns[0]));
                } catch (NumberFormatException e) {
                    throw new IllegalStateException(ERROR_RECORD_FORMAT + ":履歴連番が数値ではありません", e);
                }
            }
            return max + 1L;
        }
    }

    private static final class ClaimRecord {
        private final String claimId;
        private final String polNo;
        private final long sumAssuredAmt;
        private final long loanBalanceAmt;
        private final String respStartDt;
        private final String eventDt;
        private final String claimStatusKbn;

        private ClaimRecord(String claimId, String polNo, long sumAssuredAmt, long loanBalanceAmt,
                            String respStartDt, String eventDt, String claimStatusKbn) {
            this.claimId = claimId;
            this.polNo = polNo;
            this.sumAssuredAmt = sumAssuredAmt;
            this.loanBalanceAmt = loanBalanceAmt;
            this.respStartDt = respStartDt;
            this.eventDt = eventDt;
            this.claimStatusKbn = claimStatusKbn;
        }

        static ClaimRecord parse(String line) {
            String[] columns = splitCsv(line);
            if (columns.length != 7) {
                throw new IllegalArgumentException(ERROR_RECORD_FORMAT + ":請求レコードの項目数が不正です");
            }
            return new ClaimRecord(
                    require(columns[0], "請求ID"),
                    require(columns[1], "証券番号"),
                    parseAmount(columns[2], "保険金額"),
                    parseAmount(columns[3], "貸付残高"),
                    require(columns[4], "責任開始日"),
                    require(columns[5], "事故日"),
                    require(columns[6], "請求ステータス")
            );
        }

        ClaimRecord withStatus(String status) {
            return new ClaimRecord(claimId, polNo, sumAssuredAmt, loanBalanceAmt, respStartDt, eventDt, status);
        }

        String format() {
            return joinCsv(new String[] {
                    claimId,
                    polNo,
                    Long.toString(sumAssuredAmt),
                    Long.toString(loanBalanceAmt),
                    respStartDt,
                    eventDt,
                    claimStatusKbn
            });
        }
    }

    private static final class PayHistoryRecord {
        private final long seqNo;
        private final String claimId;
        private final String statusFrom;
        private final String statusTo;
        private final String changeDt;
        private final String operatorId;

        private PayHistoryRecord(long seqNo, String claimId, String statusFrom, String statusTo,
                                 String changeDt, String operatorId) {
            this.seqNo = seqNo;
            this.claimId = claimId;
            this.statusFrom = statusFrom;
            this.statusTo = statusTo;
            this.changeDt = changeDt;
            this.operatorId = operatorId;
        }

        String format() {
            return joinCsv(new String[] {
                    Long.toString(seqNo),
                    claimId,
                    statusFrom,
                    statusTo,
                    changeDt,
                    operatorId
            });
        }
    }

    private static java.util.List<String> readLinesIfExists(java.nio.file.Path path) throws java.io.IOException {
        if (!java.nio.file.Files.exists(path)) {
            return new java.util.ArrayList<>();
        }
        return java.nio.file.Files.readAllLines(path, java.nio.charset.StandardCharsets.UTF_8);
    }

    private static void writeLines(java.nio.file.Path path, java.util.List<String> lines) throws java.io.IOException {
        java.nio.file.Files.write(path, lines, java.nio.charset.StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.TRUNCATE_EXISTING);
    }

    private static void moveAtomically(java.nio.file.Path source, java.nio.file.Path target) throws java.io.IOException {
        try {
            java.nio.file.Files.move(source, target,
                    java.nio.file.StandardCopyOption.REPLACE_EXISTING,
                    java.nio.file.StandardCopyOption.ATOMIC_MOVE);
        } catch (java.nio.file.AtomicMoveNotSupportedException e) {
            java.nio.file.Files.move(source, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        }
    }

    private static String require(String value, String name) {
        if (isBlank(value)) {
            throw new IllegalArgumentException(ERROR_RECORD_FORMAT + ":" + name + "が未設定です");
        }
        return value.trim();
    }

    private static long parseAmount(String value, String name) {
        try {
            long amount = Long.parseLong(require(value, name));
            if (amount < 0L) {
                throw new IllegalArgumentException(ERROR_RECORD_FORMAT + ":" + name + "が負数です");
            }
            return amount;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(ERROR_RECORD_FORMAT + ":" + name + "が数値ではありません", e);
        }
    }

    private static boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private static String[] splitCsv(String line) {
        return line.split(",", -1);
    }

    private static String joinCsv(String[] columns) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < columns.length; i++) {
            if (i > 0) {
                builder.append(',');
            }
            builder.append(columns[i]);
        }
        return builder.toString();
    }
}
