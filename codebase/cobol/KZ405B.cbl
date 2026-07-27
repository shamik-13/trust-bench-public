       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ405B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和05年04月01日  システム部 勘定系チーム  新規作成
      * 1.01  令和06年10月15日  システム部 勘定系チーム  利率更改判定追加
      * 1.02  令和07年03月31日  システム部 勘定系チーム  終了ログ見直し

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZIRMF ASSIGN TO "KZIRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RM-RATE-KEY
               FILE STATUS IS WS-KZIRMF-ST.
           SELECT KZCTLF ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS WS-KZCTLF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  KZIRMF.
           COPY KZIRMFC.

       FD  KZCTLF.
           COPY KZCTLFC.

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS.
           05  WS-KZIRMF-ST             PIC XX VALUE SPACES.
           05  WS-KZCTLF-ST             PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-EOF-SW                PIC X VALUE "N".
               88  WS-EOF                    VALUE "Y".
           05  WS-HARD-ERR-SW           PIC X VALUE "N".
               88  WS-HARD-ERR               VALUE "Y".
           05  WS-VALID-SW              PIC X VALUE "N".
               88  WS-VALID-ROW              VALUE "Y".
           05  WS-DUP-SW                PIC X VALUE "N".
               88  WS-DUPLICATE-ROW          VALUE "Y".

       01  WS-COUNTERS.
           05  WS-READ-CNT              PIC 9(9) VALUE ZERO.
           05  WS-ACTIVE-CNT            PIC 9(9) VALUE ZERO.
           05  WS-HOLD-CNT              PIC 9(9) VALUE ZERO.
           05  WS-SKIP-CNT              PIC 9(9) VALUE ZERO.
           05  WS-ERR-CNT               PIC 9(9) VALUE ZERO.

       01  WS-WORK-AREA.
           05  WS-PGM-ID                PIC X(8) VALUE "KZ405B".
           05  WS-CTL-KEY               PIC X(16)
               VALUE "KZ405B-RUN".
           05  WS-PREV-PRODUCT-CD       PIC X(6) VALUE LOW-VALUES.
           05  WS-PREV-EFFECTIVE-DT     PIC 9(8) VALUE ZERO.
           05  WS-CUR-PRODUCT-CD        PIC X(6) VALUE SPACES.
           05  WS-CUR-EFFECTIVE-DT      PIC 9(8) VALUE ZERO.
           05  WS-BASE-DT               PIC 9(8) VALUE ZERO.
           05  WS-DATE-INT              PIC 9(8) VALUE ZERO.
           05  WS-REASON                PIC X(80) VALUE SPACES.
           05  WS-CTL-EXISTS-SW         PIC X VALUE "N".
               88  WS-CTL-EXISTS             VALUE "Y".
           05  WS-NUMERIC-DATE          PIC 9(8) VALUE ZERO.

       01  KZ415S-LK.
           05  LK-IN-PRODUCT-CD         PIC X(6).
           05  LK-IN-BASE-DT            PIC 9(8).
           05  LK-IN-TAX-KBN            PIC X.
           05  LK-IN-USE-KBN            PIC X.
           05  LK-OUT-RESULT-CD         PIC X(2).
           05  LK-OUT-EFFECTIVE-DT      PIC 9(8).
           05  LK-OUT-INT-RATE          PIC S9(3)V9(6) COMP-3.
           05  LK-OUT-OD-RATE           PIC S9(3)V9(6) COMP-3.
           05  LK-OUT-MSG               PIC X(80).

       PROCEDURE DIVISION.

       0000-MAIN.
           MOVE ZERO TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT WS-HARD-ERR
               PERFORM 2000-READ-CONTROL
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 3000-PROCESS-RATE-MASTER
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 8000-WRITE-CONTROL
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-HARD-ERR
               MOVE 12 TO RETURN-CODE
           ELSE
               DISPLAY "KZ405B END READ=" WS-READ-CNT
               DISPLAY "ACTIVE=" WS-ACTIVE-CNT
                       " HOLD=" WS-HOLD-CNT
                       " SKIP=" WS-SKIP-CNT
               MOVE ZERO TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN I-O KZIRMF
           IF WS-KZIRMF-ST NOT = "00"
               DISPLAY "KZIRMF OPEN ERROR ST=" WS-KZIRMF-ST
               SET WS-HARD-ERR TO TRUE
           END-IF
           IF NOT WS-HARD-ERR
               OPEN I-O KZCTLF
               IF WS-KZCTLF-ST NOT = "00"
                   DISPLAY "KZCTLF OPEN ERROR ST=" WS-KZCTLF-ST
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF.

       2000-READ-CONTROL.
           MOVE WS-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
               INVALID KEY
                   IF WS-KZCTLF-ST = "23"
                       MOVE "N" TO WS-CTL-EXISTS-SW
                       DISPLAY "KZCTLF CONTROL NOT FOUND"
                   ELSE
                       DISPLAY "KZCTLF READ ERROR ST=" WS-KZCTLF-ST
                       SET WS-HARD-ERR TO TRUE
                   END-IF
               NOT INVALID KEY
                   MOVE "Y" TO WS-CTL-EXISTS-SW
                   IF CL-RUN-STATUS = "R"
                       DISPLAY "RESTART POINT=" CL-RESTART-POINT
                   END-IF
           END-READ.

       3000-PROCESS-RATE-MASTER.
           MOVE LOW-VALUES TO RM-RATE-KEY
           START KZIRMF KEY IS GREATER THAN RM-RATE-KEY
               INVALID KEY
                   IF WS-KZIRMF-ST = "23"
                       SET WS-EOF TO TRUE
                   ELSE
                       DISPLAY "KZIRMF START ERROR ST=" WS-KZIRMF-ST
                       SET WS-HARD-ERR TO TRUE
                   END-IF
           END-START
           PERFORM UNTIL WS-EOF OR WS-HARD-ERR
               READ KZIRMF NEXT RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ-CNT
                       PERFORM 3100-EVALUATE-RATE-ROW
               END-READ
           END-PERFORM.

       3100-EVALUATE-RATE-ROW.
           IF RM-APPROVAL-STAT NOT = "A"
               ADD 1 TO WS-SKIP-CNT
           ELSE
               PERFORM 3200-VALIDATE-ROW
               IF WS-VALID-ROW
                   PERFORM 3300-CALL-KZ415S
                   IF LK-OUT-RESULT-CD = "00"
                       PERFORM 3400-ACTIVATE-ROW
                   ELSE
                       MOVE LK-OUT-MSG TO WS-REASON
                       PERFORM 3500-HOLD-ROW
                   END-IF
               ELSE
                   PERFORM 3500-HOLD-ROW
               END-IF
           END-IF.

       3200-VALIDATE-ROW.
           MOVE "Y" TO WS-VALID-SW
           MOVE "N" TO WS-DUP-SW
           MOVE SPACES TO WS-REASON
           MOVE RM-PRODUCT-CD TO WS-CUR-PRODUCT-CD
           MOVE RM-EFFECTIVE-DT TO WS-CUR-EFFECTIVE-DT

           IF RM-PRODUCT-CD = SPACES
               MOVE "PRODUCT CODE REQUIRED" TO WS-REASON
               MOVE "N" TO WS-VALID-SW
           END-IF

           IF WS-VALID-ROW
               IF RM-EFFECTIVE-DT < 19000101
                   OR RM-EFFECTIVE-DT > 20991231
                   MOVE "EFFECTIVE DATE RANGE ERROR" TO WS-REASON
                   MOVE "N" TO WS-VALID-SW
               END-IF
           END-IF

           IF WS-VALID-ROW
               IF RM-WITHHOLD-TAX-KBN NOT = "1"
                   AND RM-WITHHOLD-TAX-KBN NOT = "2"
                   AND RM-WITHHOLD-TAX-KBN NOT = "3"
                   MOVE "WITHHOLD TAX KBN ERROR" TO WS-REASON
                   MOVE "N" TO WS-VALID-SW
               END-IF
           END-IF

           IF WS-VALID-ROW
               IF RM-INT-RATE < ZERO
                   OR RM-INT-RATE > 20
                   MOVE "INTEREST RATE RANGE ERROR" TO WS-REASON
                   MOVE "N" TO WS-VALID-SW
               END-IF
           END-IF

           IF WS-VALID-ROW
               IF RM-OD-RATE < ZERO
                   OR RM-OD-RATE > 30
                   MOVE "OVERDRAFT RATE RANGE ERROR" TO WS-REASON
                   MOVE "N" TO WS-VALID-SW
               END-IF
           END-IF

           IF WS-VALID-ROW
               IF WS-CUR-PRODUCT-CD = WS-PREV-PRODUCT-CD
                   AND WS-CUR-EFFECTIVE-DT = WS-PREV-EFFECTIVE-DT
                   MOVE "DUPLICATE PRODUCT DATE" TO WS-REASON
                   MOVE "Y" TO WS-DUP-SW
                   MOVE "N" TO WS-VALID-SW
               END-IF
           END-IF

           IF NOT WS-DUPLICATE-ROW
               MOVE WS-CUR-PRODUCT-CD TO WS-PREV-PRODUCT-CD
               MOVE WS-CUR-EFFECTIVE-DT TO WS-PREV-EFFECTIVE-DT
           END-IF.

       3300-CALL-KZ415S.
           MOVE RM-PRODUCT-CD TO LK-IN-PRODUCT-CD
           PERFORM 3310-CALC-BASE-DATE
           MOVE WS-BASE-DT TO LK-IN-BASE-DT
           MOVE RM-WITHHOLD-TAX-KBN TO LK-IN-TAX-KBN
           MOVE "1" TO LK-IN-USE-KBN
           MOVE SPACES TO LK-OUT-RESULT-CD
           MOVE ZERO TO LK-OUT-EFFECTIVE-DT
           MOVE ZERO TO LK-OUT-INT-RATE
           MOVE ZERO TO LK-OUT-OD-RATE
           MOVE SPACES TO LK-OUT-MSG
           CALL "KZ415S" USING KZ415S-LK
           IF LK-OUT-RESULT-CD NOT = "00"
               IF LK-OUT-MSG = SPACES
                   MOVE "KZ415S CONSISTENCY ERROR" TO LK-OUT-MSG
               END-IF
               DISPLAY "KZ415S HOLD PRODUCT=" RM-PRODUCT-CD
                       " BASE=" LK-IN-BASE-DT
                       " RESULT=" LK-OUT-RESULT-CD
           END-IF.

       3310-CALC-BASE-DATE.
           COMPUTE WS-DATE-INT =
               FUNCTION INTEGER-OF-DATE(RM-EFFECTIVE-DT)
           IF WS-DATE-INT > 1
               SUBTRACT 1 FROM WS-DATE-INT
               COMPUTE WS-BASE-DT =
                   FUNCTION DATE-OF-INTEGER(WS-DATE-INT)
           ELSE
               MOVE RM-EFFECTIVE-DT TO WS-BASE-DT
           END-IF.

       3400-ACTIVATE-ROW.
           MOVE "E" TO RM-APPROVAL-STAT
           REWRITE KZIRMF-REC
               INVALID KEY
                   DISPLAY "KZIRMF REWRITE ERROR KEY=" RM-RATE-KEY
                           " ST=" WS-KZIRMF-ST
                   ADD 1 TO WS-ERR-CNT
                   SET WS-HARD-ERR TO TRUE
               NOT INVALID KEY
                   ADD 1 TO WS-ACTIVE-CNT
                   DISPLAY "RATE ACTIVE PRODUCT=" RM-PRODUCT-CD
                           " DATE=" RM-EFFECTIVE-DT
           END-REWRITE.

       3500-HOLD-ROW.
           MOVE "H" TO RM-APPROVAL-STAT
           REWRITE KZIRMF-REC
               INVALID KEY
                   DISPLAY "KZIRMF HOLD ERROR KEY=" RM-RATE-KEY
                           " ST=" WS-KZIRMF-ST
                   ADD 1 TO WS-ERR-CNT
                   SET WS-HARD-ERR TO TRUE
               NOT INVALID KEY
                   ADD 1 TO WS-HOLD-CNT
                   DISPLAY "RATE HOLD PRODUCT=" RM-PRODUCT-CD
                           " DATE=" RM-EFFECTIVE-DT
                           " REASON=" WS-REASON
           END-REWRITE.

       8000-WRITE-CONTROL.
           MOVE WS-CTL-KEY TO CL-CONTROL-KEY
           ACCEPT WS-NUMERIC-DATE FROM DATE YYYYMMDD
           MOVE WS-NUMERIC-DATE TO CL-RUN-DT
           MOVE "KZ" TO CL-CYCLE-ID
           MOVE "KZ405B-END" TO CL-RESTART-POINT
           MOVE "C" TO CL-RUN-STATUS
           MOVE WS-PGM-ID TO CL-LAST-PGM-ID
           IF WS-CTL-EXISTS
               REWRITE KZCTLF-REC
                   INVALID KEY
                       DISPLAY "KZCTLF REWRITE ERROR ST="
                               WS-KZCTLF-ST
                       SET WS-HARD-ERR TO TRUE
               END-REWRITE
           ELSE
               WRITE KZCTLF-REC
                   INVALID KEY
                       DISPLAY "KZCTLF WRITE ERROR ST="
                               WS-KZCTLF-ST
                       SET WS-HARD-ERR TO TRUE
               END-WRITE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE KZIRMF
           IF WS-KZIRMF-ST NOT = "00"
               DISPLAY "KZIRMF CLOSE ERROR ST=" WS-KZIRMF-ST
               SET WS-HARD-ERR TO TRUE
           END-IF
           CLOSE KZCTLF
           IF WS-KZCTLF-ST NOT = "00"
               DISPLAY "KZCTLF CLOSE ERROR ST=" WS-KZCTLF-ST
               SET WS-HARD-ERR TO TRUE
           END-IF.
