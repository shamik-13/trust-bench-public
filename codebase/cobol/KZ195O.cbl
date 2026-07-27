       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ195O.
      * 変更履歴
      * 版数  年月日(和暦)   担当                         概要
      * 1.00  平成18年04月01日 システム部 勘定系チーム     新規作成
      * 1.01  平成24年07月16日 システム部 勘定系チーム     祝日判定見直し
      * 1.02  令和02年01月06日 システム部 勘定系チーム     令和暦対応
      *
      * CHANGE LOG
      * 0.10  20260115  KZB01   INITIAL
      * 0.20  20260302  KZB02   REPROCESS WARNING
      * 0.30  20260618  KZB03   BUSINESS DAY INQUIRY
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS FS-KZCALF.
           SELECT KZHOLF ASSIGN TO "KZHOLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HO-HOLIDAY-DT
               FILE STATUS IS FS-KZHOLF.
           SELECT KZFPRF ASSIGN TO "KZFPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FP-FISCAL-PERIOD-ID
               FILE STATUS IS FS-KZFPRF.
           SELECT KZSCHF ASSIGN TO "KZSCHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZSCHF.
           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZCYRF.
           SELECT KZRPRF ASSIGN TO "KZRPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RP-REPROCESS-ID
               FILE STATUS IS FS-KZRPRF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCALF.
       COPY KZCALFC.
       FD  KZHOLF.
       COPY KZHOLFC.
       FD  KZFPRF.
       COPY KZFPRFC.
       FD  KZSCHF.
       COPY KZSCHFC.
       FD  KZCYRF.
       COPY KZCYRFC.
       FD  KZRPRF.
       COPY KZRPRFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZCALF                 PIC XX.
           05 FS-KZHOLF                 PIC XX.
           05 FS-KZFPRF                 PIC XX.
           05 FS-KZSCHF                 PIC XX.
           05 FS-KZCYRF                 PIC XX.
           05 FS-KZRPRF                 PIC XX.

       01  SW-AREA.
           05 SW-HARD-ERR               PIC X VALUE "N".
           05 SW-CAL-FOUND              PIC X VALUE "N".
           05 SW-HOL-FOUND              PIC X VALUE "N".
           05 SW-FP-FOUND               PIC X VALUE "N".
           05 SW-SCH-FOUND              PIC X VALUE "N".
           05 SW-CYR-FOUND              PIC X VALUE "N".
           05 SW-RP-WARN                PIC X VALUE "N".
           05 SW-EOF-SCH                PIC X VALUE "N".
           05 SW-EOF-CYR                PIC X VALUE "N".
           05 SW-EOF-RP                 PIC X VALUE "N".
           05 SW-WARN                   PIC X VALUE "N".

       01  IN-AREA.
           05 IN-CAL-DT                 PIC 9(8).
           05 IN-CYCLE-ID               PIC X(10).
           05 IN-PERIOD-ID              PIC X(10).

       01  WK-AREA.
           05 WK-TODAY-DT               PIC 9(8).
           05 WK-RESOLVED-DT            PIC 9(8).
           05 WK-ROLLED-FLAG            PIC X.
           05 WK-HOLIDAY-KBN            PIC X(20).
           05 WK-ELAPSED-DAYS           PIC S9(5) COMP-3 VALUE 0.
           05 WK-SCH-COUNT              PIC 9(4) VALUE 0.
           05 WK-RP-COUNT               PIC 9(4) VALUE 0.
           05 WK-VALID-DATE             PIC X VALUE "N".
           05 WK-YYYY                   PIC 9(4).
           05 WK-MM                     PIC 9(2).
           05 WK-DD                     PIC 9(2).
           05 WK-MAX-DD                 PIC 9(2).
           05 WK-DIV-Q                  PIC 9(4).
           05 WK-DIV-R                  PIC 9(4).
           05 WK-DISP-NUM               PIC ZZZZ9.

       COPY LK-CAL-PARM.

       01  LK140-PARM.
           05 LK140-FROM-DT             PIC 9(8).
           05 LK140-TO-DT               PIC 9(8).
           05 LK140-BUS-DAYS            PIC S9(5) COMP-3.
           05 LK140-RET                 PIC S9(4) COMP.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF SW-HARD-ERR = "N"
               PERFORM 2000-VALIDATE
           END-IF
           IF SW-HARD-ERR = "N"
               PERFORM 3000-INQUIRE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           ACCEPT WK-TODAY-DT FROM DATE YYYYMMDD
           ACCEPT IN-CAL-DT
           ACCEPT IN-CYCLE-ID
           ACCEPT IN-PERIOD-ID
           OPEN INPUT KZCALF KZHOLF KZFPRF KZSCHF KZCYRF KZRPRF
           IF FS-KZCALF NOT = "00"
               DISPLAY "KZCALF OPEN ERR ST=" FS-KZCALF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-KZHOLF NOT = "00"
               DISPLAY "KZHOLF OPEN ERR ST=" FS-KZHOLF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-KZFPRF NOT = "00"
               DISPLAY "KZFPRF OPEN ERR ST=" FS-KZFPRF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-KZSCHF NOT = "00"
               DISPLAY "KZSCHF OPEN ERR ST=" FS-KZSCHF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-KZCYRF NOT = "00"
               DISPLAY "KZCYRF OPEN ERR ST=" FS-KZCYRF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-KZRPRF NOT = "00"
               DISPLAY "KZRPRF OPEN ERR ST=" FS-KZRPRF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF.

       2000-VALIDATE.
           IF IN-CAL-DT = ZERO
               DISPLAY "INPUT DATE REQUIRED"
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               PERFORM 2100-CHECK-DATE
               IF WK-VALID-DATE NOT = "Y"
                   DISPLAY "INVALID INPUT DATE=" IN-CAL-DT
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
           IF IN-CYCLE-ID = SPACES
               DISPLAY "CYCLE ID REQUIRED"
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           IF IN-PERIOD-ID = SPACES
               DISPLAY "PERIOD ID REQUIRED"
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF.

       2100-CHECK-DATE.
           MOVE "N" TO WK-VALID-DATE
           MOVE IN-CAL-DT(1:4) TO WK-YYYY
           MOVE IN-CAL-DT(5:2) TO WK-MM
           MOVE IN-CAL-DT(7:2) TO WK-DD
           IF WK-YYYY < 1900 OR WK-YYYY > 2099
               EXIT PARAGRAPH
           END-IF
           IF WK-MM < 1 OR WK-MM > 12
               EXIT PARAGRAPH
           END-IF
           EVALUATE WK-MM
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31 TO WK-MAX-DD
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WK-MAX-DD
               WHEN 2
                   DIVIDE WK-YYYY BY 400 GIVING WK-DIV-Q
                       REMAINDER WK-DIV-R
                   IF WK-DIV-R = 0
                       MOVE 29 TO WK-MAX-DD
                   ELSE
                       DIVIDE WK-YYYY BY 100 GIVING WK-DIV-Q
                           REMAINDER WK-DIV-R
                       IF WK-DIV-R = 0
                           MOVE 28 TO WK-MAX-DD
                       ELSE
                           DIVIDE WK-YYYY BY 4 GIVING WK-DIV-Q
                               REMAINDER WK-DIV-R
                           IF WK-DIV-R = 0
                               MOVE 29 TO WK-MAX-DD
                           ELSE
                               MOVE 28 TO WK-MAX-DD
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
           IF WK-DD >= 1 AND WK-DD <= WK-MAX-DD
               MOVE "Y" TO WK-VALID-DATE
           END-IF.

       3000-INQUIRE.
           PERFORM 3100-READ-CALENDAR
           IF SW-HARD-ERR = "N"
               PERFORM 3200-RESOLVE-CALENDAR
           END-IF
           IF SW-HARD-ERR = "N"
               PERFORM 3300-READ-FISCAL
           END-IF
           IF SW-HARD-ERR = "N"
               PERFORM 3400-SCAN-SCHEDULE
           END-IF
           IF SW-HARD-ERR = "N"
               PERFORM 3500-SCAN-CYCLE-RESULT
           END-IF
           IF SW-HARD-ERR = "N"
               PERFORM 3600-SCAN-REPROCESS
           END-IF
           IF SW-HARD-ERR = "N"
               PERFORM 3700-CALC-ELAPSED
           END-IF
           PERFORM 3800-DISPLAY-RESULT.

       3100-READ-CALENDAR.
           MOVE IN-CAL-DT TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
               INVALID KEY
                   MOVE "N" TO SW-CAL-FOUND
                   MOVE "Y" TO SW-WARN
                   DISPLAY "WARN CALENDAR NOT FOUND DT=" IN-CAL-DT
               NOT INVALID KEY
                   MOVE "Y" TO SW-CAL-FOUND
                   EVALUATE CA-HOLIDAY-FLAG
                       WHEN "Y"
                           MOVE "BANK HOLIDAY" TO WK-HOLIDAY-KBN
                       WHEN "N"
                           MOVE "BUSINESS DAY" TO WK-HOLIDAY-KBN
                       WHEN OTHER
                           DISPLAY "BAD HOLIDAY FLAG DT=" CA-CAL-DT
                           MOVE "Y" TO SW-HARD-ERR
                           MOVE 12 TO RETURN-CODE
                   END-EVALUATE
           END-READ
           IF FS-KZCALF NOT = "00" AND FS-KZCALF NOT = "23"
               DISPLAY "KZCALF READ ERR ST=" FS-KZCALF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           IF SW-CAL-FOUND = "N"
               MOVE "NOT REGISTERED" TO WK-HOLIDAY-KBN
           END-IF.

       3200-RESOLVE-CALENDAR.
           MOVE IN-CAL-DT TO LK-CAL-NOMINAL-DT
           MOVE ZERO TO LK-CAL-RESOLVED-DT
           MOVE SPACE TO LK-CAL-ROLLED-FLAG
           MOVE ZERO TO LK-CAL-RET
           CALL "KZ900S" USING LK-CAL-PARM
           IF LK-CAL-RET NOT = 0
               DISPLAY "KZ900S DATE RESOLVE ERR RC=" LK-CAL-RET
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE LK-CAL-RESOLVED-DT TO WK-RESOLVED-DT
               MOVE LK-CAL-ROLLED-FLAG TO WK-ROLLED-FLAG
           END-IF
           IF SW-HARD-ERR = "N"
               MOVE WK-RESOLVED-DT TO HO-HOLIDAY-DT
               READ KZHOLF KEY IS HO-HOLIDAY-DT
                   INVALID KEY
                       MOVE "N" TO SW-HOL-FOUND
                   NOT INVALID KEY
                       IF HO-VALID-FLAG = "Y"
                           MOVE "Y" TO SW-HOL-FOUND
                       ELSE
                           MOVE "N" TO SW-HOL-FOUND
                           MOVE "Y" TO SW-WARN
                           DISPLAY "WARN HOLIDAY INVALID DT="
                               HO-HOLIDAY-DT
                       END-IF
               END-READ
               IF FS-KZHOLF NOT = "00" AND FS-KZHOLF NOT = "23"
                   DISPLAY "KZHOLF READ ERR ST=" FS-KZHOLF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       3300-READ-FISCAL.
           MOVE IN-PERIOD-ID TO FP-FISCAL-PERIOD-ID
           READ KZFPRF KEY IS FP-FISCAL-PERIOD-ID
               INVALID KEY
                   DISPLAY "FISCAL PERIOD NOT FOUND ID="
                       IN-PERIOD-ID
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               NOT INVALID KEY
                   MOVE "Y" TO SW-FP-FOUND
                   IF IN-CAL-DT < FP-PERIOD-START-DT
                       OR IN-CAL-DT > FP-PERIOD-END-DT
                       MOVE "Y" TO SW-WARN
                       DISPLAY "WARN DATE OUT OF PERIOD"
                   END-IF
           END-READ
           IF FS-KZFPRF NOT = "00" AND FS-KZFPRF NOT = "23"
               DISPLAY "KZFPRF READ ERR ST=" FS-KZFPRF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF.

       3400-SCAN-SCHEDULE.
           MOVE ZERO TO WK-SCH-COUNT
           MOVE "N" TO SW-SCH-FOUND
           MOVE "N" TO SW-EOF-SCH
           PERFORM UNTIL SW-EOF-SCH = "Y"
               READ KZSCHF
                   AT END
                       MOVE "Y" TO SW-EOF-SCH
                   NOT AT END
                       IF SC-CYCLE-ID = IN-CYCLE-ID
                          AND SC-PERIOD-ID = IN-PERIOD-ID
                           ADD 1 TO WK-SCH-COUNT
                           MOVE "Y" TO SW-SCH-FOUND
                           IF SC-GENERATE-STATUS NOT = "G"
                               MOVE "Y" TO SW-WARN
                               DISPLAY "WARN JOBNET NOT GENERATED "
                                   SC-JOBNET-ID
                           END-IF
                       END-IF
               END-READ
               IF FS-KZSCHF NOT = "00" AND FS-KZSCHF NOT = "10"
                   DISPLAY "KZSCHF READ ERR ST=" FS-KZSCHF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO SW-EOF-SCH
               END-IF
           END-PERFORM
           IF WK-SCH-COUNT = ZERO
               MOVE "Y" TO SW-WARN
               DISPLAY "WARN NO GENERATED SCHEDULE CYCLE="
                   IN-CYCLE-ID
           END-IF.

       3500-SCAN-CYCLE-RESULT.
           MOVE "N" TO SW-CYR-FOUND
           MOVE "N" TO SW-EOF-CYR
           PERFORM UNTIL SW-EOF-CYR = "Y"
               READ KZCYRF
                   AT END
                       MOVE "Y" TO SW-EOF-CYR
                   NOT AT END
                       IF CR-CYCLE-ID = IN-CYCLE-ID
                          AND CR-NOMINAL-DT = IN-CAL-DT
                           MOVE "Y" TO SW-CYR-FOUND
                           IF CR-RESOLVED-DT NOT = WK-RESOLVED-DT
                               MOVE "Y" TO SW-WARN
                               DISPLAY "WARN RESOLVED DATE DIFF"
                           END-IF
                           IF CR-ROLLED-FLAG NOT = WK-ROLLED-FLAG
                               MOVE "Y" TO SW-WARN
                               DISPLAY "WARN ROLLED FLAG DIFF"
                           END-IF
                       END-IF
               END-READ
               IF FS-KZCYRF NOT = "00" AND FS-KZCYRF NOT = "10"
                   DISPLAY "KZCYRF READ ERR ST=" FS-KZCYRF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO SW-EOF-CYR
               END-IF
           END-PERFORM
           IF SW-CYR-FOUND = "N"
               MOVE "Y" TO SW-WARN
               DISPLAY "WARN CYCLE RESULT NOT FOUND"
           END-IF.

       3600-SCAN-REPROCESS.
           MOVE ZERO TO WK-RP-COUNT
           MOVE "N" TO SW-RP-WARN
           MOVE "N" TO SW-EOF-RP
           MOVE LOW-VALUE TO RP-REPROCESS-ID
           START KZRPRF KEY IS GREATER THAN RP-REPROCESS-ID
               INVALID KEY
                   MOVE "Y" TO SW-EOF-RP
           END-START
           IF FS-KZRPRF NOT = "00" AND FS-KZRPRF NOT = "23"
               DISPLAY "KZRPRF START ERR ST=" FS-KZRPRF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF
           PERFORM UNTIL SW-EOF-RP = "Y"
               READ KZRPRF NEXT RECORD
                   AT END
                       MOVE "Y" TO SW-EOF-RP
                   NOT AT END
                       IF RP-CYCLE-ID = IN-CYCLE-ID
                          AND (RP-ORIGINAL-DT = IN-CAL-DT
                               OR RP-REDATE-DT = WK-RESOLVED-DT)
                           ADD 1 TO WK-RP-COUNT
                           IF RP-REQUEST-STATUS = "P"
                               MOVE "Y" TO SW-RP-WARN
                               MOVE "Y" TO SW-WARN
                               DISPLAY "WARN REPROCESS PENDING "
                                   RP-REPROCESS-ID
                           END-IF
                       END-IF
               END-READ
               IF FS-KZRPRF NOT = "00" AND FS-KZRPRF NOT = "10"
                   DISPLAY "KZRPRF READ ERR ST=" FS-KZRPRF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO SW-EOF-RP
               END-IF
           END-PERFORM.

       3700-CALC-ELAPSED.
           MOVE WK-RESOLVED-DT TO LK140-FROM-DT
           MOVE WK-TODAY-DT TO LK140-TO-DT
           MOVE ZERO TO LK140-BUS-DAYS
           MOVE ZERO TO LK140-RET
           CALL "KZ140S" USING LK140-PARM
           IF LK140-RET NOT = 0
               DISPLAY "KZ140S BUSINESS DAYS ERR RC=" LK140-RET
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE LK140-BUS-DAYS TO WK-ELAPSED-DAYS
           END-IF.

       3800-DISPLAY-RESULT.
           IF SW-HARD-ERR = "Y"
               EXIT PARAGRAPH
           END-IF
           DISPLAY "CALENDAR CYCLE INQUIRY RESULT"
           DISPLAY "INPUT DATE=" IN-CAL-DT
           DISPLAY "RESOLVED DATE=" WK-RESOLVED-DT
           DISPLAY "ROLLED FLAG=" WK-ROLLED-FLAG
           DISPLAY "HOLIDAY TYPE=" WK-HOLIDAY-KBN
           IF SW-HOL-FOUND = "Y"
               DISPLAY "HOLIDAY CODE=" HO-HOLIDAY-CD
               DISPLAY "MARKET CODE=" HO-MARKET-CD
               DISPLAY "HOLIDAY NAME=" HO-HOLIDAY-NAME
               DISPLAY "SOURCE CODE=" HO-SOURCE-CD
           END-IF
           IF SW-FP-FOUND = "Y"
               DISPLAY "FISCAL PERIOD=" FP-FISCAL-PERIOD-ID
               DISPLAY "PERIOD START=" FP-PERIOD-START-DT
               DISPLAY "PERIOD END=" FP-PERIOD-END-DT
               DISPLAY "CLOSE STATUS=" FP-CLOSE-STATUS
               DISPLAY "BASE CYCLE=" FP-BASE-CYCLE-ID
           END-IF
           MOVE WK-SCH-COUNT TO WK-DISP-NUM
           DISPLAY "SCHEDULE COUNT=" WK-DISP-NUM
           MOVE WK-RP-COUNT TO WK-DISP-NUM
           DISPLAY "REPROCESS COUNT=" WK-DISP-NUM
           MOVE WK-ELAPSED-DAYS TO WK-DISP-NUM
           DISPLAY "BUSINESS DAYS ELAPSED=" WK-DISP-NUM
           IF SW-WARN = "Y"
               DISPLAY "INQUIRY HAS WARNING"
           ELSE
               DISPLAY "INQUIRY HAS NO WARNING"
           END-IF.

       9000-FINAL.
           CLOSE KZCALF KZHOLF KZFPRF KZSCHF KZCYRF KZRPRF
           IF RETURN-CODE = 0
               DISPLAY "KZ195O NORMAL END"
           ELSE
               DISPLAY "KZ195O ABNORMAL END RC=" RETURN-CODE
           END-IF.
