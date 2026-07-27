       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ109B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240401  基盤    初版作成
      * 1.01  20240715  運用    休日抑止判定を追加
      * 1.02  20241120  勘定    営業日間隔超過時の保留判定を追加
      ******************************************************************
      * ジョブネット稼働暦展開バッチ
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZJNCF
               ASSIGN TO "KZJNCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS JN-JOBNET-ID
               FILE STATUS IS WS-KZJNCF-ST.

           SELECT KZSCHF
               ASSIGN TO "KZSCHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-KZSCHF-ST.

           SELECT KZCALF
               ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS WS-KZCALF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  KZJNCF.
           COPY KZJNCFC.

       FD  KZSCHF.
           COPY KZSCHFC.

       FD  KZCALF.
           COPY KZCALFC.

       WORKING-STORAGE SECTION.
           COPY LK-CAL-PARM.

       01  WS-FILE-STATUS.
           05  WS-KZJNCF-ST              PIC XX VALUE SPACES.
           05  WS-KZSCHF-ST              PIC XX VALUE SPACES.
           05  WS-KZCALF-ST              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-EOF-SW                 PIC X VALUE "N".
               88  WS-EOF                     VALUE "Y".
               88  WS-NOT-EOF                 VALUE "N".
           05  WS-HARD-ERROR-SW          PIC X VALUE "N".
               88  WS-HARD-ERROR              VALUE "Y".
               88  WS-NORMAL                  VALUE "N".

       01  WS-COUNTERS.
           05  WS-READ-CNT               PIC 9(09) VALUE ZERO.
           05  WS-REWRITE-CNT            PIC 9(09) VALUE ZERO.
           05  WS-OK-CNT                 PIC 9(09) VALUE ZERO.
           05  WS-HOLD-CNT               PIC 9(09) VALUE ZERO.
           05  WS-SUPPRESS-CNT           PIC 9(09) VALUE ZERO.
           05  WS-INACTIVE-CNT           PIC 9(09) VALUE ZERO.
           05  WS-ERROR-CNT              PIC 9(09) VALUE ZERO.

       01  WS-DATE-WORK.
           05  WS-SCHEDULED-DT-N         PIC 9(08) VALUE ZERO.
           05  WS-RESOLVED-DT-N          PIC 9(08) VALUE ZERO.
           05  WS-SCHED-INT              PIC S9(09) VALUE ZERO.
           05  WS-RESOLVED-INT           PIC S9(09) VALUE ZERO.
           05  WS-BUSINESS-GAP           PIC S9(05) VALUE ZERO.
           05  WS-DATE-VALID-SW          PIC X VALUE "N".
               88  WS-DATE-VALID              VALUE "Y".
               88  WS-DATE-INVALID            VALUE "N".

       01  WS-CONSTANTS.
           05  CT-ST-OK                  PIC XX VALUE "00".
           05  CT-ST-EOF                 PIC XX VALUE "10".
           05  CT-ST-NOTFND              PIC XX VALUE "23".
           05  CT-HOLIDAY                PIC X  VALUE "Y".
           05  CT-BUSINESS               PIC X  VALUE "N".
           05  CT-ACTIVE                 PIC X  VALUE "Y".
           05  CT-INACTIVE               PIC X  VALUE "N".
           05  CT-SUPPRESS               PIC X  VALUE "Y".
           05  CT-STAT-READY             PIC X  VALUE "1".
           05  CT-STAT-HOLD              PIC X  VALUE "H".
           05  CT-STAT-SUPPRESS          PIC X  VALUE "S".
           05  CT-STAT-INACTIVE          PIC X  VALUE "N".
           05  CT-STAT-ERROR             PIC X  VALUE "E".
           05  CT-MAX-GAP                PIC S9(05) VALUE 10.
           05  CT-MIN-RETRY              PIC 9(03) VALUE 0.
           05  CT-MAX-RETRY              PIC 9(03) VALUE 9.

       01  LK140-PARM.
           05  LK140-FROM-DT             PIC 9(08) VALUE ZERO.
           05  LK140-TO-DT               PIC 9(08) VALUE ZERO.
           05  LK140-BUSINESS-DAYS       PIC S9(05) VALUE ZERO.
           05  LK140-RET                 PIC X(02) VALUE "00".

       PROCEDURE DIVISION.

       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WS-NORMAL
               PERFORM 2000-PROCESS UNTIL WS-EOF OR WS-HARD-ERROR
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "KZ170B OK READ=" WS-READ-CNT
               DISPLAY "UPDATE=" WS-REWRITE-CNT
                       " READY=" WS-OK-CNT
                       " HOLD=" WS-HOLD-CNT
               DISPLAY "SUPPRESS=" WS-SUPPRESS-CNT
                       " INACTIVE=" WS-INACTIVE-CNT
                       " ERROR=" WS-ERROR-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES SECTION.
       1000-START.
           OPEN INPUT KZJNCF
           IF WS-KZJNCF-ST NOT = CT-ST-OK
               DISPLAY "KZJNCF OPEN ERROR ST=" WS-KZJNCF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF

           IF WS-NORMAL
               OPEN I-O KZSCHF
               IF WS-KZSCHF-ST NOT = CT-ST-OK
                   DISPLAY "KZSCHF OPEN ERROR ST=" WS-KZSCHF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN INPUT KZCALF
               IF WS-KZCALF-ST NOT = CT-ST-OK
                   DISPLAY "KZCALF OPEN ERROR ST=" WS-KZCALF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       2000-PROCESS SECTION.
       2000-START.
           READ KZSCHF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
                   PERFORM 2100-EDIT-SCHEDULE
                   IF WS-NORMAL
                       PERFORM 2200-LOAD-JOBNET
                   END-IF
                   IF WS-NORMAL
                       PERFORM 2300-DECIDE-SCHEDULE
                   END-IF
                   IF WS-NORMAL
                       PERFORM 2900-REWRITE-SCHEDULE
                   END-IF
           END-READ

           IF WS-KZSCHF-ST NOT = CT-ST-OK
              AND WS-KZSCHF-ST NOT = CT-ST-EOF
               DISPLAY "KZSCHF READ ERROR ST=" WS-KZSCHF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF
           .

       2100-EDIT-SCHEDULE SECTION.
       2100-START.
           SET WS-DATE-INVALID TO TRUE
           IF SC-CYCLE-ID = SPACES
               MOVE CT-STAT-ERROR TO SC-GENERATE-STATUS
               ADD 1 TO WS-ERROR-CNT
               DISPLAY "CYCLE ID MISSING"
           ELSE
               IF SC-JOBNET-ID = SPACES
                   MOVE CT-STAT-ERROR TO SC-GENERATE-STATUS
                   ADD 1 TO WS-ERROR-CNT
                   DISPLAY "JOBNET ID MISSING"
                           " CYCLE=" SC-CYCLE-ID
               ELSE
                   MOVE SC-SCHEDULED-DT TO WS-SCHEDULED-DT-N
                   PERFORM 2110-CHECK-DATE
                   IF NOT WS-DATE-VALID
                       MOVE CT-STAT-ERROR TO SC-GENERATE-STATUS
                       ADD 1 TO WS-ERROR-CNT
                       DISPLAY "BAD SCHEDULE DATE"
                               " CYCLE=" SC-CYCLE-ID
                               " DT=" SC-SCHEDULED-DT
                   END-IF
               END-IF
           END-IF
           .

       2110-CHECK-DATE SECTION.
       2110-START.
           IF WS-SCHEDULED-DT-N < 19000101
              OR WS-SCHEDULED-DT-N > 20991231
               SET WS-DATE-INVALID TO TRUE
           ELSE
               COMPUTE WS-SCHED-INT =
                   FUNCTION INTEGER-OF-DATE(WS-SCHEDULED-DT-N)
               IF WS-SCHED-INT > ZERO
                   SET WS-DATE-VALID TO TRUE
               ELSE
                   SET WS-DATE-INVALID TO TRUE
               END-IF
           END-IF
           .

       2200-LOAD-JOBNET SECTION.
       2200-START.
           IF SC-GENERATE-STATUS = CT-STAT-ERROR
               CONTINUE
           ELSE
               MOVE SC-JOBNET-ID TO JN-JOBNET-ID
               READ KZJNCF KEY IS JN-JOBNET-ID
                   INVALID KEY
                       IF WS-KZJNCF-ST = CT-ST-NOTFND
                           MOVE CT-STAT-ERROR
                             TO SC-GENERATE-STATUS
                           ADD 1 TO WS-ERROR-CNT
                           DISPLAY "JOBNET NOT FOUND"
                                   " CYCLE=" SC-CYCLE-ID
                                   " JOBNET=" SC-JOBNET-ID
                       ELSE
                           DISPLAY "KZJNCF READ ERROR"
                                   " ST=" WS-KZJNCF-ST
                                   " JOBNET=" SC-JOBNET-ID
                           SET WS-HARD-ERROR TO TRUE
                       END-IF
                   NOT INVALID KEY
                       IF JN-ACTIVE-FLAG NOT = CT-ACTIVE
                          AND JN-ACTIVE-FLAG NOT = CT-INACTIVE
                           MOVE CT-STAT-ERROR
                             TO SC-GENERATE-STATUS
                           ADD 1 TO WS-ERROR-CNT
                           DISPLAY "BAD ACTIVE FLAG"
                                   " JOBNET=" JN-JOBNET-ID
                                   " FLG=" JN-ACTIVE-FLAG
                       END-IF
               END-READ
           END-IF
           .

       2300-DECIDE-SCHEDULE SECTION.
       2300-START.
           IF SC-GENERATE-STATUS = CT-STAT-ERROR
               CONTINUE
           ELSE
               IF JN-ACTIVE-FLAG = CT-INACTIVE
                   MOVE CT-STAT-INACTIVE TO SC-GENERATE-STATUS
                   ADD 1 TO WS-INACTIVE-CNT
               ELSE
                   PERFORM 2310-APPLY-CALENDAR
               END-IF
           END-IF
           .

       2310-APPLY-CALENDAR SECTION.
       2310-START.
           MOVE JN-CALENDAR-CD TO SC-CALENDAR-CD

           MOVE SC-SCHEDULED-DT TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
               INVALID KEY
                   IF WS-KZCALF-ST = CT-ST-NOTFND
                       MOVE CT-STAT-HOLD TO SC-GENERATE-STATUS
                       ADD 1 TO WS-HOLD-CNT
                       DISPLAY "CALENDAR DATE NOT FOUND"
                               " CYCLE=" SC-CYCLE-ID
                               " DT=" SC-SCHEDULED-DT
                   ELSE
                       DISPLAY "KZCALF READ ERROR"
                               " ST=" WS-KZCALF-ST
                               " DT=" SC-SCHEDULED-DT
                       SET WS-HARD-ERROR TO TRUE
                   END-IF
               NOT INVALID KEY
                   PERFORM 2320-CHECK-CALENDAR-ROW
           END-READ
           .

       2320-CHECK-CALENDAR-ROW SECTION.
       2320-START.
           IF CA-HOLIDAY-FLAG NOT = CT-HOLIDAY
              AND CA-HOLIDAY-FLAG NOT = CT-BUSINESS
               MOVE CT-STAT-ERROR TO SC-GENERATE-STATUS
               ADD 1 TO WS-ERROR-CNT
               DISPLAY "BAD HOLIDAY FLAG"
                       " DT=" CA-CAL-DT
                       " FLG=" CA-HOLIDAY-FLAG
           ELSE
               IF CA-HOLIDAY-FLAG = CT-HOLIDAY
                  AND JN-SUPPRESS-HOLIDAY-FLAG = CT-SUPPRESS
                   MOVE CT-STAT-SUPPRESS TO SC-GENERATE-STATUS
                   ADD 1 TO WS-SUPPRESS-CNT
               ELSE
                   PERFORM 2330-RESOLVE-DATE
               END-IF
           END-IF
           .

       2330-RESOLVE-DATE SECTION.
       2330-START.
           IF JN-RETRY-LIMIT < CT-MIN-RETRY
              OR JN-RETRY-LIMIT > CT-MAX-RETRY
               MOVE CT-STAT-ERROR TO SC-GENERATE-STATUS
               ADD 1 TO WS-ERROR-CNT
               DISPLAY "BAD RETRY LIMIT"
                       " JOBNET=" JN-JOBNET-ID
                       " LIMIT=" JN-RETRY-LIMIT
           ELSE
               MOVE SC-SCHEDULED-DT TO LK-CAL-NOMINAL-DT
               MOVE ZERO            TO LK-CAL-RESOLVED-DT
               MOVE SPACE           TO LK-CAL-ROLLED-FLAG
               MOVE "00"            TO LK-CAL-RET
               CALL "KZ900S" USING LK-CAL-PARM
               IF LK-CAL-RET NOT = "00"
                   MOVE CT-STAT-HOLD TO SC-GENERATE-STATUS
                   ADD 1 TO WS-HOLD-CNT
                   DISPLAY "DATE RESOLVE HOLD"
                           " CYCLE=" SC-CYCLE-ID
                           " RET=" LK-CAL-RET
               ELSE
                   PERFORM 2340-CHECK-GAP
               END-IF
           END-IF
           .

       2340-CHECK-GAP SECTION.
       2340-START.
           MOVE LK-CAL-RESOLVED-DT TO WS-RESOLVED-DT-N
           COMPUTE WS-RESOLVED-INT =
               FUNCTION INTEGER-OF-DATE(WS-RESOLVED-DT-N)
           COMPUTE WS-BUSINESS-GAP =
               WS-RESOLVED-INT - WS-SCHED-INT

           IF WS-BUSINESS-GAP > CT-MAX-GAP
               MOVE SC-SCHEDULED-DT    TO LK140-FROM-DT
               MOVE LK-CAL-RESOLVED-DT TO LK140-TO-DT
               MOVE ZERO               TO LK140-BUSINESS-DAYS
               MOVE "00"               TO LK140-RET
               CALL "KZ140S" USING LK140-PARM
               IF LK140-RET = "00"
                   MOVE LK140-BUSINESS-DAYS TO WS-BUSINESS-GAP
               ELSE
                   DISPLAY "BUSINESS DAY CALC ERROR"
                           " CYCLE=" SC-CYCLE-ID
                           " RET=" LK140-RET
               END-IF
               MOVE CT-STAT-HOLD TO SC-GENERATE-STATUS
               ADD 1 TO WS-HOLD-CNT
               DISPLAY "BUSINESS GAP HOLD"
                       " CYCLE=" SC-CYCLE-ID
                       " GAP=" WS-BUSINESS-GAP
           ELSE
               MOVE CT-STAT-READY TO SC-GENERATE-STATUS
               ADD 1 TO WS-OK-CNT
           END-IF
           .

       2900-REWRITE-SCHEDULE SECTION.
       2900-START.
           REWRITE KZSCHF-REC
           IF WS-KZSCHF-ST = CT-ST-OK
               ADD 1 TO WS-REWRITE-CNT
           ELSE
               DISPLAY "KZSCHF REWRITE ERROR"
                       " ST=" WS-KZSCHF-ST
                       " CYCLE=" SC-CYCLE-ID
               SET WS-HARD-ERROR TO TRUE
           END-IF
           .

       9000-CLOSE-FILES SECTION.
       9000-START.
           IF WS-KZCALF-ST NOT = SPACES
               CLOSE KZCALF
               IF WS-KZCALF-ST NOT = CT-ST-OK
                   DISPLAY "KZCALF CLOSE ERROR ST=" WS-KZCALF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WS-KZSCHF-ST NOT = SPACES
               CLOSE KZSCHF
               IF WS-KZSCHF-ST NOT = CT-ST-OK
                   DISPLAY "KZSCHF CLOSE ERROR ST=" WS-KZSCHF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WS-KZJNCF-ST NOT = SPACES
               CLOSE KZJNCF
               IF WS-KZJNCF-ST NOT = CT-ST-OK
                   DISPLAY "KZJNCF CLOSE ERROR ST=" WS-KZJNCF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF
           .
