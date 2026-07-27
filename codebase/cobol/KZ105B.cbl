       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ105B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                       概要
      * 1.00  令和03.04.01  システム部 勘定系チーム    新規作成
      * 1.01  令和04.10.17  システム部 勘定系チーム    休日判定処理見直し
      * 1.02  令和06.02.05  システム部 勘定系チーム    サイクル予定生成条件追加

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCYCF ASSIGN TO "KZCYCF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZCYCF.

           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS FS-KZCALF.

           SELECT KZFPRF ASSIGN TO "KZFPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FP-FISCAL-PERIOD-ID
               FILE STATUS IS FS-KZFPRF.

           SELECT KZSCHF ASSIGN TO "KZSCHF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZSCHF.

           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZCYRF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCYCF.
           COPY KZCYCFC.

       FD  KZCALF.
           COPY KZCALFC.

       FD  KZFPRF.
           COPY KZFPRFC.

       FD  KZSCHF.
           COPY KZSCHFC.

       FD  KZCYRF.
           COPY KZCYRFC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05  FS-KZCYCF             PIC XX.
           05  FS-KZCALF             PIC XX.
           05  FS-KZFPRF             PIC XX.
           05  FS-KZSCHF             PIC XX.
           05  FS-KZCYRF             PIC XX.

       01  SWITCH-AREA.
           05  EOF-KZCYCF            PIC X VALUE "N".
               88  KZCYCF-END              VALUE "Y".
           05  EOF-KZFPRF            PIC X VALUE "N".
               88  KZFPRF-END              VALUE "Y".
           05  HARD-ERROR-SW         PIC X VALUE "N".
               88  HARD-ERROR             VALUE "Y".
           05  PERIOD-MATCH-SW       PIC X VALUE "N".
               88  PERIOD-MATCH           VALUE "Y".
           05  SCHEDULE-OK-SW        PIC X VALUE "N".
               88  SCHEDULE-OK            VALUE "Y".
           05  ROLLED-SW             PIC X VALUE "N".
               88  DATE-ROLLED            VALUE "Y".

       01  WORK-AREA.
           05  WK-CYCLE-ID           PIC X(20).
           05  WK-NOMINAL-DT         PIC 9(08).
           05  WK-RESOLVED-DT        PIC 9(08).
           05  WK-WORK-DT            PIC 9(08).
           05  WK-WORK-INT           PIC S9(09) COMP.
           05  WK-END-INT            PIC S9(09) COMP.
           05  WK-BUSINESS-DAYS      PIC 9(03).
           05  WK-MIN-BUSINESS-DAYS  PIC 9(03) VALUE 003.
           05  WK-READ-CYCLES        PIC 9(09) VALUE ZERO.
           05  WK-WRITE-SCHEDULES    PIC 9(09) VALUE ZERO.
           05  WK-WRITE-RESULTS      PIC 9(09) VALUE ZERO.
           05  WK-SKIP-COUNT         PIC 9(09) VALUE ZERO.
           05  WK-ABEND-COUNT        PIC 9(09) VALUE ZERO.
           05  WK-JOBNET-ID          PIC X(12).
           05  WK-REASON             PIC X(40).
           05  WK-DATE-CCYY          PIC 9(04).
           05  WK-DATE-MM            PIC 9(02).
           05  WK-DATE-DD            PIC 9(02).
           05  WK-MAX-DD             PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
               PERFORM 2000-MAIN
                   UNTIL KZCYCF-END OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT KZCYCF
           IF FS-KZCYCF NOT = "00"
               DISPLAY "KZCYCF OPEN ST=" FS-KZCYCF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT KZCALF
               IF FS-KZCALF NOT = "00"
                   DISPLAY "KZCALF OPEN ST=" FS-KZCALF
                   MOVE "Y" TO HARD-ERROR-SW
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT KZFPRF
               IF FS-KZFPRF NOT = "00"
                   DISPLAY "KZFPRF OPEN ST=" FS-KZFPRF
                   MOVE "Y" TO HARD-ERROR-SW
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT KZSCHF
               IF FS-KZSCHF NOT = "00"
                   DISPLAY "KZSCHF OPEN ST=" FS-KZSCHF
                   MOVE "Y" TO HARD-ERROR-SW
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT KZCYRF
               IF FS-KZCYRF NOT = "00"
                   DISPLAY "KZCYRF OPEN ST=" FS-KZCYRF
                   MOVE "Y" TO HARD-ERROR-SW
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               PERFORM 2100-READ-CYCLE
           END-IF.

       2000-MAIN.
           ADD 1 TO WK-READ-CYCLES
           MOVE CY-CYCLE-ID TO WK-CYCLE-ID
           MOVE CY-NOMINAL-DT TO WK-NOMINAL-DT
           MOVE "N" TO PERIOD-MATCH-SW
           MOVE "N" TO SCHEDULE-OK-SW
           MOVE "N" TO ROLLED-SW
           MOVE SPACES TO WK-REASON

           PERFORM 3000-VALIDATE-CYCLE

           IF SCHEDULE-OK
               PERFORM 4000-FIND-PERIOD
           END-IF

           IF SCHEDULE-OK AND PERIOD-MATCH
               PERFORM 5000-RESOLVE-DATE
           END-IF

           IF SCHEDULE-OK AND PERIOD-MATCH
               PERFORM 6000-CHECK-BUSINESS-DAYS
           END-IF

           IF SCHEDULE-OK AND PERIOD-MATCH
               PERFORM 7000-WRITE-SCHEDULE
           ELSE
               ADD 1 TO WK-SKIP-COUNT
               DISPLAY "SKIP ID=" WK-CYCLE-ID
               DISPLAY "REASON=" WK-REASON
           END-IF

           IF PERIOD-MATCH
               PERFORM 7100-WRITE-CYCLE-RESULT
           END-IF

           PERFORM 2100-READ-CYCLE.

       2100-READ-CYCLE.
           READ KZCYCF
               AT END
                   MOVE "Y" TO EOF-KZCYCF
               NOT AT END
                   IF FS-KZCYCF NOT = "00"
                       DISPLAY "KZCYCF READ ST=" FS-KZCYCF
                       MOVE "Y" TO HARD-ERROR-SW
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-READ.

       3000-VALIDATE-CYCLE.
           MOVE "Y" TO SCHEDULE-OK-SW

           IF CY-CYCLE-ID = SPACES
               MOVE "N" TO SCHEDULE-OK-SW
               MOVE "CYCLE-ID MISSING" TO WK-REASON
           END-IF

           IF SCHEDULE-OK
               MOVE CY-NOMINAL-DT TO WK-WORK-DT
               PERFORM 3100-VALIDATE-DATE
               IF NOT SCHEDULE-OK
                   MOVE "NOMINAL DATE INVALID" TO WK-REASON
               END-IF
           END-IF.

       3100-VALIDATE-DATE.
           MOVE WK-WORK-DT(1:4) TO WK-DATE-CCYY
           MOVE WK-WORK-DT(5:2) TO WK-DATE-MM
           MOVE WK-WORK-DT(7:2) TO WK-DATE-DD

           IF WK-DATE-CCYY < 1900 OR WK-DATE-CCYY > 2099
               MOVE "N" TO SCHEDULE-OK-SW
           ELSE
               IF WK-DATE-MM < 1 OR WK-DATE-MM > 12
                   MOVE "N" TO SCHEDULE-OK-SW
               ELSE
                   EVALUATE WK-DATE-MM
                       WHEN 01
                       WHEN 03
                       WHEN 05
                       WHEN 07
                       WHEN 08
                       WHEN 10
                       WHEN 12
                           MOVE 31 TO WK-MAX-DD
                       WHEN 04
                       WHEN 06
                       WHEN 09
                       WHEN 11
                           MOVE 30 TO WK-MAX-DD
                       WHEN 02
                           PERFORM 3200-SET-FEBRUARY-MAX
                   END-EVALUATE
                   IF WK-DATE-DD < 1 OR WK-DATE-DD > WK-MAX-DD
                       MOVE "N" TO SCHEDULE-OK-SW
                   END-IF
               END-IF
           END-IF.

       3200-SET-FEBRUARY-MAX.
           MOVE 28 TO WK-MAX-DD
           IF FUNCTION MOD(WK-DATE-CCYY 400) = 0
               MOVE 29 TO WK-MAX-DD
           ELSE
               IF FUNCTION MOD(WK-DATE-CCYY 4) = 0
                  AND FUNCTION MOD(WK-DATE-CCYY 100) NOT = 0
                   MOVE 29 TO WK-MAX-DD
               END-IF
           END-IF.

       4000-FIND-PERIOD.
           MOVE "N" TO EOF-KZFPRF
           MOVE "N" TO PERIOD-MATCH-SW
           MOVE LOW-VALUES TO FP-FISCAL-PERIOD-ID

           START KZFPRF
               KEY IS NOT LESS THAN FP-FISCAL-PERIOD-ID
               INVALID KEY
                   MOVE "Y" TO EOF-KZFPRF
               NOT INVALID KEY
                   CONTINUE
           END-START

           IF FS-KZFPRF NOT = "00" AND FS-KZFPRF NOT = "23"
               DISPLAY "KZFPRF START ST=" FS-KZFPRF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           PERFORM UNTIL KZFPRF-END OR PERIOD-MATCH OR HARD-ERROR
               READ KZFPRF NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF-KZFPRF
                   NOT AT END
                       IF FS-KZFPRF NOT = "00"
                           DISPLAY "KZFPRF NEXT ST="
                               FS-KZFPRF
                           MOVE "Y" TO HARD-ERROR-SW
                           MOVE 12 TO RETURN-CODE
                       ELSE
                           PERFORM 4100-EVALUATE-PERIOD
                       END-IF
               END-READ
           END-PERFORM

           IF NOT HARD-ERROR AND NOT PERIOD-MATCH
               MOVE "N" TO SCHEDULE-OK-SW
               MOVE "FISCAL PERIOD NOT FOUND" TO WK-REASON
           END-IF.

       4100-EVALUATE-PERIOD.
           IF FP-BASE-CYCLE-ID = CY-CYCLE-ID
              AND CY-NOMINAL-DT >= FP-PERIOD-START-DT
              AND CY-NOMINAL-DT <= FP-PERIOD-END-DT
               MOVE "Y" TO PERIOD-MATCH-SW
               IF FP-CLOSE-STATUS NOT = "0"
                   MOVE "N" TO SCHEDULE-OK-SW
                   MOVE "FISCAL PERIOD CLOSED" TO WK-REASON
               END-IF
           END-IF.

       5000-RESOLVE-DATE.
           MOVE "N" TO ROLLED-SW
           MOVE CY-NOMINAL-DT TO WK-RESOLVED-DT

           PERFORM UNTIL HARD-ERROR OR NOT SCHEDULE-OK
               MOVE WK-RESOLVED-DT TO CA-CAL-DT
               READ KZCALF
                   INVALID KEY
                       MOVE "N" TO SCHEDULE-OK-SW
                       MOVE "CALENDAR DATE NOT FOUND" TO WK-REASON
                   NOT INVALID KEY
                       IF FS-KZCALF NOT = "00"
                           DISPLAY "KZCALF READ ST="
                               FS-KZCALF
                           MOVE "Y" TO HARD-ERROR-SW
                           MOVE 12 TO RETURN-CODE
                       ELSE
                           IF CA-HOLIDAY-FLAG = "N"
                               EXIT PERFORM
                           ELSE
                               IF CA-HOLIDAY-FLAG = "Y"
                                   MOVE "Y" TO ROLLED-SW
                                   PERFORM 5100-ADD-ONE-DAY
                               ELSE
                                   MOVE "N" TO SCHEDULE-OK-SW
                                   MOVE "HOLIDAY FLAG INVALID"
                                       TO WK-REASON
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF SCHEDULE-OK
              AND WK-RESOLVED-DT > FP-PERIOD-END-DT
               MOVE "N" TO SCHEDULE-OK-SW
               MOVE "RESOLVED DATE OUT OF PERIOD" TO WK-REASON
           END-IF.

       5100-ADD-ONE-DAY.
           COMPUTE WK-WORK-INT =
               FUNCTION INTEGER-OF-DATE(WK-RESOLVED-DT) + 1
           MOVE FUNCTION DATE-OF-INTEGER(WK-WORK-INT)
               TO WK-RESOLVED-DT.

       6000-CHECK-BUSINESS-DAYS.
           MOVE ZERO TO WK-BUSINESS-DAYS
           MOVE FP-PERIOD-START-DT TO WK-WORK-DT
           COMPUTE WK-WORK-INT =
               FUNCTION INTEGER-OF-DATE(WK-WORK-DT)
           COMPUTE WK-END-INT =
               FUNCTION INTEGER-OF-DATE(WK-RESOLVED-DT)

           PERFORM UNTIL WK-WORK-INT > WK-END-INT
              OR HARD-ERROR
              OR NOT SCHEDULE-OK
               MOVE FUNCTION DATE-OF-INTEGER(WK-WORK-INT)
                   TO CA-CAL-DT
               READ KZCALF
                   INVALID KEY
                       MOVE "N" TO SCHEDULE-OK-SW
                       MOVE "BUSINESS DAY COUNT FAILED"
                           TO WK-REASON
                   NOT INVALID KEY
                       IF FS-KZCALF NOT = "00"
                           DISPLAY "KZCALF BD READ ST="
                               FS-KZCALF
                           MOVE "Y" TO HARD-ERROR-SW
                           MOVE 12 TO RETURN-CODE
                       ELSE
                           IF CA-HOLIDAY-FLAG = "N"
                               ADD 1 TO WK-BUSINESS-DAYS
                           END-IF
                       END-IF
               END-READ
               ADD 1 TO WK-WORK-INT
           END-PERFORM

           IF SCHEDULE-OK
              AND WK-BUSINESS-DAYS < WK-MIN-BUSINESS-DAYS
               MOVE "N" TO SCHEDULE-OK-SW
               MOVE "BUSINESS DAYS SHORT" TO WK-REASON
           END-IF.

       7000-WRITE-SCHEDULE.
           INITIALIZE KZSCHF-REC
           MOVE CY-CYCLE-ID TO SC-CYCLE-ID
           MOVE WK-RESOLVED-DT TO SC-SCHEDULED-DT
           MOVE FP-FISCAL-PERIOD-ID TO SC-PERIOD-ID
           MOVE "JPBK" TO SC-CALENDAR-CD
           MOVE "0" TO SC-GENERATE-STATUS
           MOVE SPACES TO WK-JOBNET-ID
           STRING "JN" DELIMITED BY SIZE
                  CY-CYCLE-ID(1:10) DELIMITED BY SIZE
               INTO WK-JOBNET-ID
           END-STRING
           MOVE WK-JOBNET-ID TO SC-JOBNET-ID

           WRITE KZSCHF-REC
           IF FS-KZSCHF NOT = "00"
               DISPLAY "KZSCHF WRITE ST=" FS-KZSCHF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WK-WRITE-SCHEDULES
           END-IF.

       7100-WRITE-CYCLE-RESULT.
           INITIALIZE KZCYRF-REC
           MOVE CY-CYCLE-ID TO CR-CYCLE-ID
           MOVE CY-NOMINAL-DT TO CR-NOMINAL-DT
           MOVE WK-RESOLVED-DT TO CR-RESOLVED-DT

           IF DATE-ROLLED
               MOVE "Y" TO CR-ROLLED-FLAG
           ELSE
               MOVE "N" TO CR-ROLLED-FLAG
           END-IF

           WRITE KZCYRF-REC
           IF FS-KZCYRF NOT = "00"
               DISPLAY "KZCYRF WRITE ST=" FS-KZCYRF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WK-WRITE-RESULTS
           END-IF.

       9000-FINAL.
           PERFORM 9100-CLOSE-FILES

           IF HARD-ERROR
               ADD 1 TO WK-ABEND-COUNT
               IF RETURN-CODE = 0
                   MOVE 12 TO RETURN-CODE
               END-IF
               DISPLAY "KZ150B ABEND RC=" RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "KZ150B NORMAL END"
           END-IF

           DISPLAY "READ COUNT=" WK-READ-CYCLES
           DISPLAY "SCHEDULE COUNT=" WK-WRITE-SCHEDULES
           DISPLAY "RESULT COUNT=" WK-WRITE-RESULTS
           DISPLAY "SKIP COUNT=" WK-SKIP-COUNT
           DISPLAY "ABEND COUNT=" WK-ABEND-COUNT.

       9100-CLOSE-FILES.
           CLOSE KZCYCF
           IF FS-KZCYCF NOT = "00"
              AND FS-KZCYCF NOT = "42"
               DISPLAY "KZCYCF CLOSE ST=" FS-KZCYCF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZCALF
           IF FS-KZCALF NOT = "00"
              AND FS-KZCALF NOT = "42"
               DISPLAY "KZCALF CLOSE ST=" FS-KZCALF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZFPRF
           IF FS-KZFPRF NOT = "00"
              AND FS-KZFPRF NOT = "42"
               DISPLAY "KZFPRF CLOSE ST=" FS-KZFPRF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZSCHF
           IF FS-KZSCHF NOT = "00"
              AND FS-KZSCHF NOT = "42"
               DISPLAY "KZSCHF CLOSE ST=" FS-KZSCHF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZCYRF
           IF FS-KZCYRF NOT = "00"
              AND FS-KZCYRF NOT = "42"
               DISPLAY "KZCYRF CLOSE ST=" FS-KZCYRF
               MOVE "Y" TO HARD-ERROR-SW
               MOVE 8 TO RETURN-CODE
           END-IF.
