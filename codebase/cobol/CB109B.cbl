       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB109B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDREVF
               ASSIGN TO "CDREVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RV-CARD-NO
               FILE STATUS IS WS-CDREVF-ST.

           SELECT CDRBALF
               ASSIGN TO "CDRBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDRBALF-ST.

           SELECT CDRSLDF
               ASSIGN TO "CDRSLDF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDRSLDF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  CDREVF.
           COPY CDREVFC.

       FD  CDRBALF.
           COPY CDRBALFC.

       FD  CDRSLDF.
           COPY CDRSLDFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CDREVF-ST        PIC X(02) VALUE SPACE.
           05  WS-CDRBALF-ST       PIC X(02) VALUE SPACE.
           05  WS-CDRSLDF-ST       PIC X(02) VALUE SPACE.

       01  WS-END-FLAGS.
           05  WS-REV-END          PIC X VALUE "N".
               88  REV-END               VALUE "Y".
           05  WS-BAL-END          PIC X VALUE "N".
               88  BAL-END               VALUE "Y".
           05  WS-SLD-END          PIC X VALUE "N".
               88  SLD-END               VALUE "Y".
           05  WS-ABEND-FLG        PIC X VALUE "N".
               88  ABEND-ON              VALUE "Y".

       01  WS-CONSTANTS.
           05  WS-PGM-ID           PIC X(08) VALUE "CB109B".
           05  WS-ACTIVE           PIC X(02) VALUE "01".
           05  WS-STOPPED          PIC X(02) VALUE "02".
           05  WS-CLOSED           PIC X(02) VALUE "03".
           05  WS-STAT-C           PIC X     VALUE "C".
           05  WS-TIER-1           PIC X(02) VALUE "T1".
           05  WS-TIER-2           PIC X(02) VALUE "T2".
           05  WS-TIER-3           PIC X(02) VALUE "T3".
           05  WS-TIER-4           PIC X(02) VALUE "T4".
           05  WS-MONTHLY-RATE     PIC 9V9(04) VALUE 0.0125.
           05  WS-TOLERANCE        PIC S9(09) COMP-3 VALUE 1.

       01  WS-WORK.
           05  WS-SYS-DATE.
               10  WS-SYS-YYYY     PIC 9(04).
               10  WS-SYS-MM       PIC 9(02).
               10  WS-SYS-DD       PIC 9(02).
           05  WS-CALC-FEE         PIC S9(11) COMP-3 VALUE ZERO.
           05  WS-CALC-PRIN        PIC S9(11) COMP-3 VALUE ZERO.
           05  WS-CALC-PAY         PIC S9(11) COMP-3 VALUE ZERO.
           05  WS-DIFF-AMT         PIC S9(11) COMP-3 VALUE ZERO.
           05  WS-ABS-DIFF         PIC S9(11) COMP-3 VALUE ZERO.
           05  WS-MATCH-BAL        PIC X VALUE "N".
               88  BAL-MATCH             VALUE "Y".
           05  WS-MATCH-SLD        PIC X VALUE "N".
               88  SLD-MATCH             VALUE "Y".
           05  WS-DUE-DT           PIC 9(08) VALUE ZERO.

       01  WS-COUNTERS.
           05  WS-REV-CNT          PIC 9(09) VALUE ZERO.
           05  WS-PROC-CNT         PIC 9(09) VALUE ZERO.
           05  WS-SKIP-CNT         PIC 9(09) VALUE ZERO.
           05  WS-NOTIF-CNT        PIC 9(09) VALUE ZERO.
           05  WS-ERR-CNT          PIC 9(09) VALUE ZERO.

           COPY CDNOTIC.
           COPY LK-SLIDE-PARM.
           COPY LK-RSLED-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           IF NOT ABEND-ON
               PERFORM 2000-MAIN-PROCESS
                   UNTIL REV-END OR ABEND-ON
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-SYS-DATE FROM DATE YYYYMMDD

           OPEN INPUT CDREVF
           IF WS-CDREVF-ST NOT = "00"
               DISPLAY "CDREVF OPEN ST=" WS-CDREVF-ST
               PERFORM 9100-SET-ABEND
           END-IF

           IF NOT ABEND-ON
               OPEN INPUT CDRBALF
               IF WS-CDRBALF-ST NOT = "00"
                   DISPLAY "CDRBALF OPEN ST=" WS-CDRBALF-ST
                   PERFORM 9100-SET-ABEND
               END-IF
           END-IF

           IF NOT ABEND-ON
               OPEN INPUT CDRSLDF
               IF WS-CDRSLDF-ST NOT = "00"
                   DISPLAY "CDRSLDF OPEN ST=" WS-CDRSLDF-ST
                   PERFORM 9100-SET-ABEND
               END-IF
           END-IF

           IF NOT ABEND-ON
               PERFORM 1100-READ-REV
               PERFORM 1200-READ-BAL
               PERFORM 1300-READ-SLD
           END-IF.

       1100-READ-REV.
           READ CDREVF NEXT RECORD
               AT END
                   SET REV-END TO TRUE
               NOT AT END
                   ADD 1 TO WS-REV-CNT
           END-READ
           IF WS-CDREVF-ST NOT = "00"
           AND WS-CDREVF-ST NOT = "10"
               DISPLAY "CDREVF READ ST=" WS-CDREVF-ST
               PERFORM 9100-SET-ABEND
           END-IF.

       1200-READ-BAL.
           READ CDRBALF
               AT END
                   SET BAL-END TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF WS-CDRBALF-ST NOT = "00"
           AND WS-CDRBALF-ST NOT = "10"
               DISPLAY "CDRBALF READ ST=" WS-CDRBALF-ST
               PERFORM 9100-SET-ABEND
           END-IF.

       1300-READ-SLD.
           READ CDRSLDF
               AT END
                   SET SLD-END TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF WS-CDRSLDF-ST NOT = "00"
           AND WS-CDRSLDF-ST NOT = "10"
               DISPLAY "CDRSLDF READ ST=" WS-CDRSLDF-ST
               PERFORM 9100-SET-ABEND
           END-IF.

       2000-MAIN-PROCESS.
           MOVE "N" TO WS-MATCH-BAL
           MOVE "N" TO WS-MATCH-SLD

           IF RV-REV-STATUS = WS-ACTIVE
               PERFORM 2100-LOCATE-BAL
               PERFORM 2200-LOCATE-SLD
               IF BAL-MATCH
               AND SLD-MATCH
                   PERFORM 2300-VALIDATE-AND-CALC
               ELSE
                   ADD 1 TO WS-ERR-CNT
                   PERFORM 2600-WRITE-MISSING-NOTICE
               END-IF
           ELSE
               IF RV-REV-STATUS = WS-STOPPED
               OR RV-REV-STATUS = WS-CLOSED
                   ADD 1 TO WS-SKIP-CNT
               ELSE
                   DISPLAY "REV STATUS CARD=" RV-CARD-NO
                   ADD 1 TO WS-ERR-CNT
                   PERFORM 2600-WRITE-MISSING-NOTICE
               END-IF
           END-IF

           IF NOT ABEND-ON
               PERFORM 1100-READ-REV
           END-IF.

       2100-LOCATE-BAL.
           PERFORM UNTIL BAL-END
              OR RB-CARD-NO >= RV-CARD-NO
               PERFORM 1200-READ-BAL
           END-PERFORM
           IF NOT BAL-END
           AND RB-CARD-NO = RV-CARD-NO
               MOVE "Y" TO WS-MATCH-BAL
           END-IF.

       2200-LOCATE-SLD.
           PERFORM UNTIL SLD-END
              OR RS-CARD-NO >= RV-CARD-NO
               PERFORM 1300-READ-SLD
           END-PERFORM
           IF NOT SLD-END
           AND RS-CARD-NO = RV-CARD-NO
               MOVE "Y" TO WS-MATCH-SLD
           END-IF.

       2300-VALIDATE-AND-CALC.
           IF RB-REV-BAL-AMT < ZERO
               DISPLAY "BAL AMT CARD=" RV-CARD-NO
               ADD 1 TO WS-ERR-CNT
               PERFORM 2600-WRITE-MISSING-NOTICE
           ELSE
               IF RS-RSLD-STATUS NOT = WS-STAT-C
                   DISPLAY "SLD STATUS CARD=" RV-CARD-NO
                   ADD 1 TO WS-SKIP-CNT
               ELSE
                   IF RS-SLIDE-TIER = WS-TIER-1
                   OR RS-SLIDE-TIER = WS-TIER-2
                   OR RS-SLIDE-TIER = WS-TIER-3
                   OR RS-SLIDE-TIER = WS-TIER-4
                       PERFORM 2400-RECALC
                       PERFORM 2500-COMPARE
                   ELSE
                       DISPLAY "SLD TIER CARD=" RV-CARD-NO
                       ADD 1 TO WS-ERR-CNT
                       PERFORM 2600-WRITE-MISSING-NOTICE
                   END-IF
               END-IF
           END-IF.

       2400-RECALC.
           MOVE ZERO TO WS-CALC-FEE
           MOVE ZERO TO WS-CALC-PRIN
           MOVE ZERO TO WS-CALC-PAY
           MOVE ZERO TO WS-DIFF-AMT
           MOVE ZERO TO WS-ABS-DIFF

           COMPUTE WS-CALC-FEE =
               FUNCTION INTEGER(RB-REV-BAL-AMT * WS-MONTHLY-RATE)

           MOVE ZERO TO LK-SLIDE-PARM
           MOVE RB-REV-BAL-AMT TO LK-SL-REV-BAL
           CALL "CB290S" USING LK-SLIDE-PARM

           IF LK-SL-RET NOT = ZERO
               DISPLAY "CB290S CARD=" RV-CARD-NO
               ADD 1 TO WS-ERR-CNT
               PERFORM 2600-WRITE-MISSING-NOTICE
           ELSE
               MOVE LK-SL-PRIN-AMT TO WS-CALC-PRIN
               COMPUTE WS-CALC-PAY =
                   WS-CALC-PRIN + WS-CALC-FEE
           END-IF.

       2500-COMPARE.
           IF LK-SL-RET = ZERO
               COMPUTE WS-DIFF-AMT =
                   WS-CALC-PAY - RS-PAY-AMT
               IF WS-DIFF-AMT < ZERO
                   COMPUTE WS-ABS-DIFF = WS-DIFF-AMT * -1
               ELSE
                   MOVE WS-DIFF-AMT TO WS-ABS-DIFF
               END-IF

               IF WS-ABS-DIFF > WS-TOLERANCE
                   PERFORM 2700-EDIT-AND-NOTIFY
               ELSE
                   ADD 1 TO WS-PROC-CNT
               END-IF
           END-IF.

       2600-WRITE-MISSING-NOTICE.
           MOVE ZERO TO WS-CALC-PAY
           MOVE ZERO TO WS-DIFF-AMT
           MOVE ZERO TO WS-ABS-DIFF
           PERFORM 2800-WRITE-NOTICE.

       2700-EDIT-AND-NOTIFY.
           MOVE ZERO TO LK-RSLED-PARM
           MOVE RV-CARD-NO  TO LK-SE-CARD-NO
           MOVE RB-CYCLE-DT TO LK-SE-CYCLE-DT
           MOVE WS-CALC-PAY TO LK-SE-PAY-AMT
           MOVE WS-DUE-DT   TO LK-SE-DUE-DT

           CALL "CB280S" USING LK-RSLED-PARM

           IF LK-SE-RET NOT = ZERO
               DISPLAY "CB280S CARD=" RV-CARD-NO
               ADD 1 TO WS-ERR-CNT
               PERFORM 9100-SET-ABEND
           ELSE
               PERFORM 2800-WRITE-NOTICE
           END-IF.

       2800-WRITE-NOTICE.
           ADD 1 TO WS-NOTIF-CNT
           DISPLAY WS-PGM-ID
           DISPLAY "CARD=" RV-CARD-NO
           DISPLAY "DATE=" WS-SYS-DATE
           DISPLAY "CALC=" WS-CALC-PAY
           DISPLAY "DIFF=" WS-ABS-DIFF.

       9000-FINAL.
           IF WS-CDREVF-ST NOT = SPACE
               CLOSE CDREVF
           END-IF
           IF WS-CDRBALF-ST NOT = SPACE
               CLOSE CDRBALF
           END-IF
           IF WS-CDRSLDF-ST NOT = SPACE
               CLOSE CDRSLDF
           END-IF

           IF ABEND-ON
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CB109B END INPUT=" WS-REV-CNT
               DISPLAY "CB109B PROC=" WS-PROC-CNT
               DISPLAY "CB109B SKIP=" WS-SKIP-CNT
               DISPLAY "CB109B NOTICE=" WS-NOTIF-CNT
               DISPLAY "CB109B ERROR=" WS-ERR-CNT
           END-IF.

       9100-SET-ABEND.
           MOVE "Y" TO WS-ABEND-FLG
           MOVE 8 TO RETURN-CODE.
