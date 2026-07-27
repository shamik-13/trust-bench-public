       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC230B.
       AUTHOR.     MFG-KYOTSU-BATCH.
       INSTALLATION. MFG-KYOTSU-KIBAN.
       DATE-WRITTEN. 20250214.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CCCALF-ST.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ER-ERROR-ID
               FILE STATUS IS WS-CCERRF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CCCALF.
           COPY CCCALFC.
      *
       FD  CCERRF.
           COPY CCERRC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CCCALF-ST              PIC XX VALUE SPACE.
           05 WS-CCERRF-ST              PIC XX VALUE SPACE.
      *
       01  WS-CONSTANTS.
           05 WS-PGM-ID                 PIC X(08) VALUE "CC230B".
           05 WS-ERR-PGM                PIC X(08) VALUE "CC230B".
           05 WS-MAX-CAL                PIC 9(04) VALUE 1000.
           05 WS-MAX-MNT                PIC 9(02) VALUE 12.
           05 WS-NORMAL-ST              PIC XX VALUE "00".
           05 WS-EOF-ST                 PIC XX VALUE "10".
           05 WS-DUP-ST                 PIC XX VALUE "22".
           05 WS-HOLIDAY                PIC X VALUE "Y".
           05 WS-BUSINESS               PIC X VALUE "N".
           05 WS-ADD-KBN                PIC X VALUE "A".
           05 WS-CANCEL-KBN             PIC X VALUE "C".
           05 WS-APPROVED               PIC X VALUE "Y".
      *
       01  WS-SWITCHES.
           05 WS-END-FLAG               PIC X VALUE "N".
              88 END-OF-CCCALF                VALUE "Y".
           05 WS-HARD-ERROR-FLAG        PIC X VALUE "N".
              88 HARD-ERROR                   VALUE "Y".
           05 WS-FOUND-FLAG             PIC X VALUE "N".
              88 CAL-FOUND                    VALUE "Y".
           05 WS-CONT-ERR-FLAG          PIC X VALUE "N".
              88 CONTINUITY-ERROR             VALUE "Y".
           05 WS-CCCALF-OPEN-FLAG       PIC X VALUE "N".
              88 CCCALF-OPEN                  VALUE "Y".
              88 CCCALF-CLOSED                VALUE "N".
           05 WS-CCERRF-OPEN-FLAG       PIC X VALUE "N".
              88 CCERRF-OPEN                  VALUE "Y".
              88 CCERRF-CLOSED                VALUE "N".
      *
       01  WS-COUNTERS.
           05 WS-CAL-CNT                PIC 9(04) VALUE ZERO.
           05 WS-MNT-IDX                PIC 9(04) VALUE ZERO.
           05 WS-CAL-IDX                PIC 9(04) VALUE ZERO.
           05 WS-CAL-IDX2               PIC 9(04) VALUE ZERO.
           05 WS-PREV-IDX               PIC 9(04) VALUE ZERO.
           05 WS-NEXT-IDX               PIC 9(04) VALUE ZERO.
           05 WS-ERR-SEQ                PIC 9(06) VALUE ZERO.
           05 WS-READ-CNT               PIC 9(07) VALUE ZERO.
           05 WS-WRITE-CNT              PIC 9(07) VALUE ZERO.
           05 WS-APPLY-CNT              PIC 9(07) VALUE ZERO.
           05 WS-ERR-CNT                PIC 9(07) VALUE ZERO.
      *
       01  WS-WORK.
           05 WS-ERR-ID-WK              PIC X(12) VALUE SPACE.
           05 WS-BASE-DT-WK             PIC X(08) VALUE SPACE.
           05 WS-RECORD-KEY-WK          PIC X(32) VALUE SPACE.
           05 WS-ERROR-KBN-WK           PIC X(04) VALUE SPACE.
           05 WS-ERROR-TEXT-WK          PIC X(80) VALUE SPACE.
           05 WS-PREV-DT                PIC 9(08) VALUE ZERO.
           05 WS-CURR-DT                PIC 9(08) VALUE ZERO.
           05 WS-NEXT-DT                PIC 9(08) VALUE ZERO.
           05 WS-DIFF                   PIC S9(09) VALUE ZERO.
      *
       01  WS-CAL-TABLE.
           05 WS-CAL-ENTRY OCCURS 1000 TIMES.
              10 WS-TB-CAL-DT           PIC X(08).
              10 WS-TB-HOLIDAY-FLAG     PIC X.
      *
       01  WS-MAINT-TABLE.
           05 WS-MAINT-ENTRY OCCURS 12 TIMES.
              10 WS-MT-DT               PIC X(08).
              10 WS-MT-KBN              PIC X.
              10 WS-MT-APPROVAL         PIC X.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0500-INIT-MAINT
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-CALENDAR
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-PROCESS-MAINT
           END-IF
           IF NOT HARD-ERROR
              PERFORM 4000-REWRITE-CALENDAR
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CC230B NORMAL END READ=" WS-READ-CNT
                      " APPLY=" WS-APPLY-CNT
                      " ERROR=" WS-ERR-CNT
           END-IF
           GOBACK.
      *
       0500-INIT-MAINT.
           MOVE "20250211" TO WS-MT-DT(1)
           MOVE "A" TO WS-MT-KBN(1)
           MOVE "Y" TO WS-MT-APPROVAL(1)
           MOVE "20250224" TO WS-MT-DT(2)
           MOVE "A" TO WS-MT-KBN(2)
           MOVE "Y" TO WS-MT-APPROVAL(2)
           MOVE "20250320" TO WS-MT-DT(3)
           MOVE "A" TO WS-MT-KBN(3)
           MOVE "Y" TO WS-MT-APPROVAL(3)
           MOVE "20250429" TO WS-MT-DT(4)
           MOVE "A" TO WS-MT-KBN(4)
           MOVE "Y" TO WS-MT-APPROVAL(4)
           MOVE "20250503" TO WS-MT-DT(5)
           MOVE "A" TO WS-MT-KBN(5)
           MOVE "Y" TO WS-MT-APPROVAL(5)
           MOVE "20250506" TO WS-MT-DT(6)
           MOVE "C" TO WS-MT-KBN(6)
           MOVE "Y" TO WS-MT-APPROVAL(6)
           MOVE "20250721" TO WS-MT-DT(7)
           MOVE "A" TO WS-MT-KBN(7)
           MOVE "N" TO WS-MT-APPROVAL(7)
           MOVE "20250811" TO WS-MT-DT(8)
           MOVE "A" TO WS-MT-KBN(8)
           MOVE "Y" TO WS-MT-APPROVAL(8)
           MOVE "20250915" TO WS-MT-DT(9)
           MOVE "A" TO WS-MT-KBN(9)
           MOVE "Y" TO WS-MT-APPROVAL(9)
           MOVE "20250923" TO WS-MT-DT(10)
           MOVE "C" TO WS-MT-KBN(10)
           MOVE "Y" TO WS-MT-APPROVAL(10)
           MOVE "20251103" TO WS-MT-DT(11)
           MOVE "A" TO WS-MT-KBN(11)
           MOVE "Y" TO WS-MT-APPROVAL(11)
           MOVE "20251124" TO WS-MT-DT(12)
           MOVE "A" TO WS-MT-KBN(12)
           MOVE "Y" TO WS-MT-APPROVAL(12).
      *
       1000-OPEN-FILES.
           OPEN INPUT CCCALF
           IF WS-CCCALF-ST NOT = WS-NORMAL-ST
              DISPLAY "CCCALF OPEN ERROR ST=" WS-CCCALF-ST
              SET HARD-ERROR TO TRUE
           ELSE
              SET CCCALF-OPEN TO TRUE
           END-IF
           OPEN OUTPUT CCERRF
           IF WS-CCERRF-ST NOT = WS-NORMAL-ST
              DISPLAY "CCERRF OPEN ERROR ST=" WS-CCERRF-ST
              SET HARD-ERROR TO TRUE
           ELSE
              SET CCERRF-OPEN TO TRUE
           END-IF.
      *
       2000-LOAD-CALENDAR.
           PERFORM UNTIL END-OF-CCCALF OR HARD-ERROR
              READ CCCALF
                 AT END
                    SET END-OF-CCCALF TO TRUE
                 NOT AT END
                    ADD 1 TO WS-READ-CNT
                    IF WS-CAL-CNT >= WS-MAX-CAL
                       DISPLAY "CCCALF COUNT LIMIT EXCEEDED"
                       SET HARD-ERROR TO TRUE
                    ELSE
                       ADD 1 TO WS-CAL-CNT
                       MOVE CL-CAL-DT TO WS-TB-CAL-DT(WS-CAL-CNT)
                       MOVE CL-HOLIDAY-FLAG
                         TO WS-TB-HOLIDAY-FLAG(WS-CAL-CNT)
                    END-IF
              END-READ
           END-PERFORM
           CLOSE CCCALF
           SET CCCALF-CLOSED TO TRUE
           IF WS-CCCALF-ST NOT = WS-NORMAL-ST
              DISPLAY "CCCALF CLOSE ERROR ST=" WS-CCCALF-ST
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       3000-PROCESS-MAINT.
           PERFORM VARYING WS-MNT-IDX FROM 1 BY 1
             UNTIL WS-MNT-IDX > WS-MAX-MNT OR HARD-ERROR
              IF WS-MT-APPROVAL(WS-MNT-IDX) = WS-APPROVED
                 PERFORM 3100-FIND-CALENDAR
                 PERFORM 3200-CHECK-CONTINUITY
                 IF CONTINUITY-ERROR
                    MOVE "CONT" TO WS-ERROR-KBN-WK
                    MOVE "CALENDAR CONTINUITY ERROR"
                      TO WS-ERROR-TEXT-WK
                    PERFORM 8000-WRITE-ERROR
                 ELSE
                    PERFORM 3300-APPLY-ONE-MAINT
                 END-IF
              END-IF
           END-PERFORM.
      *
       3100-FIND-CALENDAR.
           MOVE "N" TO WS-FOUND-FLAG
           MOVE ZERO TO WS-CAL-IDX
           PERFORM VARYING WS-CAL-IDX2 FROM 1 BY 1
             UNTIL WS-CAL-IDX2 > WS-CAL-CNT OR CAL-FOUND
              IF WS-TB-CAL-DT(WS-CAL-IDX2) = WS-MT-DT(WS-MNT-IDX)
                 MOVE WS-CAL-IDX2 TO WS-CAL-IDX
                 SET CAL-FOUND TO TRUE
              END-IF
           END-PERFORM.
      *
       3200-CHECK-CONTINUITY.
           MOVE "N" TO WS-CONT-ERR-FLAG
           IF CAL-FOUND
              IF WS-CAL-IDX > 1
                 COMPUTE WS-PREV-IDX = WS-CAL-IDX - 1
                 MOVE WS-TB-CAL-DT(WS-PREV-IDX) TO WS-PREV-DT
                 MOVE WS-TB-CAL-DT(WS-CAL-IDX) TO WS-CURR-DT
                 COMPUTE WS-DIFF = WS-CURR-DT - WS-PREV-DT
                 IF WS-DIFF < 1 OR WS-DIFF > 73
                    SET CONTINUITY-ERROR TO TRUE
                 END-IF
              END-IF
              IF WS-CAL-IDX < WS-CAL-CNT
                 COMPUTE WS-NEXT-IDX = WS-CAL-IDX + 1
                 MOVE WS-TB-CAL-DT(WS-CAL-IDX) TO WS-CURR-DT
                 MOVE WS-TB-CAL-DT(WS-NEXT-IDX) TO WS-NEXT-DT
                 COMPUTE WS-DIFF = WS-NEXT-DT - WS-CURR-DT
                 IF WS-DIFF < 1 OR WS-DIFF > 73
                    SET CONTINUITY-ERROR TO TRUE
                 END-IF
              END-IF
           ELSE
              SET CONTINUITY-ERROR TO TRUE
           END-IF.
      *
       3300-APPLY-ONE-MAINT.
           EVALUATE WS-MT-KBN(WS-MNT-IDX)
              WHEN WS-ADD-KBN
                 IF WS-TB-HOLIDAY-FLAG(WS-CAL-IDX) = WS-HOLIDAY
                    MOVE "DUPA" TO WS-ERROR-KBN-WK
                    MOVE "HOLIDAY ALREADY REGISTERED"
                      TO WS-ERROR-TEXT-WK
                    PERFORM 8000-WRITE-ERROR
                 ELSE
                    MOVE WS-HOLIDAY
                      TO WS-TB-HOLIDAY-FLAG(WS-CAL-IDX)
                    ADD 1 TO WS-APPLY-CNT
                 END-IF
              WHEN WS-CANCEL-KBN
                 IF WS-TB-HOLIDAY-FLAG(WS-CAL-IDX) = WS-BUSINESS
                    MOVE "MISS" TO WS-ERROR-KBN-WK
                    MOVE "HOLIDAY TO CANCEL NOT FOUND"
                      TO WS-ERROR-TEXT-WK
                    PERFORM 8000-WRITE-ERROR
                 ELSE
                    MOVE WS-BUSINESS
                      TO WS-TB-HOLIDAY-FLAG(WS-CAL-IDX)
                    ADD 1 TO WS-APPLY-CNT
                 END-IF
              WHEN OTHER
                 MOVE "KBN" TO WS-ERROR-KBN-WK
                 MOVE "MAINTENANCE TYPE ERROR"
                   TO WS-ERROR-TEXT-WK
                 PERFORM 8000-WRITE-ERROR
           END-EVALUATE.
      *
       4000-REWRITE-CALENDAR.
           OPEN OUTPUT CCCALF
           IF WS-CCCALF-ST NOT = WS-NORMAL-ST
              DISPLAY "CCCALF OUTPUT OPEN ERROR ST=" WS-CCCALF-ST
              SET HARD-ERROR TO TRUE
           ELSE
              SET CCCALF-OPEN TO TRUE
              PERFORM VARYING WS-CAL-IDX FROM 1 BY 1
                UNTIL WS-CAL-IDX > WS-CAL-CNT OR HARD-ERROR
                 MOVE WS-TB-CAL-DT(WS-CAL-IDX) TO CL-CAL-DT
                 MOVE WS-TB-HOLIDAY-FLAG(WS-CAL-IDX)
                   TO CL-HOLIDAY-FLAG
                 WRITE CCCALF-REC
                 IF WS-CCCALF-ST NOT = WS-NORMAL-ST
                    DISPLAY "CCCALF WRITE ERROR ST=" WS-CCCALF-ST
                    SET HARD-ERROR TO TRUE
                 ELSE
                    ADD 1 TO WS-WRITE-CNT
                 END-IF
              END-PERFORM
           END-IF.
      *
       8000-WRITE-ERROR.
           ADD 1 TO WS-ERR-SEQ
           ADD 1 TO WS-ERR-CNT
           MOVE WS-MT-DT(WS-MNT-IDX) TO WS-BASE-DT-WK
           MOVE SPACE TO WS-RECORD-KEY-WK
           STRING WS-MT-DT(WS-MNT-IDX) DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-MT-KBN(WS-MNT-IDX) DELIMITED BY SIZE
             INTO WS-RECORD-KEY-WK
           MOVE SPACE TO WS-ERR-ID-WK
           STRING WS-PGM-ID DELIMITED BY SIZE
                  WS-ERR-SEQ DELIMITED BY SIZE
             INTO WS-ERR-ID-WK
           MOVE WS-ERR-ID-WK TO ER-ERROR-ID
           MOVE WS-ERR-PGM TO ER-PGM-ID
           MOVE WS-BASE-DT-WK TO ER-BASE-DT
           MOVE WS-RECORD-KEY-WK TO ER-RECORD-KEY
           MOVE WS-ERROR-KBN-WK TO ER-ERROR-KBN
           MOVE WS-ERROR-TEXT-WK TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF WS-CCERRF-ST NOT = WS-NORMAL-ST
              AND WS-CCERRF-ST NOT = WS-DUP-ST
              DISPLAY "CCERRF WRITE ERROR ST=" WS-CCERRF-ST
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       9000-CLOSE-FILES.
           IF CCCALF-OPEN
              CLOSE CCCALF
              SET CCCALF-CLOSED TO TRUE
              IF WS-CCCALF-ST NOT = WS-NORMAL-ST
                 DISPLAY "CCCALF CLOSE ERROR ST=" WS-CCCALF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF CCERRF-OPEN
              CLOSE CCERRF
              SET CCERRF-CLOSED TO TRUE
              IF WS-CCERRF-ST NOT = WS-NORMAL-ST
                 DISPLAY "CCERRF CLOSE ERROR ST=" WS-CCERRF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.
