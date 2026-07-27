       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC220S.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
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
       01  WS-CCCALF-ST              PIC XX VALUE SPACES.
       01  WS-CCERRF-ST              PIC XX VALUE SPACES.
      *
       01  WS-EOF-SW                 PIC X VALUE "N".
           88  WS-EOF                      VALUE "Y".
           88  WS-NOT-EOF                  VALUE "N".
      *
       01  WS-BASE-DT                PIC 9(8).
       01  WS-BASE-INT               PIC 9(8).
       01  WS-PREV-DT                PIC 9(8).
       01  WS-NEXT-DT                PIC 9(8).
       01  WS-PREV-INT               PIC 9(8).
       01  WS-NEXT-INT               PIC 9(8).
       01  WS-CAL-INT                PIC 9(8).
       01  WS-LAST-DT                PIC 9(8) VALUE ZERO.
       01  WS-HOLD-DT                PIC 9(8) VALUE ZERO.
       01  WS-HOLD-HOLIDAY-FLAG      PIC X VALUE SPACE.
      *
       01  WS-FOUND-SW               PIC X VALUE "N".
           88  WS-FOUND                    VALUE "Y".
           88  WS-NOT-FOUND                VALUE "N".
       01  WS-PREV-FOUND-SW          PIC X VALUE "N".
           88  WS-PREV-FOUND               VALUE "Y".
           88  WS-PREV-NOT-FOUND           VALUE "N".
       01  WS-NEXT-FOUND-SW          PIC X VALUE "N".
           88  WS-NEXT-FOUND               VALUE "Y".
           88  WS-NEXT-NOT-FOUND           VALUE "N".
      *
       01  WS-RESULT-CD              PIC 9(2) VALUE ZERO.
       01  WS-REASON-KBN             PIC X(2) VALUE SPACE.
       01  WS-ERROR-TEXT             PIC X(80) VALUE SPACE.
       01  WS-RECORD-KEY             PIC X(32) VALUE SPACE.
      *
       01  WS-CURRENT-DATE.
           05  WS-CUR-YYYY           PIC 9(4).
           05  WS-CUR-MM             PIC 9(2).
           05  WS-CUR-DD             PIC 9(2).
           05  WS-CUR-HH             PIC 9(2).
           05  WS-CUR-MI             PIC 9(2).
           05  WS-CUR-SS             PIC 9(2).
           05  WS-CUR-CC             PIC 9(2).
           05  WS-CUR-GMTOFF         PIC X(5).
      *
       01  WS-ERROR-ID-WK.
           05  WS-EID-DATE           PIC 9(8).
           05  WS-EID-TIME           PIC 9(8).
           05  WS-EID-SUFFIX         PIC 9(2).
      *
       LINKAGE SECTION.
       01  LK-CC220S-PARM.
           05  LK-CALLER-PGM-ID      PIC X(8).
           05  LK-BASE-DT            PIC 9(8).
           05  LK-RETURN-CD          PIC 9(2).
           05  LK-REASON-KBN         PIC X(2).
      *
       PROCEDURE DIVISION USING LK-CC220S-PARM.
      *
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           MOVE ZERO TO LK-RETURN-CD
           MOVE SPACE TO LK-REASON-KBN
           MOVE LK-BASE-DT TO WS-BASE-DT
           PERFORM 1000-INIT
           IF WS-RESULT-CD = ZERO
               PERFORM 2000-VALIDATE-DATE
           END-IF
           IF WS-RESULT-CD = ZERO
               PERFORM 3000-SCAN-CALENDAR
           END-IF
           IF WS-RESULT-CD = ZERO
               PERFORM 4000-JUDGE-CALENDAR
           END-IF
           IF WS-RESULT-CD NOT = ZERO
               PERFORM 8000-WRITE-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       1000-INIT.
           SET WS-NOT-EOF TO TRUE
           SET WS-NOT-FOUND TO TRUE
           SET WS-PREV-NOT-FOUND TO TRUE
           SET WS-NEXT-NOT-FOUND TO TRUE
           MOVE ZERO TO WS-LAST-DT
           MOVE ZERO TO WS-HOLD-DT
           MOVE SPACE TO WS-HOLD-HOLIDAY-FLAG
           MOVE ZERO TO WS-RESULT-CD
           MOVE SPACE TO WS-REASON-KBN
           MOVE SPACE TO WS-ERROR-TEXT
           MOVE SPACE TO WS-RECORD-KEY
           OPEN INPUT CCCALF
           IF WS-CCCALF-ST NOT = "00"
               MOVE 12 TO WS-RESULT-CD
               MOVE "F1" TO WS-REASON-KBN
               STRING "CCCALF OPEN ERROR ST="
                      WS-CCCALF-ST
                 DELIMITED BY SIZE INTO WS-ERROR-TEXT
               END-STRING
           END-IF
           IF WS-RESULT-CD = ZERO
               OPEN I-O CCERRF
               IF WS-CCERRF-ST NOT = "00"
                   MOVE 12 TO WS-RESULT-CD
                   MOVE "F2" TO WS-REASON-KBN
                   STRING "CCERRF OPEN ERROR ST="
                          WS-CCERRF-ST
                     DELIMITED BY SIZE INTO WS-ERROR-TEXT
                   END-STRING
               END-IF
           END-IF.
      *
       2000-VALIDATE-DATE.
           IF LK-BASE-DT NOT NUMERIC
               MOVE 4 TO WS-RESULT-CD
               MOVE "D1" TO WS-REASON-KBN
               MOVE "BASE DATE NOT NUMERIC" TO WS-ERROR-TEXT
           ELSE
               COMPUTE WS-BASE-INT =
                   FUNCTION INTEGER-OF-DATE(WS-BASE-DT)
               IF FUNCTION DATE-OF-INTEGER(WS-BASE-INT) NOT =
                  WS-BASE-DT
                   MOVE 4 TO WS-RESULT-CD
                   MOVE "D2" TO WS-REASON-KBN
                   MOVE "BASE DATE INVALID" TO WS-ERROR-TEXT
               ELSE
                   COMPUTE WS-PREV-INT = WS-BASE-INT - 1
                   COMPUTE WS-NEXT-INT = WS-BASE-INT + 1
                   MOVE FUNCTION DATE-OF-INTEGER(WS-PREV-INT)
                     TO WS-PREV-DT
                   MOVE FUNCTION DATE-OF-INTEGER(WS-NEXT-INT)
                     TO WS-NEXT-DT
               END-IF
           END-IF.
      *
       3000-SCAN-CALENDAR.
           PERFORM UNTIL WS-EOF OR WS-RESULT-CD NOT = ZERO
               READ CCCALF
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM 3100-CHECK-RECORD
               END-READ
           END-PERFORM.
      *
       3100-CHECK-RECORD.
           IF CL-CAL-DT NOT NUMERIC
               MOVE 12 TO WS-RESULT-CD
               MOVE "C1" TO WS-REASON-KBN
               MOVE "CALENDAR DATE NOT NUMERIC" TO WS-ERROR-TEXT
           ELSE
               COMPUTE WS-CAL-INT =
                   FUNCTION INTEGER-OF-DATE(CL-CAL-DT)
               IF FUNCTION DATE-OF-INTEGER(WS-CAL-INT) NOT =
                  CL-CAL-DT
                   MOVE 12 TO WS-RESULT-CD
                   MOVE "C2" TO WS-REASON-KBN
                   MOVE "CALENDAR DATE INVALID" TO WS-ERROR-TEXT
               ELSE
                   PERFORM 3200-CHECK-SEQUENCE
                   IF WS-RESULT-CD = ZERO
                       PERFORM 3300-CHECK-TARGET
                   END-IF
               END-IF
           END-IF.
      *
       3200-CHECK-SEQUENCE.
           IF WS-LAST-DT NOT = ZERO
               IF CL-CAL-DT <= WS-LAST-DT
                   MOVE 12 TO WS-RESULT-CD
                   MOVE "C3" TO WS-REASON-KBN
                   MOVE "CALENDAR DATE SEQUENCE ERROR"
                     TO WS-ERROR-TEXT
               END-IF
           END-IF
           IF WS-RESULT-CD = ZERO
               MOVE CL-CAL-DT TO WS-LAST-DT
               IF CL-HOLIDAY-FLAG NOT = "Y" AND
                  CL-HOLIDAY-FLAG NOT = "N"
                   MOVE 12 TO WS-RESULT-CD
                   MOVE "C4" TO WS-REASON-KBN
                   MOVE "CALENDAR HOLIDAY FLAG ERROR"
                     TO WS-ERROR-TEXT
               END-IF
           END-IF.
      *
       3300-CHECK-TARGET.
           IF CL-CAL-DT = WS-PREV-DT
               MOVE CL-CAL-DT TO WS-HOLD-DT
               MOVE CL-HOLIDAY-FLAG TO WS-HOLD-HOLIDAY-FLAG
               SET WS-PREV-FOUND TO TRUE
           END-IF
           IF CL-CAL-DT = WS-BASE-DT
               MOVE CL-CAL-DT TO WS-HOLD-DT
               MOVE CL-HOLIDAY-FLAG TO WS-HOLD-HOLIDAY-FLAG
               SET WS-FOUND TO TRUE
           END-IF
           IF CL-CAL-DT = WS-NEXT-DT
               MOVE CL-CAL-DT TO WS-HOLD-DT
               MOVE CL-HOLIDAY-FLAG TO WS-HOLD-HOLIDAY-FLAG
               SET WS-NEXT-FOUND TO TRUE
           END-IF
           IF CL-CAL-DT > WS-NEXT-DT
               SET WS-EOF TO TRUE
           END-IF.
      *
       4000-JUDGE-CALENDAR.
           IF WS-NOT-FOUND
               MOVE 4 TO WS-RESULT-CD
               MOVE "N1" TO WS-REASON-KBN
               MOVE "BASE DATE NOT FOUND" TO WS-ERROR-TEXT
           ELSE
               IF WS-PREV-NOT-FOUND
                   MOVE 4 TO WS-RESULT-CD
                   MOVE "N2" TO WS-REASON-KBN
                   MOVE "PREVIOUS DATE NOT FOUND" TO WS-ERROR-TEXT
               END-IF
               IF WS-RESULT-CD = ZERO AND WS-NEXT-NOT-FOUND
                   MOVE 4 TO WS-RESULT-CD
                   MOVE "N3" TO WS-REASON-KBN
                   MOVE "NEXT DATE NOT FOUND" TO WS-ERROR-TEXT
               END-IF
           END-IF.
      *
       8000-WRITE-ERROR.
           IF WS-CCERRF-ST = "00"
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
               MOVE WS-CUR-YYYY TO WS-EID-DATE(1:4)
               MOVE WS-CUR-MM TO WS-EID-DATE(5:2)
               MOVE WS-CUR-DD TO WS-EID-DATE(7:2)
               MOVE WS-CUR-HH TO WS-EID-TIME(1:2)
               MOVE WS-CUR-MI TO WS-EID-TIME(3:2)
               MOVE WS-CUR-SS TO WS-EID-TIME(5:2)
               MOVE WS-CUR-CC TO WS-EID-TIME(7:2)
               MOVE ZERO TO WS-EID-SUFFIX
               MOVE SPACES TO CCERRF-REC
               MOVE WS-ERROR-ID-WK TO ER-ERROR-ID
               MOVE LK-CALLER-PGM-ID TO ER-PGM-ID
               MOVE WS-BASE-DT TO ER-BASE-DT
               MOVE WS-BASE-DT TO WS-RECORD-KEY
               MOVE WS-RECORD-KEY TO ER-RECORD-KEY
               MOVE WS-REASON-KBN TO ER-ERROR-KBN
               MOVE WS-ERROR-TEXT TO ER-ERROR-TEXT
               PERFORM 8100-WRITE-ERROR-RETRY
           END-IF.
      *
       8100-WRITE-ERROR-RETRY.
           WRITE CCERRF-REC
               INVALID KEY
                   ADD 1 TO WS-EID-SUFFIX
                   MOVE WS-ERROR-ID-WK TO ER-ERROR-ID
                   WRITE CCERRF-REC
                       INVALID KEY
                           DISPLAY "CCERRF WRITE ERROR ST="
                                   WS-CCERRF-ST
                           MOVE 12 TO WS-RESULT-CD
                           MOVE "F3" TO WS-REASON-KBN
                   END-WRITE
           END-WRITE.
      *
       9000-FINAL.
           IF WS-CCCALF-ST NOT = SPACES
               IF WS-CCCALF-ST NOT = "42"
                   CLOSE CCCALF
                   IF WS-CCCALF-ST NOT = "00" AND
                      WS-CCCALF-ST NOT = "42"
                       DISPLAY "CCCALF CLOSE ERROR ST="
                               WS-CCCALF-ST
                       MOVE 12 TO WS-RESULT-CD
                       MOVE "F4" TO WS-REASON-KBN
                   END-IF
               END-IF
           END-IF
           IF WS-CCERRF-ST NOT = SPACES
               IF WS-CCERRF-ST NOT = "42"
                   CLOSE CCERRF
                   IF WS-CCERRF-ST NOT = "00" AND
                      WS-CCERRF-ST NOT = "42"
                       DISPLAY "CCERRF CLOSE ERROR ST="
                               WS-CCERRF-ST
                       MOVE 12 TO WS-RESULT-CD
                       MOVE "F5" TO WS-REASON-KBN
                   END-IF
               END-IF
           END-IF
           MOVE WS-RESULT-CD TO LK-RETURN-CD
           MOVE WS-REASON-KBN TO LK-REASON-KBN
           IF WS-RESULT-CD = ZERO
               MOVE 0 TO RETURN-CODE
           ELSE
               IF WS-RESULT-CD >= 12
                   MOVE 12 TO RETURN-CODE
               ELSE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
