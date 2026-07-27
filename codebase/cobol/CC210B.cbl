       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC210B.
       AUTHOR. MFG-KYOTSU-BATCH.
       DATE-WRITTEN. 2024-10-08.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCCALF
               ASSIGN TO "CCCALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CL-STATUS.
      *
           SELECT CCERRF
               ASSIGN TO "CCERRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ER-ERROR-ID
               FILE STATUS IS WS-ER-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  CCCALF.
           COPY CCCALFC.
      *
       FD  CCERRF.
           COPY CCERRC.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-CONST.
           05  WC-PGM-ID              PIC X(08) VALUE "CC210B".
           05  WC-HOLIDAY             PIC X(01) VALUE "Y".
           05  WC-BUSINESS            PIC X(01) VALUE "N".
           05  WC-ERR-DUP             PIC X(02) VALUE "01".
           05  WC-ERR-BLANK           PIC X(02) VALUE "02".
           05  WC-ERR-REVERSE         PIC X(02) VALUE "03".
           05  WC-ERR-MISMATCH        PIC X(02) VALUE "04".
           05  WC-ERR-CALL            PIC X(02) VALUE "05".
      *
       01  WS-FILE-STATUS.
           05  WS-CL-STATUS           PIC X(02) VALUE SPACES.
           05  WS-ER-STATUS           PIC X(02) VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW              PIC X(01) VALUE "N".
               88  END-OF-CCCALF                VALUE "Y".
           05  WS-HARD-ERR-SW         PIC X(01) VALUE "N".
               88  HARD-ERROR                   VALUE "Y".
           05  WS-VALID-REC-SW        PIC X(01) VALUE "N".
               88  VALID-RECORD                 VALUE "Y".
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT            PIC 9(09) VALUE ZERO.
           05  WS-OK-CNT              PIC 9(09) VALUE ZERO.
           05  WS-ERR-CNT             PIC 9(09) VALUE ZERO.
           05  WS-ERR-SEQ             PIC 9(09) VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-PREV-CAL-DT         PIC 9(08) VALUE ZERO.
           05  WS-FIRST-SW            PIC X(01) VALUE "Y".
               88  FIRST-RECORD                 VALUE "Y".
           05  WS-RECORD-KEY          PIC X(40) VALUE SPACES.
           05  WS-TEXT                PIC X(120) VALUE SPACES.
           05  WS-BASE-DT             PIC 9(08) VALUE ZERO.
      *
       01  CC190S-PARM.
           05  C9-CAL-DT              PIC 9(08) VALUE ZERO.
           05  C9-BUSINESS-KBN        PIC X(01) VALUE SPACE.
           05  C9-RETURN-CD           PIC 9(02) VALUE ZERO.
      *
       PROCEDURE DIVISION.
      *
       MAIN-RTN.
           PERFORM INIT-RTN
           IF NOT HARD-ERROR
               PERFORM READ-CCCALF-RTN
               PERFORM UNTIL END-OF-CCCALF OR HARD-ERROR
                   PERFORM CHECK-CALENDAR-RTN
                   PERFORM READ-CCCALF-RTN
               END-PERFORM
           END-IF
           PERFORM FINAL-RTN
           GOBACK.
      *
       INIT-RTN.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT CCCALF
           IF WS-CL-STATUS NOT = "00"
               MOVE "Y" TO WS-HARD-ERR-SW
               MOVE 12 TO RETURN-CODE
               DISPLAY "CCCALF OPEN ERROR ST=" WS-CL-STATUS
           END-IF
      *
           OPEN OUTPUT CCERRF
           IF WS-ER-STATUS NOT = "00"
               MOVE "Y" TO WS-HARD-ERR-SW
               MOVE 12 TO RETURN-CODE
               DISPLAY "CCERRF OPEN ERROR ST=" WS-ER-STATUS
           END-IF.
      *
       READ-CCCALF-RTN.
           READ CCCALF
               AT END
                   MOVE "Y" TO WS-EOF-SW
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
      *
           IF WS-CL-STATUS NOT = "00" AND WS-CL-STATUS NOT = "10"
               MOVE "Y" TO WS-HARD-ERR-SW
               MOVE 12 TO RETURN-CODE
               DISPLAY "CCCALF READ ERROR ST=" WS-CL-STATUS
           END-IF.
      *
       CHECK-CALENDAR-RTN.
           MOVE "Y" TO WS-VALID-REC-SW
           MOVE CL-CAL-DT TO WS-BASE-DT
           MOVE CL-CAL-DT TO WS-RECORD-KEY
      *
           IF FIRST-RECORD
               MOVE "N" TO WS-FIRST-SW
           ELSE
               IF CL-CAL-DT = WS-PREV-CAL-DT
                   MOVE "N" TO WS-VALID-REC-SW
                   MOVE "DUPLICATE DATE" TO WS-TEXT
                   PERFORM WRITE-ERROR-DUP-RTN
               END-IF
               IF CL-CAL-DT < WS-PREV-CAL-DT
                   MOVE "N" TO WS-VALID-REC-SW
                   MOVE "DATE ORDER ERROR" TO WS-TEXT
                   PERFORM WRITE-ERROR-REV-RTN
               END-IF
           END-IF
      *
           IF CL-HOLIDAY-FLAG NOT = WC-HOLIDAY
               AND CL-HOLIDAY-FLAG NOT = WC-BUSINESS
               MOVE "N" TO WS-VALID-REC-SW
               MOVE "HOLIDAY FLAG ERROR" TO WS-TEXT
               PERFORM WRITE-ERROR-BLK-RTN
           END-IF
      *
           IF VALID-RECORD
               PERFORM CALL-CC190S-RTN
           END-IF
      *
           IF VALID-RECORD
               ADD 1 TO WS-OK-CNT
           END-IF
      *
           MOVE CL-CAL-DT TO WS-PREV-CAL-DT.
      *
       CALL-CC190S-RTN.
           MOVE CL-CAL-DT TO C9-CAL-DT
           MOVE SPACE TO C9-BUSINESS-KBN
           MOVE ZERO TO C9-RETURN-CD
      *
           CALL "CC190S" USING CC190S-PARM
      *
           IF C9-RETURN-CD NOT = ZERO
               MOVE "N" TO WS-VALID-REC-SW
               MOVE "CC190S RETURN ERROR" TO WS-TEXT
               PERFORM WRITE-ERROR-CALL-RTN
           ELSE
               IF CL-HOLIDAY-FLAG = WC-BUSINESS
                   AND C9-BUSINESS-KBN NOT = WC-BUSINESS
                   MOVE "N" TO WS-VALID-REC-SW
                   MOVE "BUSINESS RESULT MISMATCH" TO WS-TEXT
                   PERFORM WRITE-ERROR-MIS-RTN
               END-IF
               IF CL-HOLIDAY-FLAG = WC-HOLIDAY
                   AND C9-BUSINESS-KBN NOT = WC-HOLIDAY
                   MOVE "N" TO WS-VALID-REC-SW
                   MOVE "HOLIDAY RESULT MISMATCH" TO WS-TEXT
                   PERFORM WRITE-ERROR-MIS-RTN
               END-IF
           END-IF.
      *
       WRITE-ERROR-DUP-RTN.
           MOVE WC-ERR-DUP TO ER-ERROR-KBN
           PERFORM WRITE-ERROR-COMMON-RTN.
      *
       WRITE-ERROR-BLK-RTN.
           MOVE WC-ERR-BLANK TO ER-ERROR-KBN
           PERFORM WRITE-ERROR-COMMON-RTN.
      *
       WRITE-ERROR-REV-RTN.
           MOVE WC-ERR-REVERSE TO ER-ERROR-KBN
           PERFORM WRITE-ERROR-COMMON-RTN.
      *
       WRITE-ERROR-MIS-RTN.
           MOVE WC-ERR-MISMATCH TO ER-ERROR-KBN
           PERFORM WRITE-ERROR-COMMON-RTN.
      *
       WRITE-ERROR-CALL-RTN.
           MOVE WC-ERR-CALL TO ER-ERROR-KBN
           PERFORM WRITE-ERROR-COMMON-RTN.
      *
       WRITE-ERROR-COMMON-RTN.
           ADD 1 TO WS-ERR-CNT
           ADD 1 TO WS-ERR-SEQ
      *
           MOVE WS-ERR-SEQ TO ER-ERROR-ID
           MOVE WC-PGM-ID TO ER-PGM-ID
           MOVE WS-BASE-DT TO ER-BASE-DT
           MOVE WS-RECORD-KEY TO ER-RECORD-KEY
           MOVE WS-TEXT TO ER-ERROR-TEXT
      *
           WRITE CCERRF-REC
           IF WS-ER-STATUS NOT = "00"
               MOVE "Y" TO WS-HARD-ERR-SW
               MOVE 12 TO RETURN-CODE
               DISPLAY "CCERRF WRITE ERROR ST=" WS-ER-STATUS
           END-IF.
      *
       FINAL-RTN.
           IF WS-CL-STATUS NOT = SPACES
               CLOSE CCCALF
               IF WS-CL-STATUS NOT = "00"
                   MOVE "Y" TO WS-HARD-ERR-SW
                   MOVE 12 TO RETURN-CODE
                   DISPLAY "CCCALF CLOSE ERROR ST=" WS-CL-STATUS
               END-IF
           END-IF
      *
           IF WS-ER-STATUS NOT = SPACES
               CLOSE CCERRF
               IF WS-ER-STATUS NOT = "00"
                   MOVE "Y" TO WS-HARD-ERR-SW
                   MOVE 12 TO RETURN-CODE
                   DISPLAY "CCERRF CLOSE ERROR ST=" WS-ER-STATUS
               END-IF
           END-IF
      *
           IF NOT HARD-ERROR
               IF WS-ERR-CNT = ZERO
                   MOVE 0 TO RETURN-CODE
               ELSE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
      *
           DISPLAY "CC210B READ=" WS-READ-CNT
                   " OK=" WS-OK-CNT
                   " ERR=" WS-ERR-CNT.
