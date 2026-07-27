       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT210B.
       AUTHOR. BATCH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCINSF ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS IN-INS-ID
               FILE STATUS  IS WS-ST-CCINSF.

           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS PS-ORG-CD
               FILE STATUS  IS WS-ST-CCPOSF.

           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-ST-CCCALF.

           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-ST-CCFCTF.

           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-ST-CCERRF.

       DATA DIVISION.
       FILE SECTION.

       FD  CCINSF.
           COPY CCINSC.

       FD  CCPOSF.
           COPY CCPOSC.

       FD  CCCALF.
           COPY CCCALFC.

       FD  CCFCTF.
           COPY CCFCTFC.

       FD  CCERRF.
           COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                  PIC X(08) VALUE "CT210B".
       01  WS-ST-CCINSF               PIC X(02) VALUE SPACE.
       01  WS-ST-CCPOSF               PIC X(02) VALUE SPACE.
       01  WS-ST-CCCALF               PIC X(02) VALUE SPACE.
       01  WS-ST-CCFCTF               PIC X(02) VALUE SPACE.
       01  WS-ST-CCERRF               PIC X(02) VALUE SPACE.

       01  WS-FLAGS.
           05 WS-EOF-CCINSF           PIC X VALUE "N".
           05 WS-EOF-CCCALF           PIC X VALUE "N".
           05 WS-HARD-ERROR           PIC X VALUE "N".
           05 WS-VALID-INS            PIC X VALUE "N".
           05 WS-CAL-FOUND            PIC X VALUE "N".
           05 WS-CAL-BUSINESS         PIC X VALUE "N".
           05 WS-POS-FOUND            PIC X VALUE "N".

       01  WS-COUNTERS.
           05 WS-IN-CNT               PIC 9(09) VALUE ZERO.
           05 WS-OK-CNT               PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT              PIC 9(09) VALUE ZERO.
           05 WS-CAL-CNT              PIC 9(05) VALUE ZERO.
           05 WS-CAL-IDX              PIC 9(05) VALUE ZERO.
           05 WS-ERROR-SEQ            PIC 9(09) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-NET-AVAILABLE-AMT    PIC S9(13)V99 VALUE ZERO.

       01  WS-CALENDAR-TABLE.
           05 WS-CAL-ENTRY OCCURS 20000 TIMES
              INDEXED BY IX-CAL.
              10 WS-TBL-CAL-DT        PIC 9(08).
              10 WS-TBL-HOLIDAY-FLAG  PIC X(01).

       01  WS-ERROR-WORK.
           05 WS-ERROR-ID-WK          PIC X(20) VALUE SPACE.
           05 WS-ERROR-KBN-WK         PIC X(03) VALUE SPACE.
           05 WS-ERROR-TEXT-WK        PIC X(80) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE

           PERFORM 1000-OPEN-FILES

           IF WS-HARD-ERROR = "N"
               PERFORM 2000-LOAD-CALENDAR
           END-IF

           IF WS-HARD-ERROR = "N"
               PERFORM 3000-PROCESS-INPUT
           END-IF

           PERFORM 9000-CLOSE-FILES

           IF WS-HARD-ERROR = "Y"
               MOVE 8 TO RETURN-CODE
           ELSE
               DISPLAY "CT210B OK IN=" WS-IN-CNT
               DISPLAY "CT210B OK OUT=" WS-OK-CNT
               DISPLAY "CT210B OK ERR=" WS-ERR-CNT
               MOVE 0 TO RETURN-CODE
           END-IF

           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT  CCINSF
                INPUT  CCPOSF
                INPUT  CCCALF
                OUTPUT CCFCTF
                OUTPUT CCERRF

           IF WS-ST-CCINSF NOT = "00"
               DISPLAY "CCINSF OPEN ERROR ST=" WS-ST-CCINSF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF

           IF WS-ST-CCPOSF NOT = "00"
               DISPLAY "CCPOSF OPEN ERROR ST=" WS-ST-CCPOSF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF

           IF WS-ST-CCCALF NOT = "00"
               DISPLAY "CCCALF OPEN ERROR ST=" WS-ST-CCCALF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF

           IF WS-ST-CCFCTF NOT = "00"
               DISPLAY "CCFCTF OPEN ERROR ST=" WS-ST-CCFCTF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF

           IF WS-ST-CCERRF NOT = "00"
               DISPLAY "CCERRF OPEN ERROR ST=" WS-ST-CCERRF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       2000-LOAD-CALENDAR.
           PERFORM UNTIL WS-EOF-CCCALF = "Y"
               READ CCCALF
                   AT END
                       MOVE "Y" TO WS-EOF-CCCALF
                   NOT AT END
                       IF WS-CAL-CNT < 20000
                           ADD 1 TO WS-CAL-CNT
                           MOVE CL-CAL-DT
                             TO WS-TBL-CAL-DT(WS-CAL-CNT)
                           MOVE CL-HOLIDAY-FLAG
                             TO WS-TBL-HOLIDAY-FLAG(WS-CAL-CNT)
                       ELSE
                           DISPLAY "CCCALF TABLE OVERFLOW"
                           MOVE "Y" TO WS-HARD-ERROR
                           MOVE "Y" TO WS-EOF-CCCALF
                       END-IF
               END-READ

               IF WS-ST-CCCALF NOT = "00" AND
                  WS-ST-CCCALF NOT = "10"
                   DISPLAY "CCCALF READ ERROR ST=" WS-ST-CCCALF
                   MOVE "Y" TO WS-HARD-ERROR
                   MOVE "Y" TO WS-EOF-CCCALF
               END-IF
           END-PERFORM.

       3000-PROCESS-INPUT.
           PERFORM UNTIL WS-EOF-CCINSF = "Y"
                         OR WS-HARD-ERROR = "Y"
               READ CCINSF
                   AT END
                       MOVE "Y" TO WS-EOF-CCINSF
                   NOT AT END
                       ADD 1 TO WS-IN-CNT
                       PERFORM 3100-JUDGE-INSTRUCTION
               END-READ

               IF WS-ST-CCINSF NOT = "00" AND
                  WS-ST-CCINSF NOT = "10"
                   DISPLAY "CCINSF READ ERROR ST=" WS-ST-CCINSF
                   MOVE "Y" TO WS-HARD-ERROR
               END-IF
           END-PERFORM.

       3100-JUDGE-INSTRUCTION.
           MOVE "Y" TO WS-VALID-INS
           MOVE "N" TO WS-CAL-FOUND
           MOVE "N" TO WS-CAL-BUSINESS
           MOVE "N" TO WS-POS-FOUND
           MOVE ZERO TO WS-NET-AVAILABLE-AMT

           PERFORM 3200-CHECK-CALENDAR

           IF WS-CAL-FOUND = "N"
               MOVE "CAL" TO ER-ERROR-KBN
               MOVE "CALENDAR NOT FOUND" TO WS-ERROR-TEXT-WK
               PERFORM 8000-WRITE-ERROR
               MOVE "N" TO WS-VALID-INS
           ELSE
               IF WS-CAL-BUSINESS = "N"
                   MOVE "CAL" TO ER-ERROR-KBN
                   MOVE "RECEIVE DATE HOLIDAY" TO WS-ERROR-TEXT-WK
                   PERFORM 8000-WRITE-ERROR
                   MOVE "N" TO WS-VALID-INS
               END-IF
           END-IF

           IF IN-INSTR-STATUS-KBN NOT = "01"
               MOVE "STA" TO ER-ERROR-KBN
               MOVE "INSTRUCTION NOT APPROVED" TO WS-ERROR-TEXT-WK
               PERFORM 8000-WRITE-ERROR
               MOVE "N" TO WS-VALID-INS
           END-IF

           MOVE IN-ORG-CD TO PS-ORG-CD

           READ CCPOSF KEY IS PS-ORG-CD
               INVALID KEY
                   MOVE "N" TO WS-POS-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-POS-FOUND
           END-READ

           IF WS-ST-CCPOSF NOT = "00" AND
              WS-ST-CCPOSF NOT = "23"
               DISPLAY "CCPOSF READ ERROR ST=" WS-ST-CCPOSF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF

           IF WS-HARD-ERROR = "N"
               IF WS-POS-FOUND = "N"
                   MOVE "ORG" TO ER-ERROR-KBN
                   MOVE "POSITION NOT FOUND" TO WS-ERROR-TEXT-WK
                   PERFORM 8000-WRITE-ERROR
                   MOVE "N" TO WS-VALID-INS
               ELSE
                   PERFORM 3300-CHECK-POSITION
               END-IF
           END-IF

           IF WS-HARD-ERROR = "N" AND
              WS-VALID-INS = "Y"
               PERFORM 4000-WRITE-TRIGGER
           END-IF.

       3200-CHECK-CALENDAR.
           PERFORM VARYING WS-CAL-IDX FROM 1 BY 1
             UNTIL WS-CAL-IDX > WS-CAL-CNT
                OR WS-CAL-FOUND = "Y"
               IF WS-TBL-CAL-DT(WS-CAL-IDX) = IN-RECV-DT
                   MOVE "Y" TO WS-CAL-FOUND
                   IF WS-TBL-HOLIDAY-FLAG(WS-CAL-IDX) = "N"
                       MOVE "Y" TO WS-CAL-BUSINESS
                   END-IF
               END-IF
           END-PERFORM.

       3300-CHECK-POSITION.
           IF PS-ORG-CD NOT = IN-ORG-CD
               MOVE "ORG" TO ER-ERROR-KBN
               MOVE "ORGANIZATION UNMATCHED" TO WS-ERROR-TEXT-WK
               PERFORM 8000-WRITE-ERROR
               MOVE "N" TO WS-VALID-INS
           END-IF

           IF PS-BASE-DT NOT = IN-RECV-DT
               MOVE "POS" TO ER-ERROR-KBN
               MOVE "POSITION BASE DATE ERROR" TO WS-ERROR-TEXT-WK
               PERFORM 8000-WRITE-ERROR
               MOVE "N" TO WS-VALID-INS
           END-IF

           IF PS-POSITION-STATUS-KBN NOT = "01"
               MOVE "POS" TO ER-ERROR-KBN
               MOVE "POSITION STATUS ERROR" TO WS-ERROR-TEXT-WK
               PERFORM 8000-WRITE-ERROR
               MOVE "N" TO WS-VALID-INS
           END-IF

           COMPUTE WS-NET-AVAILABLE-AMT =
                   PS-AVAILABLE-AMT - PS-RESERVED-AMT

           IF IN-INSTR-AMT <= ZERO
               MOVE "AMT" TO ER-ERROR-KBN
               MOVE "INSTRUCTION AMOUNT ERROR" TO WS-ERROR-TEXT-WK
               PERFORM 8000-WRITE-ERROR
               MOVE "N" TO WS-VALID-INS
           ELSE
               IF IN-INSTR-AMT > WS-NET-AVAILABLE-AMT
                   MOVE "AMT" TO ER-ERROR-KBN
                   MOVE "INSTRUCTION AMOUNT OVER" TO WS-ERROR-TEXT-WK
                   PERFORM 8000-WRITE-ERROR
                   MOVE "N" TO WS-VALID-INS
               END-IF
           END-IF.

       4000-WRITE-TRIGGER.
           INITIALIZE CCFCTF-REC
           MOVE IN-FCT-ID     TO FC-FCT-ID
           MOVE IN-RECV-DT    TO FC-TRIGGER-DT
           MOVE IN-INSTR-AMT  TO FC-CONC-AMT
           MOVE "01"          TO FC-FCT-STATUS-KBN

           WRITE CCFCTF-REC

           IF WS-ST-CCFCTF = "00"
               ADD 1 TO WS-OK-CNT
           ELSE
               DISPLAY "CCFCTF WRITE ERROR ST=" WS-ST-CCFCTF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       8000-WRITE-ERROR.
           MOVE ER-ERROR-KBN TO WS-ERROR-KBN-WK

           INITIALIZE CCERRF-REC
           ADD 1 TO WS-ERROR-SEQ
           ADD 1 TO WS-ERR-CNT
           INITIALIZE WS-ERROR-ID-WK

           MOVE WS-PGM-ID        TO WS-ERROR-ID-WK(1:8)
           MOVE "-"              TO WS-ERROR-ID-WK(9:1)
           MOVE WS-ERROR-KBN-WK  TO WS-ERROR-ID-WK(10:3)
           MOVE WS-ERROR-SEQ     TO WS-ERROR-ID-WK(12:9)

           MOVE WS-ERROR-ID-WK   TO ER-ERROR-ID
           MOVE WS-ERROR-KBN-WK  TO ER-ERROR-KBN
           MOVE WS-PGM-ID        TO ER-PGM-ID
           MOVE IN-RECV-DT       TO ER-BASE-DT
           MOVE IN-INS-ID        TO ER-RECORD-KEY
           MOVE WS-ERROR-TEXT-WK TO ER-ERROR-TEXT

           WRITE CCERRF-REC

           IF WS-ST-CCERRF NOT = "00"
               DISPLAY "CCERRF WRITE ERROR ST=" WS-ST-CCERRF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CCINSF
                 CCPOSF
                 CCCALF
                 CCFCTF
                 CCERRF.
