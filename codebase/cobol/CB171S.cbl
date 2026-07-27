       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB171S.
       AUTHOR. TRUST-BATCH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCANF ASSIGN TO "CDCANF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CAN-CANCEL-ID
               FILE STATUS IS WS-CAN-STATUS.
           SELECT CDEXCPF2 ASSIGN TO "CDEXCPF2"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EXP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CDCANF.
       COPY CDCANC.
       FD  CDEXCPF2.
       COPY CDEXCPF2C.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CAN-STATUS        PIC XX VALUE SPACES.
           05  WS-EXP-STATUS        PIC XX VALUE SPACES.

       01  WS-CONTROL.
           05  WS-EOF-SW            PIC X VALUE "N".
               88  WS-EOF                 VALUE "Y".
           05  WS-HARD-ERROR-SW     PIC X VALUE "N".
               88  WS-HARD-ERROR          VALUE "Y".
           05  WS-EXCEPTION-SW      PIC X VALUE "N".
               88  WS-HAS-EXCEPTION       VALUE "Y".
           05  WS-EXCEPTION-SEQ     PIC 9(7) VALUE ZERO.
           05  WS-READ-COUNT        PIC 9(9) VALUE ZERO.
           05  WS-EXCEPTION-COUNT   PIC 9(9) VALUE ZERO.

       01  WS-EDIT-AREA.
           05  WS-REQUEST-USER      PIC X(32) VALUE SPACES.
           05  WS-APPROVAL-ID       PIC X(10) VALUE SPACES.
           05  WS-REASON-CD         PIC X(2)  VALUE SPACES.
           05  WS-PAY-ID            PIC X(32) VALUE SPACES.
           05  WS-PAY-CLASS         PIC X     VALUE SPACE.
           05  WS-EXCEPTION-CD      PIC X(4)  VALUE SPACES.
           05  WS-MOTO-NYUKIN-AMT   PIC 9(13)V99 VALUE ZERO.

       01  WS-DATE-AREA.
           05  WS-CURRENT-DATE      PIC 9(8) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NOT WS-HARD-ERROR
               PERFORM 2000-READ-CAN
               PERFORM UNTIL WS-EOF OR WS-HARD-ERROR
                   ADD 1 TO WS-READ-COUNT
                   PERFORM 3000-VALIDATE-CANCEL
                   PERFORM 2000-READ-CAN
               END-PERFORM
           END-IF
           PERFORM 9000-CLOSE
           IF WS-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CB171S NORMAL END IN="
                       WS-READ-COUNT
                       " EX="
                       WS-EXCEPTION-COUNT
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN INPUT CDCANF
           IF WS-CAN-STATUS NOT = "00"
               DISPLAY "CDCANF OPEN ERROR ST="
                       WS-CAN-STATUS
               SET WS-HARD-ERROR TO TRUE
           END-IF
           IF NOT WS-HARD-ERROR
               OPEN OUTPUT CDEXCPF2
               IF WS-EXP-STATUS NOT = "00"
                   DISPLAY "CDEXCPF2 OPEN ERROR ST="
                           WS-EXP-STATUS
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF.

       2000-READ-CAN.
           READ CDCANF NEXT RECORD
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   IF WS-CAN-STATUS NOT = "00"
                       DISPLAY "CDCANF READ ERROR ST="
                               WS-CAN-STATUS
                       SET WS-HARD-ERROR TO TRUE
                   END-IF
           END-READ.

       3000-VALIDATE-CANCEL.
           MOVE "N" TO WS-EXCEPTION-SW
           MOVE SPACES TO WS-EXCEPTION-CD
           MOVE CAN-CANCEL-REASON TO WS-REASON-CD
           MOVE CAN-REQUEST-USER  TO WS-REQUEST-USER
           MOVE CAN-PAY-ID        TO WS-PAY-ID
           MOVE WS-PAY-ID(1:1)    TO WS-PAY-CLASS
           MOVE WS-REQUEST-USER(9:10) TO WS-APPROVAL-ID
           PERFORM 3100-CALC-MOTO-NYUKIN
           EVALUATE TRUE
               WHEN WS-REASON-CD = "01"
               WHEN WS-REASON-CD = "02"
               WHEN WS-REASON-CD = "03"
               WHEN WS-REASON-CD = "04"
               WHEN WS-REASON-CD = "05"
               WHEN WS-REASON-CD = "91"
               WHEN WS-REASON-CD = "92"
                   CONTINUE
               WHEN OTHER
                   MOVE "R001" TO WS-EXCEPTION-CD
                   PERFORM 8000-WRITE-EXCEPTION
           END-EVALUATE
           IF NOT WS-HAS-EXCEPTION
               PERFORM 3200-CHECK-AUTHORITY
           END-IF
           IF NOT WS-HAS-EXCEPTION
               IF CAN-CANCEL-AMT > WS-MOTO-NYUKIN-AMT
                   MOVE "A001" TO WS-EXCEPTION-CD
                   PERFORM 8000-WRITE-EXCEPTION
               END-IF
           END-IF
           IF NOT WS-HAS-EXCEPTION
               IF (WS-REASON-CD = "91" OR WS-REASON-CD = "92")
                  AND WS-APPROVAL-ID = SPACES
                   MOVE "K001" TO WS-EXCEPTION-CD
                   PERFORM 8000-WRITE-EXCEPTION
               END-IF
           END-IF.

       3100-CALC-MOTO-NYUKIN.
           EVALUATE WS-PAY-CLASS
               WHEN "A"
                   MOVE 1000000.00 TO WS-MOTO-NYUKIN-AMT
               WHEN "B"
                   MOVE 5000000.00 TO WS-MOTO-NYUKIN-AMT
               WHEN "C"
                   MOVE 10000000.00 TO WS-MOTO-NYUKIN-AMT
               WHEN "D"
                   MOVE 50000000.00 TO WS-MOTO-NYUKIN-AMT
               WHEN OTHER
                   MOVE 300000.00 TO WS-MOTO-NYUKIN-AMT
           END-EVALUATE.

       3200-CHECK-AUTHORITY.
           EVALUATE TRUE
               WHEN CAN-CANCEL-AMT <= 100000.00
                   CONTINUE
               WHEN CAN-CANCEL-AMT <= 1000000.00
                   IF WS-REQUEST-USER(1:2) NOT = "SV"
                      AND WS-REQUEST-USER(1:2) NOT = "MG"
                       MOVE "U001" TO WS-EXCEPTION-CD
                       PERFORM 8000-WRITE-EXCEPTION
                   END-IF
               WHEN OTHER
                   IF WS-REQUEST-USER(1:2) NOT = "MG"
                       MOVE "U002" TO WS-EXCEPTION-CD
                       PERFORM 8000-WRITE-EXCEPTION
                   END-IF
           END-EVALUATE.

       8000-WRITE-EXCEPTION.
           SET WS-HAS-EXCEPTION TO TRUE
           ADD 1 TO WS-EXCEPTION-SEQ
           ADD 1 TO WS-EXCEPTION-COUNT
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           INITIALIZE CDEXCPF2-REC
           MOVE WS-EXCEPTION-SEQ    TO EXP-EXCEPTION-ID
           MOVE CAN-PAY-ID          TO EXP-PAY-ID
           MOVE CAN-CARD-NO         TO EXP-CARD-NO
           MOVE WS-EXCEPTION-CD     TO EXP-EXCEPTION-CD
           MOVE CAN-CANCEL-AMT      TO EXP-EXCEPTION-AMT
           MOVE "CB171S"            TO EXP-DETECTED-PROGRAM
           MOVE WS-CURRENT-DATE     TO EXP-DETECTED-DT
           WRITE CDEXCPF2-REC
           IF WS-EXP-STATUS NOT = "00"
               DISPLAY "CDEXCPF2 WRITE ERROR ST="
                       WS-EXP-STATUS
               SET WS-HARD-ERROR TO TRUE
           END-IF.

       9000-CLOSE.
           IF WS-CAN-STATUS = "00" OR WS-CAN-STATUS = "10"
               CLOSE CDCANF
               IF WS-CAN-STATUS NOT = "00"
                   DISPLAY "CDCANF CLOSE ERROR ST="
                           WS-CAN-STATUS
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF
           IF WS-EXP-STATUS = "00"
               CLOSE CDEXCPF2
               IF WS-EXP-STATUS NOT = "00"
                   DISPLAY "CDEXCPF2 CLOSE ERROR ST="
                           WS-EXP-STATUS
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF.
