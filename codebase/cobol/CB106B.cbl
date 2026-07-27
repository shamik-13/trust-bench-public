       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB106B.
       AUTHOR. 内田 亮.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDPAYF2
               ASSIGN TO "CDPAYF2"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDPAYF2.

           SELECT CDSTMTF2
               ASSIGN TO "CDSTMTF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ST-CARD-NO
               FILE STATUS IS FS-CDSTMTF2.

           SELECT CDRSLDF
               ASSIGN TO "CDRSLDF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDRSLDF.

           SELECT CDRBALF
               ASSIGN TO "CDRBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDRBALF.

       DATA DIVISION.
       FILE SECTION.

       FD  CDPAYF2.
       COPY "CDPAYF2C".

       FD  CDSTMTF2.
       COPY "CDSTMTF2C".

       FD  CDRSLDF.
       COPY "CDRSLDFC".

       FD  CDRBALF.
       COPY "CDRBALFC".

       WORKING-STORAGE SECTION.
       01  FS-CDPAYF2                  PIC XX VALUE SPACES.
       01  FS-CDSTMTF2                 PIC XX VALUE SPACES.
       01  FS-CDRSLDF                  PIC XX VALUE SPACES.
       01  FS-CDRBALF                  PIC XX VALUE SPACES.

       01  SW-AREA.
           05  SW-PAY-EOF              PIC X VALUE "N".
               88  PAY-EOF                   VALUE "Y".
           05  SW-RSLD-EOF             PIC X VALUE "N".
               88  RSLD-EOF                  VALUE "Y".
           05  SW-HARD-ERR             PIC X VALUE "N".
               88  HARD-ERR                  VALUE "Y".
           05  SW-RSLD-FOUND           PIC X VALUE "N".
               88  RSLD-FOUND                VALUE "Y".

       01  CTL-AREA.
           05  CT-PAY-READ             PIC 9(9) VALUE ZERO.
           05  CT-PAY-OK               PIC 9(9) VALUE ZERO.
           05  CT-PAY-SKIP             PIC 9(9) VALUE ZERO.
           05  CT-PAY-OVER             PIC 9(9) VALUE ZERO.
           05  CT-PAY-SHORT            PIC 9(9) VALUE ZERO.
           05  CT-STMT-UPD             PIC 9(9) VALUE ZERO.
           05  CT-BAL-WRITE            PIC 9(9) VALUE ZERO.
           05  CT-RSLD-LOAD            PIC 9(5) VALUE ZERO.
           05  IX-RSLD                 PIC 9(5) VALUE ZERO.
           05  IX-RSLD-MAX             PIC 9(5) VALUE 30000.

       01  WK-AREA.
           05  WK-MONTH-RATE           PIC 9V9(4) VALUE 0.0125.
           05  WK-CALC-FEE             PIC 9(11) VALUE ZERO.
           05  WK-APPLY-AMT            PIC S9(11) VALUE ZERO.
           05  WK-FEE-APPLY            PIC S9(11) VALUE ZERO.
           05  WK-PRIN-APPLY           PIC S9(11) VALUE ZERO.
           05  WK-REMAIN-AMT           PIC S9(11) VALUE ZERO.
           05  WK-NEW-BILL             PIC S9(11) VALUE ZERO.
           05  WK-VALID-PAY            PIC X VALUE SPACE.
           05  WK-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.

       01  RSLD-TABLE.
           05  TB-RSLD OCCURS 30000 TIMES.
               10  TB-RS-CARD-NO       PIC X(16).
               10  TB-RS-CYCLE-DT      PIC 9(8).
               10  TB-RS-PRIN-AMT      PIC 9(11).
               10  TB-RS-FEE-AMT       PIC 9(11).
               10  TB-RS-PAY-AMT       PIC 9(11).
               10  TB-RS-SLIDE-TIER    PIC X(02).
               10  TB-RS-RSLD-STATUS   PIC X(01).
               10  TB-RS-PROGRAM-ID    PIC X(08).

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF NOT HARD-ERR
               PERFORM LOAD-RSLD-RTN
           END-IF
           IF NOT HARD-ERR
               PERFORM PROCESS-PAY-RTN UNTIL PAY-EOF OR HARD-ERR
           END-IF
           PERFORM TERM-RTN
           GOBACK.

       INIT-RTN.
           OPEN INPUT CDPAYF2
           IF FS-CDPAYF2 NOT = "00"
               DISPLAY "CDPAYF2 OPEN ERROR ST=" FS-CDPAYF2
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN I-O CDSTMTF2
           IF FS-CDSTMTF2 NOT = "00"
               DISPLAY "CDSTMTF2 OPEN ERROR ST=" FS-CDSTMTF2
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT CDRSLDF
           IF FS-CDRSLDF NOT = "00"
               DISPLAY "CDRSLDF OPEN ERROR ST=" FS-CDRSLDF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT CDRBALF
           IF FS-CDRBALF NOT = "00"
               DISPLAY "CDRBALF OPEN ERROR ST=" FS-CDRBALF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF.

       LOAD-RSLD-RTN.
           PERFORM UNTIL RSLD-EOF OR HARD-ERR
               READ CDRSLDF
                   AT END
                       MOVE "Y" TO SW-RSLD-EOF
                   NOT AT END
                       IF CT-RSLD-LOAD >= IX-RSLD-MAX
                           DISPLAY "CDRSLDF TABLE OVERFLOW"
                           MOVE "Y" TO SW-HARD-ERR
                           MOVE 8 TO RETURN-CODE
                       ELSE
                           ADD 1 TO CT-RSLD-LOAD
                           MOVE RS-CARD-NO
                               TO TB-RS-CARD-NO(CT-RSLD-LOAD)
                           MOVE RS-CYCLE-DT
                               TO TB-RS-CYCLE-DT(CT-RSLD-LOAD)
                           MOVE RS-PRIN-AMT
                               TO TB-RS-PRIN-AMT(CT-RSLD-LOAD)
                           MOVE RS-FEE-AMT
                               TO TB-RS-FEE-AMT(CT-RSLD-LOAD)
                           MOVE RS-PAY-AMT
                               TO TB-RS-PAY-AMT(CT-RSLD-LOAD)
                           MOVE RS-SLIDE-TIER
                               TO TB-RS-SLIDE-TIER(CT-RSLD-LOAD)
                           MOVE RS-RSLD-STATUS
                               TO TB-RS-RSLD-STATUS(CT-RSLD-LOAD)
                           MOVE RS-PROGRAM-ID
                               TO TB-RS-PROGRAM-ID(CT-RSLD-LOAD)
                       END-IF
               END-READ

               IF FS-CDRSLDF NOT = "00"
                  AND FS-CDRSLDF NOT = "10"
                   DISPLAY "CDRSLDF READ ERROR ST=" FS-CDRSLDF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-PERFORM.

       PROCESS-PAY-RTN.
           READ CDPAYF2
               AT END
                   MOVE "Y" TO SW-PAY-EOF
               NOT AT END
                   ADD 1 TO CT-PAY-READ
                   PERFORM VALIDATE-PAY-RTN
                   IF WK-VALID-PAY = "Y"
                       PERFORM MATCH-STMT-RTN
                   ELSE
                       ADD 1 TO CT-PAY-SKIP
                   END-IF
           END-READ

           IF FS-CDPAYF2 NOT = "00"
              AND FS-CDPAYF2 NOT = "10"
               DISPLAY "CDPAYF2 READ ERROR ST=" FS-CDPAYF2
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           END-IF.

       VALIDATE-PAY-RTN.
           MOVE "Y" TO WK-VALID-PAY

           IF PY-PAYMENT-ID = SPACES
               DISPLAY "PAYMENT ID MISSING"
               MOVE "N" TO WK-VALID-PAY
           END-IF

           IF PY-CARD-NO = SPACES
               DISPLAY "CARD NO MISSING PAYMENT=" PY-PAYMENT-ID
               MOVE "N" TO WK-VALID-PAY
           END-IF

           IF PY-PAYMENT-DT = ZERO
               DISPLAY "PAYMENT DATE INVALID PAYMENT=" PY-PAYMENT-ID
               MOVE "N" TO WK-VALID-PAY
           END-IF

           IF PY-PAYMENT-AMT <= ZERO
               DISPLAY "PAYMENT AMOUNT INVALID PAYMENT=" PY-PAYMENT-ID
               MOVE "N" TO WK-VALID-PAY
           END-IF

           IF PY-PAYMENT-CHANNEL NOT = "ATM"
              AND PY-PAYMENT-CHANNEL NOT = "BNK"
              AND PY-PAYMENT-CHANNEL NOT = "CNV"
              AND PY-PAYMENT-CHANNEL NOT = "WEB"
               DISPLAY "PAYMENT CHANNEL INVALID PAYMENT="
                   PY-PAYMENT-ID
               MOVE "N" TO WK-VALID-PAY
           END-IF.

       MATCH-STMT-RTN.
           MOVE PY-CARD-NO TO ST-CARD-NO

           READ CDSTMTF2
               INVALID KEY
                   DISPLAY "STATEMENT NOT FOUND PAYMENT="
                       PY-PAYMENT-ID
                   ADD 1 TO CT-PAY-SKIP
                   EXIT PARAGRAPH
           END-READ

           IF FS-CDSTMTF2 NOT = "00"
               DISPLAY "CDSTMTF2 READ ERROR ST=" FS-CDSTMTF2
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           IF ST-STMT-STATUS NOT = "C"
               DISPLAY "STATEMENT STATUS SKIP PAYMENT="
                   PY-PAYMENT-ID
               ADD 1 TO CT-PAY-SKIP
               EXIT PARAGRAPH
           END-IF

           PERFORM FIND-RSLD-RTN

           IF NOT RSLD-FOUND
               DISPLAY "REVOLVING DETAIL NOT FOUND PAYMENT="
                   PY-PAYMENT-ID
               ADD 1 TO CT-PAY-SKIP
               EXIT PARAGRAPH
           END-IF

           IF TB-RS-RSLD-STATUS(IX-RSLD) NOT = "C"
               DISPLAY "REVOLVING STATUS SKIP PAYMENT="
                   PY-PAYMENT-ID
               ADD 1 TO CT-PAY-SKIP
               EXIT PARAGRAPH
           END-IF

           IF TB-RS-SLIDE-TIER(IX-RSLD) NOT = "T1"
              AND TB-RS-SLIDE-TIER(IX-RSLD) NOT = "T2"
              AND TB-RS-SLIDE-TIER(IX-RSLD) NOT = "T3"
              AND TB-RS-SLIDE-TIER(IX-RSLD) NOT = "T4"
               DISPLAY "SLIDE TIER INVALID PAYMENT="
                   PY-PAYMENT-ID
               ADD 1 TO CT-PAY-SKIP
               EXIT PARAGRAPH
           END-IF

           PERFORM APPLY-PAY-RTN.

       FIND-RSLD-RTN.
           MOVE "N" TO SW-RSLD-FOUND
           MOVE 1 TO IX-RSLD

           PERFORM UNTIL IX-RSLD > CT-RSLD-LOAD OR RSLD-FOUND
               IF TB-RS-CARD-NO(IX-RSLD) = ST-CARD-NO
                  AND TB-RS-CYCLE-DT(IX-RSLD) = ST-CYCLE-DT
                   MOVE "Y" TO SW-RSLD-FOUND
               ELSE
                   ADD 1 TO IX-RSLD
               END-IF
           END-PERFORM.

       APPLY-PAY-RTN.
           COMPUTE WK-CALC-FEE =
               FUNCTION INTEGER
                   (TB-RS-PRIN-AMT(IX-RSLD) * WK-MONTH-RATE)

           IF WK-CALC-FEE NOT = TB-RS-FEE-AMT(IX-RSLD)
               DISPLAY "FEE MISMATCH PAYMENT=" PY-PAYMENT-ID
               ADD 1 TO CT-PAY-SKIP
               EXIT PARAGRAPH
           END-IF

           MOVE PY-PAYMENT-AMT TO WK-APPLY-AMT

           IF WK-APPLY-AMT > ST-BILL-AMT
               ADD 1 TO CT-PAY-OVER
               MOVE ST-BILL-AMT TO PY-APPLIED-AMT
               COMPUTE PY-UNAPPLIED-AMT =
                   WK-APPLY-AMT - ST-BILL-AMT
               MOVE ST-BILL-AMT TO WK-APPLY-AMT
           ELSE
               MOVE WK-APPLY-AMT TO PY-APPLIED-AMT
               MOVE ZERO TO PY-UNAPPLIED-AMT
           END-IF

           IF WK-APPLY-AMT < ST-BILL-AMT
               ADD 1 TO CT-PAY-SHORT
           END-IF

           IF WK-APPLY-AMT >= TB-RS-FEE-AMT(IX-RSLD)
               MOVE TB-RS-FEE-AMT(IX-RSLD) TO WK-FEE-APPLY
           ELSE
               MOVE WK-APPLY-AMT TO WK-FEE-APPLY
           END-IF

           COMPUTE WK-PRIN-APPLY = WK-APPLY-AMT - WK-FEE-APPLY
           COMPUTE WK-REMAIN-AMT =
               TB-RS-PRIN-AMT(IX-RSLD) - WK-PRIN-APPLY

           IF WK-REMAIN-AMT < ZERO
               MOVE ZERO TO WK-REMAIN-AMT
           END-IF

           COMPUTE WK-NEW-BILL = ST-BILL-AMT - WK-APPLY-AMT

           IF WK-NEW-BILL < ZERO
               MOVE ZERO TO WK-NEW-BILL
           END-IF

           MOVE WK-NEW-BILL TO ST-BILL-AMT

           REWRITE CDSTMTF2-REC
               INVALID KEY
                   DISPLAY "CDSTMTF2 REWRITE ERROR ST=" FS-CDSTMTF2
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
                   EXIT PARAGRAPH
           END-REWRITE

           IF FS-CDSTMTF2 NOT = "00"
               DISPLAY "CDSTMTF2 REWRITE ERROR ST=" FS-CDSTMTF2
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO CT-STMT-UPD

           PERFORM WRITE-BAL-RTN

           IF NOT HARD-ERR
               ADD 1 TO CT-PAY-OK
           END-IF.

       WRITE-BAL-RTN.
           INITIALIZE CDRBALF-REC

           MOVE ST-CARD-NO TO RB-CARD-NO
           MOVE ST-CYCLE-DT TO RB-CYCLE-DT
           MOVE WK-REMAIN-AMT TO RB-REV-BAL-AMT

           COMPUTE RB-CARRIED-FEE-AMT =
               TB-RS-FEE-AMT(IX-RSLD) - WK-FEE-APPLY

           IF RB-CARRIED-FEE-AMT < ZERO
               MOVE ZERO TO RB-CARRIED-FEE-AMT
           END-IF

           COMPUTE RB-NEW-REV-AMT =
               RB-REV-BAL-AMT + RB-CARRIED-FEE-AMT

           WRITE CDRBALF-REC

           IF FS-CDRBALF NOT = "00"
               DISPLAY "CDRBALF WRITE ERROR ST=" FS-CDRBALF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               ADD 1 TO CT-BAL-WRITE
           END-IF.

       TERM-RTN.
           CLOSE CDPAYF2
           CLOSE CDSTMTF2
           CLOSE CDRSLDF
           CLOSE CDRBALF

           MOVE CT-PAY-READ TO WK-DISP-COUNT
           DISPLAY "PAYMENT READ COUNT=" WK-DISP-COUNT
           MOVE CT-PAY-OK TO WK-DISP-COUNT
           DISPLAY "PAYMENT APPLIED COUNT=" WK-DISP-COUNT
           MOVE CT-PAY-SKIP TO WK-DISP-COUNT
           DISPLAY "PAYMENT SKIP COUNT=" WK-DISP-COUNT
           MOVE CT-PAY-OVER TO WK-DISP-COUNT
           DISPLAY "OVER PAYMENT COUNT=" WK-DISP-COUNT
           MOVE CT-PAY-SHORT TO WK-DISP-COUNT
           DISPLAY "SHORT PAYMENT COUNT=" WK-DISP-COUNT
           MOVE CT-STMT-UPD TO WK-DISP-COUNT
           DISPLAY "STATEMENT UPDATE COUNT=" WK-DISP-COUNT
           MOVE CT-BAL-WRITE TO WK-DISP-COUNT
           DISPLAY "BALANCE WRITE COUNT=" WK-DISP-COUNT

           IF HARD-ERR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
