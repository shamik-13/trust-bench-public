       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP260S.
       AUTHOR. BATCH-SYSTEM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LPCLMF ASSIGN TO "LPCLMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CLAIM-ID
               FILE STATUS IS WS-LPCLMF-ST.
           SELECT LPPAYF ASSIGN TO "LPPAYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LPPAYF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LPCLMF.
           COPY LPCLMFC.

       FD  LPPAYF.
           COPY LPPAYFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LPCLMF-ST              PIC XX VALUE SPACE.
           05 WS-LPPAYF-ST              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-EOF-CLM                PIC X VALUE "N".
              88 EOF-CLM                      VALUE "Y".
           05 WS-EOF-PAY                PIC X VALUE "N".
              88 EOF-PAY                      VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                   VALUE "Y".
           05 WS-SWAPPED                PIC X VALUE "N".
              88 SWAPPED                      VALUE "Y".

       01  WS-CONSTANTS.
           05 WS-MAX-CLAIM              PIC 9(04) VALUE 300.
           05 WS-MAX-PAY                PIC 9(04) VALUE 200.
           05 WS-STS-OK                 PIC XX VALUE "00".
           05 WS-STS-EOF                PIC XX VALUE "10".

       01  WS-COUNTERS.
           05 WS-I                      PIC 9(04) VALUE ZERO.
           05 WS-J                      PIC 9(04) VALUE ZERO.
           05 WS-K                      PIC 9(04) VALUE ZERO.
           05 WS-CLAIM-CNT              PIC 9(04) VALUE ZERO.
           05 WS-PAY-CNT                PIC 9(04) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-TOTAL-PAY-AMT          PIC S9(11)V99 VALUE ZERO.
           05 WS-AVAILABLE-AMT          PIC S9(11)V99 VALUE ZERO.
           05 WS-ALLOC-AMT              PIC S9(11)V99 VALUE ZERO.
           05 WS-OVERPAY-AMT            PIC S9(11)V99 VALUE ZERO.

       01  WS-WORK-DATES.
           05 WS-FROM-MONTH             PIC 99 VALUE ZERO.
           05 WS-TO-MONTH               PIC 99 VALUE ZERO.
           05 WS-PAY-MONTH              PIC 99 VALUE ZERO.
           05 WS-CLM-MONTH              PIC 99 VALUE ZERO.

       01  WS-CLAIM-TABLE.
           05 WS-CLM OCCURS 300 TIMES.
              10 WS-CLM-ID              PIC X(20).
              10 WS-CLM-POL-NO          PIC X(20).
              10 WS-CLM-DUE-YM          PIC 9(06).
              10 WS-CLM-BILL-AMT        PIC S9(11)V99.
              10 WS-CLM-RECEIPT-AMT     PIC S9(11)V99.
              10 WS-CLM-STATUS-KBN      PIC X(02).
              10 WS-CLM-TR-RESULT-KBN   PIC X(02).
              10 WS-CLM-ALLOC-AMT       PIC S9(11)V99.
              10 WS-CLM-BAL-AMT         PIC S9(11)V99.
              10 WS-CLM-CLEAR-KBN       PIC X(01).

       01  WS-SAVE-CLAIM.
           05 SV-CLM-ID                 PIC X(20).
           05 SV-CLM-POL-NO             PIC X(20).
           05 SV-CLM-DUE-YM             PIC 9(06).
           05 SV-CLM-BILL-AMT           PIC S9(11)V99.
           05 SV-CLM-RECEIPT-AMT        PIC S9(11)V99.
           05 SV-CLM-STATUS-KBN         PIC X(02).
           05 SV-CLM-TR-RESULT-KBN      PIC X(02).
           05 SV-CLM-ALLOC-AMT          PIC S9(11)V99.
           05 SV-CLM-BAL-AMT            PIC S9(11)V99.
           05 SV-CLM-CLEAR-KBN          PIC X(01).

       01  WS-PAY-TABLE.
           05 WS-PAY OCCURS 200 TIMES.
              10 WS-PAY-ID              PIC X(20).
              10 WS-PAY-POL-NO          PIC X(20).
              10 WS-PAY-DUE-YM          PIC 9(06).
              10 WS-PAY-AMT             PIC S9(11)V99.
              10 WS-PAY-DATE            PIC 9(08).
              10 WS-PAY-CHANNEL-KBN     PIC X(02).
              10 WS-PAY-MATCH-KBN       PIC X(02).

       01  WS-JUDGEMENT.
           05 WS-OVERPAY-KBN            PIC X(01) VALUE SPACE.
              88 REFUND-CANDIDATE              VALUE "R".
              88 DEPOSIT-CANDIDATE             VALUE "D".
              88 NO-OVERPAY                    VALUE "N".
           05 WS-REASON-KBN             PIC X(04) VALUE SPACE.
           05 WS-ERROR-TEXT             PIC X(40) VALUE SPACE.

       LINKAGE SECTION.
       01  LP260S-PARM.
           05 LS-POL-NO                 PIC X(20).
           05 LS-REQ-DUE-FROM           PIC 9(06).
           05 LS-REQ-DUE-TO             PIC 9(06).
           05 LS-RETURN-KBN             PIC X(01).
           05 LS-RETURN-REASON-KBN      PIC X(04).
           05 LS-TOTAL-PAY-AMT          PIC S9(11)V99.
           05 LS-OVERPAY-AMT            PIC S9(11)V99.
           05 LS-CLAIM-COUNT            PIC 9(04).
           05 LS-CLAIM OCCURS 300 TIMES.
              10 LS-CLAIM-ID            PIC X(20).
              10 LS-DUE-YM              PIC 9(06).
              10 LS-BILL-AMT            PIC S9(11)V99.
              10 LS-ALLOC-AMT           PIC S9(11)V99.
              10 LS-BAL-AMT             PIC S9(11)V99.
              10 LS-CLEAR-KBN           PIC X(01).

       PROCEDURE DIVISION USING LP260S-PARM.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
              PERFORM 1000-LOAD-PAYMENT
           END-IF
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-CLAIM
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-SORT-CLAIM
              PERFORM 4000-ALLOCATE-PAYMENT
              PERFORM 5000-JUDGE-OVERPAY
              PERFORM 6000-SET-LINKAGE
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK
           .

       0000-INIT.
           MOVE "N" TO WS-HARD-ERROR
           MOVE "N" TO WS-EOF-CLM
           MOVE "N" TO WS-EOF-PAY
           MOVE ZERO TO WS-CLAIM-CNT
           MOVE ZERO TO WS-PAY-CNT
           MOVE ZERO TO WS-TOTAL-PAY-AMT
           MOVE ZERO TO WS-AVAILABLE-AMT
           MOVE ZERO TO WS-OVERPAY-AMT
           SET NO-OVERPAY TO TRUE
           MOVE SPACE TO WS-REASON-KBN
           MOVE SPACE TO LS-RETURN-KBN
           MOVE SPACE TO LS-RETURN-REASON-KBN
           MOVE ZERO TO LS-TOTAL-PAY-AMT
           MOVE ZERO TO LS-OVERPAY-AMT
           MOVE ZERO TO LS-CLAIM-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1
              UNTIL WS-I > WS-MAX-CLAIM
              MOVE SPACE TO LS-CLAIM-ID(WS-I)
              MOVE ZERO  TO LS-DUE-YM(WS-I)
              MOVE ZERO  TO LS-BILL-AMT(WS-I)
              MOVE ZERO  TO LS-ALLOC-AMT(WS-I)
              MOVE ZERO  TO LS-BAL-AMT(WS-I)
              MOVE SPACE TO LS-CLEAR-KBN(WS-I)
           END-PERFORM
           PERFORM 0100-VALIDATE-PARM
           IF NOT HARD-ERROR
              OPEN INPUT LPCLMF
              IF WS-LPCLMF-ST NOT = WS-STS-OK
                 MOVE "LPCLMF OPEN ERROR" TO WS-ERROR-TEXT
                 DISPLAY WS-ERROR-TEXT " ST=" WS-LPCLMF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT LPPAYF
              IF WS-LPPAYF-ST NOT = WS-STS-OK
                 MOVE "LPPAYF OPEN ERROR" TO WS-ERROR-TEXT
                 DISPLAY WS-ERROR-TEXT " ST=" WS-LPPAYF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           .

       0100-VALIDATE-PARM.
           IF LS-POL-NO = SPACE
              DISPLAY "POL-NO REQUIRED"
              SET HARD-ERROR TO TRUE
           END-IF
           IF LS-REQ-DUE-FROM = ZERO
              DISPLAY "DUE-FROM REQUIRED"
              SET HARD-ERROR TO TRUE
           END-IF
           IF LS-REQ-DUE-TO = ZERO
              DISPLAY "DUE-TO REQUIRED"
              SET HARD-ERROR TO TRUE
           END-IF
           IF LS-REQ-DUE-FROM > LS-REQ-DUE-TO
              DISPLAY "DUE RANGE ERROR"
              SET HARD-ERROR TO TRUE
           END-IF
           MOVE LS-REQ-DUE-FROM(5:2) TO WS-FROM-MONTH
           IF WS-FROM-MONTH < 1 OR WS-FROM-MONTH > 12
              DISPLAY "DUE-FROM MONTH ERROR"
              SET HARD-ERROR TO TRUE
           END-IF
           MOVE LS-REQ-DUE-TO(5:2) TO WS-TO-MONTH
           IF WS-TO-MONTH < 1 OR WS-TO-MONTH > 12
              DISPLAY "DUE-TO MONTH ERROR"
              SET HARD-ERROR TO TRUE
           END-IF
           .

       1000-LOAD-PAYMENT.
           PERFORM UNTIL EOF-PAY OR HARD-ERROR
              READ LPPAYF
                 AT END
                    SET EOF-PAY TO TRUE
                 NOT AT END
                    PERFORM 1100-CHECK-AND-KEEP-PAY
              END-READ
           END-PERFORM
           IF NOT HARD-ERROR
              MOVE WS-TOTAL-PAY-AMT TO WS-AVAILABLE-AMT
              IF WS-PAY-CNT = ZERO
                 DISPLAY "NO PAYMENT POL=" LS-POL-NO
              END-IF
           END-IF
           .

       1100-CHECK-AND-KEEP-PAY.
           IF PY-POL-NO = LS-POL-NO
              IF PY-DUE-YM >= LS-REQ-DUE-FROM
                 AND PY-DUE-YM <= LS-REQ-DUE-TO
                 PERFORM 1110-VALIDATE-PAY
                 IF NOT HARD-ERROR
                    ADD 1 TO WS-PAY-CNT
                    IF WS-PAY-CNT > WS-MAX-PAY
                       DISPLAY "PAY COUNT OVER POL=" LS-POL-NO
                       SET HARD-ERROR TO TRUE
                    ELSE
                       MOVE PY-PAY-ID TO WS-PAY-ID(WS-PAY-CNT)
                       MOVE PY-POL-NO TO WS-PAY-POL-NO(WS-PAY-CNT)
                       MOVE PY-DUE-YM TO WS-PAY-DUE-YM(WS-PAY-CNT)
                       MOVE PY-PAY-AMT TO WS-PAY-AMT(WS-PAY-CNT)
                       MOVE PY-PAY-DATE TO WS-PAY-DATE(WS-PAY-CNT)
                       MOVE PY-PAY-CHANNEL-KBN
                         TO WS-PAY-CHANNEL-KBN(WS-PAY-CNT)
                       MOVE PY-MATCH-STATUS-KBN
                         TO WS-PAY-MATCH-KBN(WS-PAY-CNT)
                       ADD PY-PAY-AMT TO WS-TOTAL-PAY-AMT
                    END-IF
                 END-IF
              END-IF
           END-IF
           .

       1110-VALIDATE-PAY.
           IF PY-PAY-AMT <= ZERO
              DISPLAY "PAY-AMT ERROR ID=" PY-PAY-ID
              SET HARD-ERROR TO TRUE
           END-IF
           MOVE PY-DUE-YM(5:2) TO WS-PAY-MONTH
           IF WS-PAY-MONTH < 1 OR WS-PAY-MONTH > 12
              DISPLAY "PAY-DUE ERROR ID=" PY-PAY-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF PY-PAY-DATE = ZERO
              DISPLAY "PAY-DATE REQUIRED ID=" PY-PAY-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF PY-PAY-CHANNEL-KBN NOT = "01"
              AND PY-PAY-CHANNEL-KBN NOT = "02"
              AND PY-PAY-CHANNEL-KBN NOT = "03"
              AND PY-PAY-CHANNEL-KBN NOT = "09"
              DISPLAY "PAY-CHANNEL ERROR ID=" PY-PAY-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF PY-MATCH-STATUS-KBN NOT = "00"
              AND PY-MATCH-STATUS-KBN NOT = "01"
              AND PY-MATCH-STATUS-KBN NOT = "02"
              AND PY-MATCH-STATUS-KBN NOT = "09"
              DISPLAY "PAY-MATCH ERROR ID=" PY-PAY-ID
              SET HARD-ERROR TO TRUE
           END-IF
           .

       2000-LOAD-CLAIM.
           PERFORM UNTIL EOF-CLM OR HARD-ERROR
              READ LPCLMF NEXT RECORD
                 AT END
                    SET EOF-CLM TO TRUE
                 NOT AT END
                    PERFORM 2100-CHECK-AND-KEEP-CLAIM
              END-READ
           END-PERFORM
           IF NOT HARD-ERROR AND WS-CLAIM-CNT = ZERO
              DISPLAY "NO CLAIM POL=" LS-POL-NO
           END-IF
           .

       2100-CHECK-AND-KEEP-CLAIM.
           IF CL-POL-NO = LS-POL-NO
              IF CL-DUE-YM >= LS-REQ-DUE-FROM
                 AND CL-DUE-YM <= LS-REQ-DUE-TO
                 PERFORM 2110-VALIDATE-CLAIM
                 IF NOT HARD-ERROR
                    ADD 1 TO WS-CLAIM-CNT
                    IF WS-CLAIM-CNT > WS-MAX-CLAIM
                       DISPLAY "CLAIM COUNT OVER POL=" LS-POL-NO
                       SET HARD-ERROR TO TRUE
                    ELSE
                       MOVE CL-CLAIM-ID
                         TO WS-CLM-ID(WS-CLAIM-CNT)
                       MOVE CL-POL-NO
                         TO WS-CLM-POL-NO(WS-CLAIM-CNT)
                       MOVE CL-DUE-YM
                         TO WS-CLM-DUE-YM(WS-CLAIM-CNT)
                       MOVE CL-BILL-AMT
                         TO WS-CLM-BILL-AMT(WS-CLAIM-CNT)
                       MOVE CL-RECEIPT-AMT
                         TO WS-CLM-RECEIPT-AMT(WS-CLAIM-CNT)
                       MOVE CL-CLAIM-STATUS-KBN
                         TO WS-CLM-STATUS-KBN(WS-CLAIM-CNT)
                       MOVE CL-TRANSFER-RESULT-KBN
                         TO WS-CLM-TR-RESULT-KBN(WS-CLAIM-CNT)
                       MOVE ZERO TO WS-CLM-ALLOC-AMT(WS-CLAIM-CNT)
                       COMPUTE WS-CLM-BAL-AMT(WS-CLAIM-CNT) =
                          CL-BILL-AMT - CL-RECEIPT-AMT
                       MOVE "N" TO WS-CLM-CLEAR-KBN(WS-CLAIM-CNT)
                    END-IF
                 END-IF
              END-IF
           END-IF
           .

       2110-VALIDATE-CLAIM.
           IF CL-BILL-AMT < ZERO
              DISPLAY "CLAIM-AMT ERROR ID=" CL-CLAIM-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF CL-RECEIPT-AMT < ZERO
              DISPLAY "RECEIPT-AMT ERROR ID=" CL-CLAIM-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF CL-RECEIPT-AMT > CL-BILL-AMT
              DISPLAY "RECEIPT OVER ID=" CL-CLAIM-ID
              SET HARD-ERROR TO TRUE
           END-IF
           MOVE CL-DUE-YM(5:2) TO WS-CLM-MONTH
           IF WS-CLM-MONTH < 1 OR WS-CLM-MONTH > 12
              DISPLAY "CLAIM-DUE ERROR ID=" CL-CLAIM-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF CL-CLAIM-STATUS-KBN NOT = "00"
              AND CL-CLAIM-STATUS-KBN NOT = "01"
              AND CL-CLAIM-STATUS-KBN NOT = "02"
              AND CL-CLAIM-STATUS-KBN NOT = "09"
              DISPLAY "CLAIM-STATUS ERROR ID=" CL-CLAIM-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF CL-TRANSFER-RESULT-KBN NOT = "00"
              AND CL-TRANSFER-RESULT-KBN NOT = "01"
              AND CL-TRANSFER-RESULT-KBN NOT = "02"
              AND CL-TRANSFER-RESULT-KBN NOT = "09"
              DISPLAY "TRANSFER ERROR ID=" CL-CLAIM-ID
              SET HARD-ERROR TO TRUE
           END-IF
           .

       3000-SORT-CLAIM.
           IF WS-CLAIM-CNT > 1
              PERFORM VARYING WS-I FROM 1 BY 1
                 UNTIL WS-I >= WS-CLAIM-CNT
                 PERFORM VARYING WS-J FROM 1 BY 1
                    UNTIL WS-J > WS-CLAIM-CNT - WS-I
                    ADD 1 TO WS-J GIVING WS-K
                    PERFORM 3100-COMPARE-CLAIM
                    IF SWAPPED
                       PERFORM 3200-SWAP-CLAIM
                    END-IF
                 END-PERFORM
              END-PERFORM
           END-IF
           .

       3100-COMPARE-CLAIM.
           MOVE "N" TO WS-SWAPPED
           IF WS-CLM-DUE-YM(WS-J) > WS-CLM-DUE-YM(WS-K)
              SET SWAPPED TO TRUE
           ELSE
              IF WS-CLM-DUE-YM(WS-J) = WS-CLM-DUE-YM(WS-K)
                 IF WS-CLM-STATUS-KBN(WS-J)
                    > WS-CLM-STATUS-KBN(WS-K)
                    SET SWAPPED TO TRUE
                 ELSE
                    IF WS-CLM-STATUS-KBN(WS-J)
                       = WS-CLM-STATUS-KBN(WS-K)
                       IF WS-CLM-TR-RESULT-KBN(WS-J)
                          > WS-CLM-TR-RESULT-KBN(WS-K)
                          SET SWAPPED TO TRUE
                       ELSE
                          IF WS-CLM-TR-RESULT-KBN(WS-J)
                             = WS-CLM-TR-RESULT-KBN(WS-K)
                             IF WS-CLM-ID(WS-J) > WS-CLM-ID(WS-K)
                                SET SWAPPED TO TRUE
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
           .

       3200-SWAP-CLAIM.
           MOVE WS-CLM-ID(WS-J) TO SV-CLM-ID
           MOVE WS-CLM-POL-NO(WS-J) TO SV-CLM-POL-NO
           MOVE WS-CLM-DUE-YM(WS-J) TO SV-CLM-DUE-YM
           MOVE WS-CLM-BILL-AMT(WS-J) TO SV-CLM-BILL-AMT
           MOVE WS-CLM-RECEIPT-AMT(WS-J) TO SV-CLM-RECEIPT-AMT
           MOVE WS-CLM-STATUS-KBN(WS-J) TO SV-CLM-STATUS-KBN
           MOVE WS-CLM-TR-RESULT-KBN(WS-J) TO SV-CLM-TR-RESULT-KBN
           MOVE WS-CLM-ALLOC-AMT(WS-J) TO SV-CLM-ALLOC-AMT
           MOVE WS-CLM-BAL-AMT(WS-J) TO SV-CLM-BAL-AMT
           MOVE WS-CLM-CLEAR-KBN(WS-J) TO SV-CLM-CLEAR-KBN

           MOVE WS-CLM-ID(WS-K) TO WS-CLM-ID(WS-J)
           MOVE WS-CLM-POL-NO(WS-K) TO WS-CLM-POL-NO(WS-J)
           MOVE WS-CLM-DUE-YM(WS-K) TO WS-CLM-DUE-YM(WS-J)
           MOVE WS-CLM-BILL-AMT(WS-K) TO WS-CLM-BILL-AMT(WS-J)
           MOVE WS-CLM-RECEIPT-AMT(WS-K)
             TO WS-CLM-RECEIPT-AMT(WS-J)
           MOVE WS-CLM-STATUS-KBN(WS-K)
             TO WS-CLM-STATUS-KBN(WS-J)
           MOVE WS-CLM-TR-RESULT-KBN(WS-K)
             TO WS-CLM-TR-RESULT-KBN(WS-J)
           MOVE WS-CLM-ALLOC-AMT(WS-K) TO WS-CLM-ALLOC-AMT(WS-J)
           MOVE WS-CLM-BAL-AMT(WS-K) TO WS-CLM-BAL-AMT(WS-J)
           MOVE WS-CLM-CLEAR-KBN(WS-K) TO WS-CLM-CLEAR-KBN(WS-J)

           MOVE SV-CLM-ID TO WS-CLM-ID(WS-K)
           MOVE SV-CLM-POL-NO TO WS-CLM-POL-NO(WS-K)
           MOVE SV-CLM-DUE-YM TO WS-CLM-DUE-YM(WS-K)
           MOVE SV-CLM-BILL-AMT TO WS-CLM-BILL-AMT(WS-K)
           MOVE SV-CLM-RECEIPT-AMT TO WS-CLM-RECEIPT-AMT(WS-K)
           MOVE SV-CLM-STATUS-KBN TO WS-CLM-STATUS-KBN(WS-K)
           MOVE SV-CLM-TR-RESULT-KBN TO WS-CLM-TR-RESULT-KBN(WS-K)
           MOVE SV-CLM-ALLOC-AMT TO WS-CLM-ALLOC-AMT(WS-K)
           MOVE SV-CLM-BAL-AMT TO WS-CLM-BAL-AMT(WS-K)
           MOVE SV-CLM-CLEAR-KBN TO WS-CLM-CLEAR-KBN(WS-K)
           .

       4000-ALLOCATE-PAYMENT.
           PERFORM VARYING WS-I FROM 1 BY 1
              UNTIL WS-I > WS-CLAIM-CNT OR WS-AVAILABLE-AMT <= ZERO
              IF WS-CLM-BAL-AMT(WS-I) > ZERO
                 IF WS-AVAILABLE-AMT >= WS-CLM-BAL-AMT(WS-I)
                    MOVE WS-CLM-BAL-AMT(WS-I) TO WS-ALLOC-AMT
                 ELSE
                    MOVE WS-AVAILABLE-AMT TO WS-ALLOC-AMT
                 END-IF
                 ADD WS-ALLOC-AMT TO WS-CLM-ALLOC-AMT(WS-I)
                 SUBTRACT WS-ALLOC-AMT FROM WS-CLM-BAL-AMT(WS-I)
                 SUBTRACT WS-ALLOC-AMT FROM WS-AVAILABLE-AMT
                 IF WS-CLM-BAL-AMT(WS-I) = ZERO
                    MOVE "Y" TO WS-CLM-CLEAR-KBN(WS-I)
                 ELSE
                    MOVE "N" TO WS-CLM-CLEAR-KBN(WS-I)
                 END-IF
              ELSE
                 MOVE "Y" TO WS-CLM-CLEAR-KBN(WS-I)
              END-IF
           END-PERFORM
           MOVE WS-AVAILABLE-AMT TO WS-OVERPAY-AMT
           .

       5000-JUDGE-OVERPAY.
           IF WS-OVERPAY-AMT <= ZERO
              SET NO-OVERPAY TO TRUE
              MOVE "0000" TO WS-REASON-KBN
           ELSE
              SET DEPOSIT-CANDIDATE TO TRUE
              MOVE "AZKR" TO WS-REASON-KBN
              PERFORM VARYING WS-I FROM 1 BY 1
                 UNTIL WS-I > WS-PAY-CNT
                 IF WS-PAY-CHANNEL-KBN(WS-I) = "01"
                    AND WS-PAY-MATCH-KBN(WS-I) = "00"
                    SET REFUND-CANDIDATE TO TRUE
                    MOVE "HNK1" TO WS-REASON-KBN
                 END-IF
                 IF WS-PAY-CHANNEL-KBN(WS-I) = "02"
                    AND WS-PAY-MATCH-KBN(WS-I) = "01"
                    SET DEPOSIT-CANDIDATE TO TRUE
                    MOVE "AZK1" TO WS-REASON-KBN
                 END-IF
                 IF WS-PAY-MATCH-KBN(WS-I) = "09"
                    SET DEPOSIT-CANDIDATE TO TRUE
                    MOVE "AZK9" TO WS-REASON-KBN
                 END-IF
              END-PERFORM
           END-IF
           .

       6000-SET-LINKAGE.
           MOVE WS-OVERPAY-KBN TO LS-RETURN-KBN
           MOVE WS-REASON-KBN TO LS-RETURN-REASON-KBN
           MOVE WS-TOTAL-PAY-AMT TO LS-TOTAL-PAY-AMT
           MOVE WS-OVERPAY-AMT TO LS-OVERPAY-AMT
           MOVE WS-CLAIM-CNT TO LS-CLAIM-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1
              UNTIL WS-I > WS-CLAIM-CNT
              MOVE WS-CLM-ID(WS-I) TO LS-CLAIM-ID(WS-I)
              MOVE WS-CLM-DUE-YM(WS-I) TO LS-DUE-YM(WS-I)
              MOVE WS-CLM-BILL-AMT(WS-I) TO LS-BILL-AMT(WS-I)
              MOVE WS-CLM-ALLOC-AMT(WS-I) TO LS-ALLOC-AMT(WS-I)
              MOVE WS-CLM-BAL-AMT(WS-I) TO LS-BAL-AMT(WS-I)
              MOVE WS-CLM-CLEAR-KBN(WS-I) TO LS-CLEAR-KBN(WS-I)
           END-PERFORM
           .

       9000-CLOSE-FILES.
           IF WS-LPPAYF-ST = WS-STS-OK OR WS-LPPAYF-ST = WS-STS-EOF
              CLOSE LPPAYF
              IF WS-LPPAYF-ST NOT = WS-STS-OK
                 DISPLAY "LPPAYF CLOSE ERROR ST=" WS-LPPAYF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF WS-LPCLMF-ST = WS-STS-OK OR WS-LPCLMF-ST = WS-STS-EOF
              CLOSE LPCLMF
              IF WS-LPCLMF-ST NOT = WS-STS-OK
                 DISPLAY "LPCLMF CLOSE ERROR ST=" WS-LPCLMF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           .
