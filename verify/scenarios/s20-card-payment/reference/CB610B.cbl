       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB610B.
       AUTHOR. JP-SHOP.
      *================================================================*
      * PAYMENT APPLICATION BATCH                                      *
      * READ CDPAYF, LOOKUP CDOSF BY CARD NO, APPLY PAYMENT TO FEE,    *
      * INTEREST, THEN PRINCIPAL, AND WRITE RESULT TO CDAPPF.          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDPAYF
               ASSIGN TO "CDPAYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CDPAYF-ST.

           SELECT CDOSF
               ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS WS-CDOSF-ST.

           SELECT CDAPPF
               ASSIGN TO "CDAPPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CDAPPF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDPAYF.
           COPY CDPAYFC.

       FD  CDOSF.
           COPY CDOSFC.

       FD  CDAPPF.
           COPY CDAPPFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CDPAYF-ST              PIC XX VALUE SPACES.
           05 WS-CDOSF-ST               PIC XX VALUE SPACES.
           05 WS-CDAPPF-ST              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-END-SW                 PIC X VALUE "N".
              88 END-OF-CDPAYF                VALUE "Y".
              88 NOT-END-OF-CDPAYF            VALUE "N".
           05 WS-HARD-ERROR-SW          PIC X VALUE "N".
              88 HARD-ERROR                   VALUE "Y".
              88 NO-HARD-ERROR                VALUE "N".
           05 WS-SKIP-PAYMENT-SW        PIC X VALUE "N".
              88 SKIP-PAYMENT                 VALUE "Y".
              88 PROCESS-PAYMENT              VALUE "N".

       01  WS-WORK-AREA.
           05 WS-PROGRAM-ID             PIC X(08) VALUE "CB610B".
           05 WS-PAY-AMT                PIC S9(13)V99 COMP-3
                                          VALUE ZERO.
           05 WS-WORK-REMAIN            PIC S9(13)V99 COMP-3
                                          VALUE ZERO.
           05 WS-TOTAL-BAL              PIC S9(13)V99 COMP-3
                                          VALUE ZERO.
           05 WS-APPLIED-FEE            PIC S9(13)V99 COMP-3
                                          VALUE ZERO.
           05 WS-APPLIED-INT            PIC S9(13)V99 COMP-3
                                          VALUE ZERO.
           05 WS-APPLIED-PRIN           PIC S9(13)V99 COMP-3
                                          VALUE ZERO.
           05 WS-OVER-AMT               PIC S9(13)V99 COMP-3
                                          VALUE ZERO.
           05 WS-READ-CNT               PIC 9(09) VALUE ZERO.
           05 WS-WRITE-CNT              PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT               PIC 9(09) VALUE ZERO.
           05 WS-ERROR-CNT              PIC 9(09) VALUE ZERO.

       01  WS-MESSAGE-AREA.
           05 WS-MSG-FILE               PIC X(08) VALUE SPACES.
           05 WS-MSG-KEY                PIC X(32) VALUE SPACES.
           05 WS-MSG-REASON             PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS
               UNTIL END-OF-CDPAYF
                  OR HARD-ERROR
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           SET NOT-END-OF-CDPAYF TO TRUE
           SET NO-HARD-ERROR TO TRUE

           OPEN INPUT CDPAYF
           IF WS-CDPAYF-ST NOT = "00"
               MOVE "CDPAYF" TO WS-MSG-FILE
               MOVE SPACES TO WS-MSG-KEY
               MOVE "CDPAYF OPEN ERROR" TO WS-MSG-REASON
               PERFORM 8000-HARD-ERROR
           END-IF

           IF NO-HARD-ERROR
               OPEN INPUT CDOSF
               IF WS-CDOSF-ST NOT = "00"
                   MOVE "CDOSF" TO WS-MSG-FILE
                   MOVE SPACES TO WS-MSG-KEY
                   MOVE "CDOSF OPEN ERROR" TO WS-MSG-REASON
                   PERFORM 8000-HARD-ERROR
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT CDAPPF
               IF WS-CDAPPF-ST NOT = "00"
                   MOVE "CDAPPF" TO WS-MSG-FILE
                   MOVE SPACES TO WS-MSG-KEY
                   MOVE "CDAPPF OPEN ERROR" TO WS-MSG-REASON
                   PERFORM 8000-HARD-ERROR
               END-IF
           END-IF.

       2000-PROCESS.
           PERFORM 2100-READ-CDPAYF
           IF NOT END-OF-CDPAYF
              AND NO-HARD-ERROR
               PERFORM 2200-VALIDATE-PAYMENT
               IF PROCESS-PAYMENT
                   PERFORM 2300-READ-CDOSF
                   IF PROCESS-PAYMENT
                      AND NO-HARD-ERROR
                       PERFORM 2400-VALIDATE-OUTSTANDING
                       IF PROCESS-PAYMENT
                           PERFORM 2500-APPLY-PAYMENT
                           PERFORM 2600-WRITE-CDAPPF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       2100-READ-CDPAYF.
           READ CDPAYF
               AT END
                   SET END-OF-CDPAYF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ

           IF WS-CDPAYF-ST NOT = "00"
              AND WS-CDPAYF-ST NOT = "10"
               MOVE "CDPAYF" TO WS-MSG-FILE
               MOVE SPACES TO WS-MSG-KEY
               MOVE "CDPAYF READ ERROR" TO WS-MSG-REASON
               PERFORM 8000-HARD-ERROR
           END-IF.

       2200-VALIDATE-PAYMENT.
           SET PROCESS-PAYMENT TO TRUE

           IF PY-PAY-ID = SPACES
               MOVE "CDPAYF" TO WS-MSG-FILE
               MOVE SPACES TO WS-MSG-KEY
               MOVE "PAYMENT ID REQUIRED" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND PY-CARD-NO = SPACES
               MOVE "CDPAYF" TO WS-MSG-FILE
               MOVE PY-PAY-ID TO WS-MSG-KEY
               MOVE "CARD NO REQUIRED" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND PY-PAY-AMT <= ZERO
               MOVE "CDPAYF" TO WS-MSG-FILE
               MOVE PY-PAY-ID TO WS-MSG-KEY
               MOVE "PAYMENT AMOUNT INVALID" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND PY-PAY-DT = ZERO
               MOVE "CDPAYF" TO WS-MSG-FILE
               MOVE PY-PAY-ID TO WS-MSG-KEY
               MOVE "PAYMENT DATE REQUIRED" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND PY-PAY-METHOD NOT = "10"
              AND PY-PAY-METHOD NOT = "20"
              AND PY-PAY-METHOD NOT = "30"
               MOVE "CDPAYF" TO WS-MSG-FILE
               MOVE PY-PAY-ID TO WS-MSG-KEY
               MOVE "PAYMENT METHOD INVALID" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF.

       2300-READ-CDOSF.
           MOVE PY-CARD-NO TO OS-CARD-NO

           READ CDOSF
               KEY IS OS-CARD-NO
               INVALID KEY
                   MOVE "CDOSF" TO WS-MSG-FILE
                   MOVE PY-CARD-NO TO WS-MSG-KEY
                   MOVE "OUTSTANDING NOT FOUND" TO WS-MSG-REASON
                   PERFORM 2700-WRITE-SKIP-CDAPPF
           END-READ

           IF WS-CDOSF-ST NOT = "00"
              AND WS-CDOSF-ST NOT = "23"
               MOVE "CDOSF" TO WS-MSG-FILE
               MOVE PY-CARD-NO TO WS-MSG-KEY
               MOVE "CDOSF READ ERROR" TO WS-MSG-REASON
               PERFORM 8000-HARD-ERROR
           END-IF.

       2400-VALIDATE-OUTSTANDING.
           IF OS-CARD-NO NOT = PY-CARD-NO
               MOVE "CDOSF" TO WS-MSG-FILE
               MOVE PY-CARD-NO TO WS-MSG-KEY
               MOVE "CARD NO MISMATCH" TO WS-MSG-REASON
               PERFORM 8000-HARD-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND OS-FEE-BAL-AMT < ZERO
               MOVE "CDOSF" TO WS-MSG-FILE
               MOVE PY-CARD-NO TO WS-MSG-KEY
               MOVE "FEE BALANCE INVALID" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND OS-INTEREST-BAL-AMT < ZERO
               MOVE "CDOSF" TO WS-MSG-FILE
               MOVE PY-CARD-NO TO WS-MSG-KEY
               MOVE "INTEREST BALANCE INVALID" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND OS-PRINCIPAL-BAL-AMT < ZERO
               MOVE "CDOSF" TO WS-MSG-FILE
               MOVE PY-CARD-NO TO WS-MSG-KEY
               MOVE "PRINCIPAL BALANCE INVALID" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF

           IF PROCESS-PAYMENT
              AND OS-CYCLE-DT = ZERO
               MOVE "CDOSF" TO WS-MSG-FILE
               MOVE PY-CARD-NO TO WS-MSG-KEY
               MOVE "CYCLE DATE REQUIRED" TO WS-MSG-REASON
               PERFORM 8100-SKIP-ERROR
           END-IF.

       2500-APPLY-PAYMENT.
           MOVE PY-PAY-AMT TO WS-PAY-AMT
           MOVE WS-PAY-AMT TO WS-WORK-REMAIN
           MOVE ZERO TO WS-APPLIED-FEE
           MOVE ZERO TO WS-APPLIED-INT
           MOVE ZERO TO WS-APPLIED-PRIN
           MOVE ZERO TO WS-OVER-AMT

           IF WS-WORK-REMAIN > ZERO
               IF WS-WORK-REMAIN >= OS-FEE-BAL-AMT
                   MOVE OS-FEE-BAL-AMT TO WS-APPLIED-FEE
                   SUBTRACT OS-FEE-BAL-AMT FROM WS-WORK-REMAIN
               ELSE
                   MOVE WS-WORK-REMAIN TO WS-APPLIED-FEE
                   MOVE ZERO TO WS-WORK-REMAIN
               END-IF
           END-IF

           IF WS-WORK-REMAIN > ZERO
               IF WS-WORK-REMAIN >= OS-INTEREST-BAL-AMT
                   MOVE OS-INTEREST-BAL-AMT TO WS-APPLIED-INT
                   SUBTRACT OS-INTEREST-BAL-AMT
                       FROM WS-WORK-REMAIN
               ELSE
                   MOVE WS-WORK-REMAIN TO WS-APPLIED-INT
                   MOVE ZERO TO WS-WORK-REMAIN
               END-IF
           END-IF

           IF WS-WORK-REMAIN > ZERO
               IF WS-WORK-REMAIN >= OS-PRINCIPAL-BAL-AMT
                   MOVE OS-PRINCIPAL-BAL-AMT TO WS-APPLIED-PRIN
                   SUBTRACT OS-PRINCIPAL-BAL-AMT
                       FROM WS-WORK-REMAIN
               ELSE
                   MOVE WS-WORK-REMAIN TO WS-APPLIED-PRIN
                   MOVE ZERO TO WS-WORK-REMAIN
               END-IF
           END-IF

           MOVE WS-WORK-REMAIN TO WS-OVER-AMT

           ADD OS-FEE-BAL-AMT
               OS-INTEREST-BAL-AMT
               OS-PRINCIPAL-BAL-AMT
             GIVING WS-TOTAL-BAL

           INITIALIZE CDAPPF-REC
           MOVE PY-PAY-ID TO AP-PAY-ID
           MOVE PY-CARD-NO TO AP-CARD-NO
           MOVE WS-APPLIED-FEE TO AP-APPLIED-FEE-AMT
           MOVE WS-APPLIED-INT TO AP-APPLIED-INT-AMT
           MOVE WS-APPLIED-PRIN TO AP-APPLIED-PRIN-AMT
           MOVE WS-OVER-AMT TO AP-REMAIN-AMT
           MOVE WS-PROGRAM-ID TO AP-PROGRAM-ID

           IF WS-OVER-AMT > ZERO
               MOVE "O" TO AP-APP-STATUS
           ELSE
               IF WS-PAY-AMT >= WS-TOTAL-BAL
                   MOVE "F" TO AP-APP-STATUS
               ELSE
                   MOVE "P" TO AP-APP-STATUS
               END-IF
           END-IF.

       2600-WRITE-CDAPPF.
           WRITE CDAPPF-REC
           IF WS-CDAPPF-ST = "00"
               ADD 1 TO WS-WRITE-CNT
           ELSE
               MOVE "CDAPPF" TO WS-MSG-FILE
               MOVE AP-PAY-ID TO WS-MSG-KEY
               MOVE "CDAPPF WRITE ERROR" TO WS-MSG-REASON
               PERFORM 8000-HARD-ERROR
           END-IF.

       2700-WRITE-SKIP-CDAPPF.
           INITIALIZE CDAPPF-REC
           MOVE PY-PAY-ID TO AP-PAY-ID
           MOVE PY-CARD-NO TO AP-CARD-NO
           MOVE ZERO TO AP-APPLIED-FEE-AMT
           MOVE ZERO TO AP-APPLIED-INT-AMT
           MOVE ZERO TO AP-APPLIED-PRIN-AMT
           MOVE PY-PAY-AMT TO AP-REMAIN-AMT
           MOVE "S" TO AP-APP-STATUS
           MOVE WS-PROGRAM-ID TO AP-PROGRAM-ID
           PERFORM 2600-WRITE-CDAPPF
           SET SKIP-PAYMENT TO TRUE
           ADD 1 TO WS-SKIP-CNT.

       8000-HARD-ERROR.
           SET HARD-ERROR TO TRUE
           ADD 1 TO WS-ERROR-CNT
           DISPLAY "ABEND FILE=" WS-MSG-FILE
                   " KEY=" WS-MSG-KEY
                   " REASON=" WS-MSG-REASON
           DISPLAY "STATUS CDPAYF=" WS-CDPAYF-ST
                   " CDOSF=" WS-CDOSF-ST
                   " CDAPPF=" WS-CDAPPF-ST
           MOVE 12 TO RETURN-CODE.

       8100-SKIP-ERROR.
           SET SKIP-PAYMENT TO TRUE
           ADD 1 TO WS-SKIP-CNT
           ADD 1 TO WS-ERROR-CNT
           DISPLAY "SKIP FILE=" WS-MSG-FILE
                   " KEY=" WS-MSG-KEY
                   " REASON=" WS-MSG-REASON.

       9000-TERMINATE.
           IF WS-CDPAYF-ST = "00"
              OR WS-CDPAYF-ST = "10"
               CLOSE CDPAYF
               IF WS-CDPAYF-ST NOT = "00"
                   MOVE "CDPAYF" TO WS-MSG-FILE
                   MOVE SPACES TO WS-MSG-KEY
                   MOVE "CDPAYF CLOSE ERROR" TO WS-MSG-REASON
                   PERFORM 8000-HARD-ERROR
               END-IF
           END-IF

           IF WS-CDOSF-ST = "00"
              OR WS-CDOSF-ST = "23"
               CLOSE CDOSF
               IF WS-CDOSF-ST NOT = "00"
                   MOVE "CDOSF" TO WS-MSG-FILE
                   MOVE SPACES TO WS-MSG-KEY
                   MOVE "CDOSF CLOSE ERROR" TO WS-MSG-REASON
                   PERFORM 8000-HARD-ERROR
               END-IF
           END-IF

           IF WS-CDAPPF-ST = "00"
               CLOSE CDAPPF
               IF WS-CDAPPF-ST NOT = "00"
                   MOVE "CDAPPF" TO WS-MSG-FILE
                   MOVE SPACES TO WS-MSG-KEY
                   MOVE "CDAPPF CLOSE ERROR" TO WS-MSG-REASON
                   PERFORM 8000-HARD-ERROR
               END-IF
           END-IF

           DISPLAY "PAYMENT APPLICATION END"
           DISPLAY "READ=" WS-READ-CNT
                   " WRITE=" WS-WRITE-CNT
                   " SKIP=" WS-SKIP-CNT
                   " ERROR=" WS-ERROR-CNT

           IF NO-HARD-ERROR
               MOVE 0 TO RETURN-CODE
           END-IF.
