       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ150B.
       AUTHOR. KZ-BATCH.
      *
      * 与信限度見直しバッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF
               ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-ACCT-ST.

           SELECT KZCUSTF
               ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CU-CUST-ID
               FILE STATUS IS WS-CUST-ST.

           SELECT KZCRLF
               ASSIGN TO "KZCRLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CRL-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC4.

       FD  KZCUSTF.
           COPY KZCUSTC.

       FD  KZCRLF.
           COPY KZCRLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ACCT-ST              PIC X(02) VALUE SPACES.
           05  WS-CUST-ST              PIC X(02) VALUE SPACES.
           05  WS-CRL-ST               PIC X(02) VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01) VALUE "N".
               88  WS-EOF                       VALUE "Y".
               88  WS-NOT-EOF                   VALUE "N".
           05  WS-ABEND-SW             PIC X(01) VALUE "N".
               88  WS-ABEND                     VALUE "Y".
               88  WS-NORMAL                    VALUE "N".

       01  WS-CONSTANTS.
           05  WS-BAL-THRESHOLD        PIC 9(09) VALUE 5000000.
           05  WS-RAISED-LIMIT         PIC 9(09) VALUE 3000000.
           05  WS-KYC-OK               PIC X(01) VALUE "1".
           05  WS-KYC-NG               PIC X(01) VALUE "0".
           05  WS-KYC-HOLD             PIC X(01) VALUE "9".
           05  WS-FLAG-YES             PIC X(01) VALUE "Y".
           05  WS-FLAG-NO              PIC X(01) VALUE "N".
           05  WS-RSN-RAISE            PIC X(04) VALUE "RAIS".
           05  WS-RSN-HKYC             PIC X(04) VALUE "HKYC".
           05  WS-RSN-HBAL             PIC X(04) VALUE "HBAL".

       01  WS-COUNTERS.
           05  WS-READ-ACCT-CNT        PIC 9(09) VALUE ZERO.
           05  WS-WRITE-CRL-CNT        PIC 9(09) VALUE ZERO.
           05  WS-RAISE-CNT            PIC 9(09) VALUE ZERO.
           05  WS-HOLD-CNT             PIC 9(09) VALUE ZERO.

       01  WS-WORK.
           05  WS-BAL-QUALIFY-SW       PIC X(01) VALUE "N".
               88  WS-BAL-QUALIFY               VALUE "Y".
               88  WS-BAL-NOT-QUALIFY           VALUE "N".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           DISPLAY "KZ150B START"
           PERFORM 1000-OPEN-FILES
           IF WS-NORMAL
               PERFORM 2000-PROCESS-FILES
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-ABEND
               MOVE 12 TO RETURN-CODE
               DISPLAY "KZ150B ABEND RC=12"
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "KZ150B NORMAL END"
               DISPLAY "ACCT COUNT=" WS-READ-ACCT-CNT
               DISPLAY "OUT COUNT=" WS-WRITE-CRL-CNT
               DISPLAY "RAISE COUNT=" WS-RAISE-CNT
               DISPLAY "HOLD COUNT=" WS-HOLD-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZACCTF
           IF WS-ACCT-ST NOT = "00"
               DISPLAY "KZACCTF OPEN ERROR ST=" WS-ACCT-ST
               SET WS-ABEND TO TRUE
           END-IF

           IF WS-NORMAL
               OPEN INPUT KZCUSTF
               IF WS-CUST-ST NOT = "00"
                   DISPLAY "KZCUSTF OPEN ERROR ST=" WS-CUST-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT KZCRLF
               IF WS-CRL-ST NOT = "00"
                   DISPLAY "KZCRLF OPEN ERROR ST=" WS-CRL-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.

       2000-PROCESS-FILES.
           SET WS-NOT-EOF TO TRUE
           PERFORM UNTIL WS-EOF OR WS-ABEND
               PERFORM 2100-READ-ACCOUNT
               IF WS-NOT-EOF AND WS-NORMAL
                   PERFORM 3000-PROCESS-ACCOUNT
               END-IF
           END-PERFORM.

       2100-READ-ACCOUNT.
           READ KZACCTF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-ACCT-CNT
           END-READ
           IF WS-ACCT-ST NOT = "00" AND WS-ACCT-ST NOT = "10"
               DISPLAY "KZACCTF READ ERROR ST=" WS-ACCT-ST
               SET WS-ABEND TO TRUE
           END-IF.

       3000-PROCESS-ACCOUNT.
           SET WS-BAL-NOT-QUALIFY TO TRUE
           IF AC-AVG-BAL >= WS-BAL-THRESHOLD
               SET WS-BAL-QUALIFY TO TRUE
           END-IF

           MOVE AC-CUST-ID TO CU-CUST-ID
           READ KZCUSTF
               INVALID KEY
                   DISPLAY "KZCUSTF CUSTOMER NOT FOUND"
                   DISPLAY "CUSTOMER ID=" AC-CUST-ID
                   DISPLAY "KZCUSTF READ ST=" WS-CUST-ST
                   SET WS-ABEND TO TRUE
           END-READ

           IF WS-NORMAL
               IF WS-CUST-ST NOT = "00"
                   DISPLAY "KZCUSTF READ ERROR ST=" WS-CUST-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               PERFORM 3100-VALIDATE-CUSTOMER
           END-IF

           IF WS-NORMAL
               PERFORM 3200-BUILD-RESULT
               PERFORM 3300-WRITE-RESULT
           END-IF.

       3100-VALIDATE-CUSTOMER.
           IF CU-KYC-STATUS NOT = WS-KYC-OK
              AND CU-KYC-STATUS NOT = WS-KYC-NG
              AND CU-KYC-STATUS NOT = WS-KYC-HOLD
               DISPLAY "KZCUSTF KYC STATUS ERROR"
               DISPLAY "CUSTOMER ID=" CU-CUST-ID
               DISPLAY "KYC STATUS=" CU-KYC-STATUS
               SET WS-ABEND TO TRUE
           END-IF.

       3200-BUILD-RESULT.
           INITIALIZE KZCRLF-REC
           MOVE AC-ACCT-NO       TO CR-ACCT-NO
           MOVE AC-CREDIT-LIMIT  TO CR-OLD-LIMIT
           MOVE AC-CREDIT-LIMIT  TO CR-NEW-LIMIT
           MOVE WS-FLAG-NO       TO CR-RAISE-FLAG
           MOVE CU-KYC-STATUS    TO CR-KYC-STATUS

           IF WS-BAL-QUALIFY
               IF CU-KYC-STATUS = WS-KYC-OK
                   MOVE WS-RAISED-LIMIT TO CR-NEW-LIMIT
                   MOVE WS-FLAG-YES     TO CR-RAISE-FLAG
                   MOVE WS-RSN-RAISE    TO CR-REASON
                   ADD 1 TO WS-RAISE-CNT
               ELSE
                   MOVE WS-RSN-HKYC     TO CR-REASON
                   ADD 1 TO WS-HOLD-CNT
               END-IF
           ELSE
               MOVE WS-RSN-HBAL         TO CR-REASON
               ADD 1 TO WS-HOLD-CNT
           END-IF.

       3300-WRITE-RESULT.
           WRITE KZCRLF-REC
           IF WS-CRL-ST = "00"
               ADD 1 TO WS-WRITE-CRL-CNT
           ELSE
               DISPLAY "KZCRLF WRITE ERROR ST=" WS-CRL-ST
               DISPLAY "ACCOUNT NO=" CR-ACCT-NO
               SET WS-ABEND TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE KZACCTF
           IF WS-ACCT-ST NOT = "00"
              AND WS-ACCT-ST NOT = "42"
               DISPLAY "KZACCTF CLOSE ERROR ST=" WS-ACCT-ST
               SET WS-ABEND TO TRUE
           END-IF

           CLOSE KZCUSTF
           IF WS-CUST-ST NOT = "00"
              AND WS-CUST-ST NOT = "42"
               DISPLAY "KZCUSTF CLOSE ERROR ST=" WS-CUST-ST
               SET WS-ABEND TO TRUE
           END-IF

           CLOSE KZCRLF
           IF WS-CRL-ST NOT = "00"
              AND WS-CRL-ST NOT = "42"
               DISPLAY "KZCRLF CLOSE ERROR ST=" WS-CRL-ST
               SET WS-ABEND TO TRUE
           END-IF.
