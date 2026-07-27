       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ230B.
      * 版数  年月日(和暦)  担当                         概要
      * 1.00  令和04年04月01日 システム部 勘定系チーム  新規作成
      * 1.01  令和05年10月16日 システム部 勘定系チーム  限度利用率判定追加
      * 1.02  令和06年07月08日 システム部 勘定系チーム  帳票集計条件見直し
      ******************************************************************
      * LIMIT USAGE RECONCILIATION BATCH
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZLUTLF ASSIGN TO "KZLUTLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LU-ST.

           SELECT KZCRLF ASSIGN TO "KZCRLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CR-ST.

           SELECT KZEXPRF ASSIGN TO "KZEXPRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-XR-ST.

           SELECT KZSCORF ASSIGN TO "KZSCORF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SC-CUST-ID
               FILE STATUS IS WS-SC-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZLUTLF.
           COPY KZLUTLFC.

       FD  KZCRLF.
           COPY KZCRLFC.

       FD  KZEXPRF.
           COPY KZEXPRC.

       FD  KZSCORF.
           COPY KZSCORFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LU-ST                 PIC XX VALUE SPACES.
           05 WS-CR-ST                 PIC XX VALUE SPACES.
           05 WS-XR-ST                 PIC XX VALUE SPACES.
           05 WS-SC-ST                 PIC XX VALUE SPACES.

       01  WS-END-FLAGS.
           05 WS-LU-END                PIC X VALUE "N".
           05 WS-CR-END                PIC X VALUE "N".
           05 WS-XR-END                PIC X VALUE "N".

       01  WS-COUNTERS.
           05 WS-LU-CNT                PIC 9(9) VALUE ZERO.
           05 WS-CR-CNT                PIC 9(9) VALUE ZERO.
           05 WS-XR-CNT                PIC 9(9) VALUE ZERO.
           05 WS-EX-CNT                PIC 9(9) VALUE ZERO.
           05 WS-IDX                   PIC 9(5) VALUE ZERO.
           05 WS-CR-HIT                PIC 9(5) VALUE ZERO.
           05 WS-XR-HIT                PIC 9(5) VALUE ZERO.

       01  WS-LIMITS.
           05 WS-CR-MAX                PIC 9(5) VALUE 10000.
           05 WS-XR-MAX                PIC 9(5) VALUE 10000.

       01  WS-WORK.
           05 WS-UTIL-CALC             PIC 9(5)V99 VALUE ZERO.
           05 WS-TOTAL-EXPOSURE        PIC S9(15)V99 VALUE ZERO.
           05 WS-CAPPED-EXPOSURE       PIC S9(15)V99 VALUE ZERO.
           05 WS-REASON                PIC X(80) VALUE SPACES.
           05 WS-HARD-ERROR            PIC X VALUE "N".

       01  WS-CR-TABLE.
           05 WS-CR-ENTRY OCCURS 10000 TIMES.
              10 T-CR-ACCT-NO          PIC X(20).
              10 T-CR-OLD-LIMIT        PIC S9(13)V99.
              10 T-CR-NEW-LIMIT        PIC S9(13)V99.
              10 T-CR-RAISE-FLAG       PIC X.
              10 T-CR-KYC-STATUS       PIC X(10).
              10 T-CR-REASON           PIC X(80).

       01  WS-XR-TABLE.
           05 WS-XR-ENTRY OCCURS 10000 TIMES.
              10 T-XR-CUST-ID          PIC X(20).
              10 T-XR-EXPOSURE-AMT     PIC S9(15)V99.
              10 T-XR-CAPPED-AMT       PIC S9(15)V99.
              10 T-XR-OVER-FLAG        PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-INPUTS
           IF WS-HARD-ERROR = "N"
              PERFORM 2000-LOAD-CR
           END-IF
           IF WS-HARD-ERROR = "N"
              PERFORM 3000-LOAD-XR
           END-IF
           IF WS-HARD-ERROR = "N"
              PERFORM 4000-PROCESS-LU
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-HARD-ERROR = "Y"
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "KZ230B OK LU=" WS-LU-CNT
                      " EX=" WS-EX-CNT
           END-IF
           GOBACK.

       1000-OPEN-INPUTS.
           OPEN INPUT KZCRLF
           IF WS-CR-ST NOT = "00"
              DISPLAY "KZCRLF OPEN INPUT ERROR ST=" WS-CR-ST
              MOVE "Y" TO WS-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZEXPRF
           IF WS-XR-ST NOT = "00"
              DISPLAY "KZEXPRF OPEN ERROR ST=" WS-XR-ST
              MOVE "Y" TO WS-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZLUTLF
           IF WS-LU-ST NOT = "00"
              DISPLAY "KZLUTLF OPEN ERROR ST=" WS-LU-ST
              MOVE "Y" TO WS-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZSCORF
           IF WS-SC-ST NOT = "00"
              DISPLAY "KZSCORF OPEN ERROR ST=" WS-SC-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       2000-LOAD-CR.
           PERFORM UNTIL WS-CR-END = "Y" OR WS-HARD-ERROR = "Y"
              READ KZCRLF
                 AT END
                    MOVE "Y" TO WS-CR-END
                 NOT AT END
                    ADD 1 TO WS-CR-CNT
                    IF WS-CR-CNT > WS-CR-MAX
                       DISPLAY "KZCRLF TOO MANY RECORDS"
                       MOVE "Y" TO WS-HARD-ERROR
                    ELSE
                       MOVE CR-ACCT-NO    TO T-CR-ACCT-NO
                                            (WS-CR-CNT)
                       MOVE CR-OLD-LIMIT  TO T-CR-OLD-LIMIT
                                            (WS-CR-CNT)
                       MOVE CR-NEW-LIMIT  TO T-CR-NEW-LIMIT
                                            (WS-CR-CNT)
                       MOVE CR-RAISE-FLAG TO T-CR-RAISE-FLAG
                                            (WS-CR-CNT)
                       MOVE CR-KYC-STATUS TO T-CR-KYC-STATUS
                                            (WS-CR-CNT)
                       MOVE CR-REASON     TO T-CR-REASON
                                            (WS-CR-CNT)
                    END-IF
              END-READ
           END-PERFORM

           CLOSE KZCRLF
           IF WS-CR-ST NOT = "00"
              DISPLAY "KZCRLF CLOSE INPUT ERROR ST=" WS-CR-ST
              MOVE "Y" TO WS-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN EXTEND KZCRLF
           IF WS-CR-ST NOT = "00"
              DISPLAY "KZCRLF OPEN EXTEND ERROR ST=" WS-CR-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       3000-LOAD-XR.
           PERFORM UNTIL WS-XR-END = "Y" OR WS-HARD-ERROR = "Y"
              READ KZEXPRF
                 AT END
                    MOVE "Y" TO WS-XR-END
                 NOT AT END
                    PERFORM 3100-ADD-XR
              END-READ
           END-PERFORM.

       3100-ADD-XR.
           MOVE ZERO TO WS-XR-HIT
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-XR-CNT OR WS-XR-HIT > ZERO
              IF T-XR-CUST-ID(WS-IDX) = XR-CUST-ID
                 MOVE WS-IDX TO WS-XR-HIT
              END-IF
           END-PERFORM

           IF WS-XR-HIT = ZERO
              IF WS-XR-CNT >= WS-XR-MAX
                 DISPLAY "KZEXPRF TOO MANY CUSTOMERS"
                 MOVE "Y" TO WS-HARD-ERROR
              ELSE
                 ADD 1 TO WS-XR-CNT
                 MOVE XR-CUST-ID      TO T-XR-CUST-ID
                                      (WS-XR-CNT)
                 MOVE XR-EXPOSURE-AMT TO T-XR-EXPOSURE-AMT
                                      (WS-XR-CNT)
                 MOVE XR-CAPPED-AMT   TO T-XR-CAPPED-AMT
                                      (WS-XR-CNT)
                 MOVE XR-OVER-FLAG    TO T-XR-OVER-FLAG
                                      (WS-XR-CNT)
              END-IF
           ELSE
              ADD XR-EXPOSURE-AMT TO T-XR-EXPOSURE-AMT
                                    (WS-XR-HIT)
              ADD XR-CAPPED-AMT   TO T-XR-CAPPED-AMT
                                    (WS-XR-HIT)
              IF XR-OVER-FLAG = "Y"
                 MOVE "Y" TO T-XR-OVER-FLAG(WS-XR-HIT)
              END-IF
           END-IF.

       4000-PROCESS-LU.
           PERFORM UNTIL WS-LU-END = "Y" OR WS-HARD-ERROR = "Y"
              READ KZLUTLF
                 AT END
                    MOVE "Y" TO WS-LU-END
                 NOT AT END
                    ADD 1 TO WS-LU-CNT
                    PERFORM 4100-VALIDATE-LU
                    IF WS-REASON = SPACES
                       PERFORM 4200-CHECK-CR
                    END-IF
                    IF WS-REASON = SPACES
                       PERFORM 4300-CHECK-XR
                    END-IF
                    IF WS-REASON = SPACES
                       PERFORM 4400-CHECK-SC
                    END-IF
                    IF WS-REASON NOT = SPACES
                       PERFORM 8000-WRITE-EXCEPTION
                    END-IF
              END-READ
           END-PERFORM.

       4100-VALIDATE-LU.
           MOVE SPACES TO WS-REASON
           IF LU-ACCT-NO = SPACES
              MOVE "ACCT-NO MISSING" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF LU-CUST-ID = SPACES
              MOVE "CUST-ID MISSING" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF LU-CREDIT-LIMIT < ZERO
              MOVE "CREDIT-LIMIT INVALID" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF LU-CUR-BAL < ZERO
              MOVE "CURRENT-BALANCE INVALID" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF LU-CREDIT-LIMIT = ZERO
              IF LU-CUR-BAL > ZERO
                 MOVE "ZERO LIMIT WITH BALANCE" TO WS-REASON
              END-IF
              EXIT PARAGRAPH
           END-IF

           COMPUTE WS-UTIL-CALC ROUNDED =
              (LU-CUR-BAL / LU-CREDIT-LIMIT) * 100
           IF WS-UTIL-CALC > 999.99
              MOVE 999.99 TO WS-UTIL-CALC
           END-IF
           IF LU-UTIL-RATE > WS-UTIL-CALC + 0.10
              MOVE "UTIL RATE TOO HIGH" TO WS-REASON
           END-IF
           IF LU-UTIL-RATE < WS-UTIL-CALC - 0.10
              MOVE "UTIL RATE TOO LOW" TO WS-REASON
           END-IF
           IF LU-WARN-FLAG = "Y" AND LU-UTIL-RATE < 80
              MOVE "WARN FLAG MISMATCH" TO WS-REASON
           END-IF.

       4200-CHECK-CR.
           MOVE ZERO TO WS-CR-HIT
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-CR-CNT OR WS-CR-HIT > ZERO
              IF T-CR-ACCT-NO(WS-IDX) = LU-ACCT-NO
                 MOVE WS-IDX TO WS-CR-HIT
              END-IF
           END-PERFORM

           IF WS-CR-HIT = ZERO
              MOVE "CREDIT REVIEW NOT MATCHED" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF T-CR-KYC-STATUS(WS-CR-HIT) NOT = "OK"
              MOVE "KYC STATUS INVALID" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF T-CR-NEW-LIMIT(WS-CR-HIT) < LU-CUR-BAL
              MOVE "NEW LIMIT BELOW BALANCE" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF T-CR-RAISE-FLAG(WS-CR-HIT) = "Y"
              IF T-CR-NEW-LIMIT(WS-CR-HIT)
                 <= T-CR-OLD-LIMIT(WS-CR-HIT)
                 MOVE "RAISE FLAG MISMATCH" TO WS-REASON
              END-IF
           END-IF.

       4300-CHECK-XR.
           MOVE ZERO TO WS-XR-HIT
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-XR-CNT OR WS-XR-HIT > ZERO
              IF T-XR-CUST-ID(WS-IDX) = LU-CUST-ID
                 MOVE WS-IDX TO WS-XR-HIT
              END-IF
           END-PERFORM

           IF WS-XR-HIT = ZERO
              MOVE "EXPOSURE NOT MATCHED" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           MOVE T-XR-EXPOSURE-AMT(WS-XR-HIT) TO WS-TOTAL-EXPOSURE
           MOVE T-XR-CAPPED-AMT(WS-XR-HIT)   TO WS-CAPPED-EXPOSURE
           IF WS-TOTAL-EXPOSURE < LU-CUR-BAL
              MOVE "EXPOSURE BELOW BALANCE" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF WS-CAPPED-EXPOSURE > T-CR-NEW-LIMIT(WS-CR-HIT)
              MOVE "CAPPED EXPOSURE OVER LIMIT" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF T-XR-OVER-FLAG(WS-XR-HIT) = "Y"
              MOVE "TOTAL CREDIT OVER" TO WS-REASON
           END-IF.

       4400-CHECK-SC.
           MOVE LU-CUST-ID TO SC-CUST-ID
           READ KZSCORF
              INVALID KEY
                 MOVE "SCORE NOT MATCHED" TO WS-REASON
              NOT INVALID KEY
                 IF SC-GRADE-CODE = SPACES
                    MOVE "GRADE CODE MISSING" TO WS-REASON
                 END-IF
                 IF SC-SCORE-POINT < 300
                    MOVE "SCORE POINT TOO LOW" TO WS-REASON
                 END-IF
                 IF SC-INPUT-HASH = SPACES
                    MOVE "SCORE HASH MISSING" TO WS-REASON
                 END-IF
           END-READ
           IF WS-SC-ST NOT = "00" AND WS-SC-ST NOT = "23"
              DISPLAY "KZSCORF READ ERROR ST=" WS-SC-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       8000-WRITE-EXCEPTION.
           INITIALIZE KZCRLF-REC
           MOVE LU-ACCT-NO      TO CR-ACCT-NO
           MOVE LU-CREDIT-LIMIT TO CR-OLD-LIMIT
           MOVE LU-CREDIT-LIMIT TO CR-NEW-LIMIT
           MOVE "N"             TO CR-RAISE-FLAG
           MOVE "NG"            TO CR-KYC-STATUS
           MOVE WS-REASON       TO CR-REASON
           WRITE KZCRLF-REC
           IF WS-CR-ST NOT = "00"
              DISPLAY "KZCRLF WRITE ERROR ST=" WS-CR-ST
              MOVE "Y" TO WS-HARD-ERROR
           ELSE
              ADD 1 TO WS-EX-CNT
           END-IF.

       9000-CLOSE-FILES.
           IF WS-LU-ST = "00"
              CLOSE KZLUTLF
              IF WS-LU-ST NOT = "00"
                 DISPLAY "KZLUTLF CLOSE ERROR ST=" WS-LU-ST
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-IF

           IF WS-XR-ST = "00"
              CLOSE KZEXPRF
              IF WS-XR-ST NOT = "00"
                 DISPLAY "KZEXPRF CLOSE ERROR ST=" WS-XR-ST
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-IF

           IF WS-SC-ST = "00"
              CLOSE KZSCORF
              IF WS-SC-ST NOT = "00"
                 DISPLAY "KZSCORF CLOSE ERROR ST=" WS-SC-ST
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-IF

           IF WS-CR-ST = "00"
              CLOSE KZCRLF
              IF WS-CR-ST NOT = "00"
                 DISPLAY "KZCRLF CLOSE ERROR ST=" WS-CR-ST
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-IF.
