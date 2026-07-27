       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ103B.
      *
      *変更履歴
      *版数  年月日    担当  概要
      *1.00  20260401  開発  初版作成
      *1.01  20260515  開発  残高検証と障害処理追加
      *1.02  20260618  開発  日次残高更新方式整理
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF
               ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-KZACCTF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC4.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-KZACCTF-ST          PIC X(02) VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-EOF-SW              PIC X(01) VALUE "N".
               88  EOF-KZACCTF                  VALUE "Y".
               88  NOT-EOF-KZACCTF              VALUE "N".
           05  WS-ABEND-SW            PIC X(01) VALUE "N".
               88  ABEND-FOUND                  VALUE "Y".
               88  NO-ABEND                     VALUE "N".
           05  WS-VALID-SW            PIC X(01) VALUE "Y".
               88  VALID-ACCOUNT                VALUE "Y".
               88  INVALID-ACCOUNT              VALUE "N".

       01  WS-COUNTERS.
           05  WS-READ-CNT            PIC 9(09) VALUE ZERO.
           05  WS-REWRITE-CNT         PIC 9(09) VALUE ZERO.
           05  WS-SKIP-CNT            PIC 9(09) VALUE ZERO.
           05  WS-ERR-CNT             PIC 9(09) VALUE ZERO.
           05  WS-IDX                 PIC 9(04) VALUE ZERO.
           05  WS-ACCT-LEN            PIC 9(04) VALUE ZERO.

       01  WS-CALC-AREA.
           05  WS-SEED                PIC 9(09) VALUE ZERO.
           05  WS-DAY-POST            PIC S9(11)V99 VALUE ZERO.
           05  WS-OLD-CUR-BAL         PIC S9(13)V99 VALUE ZERO.
           05  WS-OLD-AVG-BAL         PIC S9(13)V99 VALUE ZERO.
           05  WS-NEW-CUR-BAL         PIC S9(13)V99 VALUE ZERO.
           05  WS-NEW-AVG-BAL         PIC S9(13)V99 VALUE ZERO.
           05  WS-CREDIT-LIMIT        PIC S9(13)V99 VALUE ZERO.
           05  WS-LIMIT-MINUS         PIC S9(13)V99 VALUE ZERO.

       01  WS-MESSAGE-AREA.
           05  WS-DISP-COUNT          PIC Z(9).
           05  WS-DISP-AMOUNT         PIC -(13)9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILE
           IF NO-ABEND
               PERFORM 2000-PROCESS
                   UNTIL EOF-KZACCTF OR ABEND-FOUND
           END-IF
           PERFORM 9000-CLOSE-FILE
           IF ABEND-FOUND
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               PERFORM 9100-DISPLAY-SUMMARY
           END-IF
           GOBACK.

       1000-OPEN-FILE.
           OPEN I-O KZACCTF
           IF WS-KZACCTF-ST NOT = "00"
               DISPLAY "KZACCTF OPEN ERROR ST="
               DISPLAY WS-KZACCTF-ST
               SET ABEND-FOUND TO TRUE
           END-IF.

       2000-PROCESS.
           READ KZACCTF NEXT RECORD
               AT END
                   SET EOF-KZACCTF TO TRUE
               NOT AT END
                   IF WS-KZACCTF-ST = "00"
                       ADD 1 TO WS-READ-CNT
                       PERFORM 3000-VALIDATE-ACCOUNT
                       IF VALID-ACCOUNT
                           PERFORM 4000-UPDATE-BALANCE
                       END-IF
                   ELSE
                       DISPLAY "KZACCTF READ ERROR ST="
                       DISPLAY WS-KZACCTF-ST
                       SET ABEND-FOUND TO TRUE
                   END-IF
           END-READ.

       3000-VALIDATE-ACCOUNT.
           SET VALID-ACCOUNT TO TRUE
           IF AC-ACCT-NO = SPACES
               DISPLAY "ACCOUNT NO ERROR"
               ADD 1 TO WS-SKIP-CNT
               SET INVALID-ACCOUNT TO TRUE
               GO TO 3000-EXIT
           END-IF
           IF AC-CUST-ID = SPACES
               DISPLAY "CUSTOMER ID ERROR"
               DISPLAY AC-ACCT-NO
               ADD 1 TO WS-SKIP-CNT
               SET INVALID-ACCOUNT TO TRUE
               GO TO 3000-EXIT
           END-IF
           IF AC-CUR-BAL NOT NUMERIC
               DISPLAY "CURRENT BALANCE ERROR"
               DISPLAY AC-ACCT-NO
               ADD 1 TO WS-ERR-CNT
               SET INVALID-ACCOUNT TO TRUE
               SET ABEND-FOUND TO TRUE
               GO TO 3000-EXIT
           END-IF
           IF AC-AVG-BAL NOT NUMERIC
               DISPLAY "AVERAGE BALANCE ERROR"
               DISPLAY AC-ACCT-NO
               ADD 1 TO WS-ERR-CNT
               SET INVALID-ACCOUNT TO TRUE
               SET ABEND-FOUND TO TRUE
               GO TO 3000-EXIT
           END-IF
           IF AC-CREDIT-LIMIT NOT NUMERIC
               DISPLAY "CREDIT LIMIT ERROR"
               DISPLAY AC-ACCT-NO
               ADD 1 TO WS-ERR-CNT
               SET INVALID-ACCOUNT TO TRUE
               SET ABEND-FOUND TO TRUE
               GO TO 3000-EXIT
           END-IF.
       3000-EXIT.
           EXIT.

       4000-UPDATE-BALANCE.
           MOVE AC-CUR-BAL TO WS-OLD-CUR-BAL
           MOVE AC-AVG-BAL TO WS-OLD-AVG-BAL
           MOVE AC-CREDIT-LIMIT TO WS-CREDIT-LIMIT
           PERFORM 4100-MAKE-POSTING
           COMPUTE WS-NEW-CUR-BAL =
               WS-OLD-CUR-BAL + WS-DAY-POST
           COMPUTE WS-NEW-AVG-BAL ROUNDED =
               ((WS-OLD-AVG-BAL * 29) + WS-NEW-CUR-BAL) / 30
           COMPUTE WS-LIMIT-MINUS = WS-CREDIT-LIMIT * -1
           IF WS-CREDIT-LIMIT > ZERO
              AND WS-NEW-CUR-BAL < WS-LIMIT-MINUS
               MOVE WS-NEW-CUR-BAL TO WS-DISP-AMOUNT
               DISPLAY "BALANCE WARNING ACCOUNT="
               DISPLAY AC-ACCT-NO
               DISPLAY "BALANCE="
               DISPLAY WS-DISP-AMOUNT
           END-IF
           MOVE WS-NEW-CUR-BAL TO AC-CUR-BAL
           MOVE WS-NEW-AVG-BAL TO AC-AVG-BAL
           REWRITE KZACCTF-REC
           IF WS-KZACCTF-ST = "00"
               ADD 1 TO WS-REWRITE-CNT
           ELSE
               DISPLAY "KZACCTF REWRITE ERROR ST="
               DISPLAY WS-KZACCTF-ST
               DISPLAY AC-ACCT-NO
               SET ABEND-FOUND TO TRUE
           END-IF.

       4100-MAKE-POSTING.
           MOVE ZERO TO WS-SEED
           MOVE LENGTH OF AC-ACCT-NO TO WS-ACCT-LEN
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-ACCT-LEN
               IF AC-ACCT-NO(WS-IDX:1) NOT = SPACE
                   COMPUTE WS-SEED =
                       WS-SEED
                       + FUNCTION ORD(AC-ACCT-NO(WS-IDX:1))
                       * WS-IDX
               END-IF
           END-PERFORM
           COMPUTE WS-DAY-POST ROUNDED =
               (FUNCTION MOD((WS-SEED * 37), 200001) - 100000)
               / 100.

       9000-CLOSE-FILE.
           IF WS-KZACCTF-ST NOT = SPACES
               CLOSE KZACCTF
               IF WS-KZACCTF-ST NOT = "00"
                  AND WS-KZACCTF-ST NOT = "42"
                   DISPLAY "KZACCTF CLOSE ERROR ST="
                   DISPLAY WS-KZACCTF-ST
                   SET ABEND-FOUND TO TRUE
               END-IF
           END-IF.

       9100-DISPLAY-SUMMARY.
           MOVE WS-READ-CNT TO WS-DISP-COUNT
           DISPLAY "READ COUNT="
           DISPLAY WS-DISP-COUNT
           MOVE WS-REWRITE-CNT TO WS-DISP-COUNT
           DISPLAY "REWRITE COUNT="
           DISPLAY WS-DISP-COUNT
           MOVE WS-SKIP-CNT TO WS-DISP-COUNT
           DISPLAY "SKIP COUNT="
           DISPLAY WS-DISP-COUNT
           MOVE WS-ERR-CNT TO WS-DISP-COUNT
           DISPLAY "ERROR COUNT="
           DISPLAY WS-DISP-COUNT.
