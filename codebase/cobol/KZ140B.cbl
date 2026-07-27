       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ140B.
      *---------------------------------------------------------------*
      *  変更履歴                                                     *
      *  版数  年月日      担当                    概要              *
      *  1.0   平成28年04月 システム部 勘定系チーム 初版作成        *
      *  1.1   令和02年10月 システム部 勘定系チーム 勘定更新対応    *
      *  1.2   令和06年03月 システム部 勘定系チーム 運用表記見直し  *
      *---------------------------------------------------------------*
       AUTHOR. システム部 勘定系チーム.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTRANF ASSIGN TO KZTRANF
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TR-STATUS.

           SELECT KZACCTF ASSIGN TO KZACCTF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-AC-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTRANF.
       COPY KZTRANC.

       FD  KZACCTF.
       COPY KZACCTC.

       WORKING-STORAGE SECTION.
       01  WS-TR-STATUS              PIC XX VALUE SPACES.
       01  WS-AC-STATUS              PIC XX VALUE SPACES.

       01  WS-EOF-SW                 PIC X VALUE 'N'.
           88  WS-END-OF-TRAN              VALUE 'Y'.
           88  WS-MORE-TRAN                VALUE 'N'.

       01  WS-ABEND-SW               PIC X VALUE 'N'.
           88  WS-ABEND                    VALUE 'Y'.
           88  WS-NOT-ABEND                VALUE 'N'.

       01  WS-RETURN-CODE            PIC 9(04) VALUE ZERO.
       01  WS-TRAN-COUNT             PIC 9(09) COMP-3 VALUE ZERO.
       01  WS-UPDATE-COUNT           PIC 9(09) COMP-3 VALUE ZERO.
       01  WS-REJECT-COUNT           PIC 9(09) COMP-3 VALUE ZERO.
       01  WS-NOTFOUND-COUNT         PIC 9(09) COMP-3 VALUE ZERO.
       01  WS-ERROR-COUNT            PIC 9(09) COMP-3 VALUE ZERO.

       01  WS-NEW-BAL                PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-EXCESS                 PIC S9(11)V99 COMP-3 VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL WS-END-OF-TRAN OR WS-ABEND
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT KZTRANF
           IF WS-TR-STATUS NOT = '00'
               PERFORM 9100-FILE-ERROR
           END-IF

           OPEN I-O KZACCTF
           IF WS-AC-STATUS NOT = '00'
               PERFORM 9100-FILE-ERROR
           END-IF

           IF WS-NOT-ABEND
               PERFORM 2100-READ-TRAN
           END-IF.

       2000-PROCESS-TRANSACTIONS.
           ADD 1 TO WS-TRAN-COUNT

           IF TR-ACCT-ID = SPACES
               ADD 1 TO WS-REJECT-COUNT
           ELSE
               PERFORM 3000-APPLY-TRANSACTION
           END-IF

           IF WS-NOT-ABEND
               PERFORM 2100-READ-TRAN
           END-IF.

       2100-READ-TRAN.
           READ KZTRANF
               AT END
                   SET WS-END-OF-TRAN TO TRUE
               NOT AT END
                   IF WS-TR-STATUS NOT = '00'
                       PERFORM 9100-FILE-ERROR
                   END-IF
           END-READ.

       3000-APPLY-TRANSACTION.
           MOVE TR-ACCT-ID TO AC-ACCT-ID

           READ KZACCTF
               INVALID KEY
                   ADD 1 TO WS-NOTFOUND-COUNT
               NOT INVALID KEY
                   PERFORM 3100-VALIDATE-AND-UPDATE
           END-READ.

       3100-VALIDATE-AND-UPDATE.
           IF WS-AC-STATUS NOT = '00'
               PERFORM 9100-FILE-ERROR
           ELSE
               IF AC-KYC-STATUS NOT = 'A'
                   ADD 1 TO WS-REJECT-COUNT
               ELSE
                   PERFORM 3200-CALCULATE-BALANCE
                   IF WS-NOT-ABEND
                       PERFORM 3300-REWRITE-ACCOUNT
                   END-IF
               END-IF
           END-IF.

       3200-CALCULATE-BALANCE.
           EVALUATE TR-TRAN-CD
               WHEN 'D'
                   COMPUTE WS-NEW-BAL =
                       AC-CYCLE-BAL - TR-TRAN-AMT
               WHEN 'P'
                   COMPUTE WS-NEW-BAL =
                       AC-CYCLE-BAL - TR-TRAN-AMT
               WHEN 'C'
                   COMPUTE WS-NEW-BAL =
                       AC-CYCLE-BAL + TR-TRAN-AMT
               WHEN 'F'
                   COMPUTE WS-NEW-BAL =
                       AC-CYCLE-BAL + TR-TRAN-AMT
               WHEN OTHER
                   ADD 1 TO WS-REJECT-COUNT
                   SET WS-ABEND TO TRUE
           END-EVALUATE

           IF WS-NOT-ABEND
               IF WS-NEW-BAL > AC-CREDIT-LIMIT
                   COMPUTE WS-EXCESS =
                       WS-NEW-BAL - AC-CREDIT-LIMIT
               ELSE
                   MOVE ZERO TO WS-EXCESS
               END-IF

               IF WS-EXCESS > AC-CREDIT-LIMIT
                   ADD 1 TO WS-REJECT-COUNT
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.

       3300-REWRITE-ACCOUNT.
           MOVE WS-NEW-BAL TO AC-CYCLE-BAL
           MOVE WS-EXCESS  TO AC-OVER-AMT

           REWRITE KZACCTF-REC
           IF WS-AC-STATUS = '00'
               ADD 1 TO WS-UPDATE-COUNT
           ELSE
               PERFORM 9100-FILE-ERROR
           END-IF.

       9000-TERMINATE.
           CLOSE KZTRANF
           IF WS-TR-STATUS NOT = '00'
               AND WS-TR-STATUS NOT = '42'
               ADD 1 TO WS-ERROR-COUNT
           END-IF

           CLOSE KZACCTF
           IF WS-AC-STATUS NOT = '00'
               AND WS-AC-STATUS NOT = '42'
               ADD 1 TO WS-ERROR-COUNT
           END-IF

           IF WS-ABEND OR WS-ERROR-COUNT > ZERO
               MOVE 16 TO WS-RETURN-CODE
           ELSE
               IF WS-REJECT-COUNT > ZERO
                   OR WS-NOTFOUND-COUNT > ZERO
                   MOVE 4 TO WS-RETURN-CODE
               ELSE
                   MOVE ZERO TO WS-RETURN-CODE
               END-IF
           END-IF

           MOVE WS-RETURN-CODE TO RETURN-CODE.

       9100-FILE-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           SET WS-ABEND TO TRUE.
