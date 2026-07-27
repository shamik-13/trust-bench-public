       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ195B.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数  年月日      担当                    概要                *
      * 1.00  令和06年05月10日 システム部 勘定系チーム 初版作成      *
      * 1.01  令和06年11月05日 システム部 勘定系チーム 表示文言整備  *
      *---------------------------------------------------------------*
       AUTHOR.     システム部 勘定系チーム.
       DATE-WRITTEN. 2024-05-10.
      *---------------------------------------------------------------*
      * 勘定残高補正投入バッチ                                      *
      * KZBALERの検査済み差額明細から補正TRANを作成する。           *
      * 本版は投入判定前の現行成果物であり、検査時点の口座状態      *
      * との厳密な突合情報は保持していない。                        *
      *---------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZBALER
               ASSIGN       TO "KZBALER"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-BALER-STAT.

           SELECT KZACCTF
               ASSIGN       TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS AC-ACCT-ID
               FILE STATUS  IS WS-ACCT-STAT.

           SELECT KZTRANF
               ASSIGN       TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-TRAN-STAT.

       DATA DIVISION.
       FILE SECTION.

       FD  KZBALER.
       COPY KZBALERC.

       FD  KZACCTF.
       COPY KZACCTC.

       FD  KZTRANF.
       COPY KZTRANC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-BALER-STAT        PIC XX VALUE SPACES.
           05 WS-ACCT-STAT         PIC XX VALUE SPACES.
           05 WS-TRAN-STAT         PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-SW            PIC X VALUE "N".
              88 BALER-EOF               VALUE "Y".
              88 BALER-NOT-EOF           VALUE "N".
           05 WS-ACCOUNT-FOUND-SW  PIC X VALUE "N".
              88 ACCOUNT-FOUND           VALUE "Y".
              88 ACCOUNT-NOT-FOUND       VALUE "N".
           05 WS-ELIGIBLE-SW       PIC X VALUE "N".
              88 ELIGIBLE-TRAN           VALUE "Y".
              88 NOT-ELIGIBLE-TRAN       VALUE "N".

       01  WS-COUNTERS.
           05 WS-READ-CNT          PIC 9(9) VALUE ZERO.
           05 WS-ACCT-HIT-CNT      PIC 9(9) VALUE ZERO.
           05 WS-WRITE-CNT         PIC 9(9) VALUE ZERO.
           05 WS-SKIP-CNT          PIC 9(9) VALUE ZERO.
           05 WS-ERR-CNT           PIC 9(9) VALUE ZERO.

       01  WS-WORK-AREA.
           05 WS-TRAN-CD           PIC X(04) VALUE SPACES.
           05 WS-RATE              PIC 9V9(4) VALUE ZERO.
           05 WS-ABS-DIFF          PIC S9(11)V99 COMP-3 VALUE ZERO.
           05 WS-NEW-BAL           PIC S9(11)V99 COMP-3 VALUE ZERO.
           05 WS-LIMIT-WORK        PIC S9(11)V99 COMP-3 VALUE ZERO.
           05 WS-OVER-WORK         PIC S9(11)V99 COMP-3 VALUE ZERO.
           05 WS-CURRENT-DIFF      PIC S9(11)V99 COMP-3 VALUE ZERO.

       01  WS-CONSTANTS.
           05 WS-STD-GROUP         PIC X(04) VALUE "STD0".
           05 WS-GLD-GROUP         PIC X(04) VALUE "GLD1".
           05 WS-PLT-GROUP         PIC X(04) VALUE "PLT2".
           05 WS-EXM-GROUP         PIC X(04) VALUE "EXMP".
           05 WS-PRM-GROUP         PIC X(04) VALUE "PREM".
           05 WS-STD-RATE          PIC 9V9(4) VALUE 0.0150.
           05 WS-GLD-RATE          PIC 9V9(4) VALUE 0.0100.
           05 WS-PLT-RATE          PIC 9V9(4) VALUE 0.0080.
           05 WS-ZERO-RATE         PIC 9V9(4) VALUE 0.0000.
           05 WS-PGM-ID            PIC X(08) VALUE "KZ195B  ".

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM 1000-OPEN-FILES
           PERFORM 2000-READ-BALER

           PERFORM UNTIL BALER-EOF
               ADD 1 TO WS-READ-CNT
               PERFORM 3000-PROCESS-DETAIL
               PERFORM 2000-READ-BALER
           END-PERFORM

           PERFORM 9000-CLOSE-FILES
           DISPLAY "KZ195B 読込件数=" WS-READ-CNT
           DISPLAY "KZ195B 口座該当=" WS-ACCT-HIT-CNT
           DISPLAY "KZ195B 書込件数=" WS-WRITE-CNT
           DISPLAY "KZ195B 除外件数=" WS-SKIP-CNT
           DISPLAY "KZ195B エラー数=" WS-ERR-CNT
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT  KZBALER
                INPUT  KZACCTF
                OUTPUT KZTRANF

           IF WS-BALER-STAT NOT = "00"
               DISPLAY "KZ195B OPEN KZBALER 状態 " WS-BALER-STAT
               STOP RUN
           END-IF

           IF WS-ACCT-STAT NOT = "00"
               DISPLAY "KZ195B OPEN KZACCTF 状態 " WS-ACCT-STAT
               STOP RUN
           END-IF

           IF WS-TRAN-STAT NOT = "00"
               DISPLAY "KZ195B OPEN KZTRANF 状態 " WS-TRAN-STAT
               STOP RUN
           END-IF.

       2000-READ-BALER.
           READ KZBALER
               AT END
                   SET BALER-EOF TO TRUE
               NOT AT END
                   IF WS-BALER-STAT NOT = "00"
                       DISPLAY "KZ195B READ KZBALER 状態 "
                               WS-BALER-STAT
                       ADD 1 TO WS-ERR-CNT
                       SET BALER-EOF TO TRUE
                   END-IF
           END-READ.

       3000-PROCESS-DETAIL.
           SET NOT-ELIGIBLE-TRAN TO TRUE
           SET ACCOUNT-NOT-FOUND TO TRUE

           PERFORM 3100-VALIDATE-BALER

           IF ELIGIBLE-TRAN
               PERFORM 3200-READ-ACCOUNT
           END-IF

           IF ELIGIBLE-TRAN AND ACCOUNT-FOUND
               PERFORM 3300-RECHECK-REASON
           END-IF

           IF ELIGIBLE-TRAN AND ACCOUNT-FOUND
               PERFORM 3400-CHECK-CURRENT-STATE
           END-IF

           IF ELIGIBLE-TRAN AND ACCOUNT-FOUND
               PERFORM 3500-WRITE-TRAN
           ELSE
               ADD 1 TO WS-SKIP-CNT
           END-IF.

       3100-VALIDATE-BALER.
      *    検査済み差額明細だけを補正候補とする。
           IF BE-ACCT-ID = SPACES
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           IF BE-SOURCE-PGM NOT = "KZBALER"
               EXIT PARAGRAPH
           END-IF

           IF BE-DIFF-AMT = ZERO
               EXIT PARAGRAPH
           END-IF

           IF BE-ERROR-CD = "B101" OR
              BE-ERROR-CD = "B102" OR
              BE-ERROR-CD = "B201" OR
              BE-ERROR-CD = "B301"
               SET ELIGIBLE-TRAN TO TRUE
           END-IF.

       3200-READ-ACCOUNT.
           MOVE BE-ACCT-ID TO AC-ACCT-ID
           READ KZACCTF KEY IS AC-ACCT-ID
               INVALID KEY
                   SET ACCOUNT-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   SET ACCOUNT-FOUND TO TRUE
                   ADD 1 TO WS-ACCT-HIT-CNT
           END-READ

           IF WS-ACCT-STAT NOT = "00" AND
              WS-ACCT-STAT NOT = "23"
               DISPLAY "KZ195B READ KZACCTF 状態 "
                       WS-ACCT-STAT " 口座 " BE-ACCT-ID
               ADD 1 TO WS-ERR-CNT
               SET NOT-ELIGIBLE-TRAN TO TRUE
           END-IF.

       3300-RECHECK-REASON.
      *    KZ181S相当の理由再確認。口座属性で補正可否を再判定する。
           PERFORM 3310-SET-GROUP-RATE

           IF AC-KYC-STATUS NOT = "1"
               SET NOT-ELIGIBLE-TRAN TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AC-GROUP-CODE = WS-EXM-GROUP
               SET NOT-ELIGIBLE-TRAN TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF BE-ERROR-CD = "B101"
               IF AC-CYCLE-BAL >= ZERO
                   SET NOT-ELIGIBLE-TRAN TO TRUE
               ELSE
                   MOVE "ADBD" TO WS-TRAN-CD
               END-IF
           ELSE
               IF BE-ERROR-CD = "B102"
                   IF AC-OVER-AMT <= ZERO
                       SET NOT-ELIGIBLE-TRAN TO TRUE
                   ELSE
                       MOVE "ADOV" TO WS-TRAN-CD
                   END-IF
               ELSE
                   IF BE-ERROR-CD = "B201"
                       IF WS-RATE = ZERO
                           SET NOT-ELIGIBLE-TRAN TO TRUE
                       ELSE
                           MOVE "ADRT" TO WS-TRAN-CD
                       END-IF
                   ELSE
                       MOVE "ADJS" TO WS-TRAN-CD
                   END-IF
               END-IF
           END-IF.

       3310-SET-GROUP-RATE.
           EVALUATE AC-GROUP-CODE
               WHEN WS-STD-GROUP
                   MOVE WS-STD-RATE  TO WS-RATE
               WHEN WS-GLD-GROUP
                   MOVE WS-GLD-RATE  TO WS-RATE
               WHEN WS-PLT-GROUP
                   MOVE WS-PLT-RATE  TO WS-RATE
               WHEN WS-EXM-GROUP
                   MOVE WS-ZERO-RATE TO WS-RATE
               WHEN WS-PRM-GROUP
                   MOVE 0.0080       TO WS-RATE
               WHEN OTHER
                   MOVE WS-STD-RATE  TO WS-RATE
           END-EVALUATE.

       3400-CHECK-CURRENT-STATE.
      *    検査時点の詳細残高を保持しないため、現残高から実害のみ確認。
           COMPUTE WS-NEW-BAL = AC-CYCLE-BAL + BE-DIFF-AMT
           MOVE AC-CREDIT-LIMIT TO WS-LIMIT-WORK
           MOVE AC-OVER-AMT     TO WS-OVER-WORK
           COMPUTE WS-CURRENT-DIFF = WS-NEW-BAL - AC-CYCLE-BAL

           IF WS-CURRENT-DIFF NOT = BE-DIFF-AMT
               SET NOT-ELIGIBLE-TRAN TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF WS-NEW-BAL > WS-LIMIT-WORK AND BE-DIFF-AMT > ZERO
               SET NOT-ELIGIBLE-TRAN TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF WS-OVER-WORK > ZERO AND BE-DIFF-AMT > ZERO
               IF WS-NEW-BAL > AC-CYCLE-BAL
                   SET NOT-ELIGIBLE-TRAN TO TRUE
               END-IF
           END-IF.

       3500-WRITE-TRAN.
           MOVE BE-ACCT-ID  TO TR-ACCT-ID
           MOVE WS-TRAN-CD  TO TR-TRAN-CD

           IF BE-DIFF-AMT < ZERO
               COMPUTE WS-ABS-DIFF = BE-DIFF-AMT * -1
           ELSE
               MOVE BE-DIFF-AMT TO WS-ABS-DIFF
           END-IF

           MOVE WS-ABS-DIFF TO TR-TRAN-AMT

           WRITE KZTRANF-REC
           IF WS-TRAN-STAT = "00"
               ADD 1 TO WS-WRITE-CNT
           ELSE
               DISPLAY "KZ195B WRITE KZTRANF 状態 "
                       WS-TRAN-STAT " 口座 " BE-ACCT-ID
               ADD 1 TO WS-ERR-CNT
               STOP RUN
           END-IF.

       9000-CLOSE-FILES.
           CLOSE KZBALER KZACCTF KZTRANF

           IF WS-BALER-STAT NOT = "00"
               DISPLAY "KZ195B CLOSE KZBALER 状態 " WS-BALER-STAT
           END-IF

           IF WS-ACCT-STAT NOT = "00"
               DISPLAY "KZ195B CLOSE KZACCTF 状態 " WS-ACCT-STAT
           END-IF

           IF WS-TRAN-STAT NOT = "00"
               DISPLAY "KZ195B CLOSE KZTRANF 状態 " WS-TRAN-STAT
           END-IF.
