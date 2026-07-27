       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ104B.
      *
      * 変更履歴
      * 版数  年月日    担当    概要
      * 0.1   20240216  KZB01   利息発生処理の初版作成
      * 0.2   20240524  KZB02   KZ151S利率引当の呼出を追加
      * 0.3   20240612  KZB03   利率引当サブの戻り値検証を追加
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-ACCT-ST.

           SELECT KZINTRF ASSIGN TO "KZINTRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IR-RATE-CD
               FILE STATUS IS WS-INTR-ST.

           SELECT KZINTAF ASSIGN TO "KZINTAF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-INTA-ST.

           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
       COPY KZACCTC.

       FD  KZINTRF.
       COPY KZINTRFC.

       FD  KZINTAF.
       COPY KZINTAFC.

       FD  KZTRANF.
       COPY KZTRANC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ACCT-ST              PIC XX VALUE SPACE.
           05 WS-INTR-ST              PIC XX VALUE SPACE.
           05 WS-INTA-ST              PIC XX VALUE SPACE.
           05 WS-TRAN-ST              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-EOF-SW               PIC X VALUE "N".
              88 WS-EOF                     VALUE "Y".
              88 WS-NOT-EOF                 VALUE "N".
           05 WS-HARD-ERR-SW          PIC X VALUE "N".
              88 WS-HARD-ERR                VALUE "Y".
              88 WS-NORMAL                  VALUE "N".

       01  WS-CONTROL.
           05 WS-PROCESS-DT           PIC 9(08) VALUE ZERO.
           05 WS-TRAN-CD-INT          PIC X(04) VALUE "IACR".
           05 WS-STD-RATE-CD          PIC X(04) VALUE "STD0".
           05 WS-STD-APR              PIC 9V9(04) VALUE 0.0150.
           05 WS-ZERO-APR             PIC 9V9(04) VALUE 0.0000.
           05 WS-DAYS-YEAR            PIC 9(03) VALUE 365.
           05 WS-HALF-YEN             PIC S9(13)V9(04) COMP-3
                                      VALUE 0.5000.
           05 WS-MAX-APR              PIC 9V9(04) VALUE 0.2000.

       01  WS-WORK.
           05 WS-USE-RATE-CD          PIC X(04) VALUE SPACE.
           05 WS-USE-APR              PIC 9V9(04) VALUE ZERO.
           05 WS-USE-ROUND-MODE       PIC X(03) VALUE SPACE.
           05 WS-RAW-INT              PIC S9(13)V9(04) COMP-3
                                      VALUE ZERO.
           05 WS-RND-INPUT            PIC S9(13)V9(04) COMP-3
                                      VALUE ZERO.
           05 WS-INT-AMT              PIC S9(13) COMP-3 VALUE ZERO.
           05 WS-SKIP-REASON          PIC X(32) VALUE SPACE.

       01  WS-COUNTERS.
           05 WS-READ-CNT             PIC 9(09) VALUE ZERO.
           05 WS-ACCR-CNT             PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT             PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT              PIC 9(09) VALUE ZERO.

       01  LK-KZ151S-PARM.
           05 LK-ACCT-ID              PIC X(12).
           05 LK-PROCESS-DT           PIC 9(08).
           05 LK-OUT-RATE-CD          PIC X(04).
           05 LK-OUT-APR              PIC 9V9(04).
           05 LK-OUT-ROUND-MODE       PIC X(03).
           05 LK-REASON-CD            PIC X(08).
           05 LK-RETURN-CD            PIC S9(04) COMP.

       COPY LK-RND-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 1000-INIT
           IF WS-NORMAL
               PERFORM 2000-PROCESS UNTIL WS-EOF OR WS-HARD-ERR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-PROCESS-DT FROM DATE YYYYMMDD
           SET WS-NORMAL TO TRUE
           SET WS-NOT-EOF TO TRUE

           OPEN INPUT KZACCTF
           IF WS-ACCT-ST NOT = "00"
               DISPLAY "KZACCTF オープン失敗 ST=" WS-ACCT-ST
               SET WS-HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
               OPEN INPUT KZINTRF
               IF WS-INTR-ST NOT = "00"
                   DISPLAY "KZINTRF オープン失敗 ST=" WS-INTR-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN EXTEND KZINTAF
               IF WS-INTA-ST NOT = "00"
                   DISPLAY "KZINTAF オープン失敗 ST=" WS-INTA-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT KZTRANF
               IF WS-TRAN-ST NOT = "00"
                   DISPLAY "KZTRANF オープン失敗 ST=" WS-TRAN-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       2000-PROCESS.
           READ KZACCTF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
                   PERFORM 2100-PROCESS-ACCOUNT
           END-READ
           IF WS-ACCT-ST NOT = "00" AND WS-ACCT-ST NOT = "10"
               DISPLAY "KZACCTF 読込失敗 ST=" WS-ACCT-ST
               SET WS-HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2100-PROCESS-ACCOUNT.
           MOVE SPACE TO WS-SKIP-REASON
           MOVE ZERO TO WS-INT-AMT
           IF AC-CYCLE-BAL <= ZERO
               MOVE "残高対象外" TO WS-SKIP-REASON
               PERFORM 2900-SKIP-ACCOUNT
           ELSE
               PERFORM 2200-VALIDATE-ACCOUNT
               IF WS-SKIP-REASON = SPACE
                   PERFORM 2300-GET-RATE
               END-IF
               IF WS-SKIP-REASON = SPACE
                   PERFORM 2400-CHECK-RATE-MASTER
               END-IF
               IF WS-SKIP-REASON = SPACE
                   PERFORM 2500-CALC-INTEREST
               END-IF
               IF WS-SKIP-REASON = SPACE AND WS-INT-AMT > ZERO
                   PERFORM 2600-WRITE-ACCRUAL
                   IF WS-NORMAL
                       PERFORM 2700-WRITE-TRAN
                   END-IF
                   IF WS-NORMAL
                       ADD 1 TO WS-ACCR-CNT
                   END-IF
               ELSE
                   IF WS-SKIP-REASON = SPACE
                       MOVE "利息ゼロ" TO WS-SKIP-REASON
                   END-IF
                   PERFORM 2900-SKIP-ACCOUNT
               END-IF
           END-IF.

       2200-VALIDATE-ACCOUNT.
           EVALUATE AC-GROUP-CODE
               WHEN "STD0"
               WHEN "GLD1"
               WHEN "PLT2"
               WHEN "EXMP"
               WHEN "PREM"
                   CONTINUE
               WHEN OTHER
                   MOVE "グループコード不正" TO WS-SKIP-REASON
           END-EVALUATE

           IF WS-SKIP-REASON = SPACE
               IF AC-KYC-STATUS NOT = "OK"
                   MOVE "本人確認状態不正" TO WS-SKIP-REASON
               END-IF
           END-IF

           IF WS-SKIP-REASON = SPACE
               IF AC-OVER-AMT > AC-CREDIT-LIMIT
                   MOVE "与信超過額不正" TO WS-SKIP-REASON
               END-IF
           END-IF.

       2300-GET-RATE.
           MOVE AC-ACCT-ID TO LK-ACCT-ID
           MOVE WS-PROCESS-DT TO LK-PROCESS-DT
           MOVE SPACE TO LK-OUT-RATE-CD
           MOVE ZERO TO LK-OUT-APR
           MOVE SPACE TO LK-OUT-ROUND-MODE
           MOVE SPACE TO LK-REASON-CD
           MOVE ZERO TO LK-RETURN-CD

           CALL "KZ151S" USING LK-KZ151S-PARM

           IF LK-RETURN-CD NOT = ZERO
               MOVE LK-REASON-CD TO WS-SKIP-REASON
               DISPLAY "KZ151S 利率引当エラー"
               DISPLAY "口座=" AC-ACCT-ID
               DISPLAY "理由=" LK-REASON-CD
               ADD 1 TO WS-ERR-CNT
           ELSE
               MOVE LK-OUT-RATE-CD TO WS-USE-RATE-CD
               MOVE LK-OUT-APR TO WS-USE-APR
               MOVE LK-OUT-ROUND-MODE TO WS-USE-ROUND-MODE
               IF AC-GROUP-CODE = "EXMP"
                   MOVE WS-ZERO-APR TO WS-USE-APR
               END-IF
           END-IF

           IF WS-SKIP-REASON = SPACE
               IF WS-USE-APR > WS-MAX-APR
                   MOVE "利率上限超過" TO WS-SKIP-REASON
                   DISPLAY "利率上限超過"
                   DISPLAY "口座=" AC-ACCT-ID
                   DISPLAY "利率コード=" WS-USE-RATE-CD
                   ADD 1 TO WS-ERR-CNT
               END-IF
           END-IF.

       2400-CHECK-RATE-MASTER.
           MOVE WS-USE-RATE-CD TO IR-RATE-CD
           READ KZINTRF KEY IS IR-RATE-CD
               INVALID KEY
                   MOVE "利率コード未登録" TO WS-SKIP-REASON
                   DISPLAY "KZINTRF 未登録"
                   DISPLAY "口座=" AC-ACCT-ID
                   DISPLAY "利率コード=" WS-USE-RATE-CD
                   ADD 1 TO WS-ERR-CNT
               NOT INVALID KEY
                   IF IR-EFFECTIVE-DT > WS-PROCESS-DT
                       MOVE "利率適用日未来" TO WS-SKIP-REASON
                       DISPLAY "利率適用日未来"
                       DISPLAY "口座=" AC-ACCT-ID
                       DISPLAY "利率コード=" WS-USE-RATE-CD
                   END-IF
           END-READ

           IF WS-INTR-ST NOT = "00" AND WS-INTR-ST NOT = "23"
               DISPLAY "KZINTRF 読込失敗 ST=" WS-INTR-ST
               SET WS-HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2500-CALC-INTEREST.
           COMPUTE WS-RAW-INT ROUNDED =
               AC-CYCLE-BAL * WS-USE-APR / WS-DAYS-YEAR

           EVALUATE WS-USE-ROUND-MODE
               WHEN "FLR"
                   MOVE WS-RAW-INT TO WS-RND-INPUT
               WHEN "RND"
                   COMPUTE WS-RND-INPUT = WS-RAW-INT + WS-HALF-YEN
               WHEN "CUT"
                   MOVE WS-RAW-INT TO WS-RND-INPUT
               WHEN OTHER
                   MOVE "丸め区分不正" TO WS-SKIP-REASON
           END-EVALUATE

           IF WS-SKIP-REASON = SPACE
               MOVE WS-RND-INPUT TO LK-AMT-RAW
               MOVE ZERO TO LK-AMT-FLOORED
               CALL "KZ130S" USING LK-RND-PARM
               MOVE LK-AMT-FLOORED TO WS-INT-AMT
           END-IF.

       2600-WRITE-ACCRUAL.
           MOVE SPACES TO KZINTAF-REC
           MOVE AC-ACCT-ID TO IA-ACCT-ID
           MOVE WS-PROCESS-DT TO IA-ACCRUAL-DT
           MOVE WS-INT-AMT TO IA-INT-AMT
           MOVE AC-CYCLE-BAL TO IA-CYCLE-BAL
           MOVE WS-USE-RATE-CD TO IA-RATE-CD
           WRITE KZINTAF-REC
           IF WS-INTA-ST NOT = "00"
               DISPLAY "KZINTAF 書込失敗 ST=" WS-INTA-ST
               DISPLAY "口座=" AC-ACCT-ID
               SET WS-HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2700-WRITE-TRAN.
           MOVE SPACES TO KZTRANF-REC
           MOVE AC-ACCT-ID TO TR-ACCT-ID
           MOVE WS-TRAN-CD-INT TO TR-TRAN-CD
           MOVE WS-INT-AMT TO TR-TRAN-AMT
           WRITE KZTRANF-REC
           IF WS-TRAN-ST NOT = "00"
               DISPLAY "KZTRANF 書込失敗 ST=" WS-TRAN-ST
               DISPLAY "口座=" AC-ACCT-ID
               SET WS-HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2900-SKIP-ACCOUNT.
           ADD 1 TO WS-SKIP-CNT
           DISPLAY "利息発生対象外"
           DISPLAY "口座=" AC-ACCT-ID
           DISPLAY "理由=" WS-SKIP-REASON.

       9000-FINAL.
           IF WS-ACCT-ST NOT = SPACE
               CLOSE KZACCTF
           END-IF
           IF WS-INTR-ST NOT = SPACE
               CLOSE KZINTRF
           END-IF
           IF WS-INTA-ST NOT = SPACE
               CLOSE KZINTAF
           END-IF
           IF WS-TRAN-ST NOT = SPACE
               CLOSE KZTRANF
           END-IF

           DISPLAY "KZ150B 終了"
           DISPLAY "読込=" WS-READ-CNT
           DISPLAY "発生=" WS-ACCR-CNT
           DISPLAY "対象外=" WS-SKIP-CNT
           DISPLAY "エラー=" WS-ERR-CNT

           IF WS-HARD-ERR
               IF RETURN-CODE = ZERO
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
