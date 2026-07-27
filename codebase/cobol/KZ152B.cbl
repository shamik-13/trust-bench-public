       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ152B.
      * 変更履歴
      * 版数  年月日       担当                         概要
      * 1.00  令和06年05月  システム部 勘定系チーム     初版作成
      * 1.01  令和07年02月  システム部 勘定系チーム     文言整備
       AUTHOR. BATCH-SEISAN.
       DATE-WRITTEN. 2024-05-12.
       DATE-COMPILED.
       REMARKS.
      * 月次利息締め転記バッチ。
      * KZINTAFを口座単位に集計し、KZACCTF残高へ締め転記する。
      * 既知のグループコードはいずれもそのまま許容する。

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZINTAF ASSIGN TO "KZINTAF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-IA-STATUS.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-AC-STATUS.
           SELECT KZGLDTF ASSIGN TO "KZGLDTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-GL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  KZINTAF.
           COPY KZINTAFC.
       FD  KZACCTF.
           COPY KZACCTC.
       FD  KZGLDTF.
           COPY KZGLDTFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-IA-STATUS          PIC XX VALUE SPACES.
           05 WS-AC-STATUS          PIC XX VALUE SPACES.
           05 WS-GL-STATUS          PIC XX VALUE SPACES.

       01  WS-RUN-CONTROL.
           05 WS-EOF-SW             PIC X VALUE "N".
              88 IA-EOF                   VALUE "Y".
           05 WS-FATAL-SW           PIC X VALUE "N".
              88 FATAL-ERROR              VALUE "Y".
           05 WS-CURRENT-ACCT       PIC X(16) VALUE SPACES.
           05 WS-RUN-DATE           PIC 9(08) VALUE 20260617.
           05 WS-TARGET-YYYYMM      PIC 9(06) VALUE 202605.
           05 WS-TARGET-FROM        PIC 9(08) VALUE ZERO.
           05 WS-TARGET-TO          PIC 9(08) VALUE ZERO.
           05 WS-BATCH-ID           PIC X(18) VALUE SPACES.

       01  WS-AMOUNTS.
           05 WS-RAW-INT-SUM        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-POST-INT-AMT       PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-NEW-CYCLE-BAL      PIC S9(13)V99 COMP-3 VALUE ZERO.

       01  WS-COUNTERS.
           05 WS-IA-READ-CNT        PIC 9(09) VALUE ZERO.
           05 WS-IA-SKIP-CNT        PIC 9(09) VALUE ZERO.
           05 WS-AC-UPD-CNT         PIC 9(09) VALUE ZERO.
           05 WS-GL-WRITE-CNT       PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT            PIC 9(09) VALUE ZERO.

       01  WS-RETURN.
           05 WS-ABEND-CODE         PIC 9(04) VALUE ZERO.
           05 WS-MSG                PIC X(80) VALUE SPACES.

       01  WS-GL-WORK.
           05 WS-GL-SEQ             PIC 9(06) VALUE ZERO.
           05 WS-ENTRY-CD-DR        PIC X(04) VALUE "IEXP".
           05 WS-ENTRY-CD-CR        PIC X(04) VALUE "IPAY".
           05 WS-DR-KBN             PIC X VALUE "D".
           05 WS-CR-KBN             PIC X VALUE "C".

       01  LK-GL-PARM.
           05 LK-GL-BATCH-ID        PIC X(18).
           05 LK-GL-ACCT-ID         PIC X(16).
           05 LK-GL-POST-DT         PIC 9(08).
           05 LK-GL-AMT             PIC S9(13)V99 COMP-3.
           05 LK-GL-ENTRY-CD-DR     PIC X(04).
           05 LK-GL-ENTRY-CD-CR     PIC X(04).
           05 LK-GL-RET-CD          PIC 9(04).
           05 LK-GL-MSG             PIC X(60).

           COPY LK-RND-PARM.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INITIALIZE
           PERFORM 1000-PROCESS UNTIL IA-EOF OR FATAL-ERROR
           IF NOT FATAL-ERROR
               PERFORM 2000-FLUSH-ACCOUNT
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK
           .

       0000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-RUN-DATE
           COMPUTE WS-TARGET-FROM = WS-TARGET-YYYYMM * 100 + 1
           COMPUTE WS-TARGET-TO   = WS-TARGET-YYYYMM * 100 + 31
           STRING "KZ152B-" WS-RUN-DATE "-" WS-TARGET-YYYYMM
               DELIMITED BY SIZE INTO WS-BATCH-ID
           END-STRING

           OPEN INPUT KZINTAF
           IF WS-IA-STATUS NOT = "00"
               MOVE 1001 TO WS-ABEND-CODE
               MOVE "KZINTAF 入力オープンエラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
           END-IF

           OPEN I-O KZACCTF
           IF WS-AC-STATUS NOT = "00"
               MOVE 1002 TO WS-ABEND-CODE
               MOVE "KZACCTF 入出力オープンエラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
           END-IF

           OPEN OUTPUT KZGLDTF
           IF WS-GL-STATUS NOT = "00"
               MOVE 1003 TO WS-ABEND-CODE
               MOVE "KZGLDTF 出力オープンエラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
           END-IF

           IF NOT FATAL-ERROR
               PERFORM 1100-READ-INTEREST
           END-IF
           .

       1000-PROCESS.
           IF IA-ACCT-ID = SPACES
               ADD 1 TO WS-IA-SKIP-CNT
               PERFORM 1100-READ-INTEREST
           ELSE
               IF IA-ACCRUAL-DT < WS-TARGET-FROM
                  OR IA-ACCRUAL-DT > WS-TARGET-TO
                   ADD 1 TO WS-IA-SKIP-CNT
                   PERFORM 1100-READ-INTEREST
               ELSE
                   IF WS-CURRENT-ACCT = SPACES
                       MOVE IA-ACCT-ID TO WS-CURRENT-ACCT
                   END-IF
                   IF IA-ACCT-ID NOT = WS-CURRENT-ACCT
                       PERFORM 2000-FLUSH-ACCOUNT
                       MOVE IA-ACCT-ID TO WS-CURRENT-ACCT
                       MOVE ZERO TO WS-RAW-INT-SUM
                   END-IF
                   PERFORM 1200-ACCUMULATE
                   PERFORM 1100-READ-INTEREST
               END-IF
           END-IF
           .

       1100-READ-INTEREST.
           READ KZINTAF
               AT END
                   SET IA-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-IA-READ-CNT
           END-READ
           IF WS-IA-STATUS NOT = "00" AND WS-IA-STATUS NOT = "10"
               MOVE 1101 TO WS-ABEND-CODE
               MOVE "KZINTAF 読込エラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
           END-IF
           .

       1200-ACCUMULATE.
           IF IA-INT-AMT < ZERO
               ADD 1 TO WS-ERR-CNT
           ELSE
               ADD IA-INT-AMT TO WS-RAW-INT-SUM
           END-IF
           .

       2000-FLUSH-ACCOUNT.
           IF WS-CURRENT-ACCT = SPACES
               EXIT PARAGRAPH
           END-IF
           IF WS-RAW-INT-SUM = ZERO
               EXIT PARAGRAPH
           END-IF

           MOVE WS-RAW-INT-SUM TO LK-AMT-RAW
           MOVE ZERO TO LK-AMT-FLOORED
           CALL "KZ130S" USING LK-RND-PARM
           MOVE LK-AMT-FLOORED TO WS-POST-INT-AMT

           IF WS-POST-INT-AMT > ZERO
               PERFORM 2100-UPDATE-ACCOUNT
               IF NOT FATAL-ERROR
                   PERFORM 2200-BUILD-GL
               END-IF
           END-IF
           .

       2100-UPDATE-ACCOUNT.
           MOVE WS-CURRENT-ACCT TO AC-ACCT-ID
           READ KZACCTF
               INVALID KEY
                   ADD 1 TO WS-ERR-CNT
                   EXIT PARAGRAPH
           END-READ

           IF WS-AC-STATUS NOT = "00"
               MOVE 2101 TO WS-ABEND-CODE
               MOVE "KZACCTF 更新読込エラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AC-KYC-STATUS NOT = "OK"
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           EVALUATE AC-GROUP-CODE
               WHEN "STD0"
               WHEN "GLD1"
               WHEN "PLT2"
               WHEN "EXMP"
               WHEN "PREM"
                   CONTINUE
               WHEN OTHER
                   MOVE "STD0" TO AC-GROUP-CODE
           END-EVALUATE

           IF AC-GROUP-CODE = "EXMP"
               ADD 1 TO WS-IA-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-NEW-CYCLE-BAL =
               AC-CYCLE-BAL + WS-POST-INT-AMT

           IF WS-NEW-CYCLE-BAL > AC-CREDIT-LIMIT
               COMPUTE AC-OVER-AMT =
                   WS-NEW-CYCLE-BAL - AC-CREDIT-LIMIT
           ELSE
               MOVE ZERO TO AC-OVER-AMT
           END-IF

           MOVE WS-NEW-CYCLE-BAL TO AC-CYCLE-BAL
           REWRITE KZACCTF-REC
           IF WS-AC-STATUS NOT = "00"
               MOVE 2102 TO WS-ABEND-CODE
               MOVE "KZACCTF 再書込エラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
           ELSE
               ADD 1 TO WS-AC-UPD-CNT
           END-IF
           .

       2200-BUILD-GL.
           MOVE WS-BATCH-ID       TO LK-GL-BATCH-ID
           MOVE WS-CURRENT-ACCT   TO LK-GL-ACCT-ID
           MOVE WS-RUN-DATE       TO LK-GL-POST-DT
           MOVE WS-POST-INT-AMT   TO LK-GL-AMT
           MOVE WS-ENTRY-CD-DR    TO LK-GL-ENTRY-CD-DR
           MOVE WS-ENTRY-CD-CR    TO LK-GL-ENTRY-CD-CR
           MOVE ZERO              TO LK-GL-RET-CD
           MOVE SPACES            TO LK-GL-MSG

           CALL "KZ310S" USING LK-GL-PARM

           IF LK-GL-RET-CD NOT = ZERO
               MOVE 2201 TO WS-ABEND-CODE
               MOVE LK-GL-MSG TO WS-MSG
               SET FATAL-ERROR TO TRUE
           ELSE
               PERFORM 2210-WRITE-GL-DEBIT
               IF NOT FATAL-ERROR
                   PERFORM 2220-WRITE-GL-CREDIT
               END-IF
           END-IF
           .

       2210-WRITE-GL-DEBIT.
           ADD 1 TO WS-GL-SEQ
           INITIALIZE KZGLDTF-REC
           STRING WS-BATCH-ID "-" WS-GL-SEQ
               DELIMITED BY SIZE INTO GL-GL-BATCH-ID
           END-STRING
           MOVE WS-CURRENT-ACCT TO GL-ACCT-ID
           MOVE WS-ENTRY-CD-DR  TO GL-ENTRY-CD
           MOVE WS-DR-KBN       TO GL-DR-CR-KBN
           MOVE WS-POST-INT-AMT TO GL-ENTRY-AMT
           MOVE WS-RUN-DATE     TO GL-POST-DT
           WRITE KZGLDTF-REC
           IF WS-GL-STATUS NOT = "00"
               MOVE 2211 TO WS-ABEND-CODE
               MOVE "KZGLDTF 借方書込エラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
           ELSE
               ADD 1 TO WS-GL-WRITE-CNT
           END-IF
           .

       2220-WRITE-GL-CREDIT.
           ADD 1 TO WS-GL-SEQ
           INITIALIZE KZGLDTF-REC
           STRING WS-BATCH-ID "-" WS-GL-SEQ
               DELIMITED BY SIZE INTO GL-GL-BATCH-ID
           END-STRING
           MOVE WS-CURRENT-ACCT TO GL-ACCT-ID
           MOVE WS-ENTRY-CD-CR  TO GL-ENTRY-CD
           MOVE WS-CR-KBN       TO GL-DR-CR-KBN
           MOVE WS-POST-INT-AMT TO GL-ENTRY-AMT
           MOVE WS-RUN-DATE     TO GL-POST-DT
           WRITE KZGLDTF-REC
           IF WS-GL-STATUS NOT = "00"
               MOVE 2221 TO WS-ABEND-CODE
               MOVE "KZGLDTF 貸方書込エラー" TO WS-MSG
               SET FATAL-ERROR TO TRUE
           ELSE
               ADD 1 TO WS-GL-WRITE-CNT
           END-IF
           .

       9000-TERMINATE.
           CLOSE KZINTAF
           CLOSE KZACCTF
           CLOSE KZGLDTF

           IF FATAL-ERROR
               DISPLAY "KZ152B 異常終了コード=" WS-ABEND-CODE
               DISPLAY "KZ152B メッセージ=" WS-MSG
               STOP RUN RETURNING WS-ABEND-CODE
           ELSE
               DISPLAY "KZ152B 正常終了"
               DISPLAY "IA 読込件数=" WS-IA-READ-CNT
               DISPLAY "IA スキップ件数=" WS-IA-SKIP-CNT
               DISPLAY "AC 更新件数=" WS-AC-UPD-CNT
               DISPLAY "GL 書込件数=" WS-GL-WRITE-CNT
               DISPLAY "エラー件数=" WS-ERR-CNT
           END-IF
           .
