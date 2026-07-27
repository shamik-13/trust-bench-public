       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ210B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                         概要
      * 1.00  令和05年04月01日 システム部 勘定系チーム   新規作成
      * 1.01  令和05年10月16日 システム部 勘定系チーム   カード状態同期抽出条件見直し
      * 1.02  令和06年03月11日 システム部 勘定系チーム   停止カード反映処理を追加
       AUTHOR. BATCH-SYSTEM.
      *===============================================================*
      * KZ210B - カード状態同期バッチ
      * 停止カードの口座処理可否を口座属性へ反映する。
      * カードと口座の対応不整合は検査エラーへ出力する。
      *===============================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCARDF ASSIGN TO "KZCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CD-CARD-NO
               FILE STATUS IS WS-CARD-STAT.

           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-ACCT-STAT.

           SELECT KZBALER ASSIGN TO "KZBALER"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-BALER-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCARDF.
           COPY KZCARDC.

       FD  KZACCTF.
           COPY KZACCTC.

       FD  KZBALER.
           COPY KZBALERC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CARD-STAT          PIC XX VALUE SPACES.
           05 WS-ACCT-STAT          PIC XX VALUE SPACES.
           05 WS-BALER-STAT         PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-SW             PIC X VALUE "N".
              88 WS-EOF                  VALUE "Y".
              88 WS-NOT-EOF              VALUE "N".
           05 WS-FATAL-SW          PIC X VALUE "N".
              88 WS-FATAL               VALUE "Y".
              88 WS-NORMAL              VALUE "N".

       01  WS-COUNTERS.
           05 WS-CARD-READ-CNT      PIC 9(9) VALUE ZERO.
           05 WS-ACCT-READ-CNT      PIC 9(9) VALUE ZERO.
           05 WS-ACCT-UPD-CNT       PIC 9(9) VALUE ZERO.
           05 WS-ERR-WRITE-CNT      PIC 9(9) VALUE ZERO.
           05 WS-ORPHAN-CNT         PIC 9(9) VALUE ZERO.
           05 WS-MISMATCH-CNT       PIC 9(9) VALUE ZERO.
           05 WS-INVALID-CNT        PIC 9(9) VALUE ZERO.

       01  WS-DATE-AREA.
           05 WS-CURRENT-DATE       PIC 9(8).
           05 WS-CURRENT-TIME       PIC 9(8).
           05 WS-DATE-NUM           PIC 9(8).

       01  WS-WORK-AREA.
           05 WS-STD-RATE           PIC 9V9(4) VALUE 0.0150.
           05 WS-RATE               PIC 9V9(4) VALUE ZERO.
           05 WS-DIFF-AMT           PIC S9(11)V99 VALUE ZERO.
           05 WS-ABS-DIFF-AMT       PIC S9(11)V99 VALUE ZERO.
           05 WS-STOPPED-CARD       PIC X VALUE "N".
              88 WS-CARD-STOPPED         VALUE "Y".
              88 WS-CARD-ACTIVE          VALUE "N".

       01  WS-ERROR-CODES.
           05 WS-ERR-ORPHAN         PIC X(08) VALUE "CDORPHAN".
           05 WS-ERR-MISMATCH       PIC X(08) VALUE "ACCTDIFF".
           05 WS-ERR-BADSTAT        PIC X(08) VALUE "BADSTAT ".
           05 WS-ERR-BADGRP         PIC X(08) VALUE "BADGRP  ".

       01  WS-CONSTANTS.
           05 WS-PGM-NAME           PIC X(08) VALUE "KZ210B  ".
           05 WS-STATUS-STOP        PIC X VALUE "S".
           05 WS-STATUS-LOST        PIC X VALUE "L".
           05 WS-STATUS-CLOSED      PIC X VALUE "C".
           05 WS-KYC-HOLD           PIC X VALUE "H".
           05 WS-GRP-STD            PIC X(04) VALUE "STD0".
           05 WS-GRP-GLD            PIC X(04) VALUE "GLD1".
           05 WS-GRP-PLT            PIC X(04) VALUE "PLT2".
           05 WS-GRP-EXM            PIC X(04) VALUE "EXMP".
           05 WS-GRP-PREM           PIC X(04) VALUE "PREM".

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF WS-NORMAL
               PERFORM 2000-PROCESS UNTIL WS-EOF OR WS-FATAL
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-DATE-NUM

           OPEN INPUT KZCARDF
           IF WS-CARD-STAT NOT = "00"
               DISPLAY "KZ210B CARD OPEN ERROR " WS-CARD-STAT
               SET WS-FATAL TO TRUE
           END-IF

           IF WS-NORMAL
               OPEN I-O KZACCTF
               IF WS-ACCT-STAT NOT = "00"
                   DISPLAY "KZ210B ACCT OPEN ERROR " WS-ACCT-STAT
                   SET WS-FATAL TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN EXTEND KZBALER
               IF WS-BALER-STAT NOT = "00"
                   DISPLAY "KZ210B BALER OPEN ERROR " WS-BALER-STAT
                   SET WS-FATAL TO TRUE
               END-IF
           END-IF.

       2000-PROCESS.
           READ KZCARDF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-CARD-READ-CNT
                   PERFORM 2100-VALIDATE-CARD
                   IF WS-NORMAL AND WS-CARD-STOPPED
                       PERFORM 2200-APPLY-ACCOUNT
                   END-IF
           END-READ.

       2100-VALIDATE-CARD.
           SET WS-CARD-ACTIVE TO TRUE

           EVALUATE CD-CARD-STATUS
               WHEN WS-STATUS-STOP
               WHEN WS-STATUS-LOST
               WHEN WS-STATUS-CLOSED
                   SET WS-CARD-STOPPED TO TRUE
               WHEN "A"
               WHEN "N"
                   CONTINUE
               WHEN OTHER
                   ADD 1 TO WS-INVALID-CNT
                   MOVE ZERO TO WS-DIFF-AMT
                   PERFORM 7000-WRITE-BADSTAT
           END-EVALUATE.

       2200-APPLY-ACCOUNT.
           MOVE CD-ACCT-ID TO AC-ACCT-ID
           READ KZACCTF
               KEY IS AC-ACCT-ID
               INVALID KEY
                   IF WS-ACCT-STAT = "23"
                       ADD 1 TO WS-ORPHAN-CNT
                       MOVE ZERO TO WS-DIFF-AMT
                       PERFORM 7100-WRITE-ORPHAN
                   ELSE
                       DISPLAY "KZ210B ACCT READ ERROR " WS-ACCT-STAT
                       SET WS-FATAL TO TRUE
                   END-IF
               NOT INVALID KEY
                   ADD 1 TO WS-ACCT-READ-CNT
                   PERFORM 2300-CHECK-ACCOUNT
           END-READ.

       2300-CHECK-ACCOUNT.
           IF AC-CARD-NO NOT = CD-CARD-NO
               ADD 1 TO WS-MISMATCH-CNT
               COMPUTE WS-DIFF-AMT =
                   AC-CYCLE-BAL - AC-OVER-AMT
               PERFORM 7200-WRITE-MISMATCH
           ELSE
               PERFORM 2400-RESOLVE-GROUP-RATE
               IF WS-NORMAL
                   PERFORM 2500-UPDATE-ELIGIBILITY
               END-IF
           END-IF.

       2400-RESOLVE-GROUP-RATE.
           EVALUATE AC-GROUP-CODE
               WHEN WS-GRP-STD
                   MOVE WS-STD-RATE TO WS-RATE
               WHEN WS-GRP-GLD
                   MOVE 0.0100 TO WS-RATE
               WHEN WS-GRP-PLT
                   MOVE 0.0080 TO WS-RATE
               WHEN WS-GRP-EXM
                   MOVE ZERO TO WS-RATE
               WHEN WS-GRP-PREM
                   MOVE 0.0080 TO WS-RATE
               WHEN OTHER
                   ADD 1 TO WS-INVALID-CNT
                   MOVE ZERO TO WS-DIFF-AMT
                   PERFORM 7300-WRITE-BADGRP
                   MOVE WS-STD-RATE TO WS-RATE
                   MOVE WS-GRP-STD TO AC-GROUP-CODE
           END-EVALUATE.

       2500-UPDATE-ELIGIBILITY.
      *    停止カードに紐づく口座は処理保留へ寄せる。
           IF AC-KYC-STATUS NOT = WS-KYC-HOLD
               MOVE WS-KYC-HOLD TO AC-KYC-STATUS
           END-IF

      *    旧グループ補正だけの場合も永続化対象とする。
           REWRITE KZACCTF-REC
               INVALID KEY
                   DISPLAY "KZ210B ACCT REWRITE ERROR " WS-ACCT-STAT
                   SET WS-FATAL TO TRUE
               NOT INVALID KEY
                   ADD 1 TO WS-ACCT-UPD-CNT
           END-REWRITE.

       7000-WRITE-BADSTAT.
           MOVE CD-ACCT-ID TO BE-ACCT-ID
           MOVE WS-DATE-NUM TO BE-EDIT-DT
           MOVE WS-ERR-BADSTAT TO BE-ERROR-CD
           MOVE WS-PGM-NAME TO BE-SOURCE-PGM
           MOVE WS-DIFF-AMT TO BE-DIFF-AMT
           PERFORM 7900-WRITE-ERROR.

       7100-WRITE-ORPHAN.
           MOVE CD-ACCT-ID TO BE-ACCT-ID
           MOVE WS-DATE-NUM TO BE-EDIT-DT
           MOVE WS-ERR-ORPHAN TO BE-ERROR-CD
           MOVE WS-PGM-NAME TO BE-SOURCE-PGM
           MOVE WS-DIFF-AMT TO BE-DIFF-AMT
           PERFORM 7900-WRITE-ERROR.

       7200-WRITE-MISMATCH.
           MOVE AC-ACCT-ID TO BE-ACCT-ID
           MOVE WS-DATE-NUM TO BE-EDIT-DT
           MOVE WS-ERR-MISMATCH TO BE-ERROR-CD
           MOVE WS-PGM-NAME TO BE-SOURCE-PGM
           MOVE WS-DIFF-AMT TO BE-DIFF-AMT
           PERFORM 7900-WRITE-ERROR.

       7300-WRITE-BADGRP.
           MOVE AC-ACCT-ID TO BE-ACCT-ID
           MOVE WS-DATE-NUM TO BE-EDIT-DT
           MOVE WS-ERR-BADGRP TO BE-ERROR-CD
           MOVE WS-PGM-NAME TO BE-SOURCE-PGM
           MOVE WS-DIFF-AMT TO BE-DIFF-AMT
           PERFORM 7900-WRITE-ERROR.

       7900-WRITE-ERROR.
           WRITE KZBALER-REC
           IF WS-BALER-STAT = "00"
               ADD 1 TO WS-ERR-WRITE-CNT
           ELSE
               DISPLAY "KZ210B BALER WRITE ERROR " WS-BALER-STAT
               SET WS-FATAL TO TRUE
           END-IF.

       9000-TERMINATE.
           IF WS-CARD-STAT NOT = SPACES
               CLOSE KZCARDF
           END-IF
           IF WS-ACCT-STAT NOT = SPACES
               CLOSE KZACCTF
           END-IF
           IF WS-BALER-STAT NOT = SPACES
               CLOSE KZBALER
           END-IF

           DISPLAY "KZ210B CARD READ     " WS-CARD-READ-CNT
           DISPLAY "KZ210B ACCT READ     " WS-ACCT-READ-CNT
           DISPLAY "KZ210B ACCT UPDATE   " WS-ACCT-UPD-CNT
           DISPLAY "KZ210B ERROR WRITE   " WS-ERR-WRITE-CNT
           DISPLAY "KZ210B ORPHAN CARD   " WS-ORPHAN-CNT
           DISPLAY "KZ210B ACCT MISMATCH " WS-MISMATCH-CNT
           DISPLAY "KZ210B INVALID DATA  " WS-INVALID-CNT.
