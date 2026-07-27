       IDENTIFICATION DIVISION.
       PROGRAM-ID. KP220B.
      *変更履歴
      *版数  年月日      担当                      概要
      *1.00  平成28.04.01 システム部 勘定系チーム 新規作成
      *1.10  令和02.10.15 システム部 勘定系チーム 仕訳出力見直し
      *1.20  令和05.06.30 システム部 勘定系チーム 延滞回復対応
       AUTHOR. BATCH.
      *延滞回復反映バッチ
      *入金取引を延滞明細へ充当し、回収仕訳を作成する。

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KPDELINF ASSIGN TO "KPDELINF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DL-ACCT-ID
               FILE STATUS IS FS-KPDELINF.
           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZTRANF.
           SELECT KPPENHF ASSIGN TO "KPPENHF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PH-ACCT-ID
               FILE STATUS IS FS-KPPENHF.
           SELECT KZGLDTF ASSIGN TO "KZGLDTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZGLDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KPDELINF.
           COPY KPDELINFC.
       FD  KZTRANF.
           COPY KZTRANC.
       FD  KPPENHF.
           COPY KPPENHFC.
       FD  KZGLDTF.
           COPY KZGLDTFC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-KPDELINF              PIC XX VALUE SPACES.
              88 FS-DL-OK             VALUE "00".
              88 FS-DL-NOTFND         VALUE "23".
           05 FS-KZTRANF              PIC XX VALUE SPACES.
              88 FS-TR-OK             VALUE "00".
              88 FS-TR-EOF            VALUE "10".
           05 FS-KPPENHF              PIC XX VALUE SPACES.
              88 FS-PH-OK             VALUE "00".
              88 FS-PH-NOTFND         VALUE "23".
           05 FS-KZGLDTF              PIC XX VALUE SPACES.
              88 FS-GL-OK             VALUE "00".

       01  SWITCH-AREA.
           05 SW-END                  PIC X VALUE "N".
              88 END-OF-TRAN          VALUE "Y".
              88 NOT-END-OF-TRAN      VALUE "N".
           05 SW-SKIP-TRAN            PIC X VALUE "N".
              88 SKIP-TRAN           VALUE "Y".
              88 USE-TRAN            VALUE "N".

       01  DATE-AREA.
           05 WS-TODAY                PIC 9(8) VALUE ZERO.
           05 WS-BATCH-ID.
              10 WS-BATCH-PGM         PIC X(6) VALUE "KP220B".
              10 WS-BATCH-DATE        PIC 9(8) VALUE ZERO.

       01  AMOUNT-AREA.
           05 WS-TRAN-AMT             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-REMAIN-AMT           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-DUE-BEFORE           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-DUE-APPLIED          PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-PEN-BEFORE           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-PEN-APPLIED          PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-TOTAL-APPLIED        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-ZERO-AMT             PIC S9(13)V99 COMP-3 VALUE ZERO.

       01  EDIT-AREA.
           05 WS-ERR-MSG              PIC X(80) VALUE SPACES.
           05 WS-ACCOUNT-SAVED        PIC X(20) VALUE SPACES.
           05 WS-GL-SEQ               PIC 9(7) VALUE ZERO.
           05 WS-ENTRY-AMT            PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-ENTRY-CD             PIC X(4) VALUE SPACES.
           05 WS-DR-CR-KBN            PIC X VALUE SPACE.

       01  COUNT-AREA.
           05 CT-TR-READ              PIC 9(9) VALUE ZERO.
           05 CT-TR-SKIP              PIC 9(9) VALUE ZERO.
           05 CT-DL-UPDATE            PIC 9(9) VALUE ZERO.
           05 CT-GL-WRITE             PIC 9(9) VALUE ZERO.
           05 CT-ERROR                PIC 9(9) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM UNTIL END-OF-TRAN
               PERFORM 2000-READ-TRAN
               IF NOT END-OF-TRAN
                   PERFORM 3000-PROCESS-TRAN
               END-IF
           END-PERFORM
           PERFORM 9000-TERM
           GOBACK.

       1000-INIT.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           MOVE WS-TODAY TO WS-BATCH-DATE
           OPEN I-O KPDELINF
           IF NOT FS-DL-OK
               MOVE "KPDELINF オープンエラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF
           OPEN INPUT KZTRANF
           IF NOT FS-TR-OK
               MOVE "KZTRANF オープンエラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF
           OPEN INPUT KPPENHF
           IF NOT FS-PH-OK
               MOVE "KPPENHF オープンエラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF
           OPEN OUTPUT KZGLDTF
           IF NOT FS-GL-OK
               MOVE "KZGLDTF オープンエラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF
           SET NOT-END-OF-TRAN TO TRUE.

       2000-READ-TRAN.
           READ KZTRANF
               AT END
                   SET END-OF-TRAN TO TRUE
               NOT AT END
                   ADD 1 TO CT-TR-READ
           END-READ
           IF NOT END-OF-TRAN
              AND NOT FS-TR-OK
               MOVE "KZTRANF 読込エラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF.

       3000-PROCESS-TRAN.
           SET USE-TRAN TO TRUE
           PERFORM 3100-VALIDATE-TRAN
           IF USE-TRAN
               MOVE TR-ACCT-ID TO DL-ACCT-ID
               READ KPDELINF
                   INVALID KEY
                       ADD 1 TO CT-TR-SKIP
                       SET SKIP-TRAN TO TRUE
                   NOT INVALID KEY
                       CONTINUE
               END-READ
               IF USE-TRAN
                   PERFORM 3200-APPLY-DELINQ
               END-IF
           END-IF.

       3100-VALIDATE-TRAN.
      *入金系TRAN-CDのみ対象とし、取消・照会系は読み飛ばす。
           IF TR-ACCT-ID = SPACES
               ADD 1 TO CT-TR-SKIP
               SET SKIP-TRAN TO TRUE
           END-IF
           IF USE-TRAN
              AND TR-TRAN-AMT <= ZERO
               ADD 1 TO CT-TR-SKIP
               SET SKIP-TRAN TO TRUE
           END-IF
           IF USE-TRAN
              AND TR-TRAN-CD NOT = "10"
              AND TR-TRAN-CD NOT = "11"
              AND TR-TRAN-CD NOT = "12"
               ADD 1 TO CT-TR-SKIP
               SET SKIP-TRAN TO TRUE
           END-IF.

       3200-APPLY-DELINQ.
           IF DL-DUE-AMT <= ZERO
               ADD 1 TO CT-TR-SKIP
               EXIT PARAGRAPH
           END-IF
           MOVE TR-TRAN-AMT TO WS-TRAN-AMT
           MOVE WS-TRAN-AMT TO WS-REMAIN-AMT
           MOVE DL-DUE-AMT TO WS-DUE-BEFORE
           PERFORM 3300-READ-PENALTY
           COMPUTE WS-PEN-APPLIED = FUNCTION MIN
               (WS-REMAIN-AMT, WS-PEN-BEFORE)
           SUBTRACT WS-PEN-APPLIED FROM WS-REMAIN-AMT
           COMPUTE WS-DUE-APPLIED = FUNCTION MIN
               (WS-REMAIN-AMT, WS-DUE-BEFORE)
           SUBTRACT WS-DUE-APPLIED FROM WS-REMAIN-AMT
           COMPUTE WS-TOTAL-APPLIED =
               WS-PEN-APPLIED + WS-DUE-APPLIED
           IF WS-TOTAL-APPLIED <= ZERO
               ADD 1 TO CT-TR-SKIP
               EXIT PARAGRAPH
           END-IF
           COMPUTE DL-DUE-AMT = WS-DUE-BEFORE - WS-DUE-APPLIED
           IF DL-DUE-AMT = ZERO
               MOVE ZERO TO DL-DAYS-PAST-DUE
               MOVE WS-TODAY TO DL-LAST-DUE-DT
           END-IF
           REWRITE KPDELINF-REC
           IF NOT FS-DL-OK
               MOVE "KPDELINF 書換エラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF
           ADD 1 TO CT-DL-UPDATE
           MOVE TR-ACCT-ID TO WS-ACCOUNT-SAVED
           PERFORM KZ310S-CREATE-JOURNAL.

       3300-READ-PENALTY.
      *遅延損害金は同一口座の最新履歴残高を充当対象にする。
           MOVE ZERO TO WS-PEN-BEFORE
           MOVE TR-ACCT-ID TO PH-ACCT-ID
           READ KPPENHF
               INVALID KEY
                   MOVE ZERO TO WS-PEN-BEFORE
               NOT INVALID KEY
                   IF PH-PENALTY-AMT > ZERO
                      AND PH-RATE-CD = DL-PENALTY-RATE-CD
                       MOVE PH-PENALTY-AMT TO WS-PEN-BEFORE
                   END-IF
           END-READ
           IF NOT FS-PH-OK
              AND NOT FS-PH-NOTFND
               MOVE "KPPENHF 読込エラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF.

       KZ310S-CREATE-JOURNAL.
      *回収仕訳は未収遅延損害金、未収元本の順で作成する。
           IF WS-PEN-APPLIED > ZERO
               MOVE WS-PEN-APPLIED TO WS-ENTRY-AMT
               MOVE "LPEN" TO WS-ENTRY-CD
               MOVE "C" TO WS-DR-CR-KBN
               PERFORM 4100-WRITE-GL
           END-IF
           IF WS-DUE-APPLIED > ZERO
               MOVE WS-DUE-APPLIED TO WS-ENTRY-AMT
               MOVE "LPRI" TO WS-ENTRY-CD
               MOVE "C" TO WS-DR-CR-KBN
               PERFORM 4100-WRITE-GL
           END-IF
           MOVE WS-TOTAL-APPLIED TO WS-ENTRY-AMT
           MOVE "CASH" TO WS-ENTRY-CD
           MOVE "D" TO WS-DR-CR-KBN
           PERFORM 4100-WRITE-GL.

       4100-WRITE-GL.
           ADD 1 TO WS-GL-SEQ
           INITIALIZE KZGLDTF-REC
           MOVE WS-BATCH-ID TO GL-GL-BATCH-ID
           MOVE WS-ACCOUNT-SAVED TO GL-ACCT-ID
           MOVE WS-ENTRY-CD TO GL-ENTRY-CD
           MOVE WS-DR-CR-KBN TO GL-DR-CR-KBN
           MOVE WS-ENTRY-AMT TO GL-ENTRY-AMT
           MOVE WS-TODAY TO GL-POST-DT
           WRITE KZGLDTF-REC
           IF NOT FS-GL-OK
               MOVE "KZGLDTF 書込エラー" TO WS-ERR-MSG
               PERFORM 9100-ABEND
           END-IF
           ADD 1 TO CT-GL-WRITE.

       9000-TERM.
           CLOSE KPDELINF
                 KZTRANF
                 KPPENHF
                 KZGLDTF
           DISPLAY "KP220B 取引読込  =" CT-TR-READ
           DISPLAY "KP220B 取引スキップ=" CT-TR-SKIP
           DISPLAY "KP220B 延滞更新  =" CT-DL-UPDATE
           DISPLAY "KP220B 仕訳書込  =" CT-GL-WRITE
           DISPLAY "KP220B エラー    =" CT-ERROR.

       9100-ABEND.
           ADD 1 TO CT-ERROR
           DISPLAY "KP220B 異常終了 " WS-ERR-MSG
           DISPLAY "FS KPDELINF=" FS-KPDELINF
                   " KZTRANF=" FS-KZTRANF
                   " KPPENHF=" FS-KPPENHF
                   " KZGLDTF=" FS-KZGLDTF
           CLOSE KPDELINF
                 KZTRANF
                 KPPENHF
                 KZGLDTF
           STOP RUN.
