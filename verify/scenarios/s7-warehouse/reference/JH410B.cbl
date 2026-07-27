       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH410B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20260401  開発一課  初版作成
      * 1.01  20260515  開発一課  確定フラグ判定追加
      * 1.02  20260619  開発一課  DWH抽出件数表示追加
      ******************************************************************
      * 分析用DWH抽出バッチ
      * KZFEEHFを順次読込み、確定済手数料のみJHDWHFへ出力する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZFEEHF
               ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZFEEHF-ST.

           SELECT JHDWHF
               ASSIGN TO "JHDWHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-JHDWHF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  KZFEEHF.
           COPY KZFEEHFC.

       FD  JHDWHF.
           COPY JHDWHFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZFEEHF-ST        PIC X(02) VALUE SPACES.
           05 WS-JHDWHF-ST         PIC X(02) VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-SW            PIC X(01) VALUE 'N'.
              88 WS-EOF                     VALUE 'Y'.
              88 WS-NOT-EOF                 VALUE 'N'.
           05 WS-ABEND-SW          PIC X(01) VALUE 'N'.
              88 WS-ABEND                   VALUE 'Y'.
              88 WS-NORMAL                  VALUE 'N'.

       01  WS-COUNTERS.
           05 WS-READ-CNT          PIC 9(11) VALUE ZERO.
           05 WS-WRITE-CNT         PIC 9(11) VALUE ZERO.
           05 WS-SKIP-CNT          PIC 9(11) VALUE ZERO.

       01  WS-DATE-AREA.
           05 WS-CURRENT-DATE      PIC X(21) VALUE SPACES.
           05 WS-RUN-DATE          PIC 9(08) VALUE ZERO.

       01  WS-MESSAGE-AREA.
           05 WS-ERR-FILE          PIC X(08) VALUE SPACES.
           05 WS-ERR-ACTION        PIC X(12) VALUE SPACES.
           05 WS-ERR-STATUS        PIC X(02) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF WS-NORMAL
               PERFORM 2000-PROCESS
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK
           .

       1000-INITIALIZE.
           DISPLAY 'JH410B 開始 分析用DWH抽出バッチ'
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-RUN-DATE

           OPEN INPUT KZFEEHF
           IF WS-KZFEEHF-ST NOT = '00'
               MOVE 'KZFEEHF'     TO WS-ERR-FILE
               MOVE 'オープン'    TO WS-ERR-ACTION
               MOVE WS-KZFEEHF-ST TO WS-ERR-STATUS
               PERFORM 8000-IO-ERROR
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT JHDWHF
               IF WS-JHDWHF-ST NOT = '00'
                   MOVE 'JHDWHF '     TO WS-ERR-FILE
                   MOVE 'オープン'    TO WS-ERR-ACTION
                   MOVE WS-JHDWHF-ST  TO WS-ERR-STATUS
                   PERFORM 8000-IO-ERROR
               END-IF
           END-IF
           .

       2000-PROCESS.
           PERFORM UNTIL WS-EOF OR WS-ABEND
               PERFORM 2100-READ-FEE
               IF WS-NOT-EOF AND WS-NORMAL
                   PERFORM 2200-EXTRACT-JUDGE
               END-IF
           END-PERFORM
           .

       2100-READ-FEE.
           READ KZFEEHF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ

           IF WS-NOT-EOF
              AND WS-KZFEEHF-ST NOT = '00'
               MOVE 'KZFEEHF'     TO WS-ERR-FILE
               MOVE 'リード'      TO WS-ERR-ACTION
               MOVE WS-KZFEEHF-ST TO WS-ERR-STATUS
               PERFORM 8000-IO-ERROR
           END-IF
           .

       2200-EXTRACT-JUDGE.
           EVALUATE FH-CONFIRM-FLAG
               WHEN 'Y'
                   PERFORM 2300-WRITE-DWH
               WHEN 'N'
                   ADD 1 TO WS-SKIP-CNT
               WHEN OTHER
                   DISPLAY '確定フラグ不正 口座=' FH-ACCT-NO
                   DISPLAY '周期日=' FH-CYCLE-DT
                   DISPLAY '値=' FH-CONFIRM-FLAG
                   MOVE 12 TO RETURN-CODE
                   SET WS-ABEND TO TRUE
           END-EVALUATE
           .

       2300-WRITE-DWH.
           INITIALIZE JHDWHF-REC
           MOVE FH-ACCT-NO  TO DW-ACCT-NO
           MOVE FH-CYCLE-DT TO DW-CYCLE-DT
           MOVE FH-FEE-AMT  TO DW-FEE-AMT
           MOVE FH-FEE-YTD  TO DW-FEE-YTD
           MOVE WS-RUN-DATE TO DW-EXTRACT-DT

           WRITE JHDWHF-REC

           IF WS-JHDWHF-ST = '00'
               ADD 1 TO WS-WRITE-CNT
           ELSE
               MOVE 'JHDWHF '    TO WS-ERR-FILE
               MOVE 'ライト'     TO WS-ERR-ACTION
               MOVE WS-JHDWHF-ST TO WS-ERR-STATUS
               PERFORM 8000-IO-ERROR
           END-IF
           .

       8000-IO-ERROR.
           DISPLAY WS-ERR-FILE ' ' WS-ERR-ACTION
           DISPLAY '失敗 ST=' WS-ERR-STATUS
           MOVE 8 TO RETURN-CODE
           SET WS-ABEND TO TRUE
           .

       9000-TERMINATE.
           IF WS-KZFEEHF-ST = '00' OR WS-KZFEEHF-ST = '10'
               CLOSE KZFEEHF
               IF WS-KZFEEHF-ST NOT = '00'
                   DISPLAY 'KZFEEHF クローズ失敗 ST='
                           WS-KZFEEHF-ST
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           IF WS-JHDWHF-ST = '00'
               CLOSE JHDWHF
               IF WS-JHDWHF-ST NOT = '00'
                   DISPLAY 'JHDWHF クローズ失敗 ST='
                           WS-JHDWHF-ST
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           DISPLAY 'JH410B 終了 読込件数=' WS-READ-CNT
           DISPLAY '出力件数=' WS-WRITE-CNT
           DISPLAY 'スキップ件数=' WS-SKIP-CNT
           DISPLAY 'RC=' RETURN-CODE
           .
