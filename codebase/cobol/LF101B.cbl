       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF101B.
      * 積立金残高更新バッチ
      * 月末の配当充当、返戻金確定、契約状態変更を受けて
      * 積立金相当の責任準備金残高を更新する。
      *
      * 版数  年月日    担当        概要
      * 1.0   20210601  契約管理部    初版作成

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFRSVF-FILE ASSIGN TO LFRSVF
               ORGANIZATION IS INDEXED
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS LFRSVF-STATUS.

           SELECT LFCVRF-FILE ASSIGN TO LFCVRF
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LFCVRF-STATUS.

           SELECT LFDIVF-FILE ASSIGN TO LFDIVF
               ORGANIZATION IS INDEXED
               RECORD KEY IS DV-POL-NO
               FILE STATUS IS LFDIVF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LFRSVF-FILE.
       COPY LFRSVC.

       FD  LFCVRF-FILE.
       COPY LFCVRFC.

       FD  LFDIVF-FILE.
       COPY LFDIVC.

       WORKING-STORAGE SECTION.
      * ファイル状態コード
       01  LFRSVF-STATUS          PIC XX VALUE SPACES.
       01  LFCVRF-STATUS          PIC XX VALUE SPACES.
       01  LFDIVF-STATUS          PIC XX VALUE SPACES.

      * プログラム制御変数
       01  WS-EOF-FLAGS.
           05  WS-EOF-LFRSVF      PIC X VALUE 'N'.
           05  WS-EOF-LFCVRF      PIC X VALUE 'N'.
           05  WS-EOF-LFDIVF      PIC X VALUE 'N'.
           05  WS-PROCESS-SW      PIC X VALUE 'Y'.

      * 処理カウンタ
       01  WS-COUNTERS.
           05  WS-REC-READ-CNT    PIC 9(8) VALUE 0.
           05  WS-REC-UPD-CNT     PIC 9(8) VALUE 0.
           05  WS-ERR-CNT         PIC 9(8) VALUE 0.

      * 計算用ワーク領域
       01  WS-CALC-AREA.
           05  WS-CUR-RESERVE-AMT PIC S9(15)V99 VALUE 0.
           05  WS-NEW-RESERVE-AMT PIC S9(15)V99 VALUE 0.
           05  WS-DIV-TOTAL-AMT   PIC S9(15)V99 VALUE 0.
           05  WS-CV-CHARGE-AMT   PIC S9(15)V99 VALUE 0.

      * 検索用ワーク領域
       01  WS-SEARCH-AREA.
           05  WS-SEARCH-POL-NO   PIC X(12) VALUE SPACES.
           05  WS-MATCH-CV-FOUND  PIC X VALUE 'N'.

      * メッセージ出力領域
       01  WS-MSG-AREA.
           05  WS-MSG-TEXT        PIC X(100) VALUE SPACES.

      * 表示用ワーク領域
       01  WS-DISPLAY-AREA.
           05  WS-DISP-RESERVE-AMT    PIC Z(15)9.99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM OPEN-FILES.
           IF WS-PROCESS-SW = 'N'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.

           PERFORM PROCESS-RESERVE-RECORDS
               UNTIL WS-EOF-LFRSVF = 'Y'.

           PERFORM CLOSE-FILES.

           IF WS-ERR-CNT > 0
               MOVE 8 TO RETURN-CODE
               GOBACK
           ELSE
               MOVE 0 TO RETURN-CODE
               GOBACK
           END-IF.

       OPEN-FILES.
           OPEN INPUT LFRSVF-FILE.
           IF LFRSVF-STATUS NOT = '00'
               STRING '積立金残高ファイルOPEN失敗 ST='
                   DELIMITED BY SIZE
                   LFRSVF-STATUS DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
               MOVE 'N' TO WS-PROCESS-SW
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFCVRF-FILE.
           IF LFCVRF-STATUS NOT = '00'
               STRING '解約返戻金計算ファイルOPEN失敗 ST='
                   DELIMITED BY SIZE
                   LFCVRF-STATUS DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
               MOVE 'N' TO WS-PROCESS-SW
               CLOSE LFRSVF-FILE
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFDIVF-FILE.
           IF LFDIVF-STATUS NOT = '00'
               STRING '配当ファイルOPEN失敗 ST='
                   DELIMITED BY SIZE
                   LFDIVF-STATUS DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
               MOVE 'N' TO WS-PROCESS-SW
               CLOSE LFRSVF-FILE
               CLOSE LFCVRF-FILE
               EXIT PARAGRAPH
           END-IF.

       PROCESS-RESERVE-RECORDS.
           READ LFRSVF-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-LFRSVF
               NOT AT END
                   ADD 1 TO WS-REC-READ-CNT
                   PERFORM VALIDATE-AND-UPDATE-RECORD
           END-READ.

       VALIDATE-AND-UPDATE-RECORD.
           IF RS-RESERVE-AMT < 0
               ADD 1 TO WS-ERR-CNT
               MOVE RS-RESERVE-AMT TO WS-DISP-RESERVE-AMT
               STRING '残高負数エラー PL='
                   DELIMITED BY SIZE
                   RS-POL-NO DELIMITED BY SIZE
                   ' AMT=' DELIMITED BY SIZE
                   WS-DISP-RESERVE-AMT DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
               EXIT PARAGRAPH
           END-IF.

           EVALUATE RS-CALC-STATUS-KBN
               WHEN '01'
                   CONTINUE
               WHEN '08'
                   EXIT PARAGRAPH
               WHEN '09'
                   EXIT PARAGRAPH
               WHEN OTHER
                   ADD 1 TO WS-ERR-CNT
                   STRING 'ステータス値不正 PL='
                       DELIMITED BY SIZE
                       RS-POL-NO DELIMITED BY SIZE
                       ' ST=' DELIMITED BY SIZE
                       RS-CALC-STATUS-KBN DELIMITED BY SIZE
                       INTO WS-MSG-TEXT
                   END-STRING
                   DISPLAY WS-MSG-TEXT
                   EXIT PARAGRAPH
           END-EVALUATE.

           MOVE RS-RESERVE-AMT TO WS-CUR-RESERVE-AMT.
           MOVE 0 TO WS-DIV-TOTAL-AMT.
           MOVE 0 TO WS-CV-CHARGE-AMT.
           MOVE 'N' TO WS-MATCH-CV-FOUND.

           PERFORM ACCUMULATE-DIVIDEND-AMOUNTS.

           PERFORM PROCESS-SURRENDER-CHARGES.

           COMPUTE WS-NEW-RESERVE-AMT = 
               WS-CUR-RESERVE-AMT - WS-DIV-TOTAL-AMT 
               - WS-CV-CHARGE-AMT.

           IF WS-NEW-RESERVE-AMT < 0
               MOVE 0 TO WS-NEW-RESERVE-AMT
           END-IF.

           MOVE WS-NEW-RESERVE-AMT TO RS-RESERVE-AMT.

           PERFORM REWRITE-RESERVE-RECORD.

       ACCUMULATE-DIVIDEND-AMOUNTS.
           MOVE RS-POL-NO TO WS-SEARCH-POL-NO.
           MOVE 0 TO WS-DIV-TOTAL-AMT.
           MOVE 'N' TO WS-EOF-LFDIVF.

           MOVE WS-SEARCH-POL-NO TO DV-POL-NO.
           START LFDIVF-FILE
               KEY IS NOT LESS THAN DV-POL-NO
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   PERFORM UNTIL WS-EOF-LFDIVF = 'Y'
                       READ LFDIVF-FILE
                           AT END
                               MOVE 'Y' TO WS-EOF-LFDIVF
                           NOT AT END
                               IF DV-POL-NO NOT = WS-SEARCH-POL-NO
                                   MOVE 'Y' TO WS-EOF-LFDIVF
                               ELSE
                                   EVALUATE DV-DIV-STATUS-KBN
                                       WHEN '01'
                                           ADD DV-DIV-AMT 
                                               TO WS-DIV-TOTAL-AMT
                                       WHEN OTHER
                                           CONTINUE
                                   END-EVALUATE
                               END-IF
                       END-READ
                   END-PERFORM
           END-START.

       PROCESS-SURRENDER-CHARGES.
           MOVE RS-POL-NO TO WS-SEARCH-POL-NO.
           MOVE 'N' TO WS-MATCH-CV-FOUND.
           MOVE 'N' TO WS-EOF-LFCVRF.

           PERFORM UNTIL WS-EOF-LFCVRF = 'Y'
               READ LFCVRF-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-LFCVRF
                   NOT AT END
                       IF CO-POL-NO = WS-SEARCH-POL-NO
                           MOVE 'Y' TO WS-MATCH-CV-FOUND
                           IF CO-CALC-STATUS-KBN = '01'
                               MOVE CO-SURR-CHARGE-AMT 
                                   TO WS-CV-CHARGE-AMT
                           ELSE
                               MOVE 0 TO WS-CV-CHARGE-AMT
                           END-IF
                           MOVE 'Y' TO WS-EOF-LFCVRF
                       END-IF
               END-READ
           END-PERFORM.

       REWRITE-RESERVE-RECORD.
           IF LFRSVF-STATUS = '00'
               ADD 1 TO WS-REC-UPD-CNT
           ELSE
               ADD 1 TO WS-ERR-CNT
               STRING '残高更新エラー PL='
                   DELIMITED BY SIZE
                   RS-POL-NO DELIMITED BY SIZE
                   ' ST=' DELIMITED BY SIZE
                   LFRSVF-STATUS DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
           END-IF.

       CLOSE-FILES.
           CLOSE LFRSVF-FILE.
           IF LFRSVF-STATUS NOT = '00'
               STRING '積立金残高ファイルCLOSE失敗 ST='
                   DELIMITED BY SIZE
                   LFRSVF-STATUS DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
               ADD 1 TO WS-ERR-CNT
           END-IF.

           CLOSE LFCVRF-FILE.
           IF LFCVRF-STATUS NOT = '00'
               STRING '解約返戻金計算ファイルCLOSE失敗 ST='
                   DELIMITED BY SIZE
                   LFCVRF-STATUS DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
               ADD 1 TO WS-ERR-CNT
           END-IF.

           CLOSE LFDIVF-FILE.
           IF LFDIVF-STATUS NOT = '00'
               STRING '配当ファイルCLOSE失敗 ST='
                   DELIMITED BY SIZE
                   LFDIVF-STATUS DELIMITED BY SIZE
                   INTO WS-MSG-TEXT
               END-STRING
               DISPLAY WS-MSG-TEXT
               ADD 1 TO WS-ERR-CNT
           END-IF.

           STRING '処理完了 読込='
               DELIMITED BY SIZE
               WS-REC-READ-CNT DELIMITED BY SIZE
               ' 更新=' DELIMITED BY SIZE
               WS-REC-UPD-CNT DELIMITED BY SIZE
               ' エラー=' DELIMITED BY SIZE
               WS-ERR-CNT DELIMITED BY SIZE
               INTO WS-MSG-TEXT
           END-STRING.
           DISPLAY WS-MSG-TEXT.
