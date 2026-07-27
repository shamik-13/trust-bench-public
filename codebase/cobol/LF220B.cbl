       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF220B.
      * 版数  年月日      担当        概要
      * 1.0   20230628    数理部      初版作成
      *
      * 解約返戻金エラー再処理バッチ
      * LF210Bの計算状態区分がエラーとなった契約を受付単位に再抽出し、
      * 契約情報と責任準備金の補正済みデータから再投入用LFCVPFを作成する。
      * 再投入可否の判定は必須項目、契約状態、金額符号に限定し、
      * 返戻控除額は算出しない。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCVRF
               ASSIGN TO 'LFCVRF'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFCVRF-STATUS.
           SELECT LFREQF
               ASSIGN TO 'LFREQF'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFREQF-STATUS.
           SELECT LFPOLF2
               ASSIGN TO 'LFPOLF2'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS WS-LFPOLF2-STATUS.
           SELECT LFRSVF
               ASSIGN TO 'LFRSVF'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS WS-LFRSVF-STATUS.
           SELECT LFCVPF
               ASSIGN TO 'LFCVPF'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFCVPF-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFCVRF.
       COPY LFCVRFC.
       
       FD  LFREQF.
       COPY LFREQC.
       
       FD  LFPOLF2.
       COPY LFPOLF2C.
       
       FD  LFRSVF.
       COPY LFRSVC.
       
       FD  LFCVPF.
       COPY LFCVPFC.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-LFCVRF-STATUS        PIC XX VALUE '00'.
           05  WS-LFREQF-STATUS        PIC XX VALUE '00'.
           05  WS-LFPOLF2-STATUS       PIC XX VALUE '00'.
           05  WS-LFRSVF-STATUS        PIC XX VALUE '00'.
           05  WS-LFCVPF-STATUS        PIC XX VALUE '00'.
       
       01  WS-CONTROL-FLAGS.
           05  WS-EOF-LFCVRF           PIC X(1) VALUE 'N'.
           05  WS-ERROR-FLG            PIC X(1) VALUE 'N'.
       
       01  WS-VALIDATION-FLAGS.
           05  WS-REQUIRED-OK          PIC X(1) VALUE 'N'.
           05  WS-AMOUNT-OK            PIC X(1) VALUE 'N'.
           05  WS-CONTRACT-OK          PIC X(1) VALUE 'N'.
           05  WS-SIGN-OK              PIC X(1) VALUE 'N'.
       
       01  WS-COUNTERS.
           05  WS-INPUT-CNT            PIC 9(7) VALUE 0.
           05  WS-OUTPUT-CNT           PIC 9(7) VALUE 0.
           05  WS-ERROR-CNT            PIC 9(7) VALUE 0.
       
       01  WS-WORK-FIELDS.
           05  WS-POL-NO               PIC X(10) VALUE SPACES.
           05  WS-RESERVE-AMT          PIC S9(13)V99 VALUE 0.
           05  WS-CV-AMT               PIC S9(13)V99 VALUE 0.
           05  WS-MSG-BUF              PIC X(100) VALUE SPACES.
       
       PROCEDURE DIVISION.
       
       0000-MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZATION.
           IF WS-ERROR-FLG = 'Y'
               MOVE 8 TO RETURN-CODE
               GO TO 9999-TERMINATION
           END-IF.
           
           PERFORM 2000-PROCESS-ERRORS
               UNTIL WS-EOF-LFCVRF = 'Y'.
           
           PERFORM 3000-FINALIZATION.
           
           IF WS-ERROR-FLG = 'Y'
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
           
           GO TO 9999-TERMINATION.
       
       1000-INITIALIZATION.
           OPEN INPUT LFCVRF.
           IF WS-LFCVRF-STATUS NOT = '00'
               MOVE 'Y' TO WS-ERROR-FLG
               STRING 'LFCVRF オープン失敗 ST=' WS-LFCVRF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUF
               DISPLAY WS-MSG-BUF
               GO TO 1000-EXIT
           END-IF.
           
           OPEN INPUT LFREQF.
           IF WS-LFREQF-STATUS NOT = '00'
               MOVE 'Y' TO WS-ERROR-FLG
               STRING 'LFREQF オープン失敗 ST=' WS-LFREQF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUF
               DISPLAY WS-MSG-BUF
               GO TO 1000-EXIT
           END-IF.
           
           OPEN INPUT LFPOLF2.
           IF WS-LFPOLF2-STATUS NOT = '00'
               MOVE 'Y' TO WS-ERROR-FLG
               STRING 'LFPOLF2 オープン失敗 ST=' WS-LFPOLF2-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUF
               DISPLAY WS-MSG-BUF
               GO TO 1000-EXIT
           END-IF.
           
           OPEN INPUT LFRSVF.
           IF WS-LFRSVF-STATUS NOT = '00'
               MOVE 'Y' TO WS-ERROR-FLG
               STRING 'LFRSVF オープン失敗 ST=' WS-LFRSVF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUF
               DISPLAY WS-MSG-BUF
               GO TO 1000-EXIT
           END-IF.
           
           OPEN OUTPUT LFCVPF.
           IF WS-LFCVPF-STATUS NOT = '00'
               MOVE 'Y' TO WS-ERROR-FLG
               STRING 'LFCVPF オープン失敗 ST=' WS-LFCVPF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUF
               DISPLAY WS-MSG-BUF
               GO TO 1000-EXIT
           END-IF.
       
       1000-EXIT.
           EXIT.
       
       2000-PROCESS-ERRORS.
           READ LFCVRF
               AT END
                   MOVE 'Y' TO WS-EOF-LFCVRF
                   GO TO 2000-EXIT
               NOT AT END
                   ADD 1 TO WS-INPUT-CNT
           END-READ.
           
           IF WS-LFCVRF-STATUS NOT = '00'
               ADD 1 TO WS-ERROR-CNT
               STRING 'LFCVRF 読み込み失敗 ST=' WS-LFCVRF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUF
               DISPLAY WS-MSG-BUF
               GO TO 2000-EXIT
           END-IF.
           
      *    計算状態区分がエラー状態のみを処理対象とする
      *    (01=計算対象, 02=計算除外, 09=無効 以外)
           EVALUATE CO-CALC-STATUS-KBN
               WHEN '01'
               WHEN '02'
               WHEN '09'
                   GO TO 2000-EXIT
           END-EVALUATE.
           
      *    必須項目の確認
           PERFORM 4100-VALIDATE-REQUIRED-ITEMS.
           IF WS-REQUIRED-OK NOT = 'Y'
               ADD 1 TO WS-ERROR-CNT
               GO TO 2000-EXIT
           END-IF.
           
      *    金額妥当性の確認
           PERFORM 4200-VALIDATE-AMOUNT-VALUES.
           IF WS-AMOUNT-OK NOT = 'Y'
               ADD 1 TO WS-ERROR-CNT
               GO TO 2000-EXIT
           END-IF.
           
      *    契約状態の確認
           PERFORM 4300-CHECK-CONTRACT-STATUS.
           IF WS-CONTRACT-OK NOT = 'Y'
               ADD 1 TO WS-ERROR-CNT
               GO TO 2000-EXIT
           END-IF.
           
      *    金額符号の確認
           PERFORM 4400-VALIDATE-AMOUNT-SIGN.
           IF WS-SIGN-OK NOT = 'Y'
               ADD 1 TO WS-ERROR-CNT
               GO TO 2000-EXIT
           END-IF.
           
      *    出力レコード作成
           PERFORM 5000-CREATE-OUTPUT-RECORD.
       
       2000-EXIT.
           EXIT.
       
       4100-VALIDATE-REQUIRED-ITEMS.
           MOVE 'Y' TO WS-REQUIRED-OK.
           
           IF CO-POL-NO = SPACES OR CO-POL-NO = LOW-VALUES
               MOVE 'N' TO WS-REQUIRED-OK
               GO TO 4100-EXIT
           END-IF.
           
      *    準備金額と返戻金額の両方がゼロは無効
           IF CO-RESERVE-AMT = 0 AND CO-CV-AMT = 0
               MOVE 'N' TO WS-REQUIRED-OK
           END-IF.
       
       4100-EXIT.
           EXIT.
       
       4200-VALIDATE-AMOUNT-VALUES.
           MOVE 'Y' TO WS-AMOUNT-OK.
           
           IF CO-RESERVE-AMT < 0
               MOVE 'N' TO WS-AMOUNT-OK
               GO TO 4200-EXIT
           END-IF.
           
           IF CO-CV-AMT < 0
               MOVE 'N' TO WS-AMOUNT-OK
           END-IF.
       
       4200-EXIT.
           EXIT.
       
       4300-CHECK-CONTRACT-STATUS.
           MOVE 'Y' TO WS-CONTRACT-OK.
           MOVE CO-POL-NO TO WS-POL-NO.
           MOVE CO-POL-NO TO PO-POL-NO.
           
      *    LFPOLF2から契約情報をキー検索
           READ LFPOLF2
               INVALID KEY
                   MOVE 'N' TO WS-CONTRACT-OK
                   GO TO 4300-EXIT
               NOT INVALID KEY
                   CONTINUE
           END-READ.
           
           IF WS-LFPOLF2-STATUS NOT = '00'
               MOVE 'N' TO WS-CONTRACT-OK
               GO TO 4300-EXIT
           END-IF.
           
      *    契約状態は有効な状態（00, 01, 02）に限定
           EVALUATE PO-CONTRACT-STATUS-KBN
               WHEN '00'
               WHEN '01'
               WHEN '02'
                   MOVE 'Y' TO WS-CONTRACT-OK
               WHEN OTHER
                   MOVE 'N' TO WS-CONTRACT-OK
           END-EVALUATE.
       
       4300-EXIT.
           EXIT.
       
       4400-VALIDATE-AMOUNT-SIGN.
           MOVE 'Y' TO WS-SIGN-OK.
           MOVE CO-RESERVE-AMT TO WS-RESERVE-AMT.
           MOVE CO-CV-AMT TO WS-CV-AMT.
           
      *    準備金と返戻金の符号整合性を確認
      *    両方プラスまたは両方マイナスのみ有効
           IF (WS-RESERVE-AMT >= 0 AND WS-CV-AMT >= 0) OR
              (WS-RESERVE-AMT < 0 AND WS-CV-AMT < 0)
               MOVE 'Y' TO WS-SIGN-OK
           ELSE
               MOVE 'N' TO WS-SIGN-OK
           END-IF.
       
       4400-EXIT.
           EXIT.
       
       5000-CREATE-OUTPUT-RECORD.
           INITIALIZE LFCVPF-REC.
           
           MOVE CO-POL-NO TO CI-POL-NO.
           MOVE CO-RESERVE-AMT TO CI-RESERVE-AMT.
           MOVE CO-SURR-CHARGE-AMT TO CI-NEWBIZ-COST-AMT.
           MOVE 0 TO CI-ELAPSED-MONTH-CNT.
           MOVE '01' TO CI-CV-STATUS-KBN.
           
           WRITE LFCVPF-REC.
           
           IF WS-LFCVPF-STATUS NOT = '00'
               ADD 1 TO WS-ERROR-CNT
               STRING 'LFCVPF 書き込み失敗 ST=' WS-LFCVPF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUF
               DISPLAY WS-MSG-BUF
               GO TO 5000-EXIT
           END-IF.
           
           ADD 1 TO WS-OUTPUT-CNT.
       
       5000-EXIT.
           EXIT.
       
       3000-FINALIZATION.
           CLOSE LFCVRF.
           CLOSE LFREQF.
           CLOSE LFPOLF2.
           CLOSE LFRSVF.
           CLOSE LFCVPF.
           
      *    処理結果をログ出力
           STRING '処理完了 入力=' WS-INPUT-CNT
               ' 出力=' WS-OUTPUT-CNT ' エラー=' WS-ERROR-CNT
               DELIMITED BY SIZE INTO WS-MSG-BUF.
           DISPLAY WS-MSG-BUF.
       
       3000-EXIT.
           EXIT.
       
       9999-TERMINATION.
           GOBACK.
