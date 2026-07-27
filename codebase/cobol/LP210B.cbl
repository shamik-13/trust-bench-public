       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP210B.
      *版数    年月日      担当        概要
      *V1.0    20190701    収納システム課  初版。LFPRMF読込→LPACCF
      *                                突合→LFCNTF確認→LPCLMF作成
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF-FILE ASSIGN TO WS-PRMF-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PRMF-STATUS.
           SELECT LPACCF-FILE ASSIGN TO WS-ACCF-NAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS AC-POL-NO
               FILE STATUS IS WS-ACCF-STATUS.
           SELECT LFCNTF-FILE ASSIGN TO WS-CNTF-NAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS WS-CNTF-STATUS.
           SELECT LPCLMF-FILE ASSIGN TO WS-CLMF-NAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS CL-CLAIM-ID
               FILE STATUS IS WS-CLMF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  LFPRMF-FILE.
       COPY LFPRMFC.
       FD  LPACCF-FILE.
       COPY LPACCFC.
       FD  LFCNTF-FILE.
       COPY LFCNTFC.
       FD  LPCLMF-FILE.
       COPY LPCLMFC.
       WORKING-STORAGE SECTION.
      * ファイル名とステータス
       01  WS-FILE-DEFS.
           05  WS-PRMF-NAME       PIC X(64).
           05  WS-ACCF-NAME       PIC X(64).
           05  WS-CNTF-NAME       PIC X(64).
           05  WS-CLMF-NAME       PIC X(64).
       01  WS-FILE-STATUS.
           05  WS-PRMF-STATUS     PIC XX.
           05  WS-ACCF-STATUS     PIC XX.
           05  WS-CNTF-STATUS     PIC XX.
           05  WS-CLMF-STATUS     PIC XX.
      * 処理カウンター
       01  WS-COUNTERS.
           05  WS-PRMF-COUNT      PIC 9(7) VALUE 0.
           05  WS-ACC-MISS        PIC 9(7) VALUE 0.
           05  WS-CNT-MISS        PIC 9(7) VALUE 0.
           05  WS-STS-INVALID     PIC 9(7) VALUE 0.
           05  WS-AMT-INVALID     PIC 9(7) VALUE 0.
           05  WS-CLAIM-WRITE     PIC 9(7) VALUE 0.
           05  WS-CLAIM-ERR       PIC 9(7) VALUE 0.
      * 作業変数
       01  WS-WORK-VARS.
           05  WS-EOF             PIC X VALUE 'N'.
           05  WS-CLAIM-ID        PIC 9(12) VALUE 100000000001.
           05  WS-ACC-STS         PIC X(2).
           05  WS-CNTRACT-VALID   PIC X VALUE 'N'.
           05  WS-MATURITY-DATE   PIC 9(8).
           05  WS-TODAY           PIC 9(8).
           05  WS-CLAIM-STATUS    PIC X(2) VALUE '01'.
           05  WS-LOOP-FLAG       PIC X VALUE 'N'.
      * システム日付（環境変数から取得）
       01  WS-SYSTEM-DATE         PIC 9(6).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 0 TO RETURN-CODE.
      * ファイル名取得
           ACCEPT WS-PRMF-NAME FROM ENVIRONMENT 'LFPRMF_PATH'.
           ACCEPT WS-ACCF-NAME FROM ENVIRONMENT 'LPACCF_PATH'.
           ACCEPT WS-CNTF-NAME FROM ENVIRONMENT 'LFCNTF_PATH'.
           ACCEPT WS-CLMF-NAME FROM ENVIRONMENT 'LPCLMF_PATH'.
      * システム日付セット
           ACCEPT WS-SYSTEM-DATE FROM DATE.
           COMPUTE WS-TODAY = 20000000 + WS-SYSTEM-DATE.
      * ファイルオープン
           OPEN INPUT LFPRMF-FILE.
           IF WS-PRMF-STATUS NOT = '00'
               DISPLAY '口座振替 LFPRMF オープン失敗 ST='
                   WS-PRMF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           OPEN I-O LPACCF-FILE.
           IF WS-ACCF-STATUS NOT = '00'
               DISPLAY '口座振替 LPACCF オープン失敗 ST='
                   WS-ACCF-STATUS
               CLOSE LFPRMF-FILE
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           OPEN I-O LFCNTF-FILE.
           IF WS-CNTF-STATUS NOT = '00'
               DISPLAY '口座振替 LFCNTF オープン失敗 ST='
                   WS-CNTF-STATUS
               CLOSE LFPRMF-FILE
               CLOSE LPACCF-FILE
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           OPEN OUTPUT LPCLMF-FILE.
           IF WS-CLMF-STATUS NOT = '00'
               DISPLAY '口座振替 LPCLMF オープン失敗 ST='
                   WS-CLMF-STATUS
               CLOSE LFPRMF-FILE
               CLOSE LPACCF-FILE
               CLOSE LFCNTF-FILE
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
      * メインループ
           PERFORM UNTIL WS-EOF = 'Y'
               READ LFPRMF-FILE INTO LFPRMF-REC
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-PRMF-COUNT
                       PERFORM PROCESS-CLAIM-RECORD
               END-READ
           END-PERFORM.
      * ファイルクローズ
           CLOSE LFPRMF-FILE.
           CLOSE LPACCF-FILE.
           CLOSE LFCNTF-FILE.
           CLOSE LPCLMF-FILE.
      * 統計表示
           DISPLAY 'LP210B 処理完了'.
           DISPLAY '保険料明細読込: ' WS-PRMF-COUNT.
           DISPLAY 'アカウント不一致: ' WS-ACC-MISS.
           DISPLAY '契約情報不一致: ' WS-CNT-MISS.
           DISPLAY '無効な契約状態: ' WS-STS-INVALID.
           DISPLAY '金額検証エラー: ' WS-AMT-INVALID.
           DISPLAY '請求作成成功: ' WS-CLAIM-WRITE.
           DISPLAY '請求作成エラー: ' WS-CLAIM-ERR.
           GOBACK.
       PROCESS-CLAIM-RECORD.
      * アカウント突合
           MOVE PR-POL-NO TO AC-POL-NO.
           READ LPACCF-FILE INTO LPACCF-REC
               INVALID KEY
                   ADD 1 TO WS-ACC-MISS
                   EXIT PARAGRAPH
           END-READ.
           MOVE AC-ACCOUNT-STATUS-KBN TO WS-ACC-STS.
      * 契約情報取得
           MOVE PR-POL-NO TO CN-POL-NO.
           READ LFCNTF-FILE INTO LFCNTF-REC
               INVALID KEY
                   ADD 1 TO WS-CNT-MISS
                   EXIT PARAGRAPH
           END-READ.
      * 契約有効性判定
           MOVE 'N' TO WS-CNTRACT-VALID.
           MOVE CN-MATURITY-DATE TO WS-MATURITY-DATE.
           IF WS-MATURITY-DATE > WS-TODAY
               MOVE 'Y' TO WS-CNTRACT-VALID
           END-IF.
           IF WS-CNTRACT-VALID = 'N'
               ADD 1 TO WS-STS-INVALID
               EXIT PARAGRAPH
           END-IF.
      * 金額妥当性確認
           IF PR-PRM-AMT <= 0
               ADD 1 TO WS-AMT-INVALID
               EXIT PARAGRAPH
           END-IF.
      * 請求レコード作成
           MOVE WS-CLAIM-ID TO CL-CLAIM-ID.
           ADD 1 TO WS-CLAIM-ID.
           MOVE PR-POL-NO TO CL-POL-NO.
           MOVE CN-NEXT-DUE-YM TO CL-DUE-YM.
           MOVE PR-PRM-AMT TO CL-BILL-AMT.
           MOVE 0 TO CL-RECEIPT-AMT.
           MOVE WS-CLAIM-STATUS TO CL-CLAIM-STATUS-KBN.
           MOVE '00' TO CL-TRANSFER-RESULT-KBN.
      * LPCLMF書込
           WRITE LPCLMF-REC
               INVALID KEY
                   ADD 1 TO WS-CLAIM-ERR
                   DISPLAY 'LPCLMF 重複キー エラー CL-CLAIM-ID='
                       CL-CLAIM-ID
               NOT INVALID KEY
                   ADD 1 TO WS-CLAIM-WRITE
           END-WRITE.
