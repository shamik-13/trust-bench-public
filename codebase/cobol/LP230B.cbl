       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP230B.
      *
      * 振替結果取込バッチ
      * 金融機関からの振替結果をLPCLMFに反映し、成功分はLPPAYFへ入金
      * レコードを作成する。口座状態の再確認により廃止・名義不一致を
      * 検出し、請求額と入金額の差異を判定する。
      *
      * 版数    年月日      担当        概要
      * 1.00    20200301    収納システム課 初版
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LPCLMF-FILE ASSIGN TO 'LPCLMF'
               ORGANIZATION IS INDEXED
               RECORD KEY IS CL-CLAIM-ID
               FILE STATUS IS LPCLMF-STATUS.
      
           SELECT LPACCF-FILE ASSIGN TO 'LPACCF'
               ORGANIZATION IS INDEXED
               RECORD KEY IS AC-POL-NO
               FILE STATUS IS LPACCF-STATUS.
      
           SELECT LPPAYF-FILE ASSIGN TO 'LPPAYF'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LPPAYF-STATUS.
      
       DATA DIVISION.
       FILE SECTION.
       FD  LPCLMF-FILE.
       COPY LPCLMFC.
      
       FD  LPACCF-FILE.
       COPY LPACCFC.
      
       FD  LPPAYF-FILE.
       COPY LPPAYFC.
      
       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05  LPCLMF-STATUS      PIC XX VALUE '00'.
           05  LPACCF-STATUS      PIC XX VALUE '00'.
           05  LPPAYF-STATUS      PIC XX VALUE '00'.
      
       01  PROCESSING-COUNTERS.
           05  TOTAL-RECORDS      PIC 9(8) VALUE 0.
           05  SUCCESS-COUNT      PIC 9(8) VALUE 0.
           05  REJECT-COUNT       PIC 9(8) VALUE 0.
           05  MISMATCH-COUNT     PIC 9(8) VALUE 0.
           05  ERROR-COUNT        PIC 9(8) VALUE 0.
      
       01  WORKING-VARIABLES.
           05  WK-EOF-FLAG        PIC X VALUE 'N'.
               88  END-OF-FILE                    VALUE 'Y'.
           05  WK-AMT-DIFF        PIC S9(13)V99 VALUE 0.
           05  WK-ACCOUNT-EXISTS  PIC X VALUE 'Y'.
           05  WK-TODAYS-DATE     PIC 9(8) VALUE 0.
           05  WK-TRANSFER-KBN    PIC X(2).
           05  WK-ACC-STATUS-KBN  PIC X(2).
           05  WK-MATCH-STATUS    PIC X(2).
      
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 0 TO TOTAL-RECORDS.
           MOVE 0 TO SUCCESS-COUNT.
           MOVE 0 TO REJECT-COUNT.
           MOVE 0 TO MISMATCH-COUNT.
           MOVE 0 TO ERROR-COUNT.
           MOVE 'N' TO WK-EOF-FLAG.
      
           PERFORM OPEN-FILES.
           IF ERROR-COUNT > 0
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
      
           PERFORM READ-AND-PROCESS-CLAIMS
               UNTIL END-OF-FILE.
      
           PERFORM CLOSE-FILES.
      
           DISPLAY '振替結果取込処理終了'
               ' 総件数=' TOTAL-RECORDS
               ' 成功=' SUCCESS-COUNT
               ' 却下=' REJECT-COUNT
               ' 不一致=' MISMATCH-COUNT
               ' エラー=' ERROR-COUNT.
      
           IF ERROR-COUNT > 0
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
      
           GOBACK.
      
       OPEN-FILES.
           OPEN INPUT LPCLMF-FILE.
           IF LPCLMF-STATUS NOT = '00'
               DISPLAY 'LPCLMF オープン失敗 ST=' LPCLMF-STATUS
               ADD 1 TO ERROR-COUNT
               EXIT PARAGRAPH
           END-IF.
      
           OPEN INPUT LPACCF-FILE.
           IF LPACCF-STATUS NOT = '00'
               DISPLAY 'LPACCF オープン失敗 ST=' LPACCF-STATUS
               ADD 1 TO ERROR-COUNT
               EXIT PARAGRAPH
           END-IF.
      
           OPEN EXTEND LPPAYF-FILE.
           IF LPPAYF-STATUS NOT = '00'
               DISPLAY 'LPPAYF オープン失敗 ST=' LPPAYF-STATUS
               ADD 1 TO ERROR-COUNT
           END-IF.
      
       CLOSE-FILES.
           CLOSE LPCLMF-FILE.
           CLOSE LPACCF-FILE.
           CLOSE LPPAYF-FILE.
      
       READ-AND-PROCESS-CLAIMS.
           READ LPCLMF-FILE
               AT END
                   MOVE 'Y' TO WK-EOF-FLAG
               NOT AT END
                   ADD 1 TO TOTAL-RECORDS
                   PERFORM PROCESS-CLAIM-RECORD
           END-READ.
      
           IF LPCLMF-STATUS NOT = '00' AND LPCLMF-STATUS NOT = '10'
               DISPLAY '請求ファイル読み込みエラー ST='
                   LPCLMF-STATUS
               ADD 1 TO ERROR-COUNT
               MOVE 'Y' TO WK-EOF-FLAG
           END-IF.
      
       PROCESS-CLAIM-RECORD.
           MOVE CL-TRANSFER-RESULT-KBN TO WK-TRANSFER-KBN.
      
           PERFORM LOOKUP-ACCOUNT-DETAILS.
      
           EVALUATE WK-TRANSFER-KBN
               WHEN '00'
                   PERFORM PROCESS-SUCCESS-TRANSFER
               WHEN '01'
                   PERFORM PROCESS-ACCOUNT-CLOSED
               WHEN '02'
                   PERFORM PROCESS-NAME-MISMATCH
               WHEN OTHER
                   PERFORM PROCESS-UNKNOWN-ERROR
           END-EVALUATE.
      
       LOOKUP-ACCOUNT-DETAILS.
           MOVE 'Y' TO WK-ACCOUNT-EXISTS.
           MOVE CL-POL-NO TO AC-POL-NO.
      
           START LPACCF-FILE KEY IS = AC-POL-NO
               INVALID KEY
                   MOVE 'N' TO WK-ACCOUNT-EXISTS
               NOT INVALID KEY
                   READ LPACCF-FILE
                       AT END
                           MOVE 'N' TO WK-ACCOUNT-EXISTS
                       NOT AT END
                           MOVE AC-ACCOUNT-STATUS-KBN
                               TO WK-ACC-STATUS-KBN
                   END-READ
           END-START.
      
           IF LPACCF-STATUS NOT = '00' AND LPACCF-STATUS NOT = '23'
               DISPLAY '口座照合エラー PL-NO=' CL-POL-NO
                   ' ST=' LPACCF-STATUS
               ADD 1 TO ERROR-COUNT
           END-IF.
      
       PROCESS-SUCCESS-TRANSFER.
           IF WK-ACCOUNT-EXISTS = 'N'
               MOVE '99' TO CL-CLAIM-STATUS-KBN
               ADD 1 TO REJECT-COUNT
               PERFORM UPDATE-CLAIM-STATUS
               EXIT PARAGRAPH
           END-IF.
      
           EVALUATE WK-ACC-STATUS-KBN
               WHEN '10'
                   PERFORM ACCOUNT-CLOSED-REJECT
               WHEN '20'
                   PERFORM NAME-MISMATCH-REJECT
               WHEN OTHER
                   IF WK-ACC-STATUS-KBN NOT = '00'
                       PERFORM ACCOUNT-STATUS-REJECT
                       EXIT PARAGRAPH
                   END-IF
           END-EVALUATE.
      
           COMPUTE WK-AMT-DIFF =
               CL-RECEIPT-AMT - CL-BILL-AMT.
      
           IF WK-AMT-DIFF = 0
               MOVE '00' TO WK-MATCH-STATUS
           ELSE
               MOVE '01' TO WK-MATCH-STATUS
               ADD 1 TO MISMATCH-COUNT
           END-IF.
      
           MOVE CL-CLAIM-ID TO PY-PAY-ID.
           MOVE CL-POL-NO TO PY-POL-NO.
           MOVE CL-DUE-YM TO PY-DUE-YM.
           MOVE CL-RECEIPT-AMT TO PY-PAY-AMT.
      
           ACCEPT WK-TODAYS-DATE FROM DATE YYYYMMDD.
           MOVE WK-TODAYS-DATE TO PY-PAY-DATE.
      
           MOVE '01' TO PY-PAY-CHANNEL-KBN.
           MOVE WK-MATCH-STATUS TO PY-MATCH-STATUS-KBN.
      
           WRITE LPPAYF-REC.
           IF LPPAYF-STATUS NOT = '00'
               DISPLAY '入金記録書込エラー ST=' LPPAYF-STATUS
               ADD 1 TO ERROR-COUNT
               EXIT PARAGRAPH
           END-IF.
      
           MOVE '10' TO CL-CLAIM-STATUS-KBN.
           ADD 1 TO SUCCESS-COUNT.
           PERFORM UPDATE-CLAIM-STATUS.
      
       ACCOUNT-CLOSED-REJECT.
           MOVE '20' TO CL-CLAIM-STATUS-KBN.
           ADD 1 TO REJECT-COUNT.
           PERFORM UPDATE-CLAIM-STATUS.
      
       NAME-MISMATCH-REJECT.
           MOVE '21' TO CL-CLAIM-STATUS-KBN.
           ADD 1 TO REJECT-COUNT.
           PERFORM UPDATE-CLAIM-STATUS.
      
       ACCOUNT-STATUS-REJECT.
           MOVE '22' TO CL-CLAIM-STATUS-KBN.
           ADD 1 TO REJECT-COUNT.
           PERFORM UPDATE-CLAIM-STATUS.
      
       PROCESS-ACCOUNT-CLOSED.
           MOVE '20' TO CL-CLAIM-STATUS-KBN.
           ADD 1 TO REJECT-COUNT.
           PERFORM UPDATE-CLAIM-STATUS.
      
       PROCESS-NAME-MISMATCH.
           MOVE '21' TO CL-CLAIM-STATUS-KBN.
           ADD 1 TO REJECT-COUNT.
           PERFORM UPDATE-CLAIM-STATUS.
      
       PROCESS-UNKNOWN-ERROR.
           MOVE '23' TO CL-CLAIM-STATUS-KBN.
           ADD 1 TO ERROR-COUNT.
           PERFORM UPDATE-CLAIM-STATUS.
      
       UPDATE-CLAIM-STATUS.
           REWRITE LPCLMF-REC.
           IF LPCLMF-STATUS NOT = '00'
               DISPLAY '請求更新エラー CL-ID=' CL-CLAIM-ID
                   ' ST=' LPCLMF-STATUS
               ADD 1 TO ERROR-COUNT
           END-IF.
