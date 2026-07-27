       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP280B.
      * 口座振替不能契約異動連携
      * 連続して振替不能となった契約を抽出し、払込方法変更または
      * 口座再登録依頼の契約異動候補をLFCHGFへ連携する
      *
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20190101  佐藤     新規作成
      * 1.01  20200615  田中     失敗理由別処理分岐追加
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LPCLMF-FILE ASSIGN TO LPCLMF
               ORGANIZATION IS INDEXED
               RECORD KEY IS CL-CLAIM-ID
               ACCESS IS SEQUENTIAL
               FILE STATUS IS LPCLMF-ST.
           
           SELECT LPACCF-FILE ASSIGN TO LPACCF
               ORGANIZATION IS INDEXED
               RECORD KEY IS AC-POL-NO
               ACCESS IS DYNAMIC
               FILE STATUS IS LPACCF-ST.
           
           SELECT LFCNTF-FILE ASSIGN TO LFCNTF
               ORGANIZATION IS INDEXED
               RECORD KEY IS CN-POL-NO
               ACCESS IS DYNAMIC
               FILE STATUS IS LFCNTF-ST.
           
           SELECT LFCHGF-FILE ASSIGN TO LFCHGF
               ORGANIZATION IS INDEXED
               RECORD KEY IS CG-CHANGE-ID
               ACCESS IS SEQUENTIAL
               FILE STATUS IS LFCHGF-ST.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LPCLMF-FILE.
       COPY LPCLMFC.
       
       FD  LPACCF-FILE.
       COPY LPACCFC.
       
       FD  LFCNTF-FILE.
       COPY LFCNTFC.
       
       FD  LFCHGF-FILE.
       COPY LFCHGFC.
       
       WORKING-STORAGE SECTION.
       01  LPCLMF-ST               PIC XX.
       01  LPACCF-ST               PIC XX.
       01  LFCNTF-ST               PIC XX.
       01  LFCHGF-ST               PIC XX.
       
       01  WS-PROCESS-FLAGS.
           05  WS-EOF-FLG          PIC X VALUE 'N'.
              88 EOF-LPCLMF        VALUE 'Y'.
           05  WS-SKIP-RECORD-FLG  PIC X VALUE 'N'.
              88 SKIP-RECORD       VALUE 'Y'.
       
       01  WS-COUNTERS.
           05  WS-CLAIM-COUNT      PIC 9(8) VALUE 0.
           05  WS-CHANGE-COUNT     PIC 9(8) VALUE 0.
           05  WS-ERROR-COUNT      PIC 9(8) VALUE 0.
       
       01  WS-WORK-FIELDS.
           05  WS-CHANGE-ID        PIC 9(12) VALUE 0.
           05  WS-CURRENT-DATE     PIC 9(8).
           05  WS-TRANSFER-REASON  PIC X(2).
           05  WS-CHANGE-TYPE-KBN  PIC X(2).
           05  WS-OLD-VALUE-TXT    PIC X(30).
           05  WS-NEW-VALUE-TXT    PIC X(30).
           05  WS-CONCAT-ACCT      PIC X(30).
           05  WS-STATUS-MSG       PIC X(50).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROCESS.
           
           PERFORM OPEN-ALL-FILES.
           IF RETURN-CODE NOT = 0
               GOBACK
           END-IF.
           
           PERFORM READ-AND-PROCESS-CLAIMS
               UNTIL EOF-LPCLMF.
           
           PERFORM CLOSE-ALL-FILES.
           
           PERFORM DISPLAY-SUMMARY.
           
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       INITIALIZE-PROCESS.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE 0 TO WS-CHANGE-ID.
           MOVE 0 TO WS-CLAIM-COUNT.
           MOVE 0 TO WS-CHANGE-COUNT.
           MOVE 0 TO WS-ERROR-COUNT.
       
       OPEN-ALL-FILES.
           OPEN INPUT LPCLMF-FILE.
           IF LPCLMF-ST NOT = '00'
               MOVE 8 TO RETURN-CODE
               STRING 'エラー：LPCLMF オープン失敗 ST=' 
                   LPCLMF-ST DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
               GOBACK
           END-IF.
           
           OPEN INPUT LPACCF-FILE.
           IF LPACCF-ST NOT = '00'
               MOVE 8 TO RETURN-CODE
               STRING 'エラー：LPACCF オープン失敗 ST=' 
                   LPACCF-ST DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
               GOBACK
           END-IF.
           
           OPEN INPUT LFCNTF-FILE.
           IF LFCNTF-ST NOT = '00'
               MOVE 8 TO RETURN-CODE
               STRING 'エラー：LFCNTF オープン失敗 ST=' 
                   LFCNTF-ST DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
               GOBACK
           END-IF.
           
           OPEN OUTPUT LFCHGF-FILE.
           IF LFCHGF-ST NOT = '00'
               MOVE 8 TO RETURN-CODE
               STRING 'エラー：LFCHGF オープン失敗 ST=' 
                   LFCHGF-ST DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
               GOBACK
           END-IF.
       
       READ-AND-PROCESS-CLAIMS.
           READ LPCLMF-FILE INTO LPCLMF-REC
               AT END
                   MOVE 'Y' TO WS-EOF-FLG
               NOT AT END
                   ADD 1 TO WS-CLAIM-COUNT
                   MOVE 'N' TO WS-SKIP-RECORD-FLG
                   PERFORM EVALUATE-TRANSFER-RESULT
           END-READ.
           
           IF LPCLMF-ST NOT = '00' AND LPCLMF-ST NOT = '10'
               MOVE 8 TO RETURN-CODE
               STRING 'エラー：LPCLMF 読み込み失敗 ST=' 
                   LPCLMF-ST DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
               GOBACK
           END-IF.
       
       EVALUATE-TRANSFER-RESULT.
      *    振替結果コードに応じた処理分岐
           EVALUATE CL-TRANSFER-RESULT-KBN
               WHEN '01'
      *        口座廃止 → 口座再登録依頼
                   MOVE '01' TO WS-TRANSFER-REASON
                   PERFORM PROCESS-ACCOUNT-CLOSURE
               WHEN '02'
      *        残高不足 → 督促対象に留める（異動作成なし）
                   MOVE '02' TO WS-TRANSFER-REASON
                   MOVE 'Y' TO WS-SKIP-RECORD-FLG
               WHEN '03'
      *        名義不一致 → 払込方法変更
                   MOVE '03' TO WS-TRANSFER-REASON
                   PERFORM PROCESS-NAME-MISMATCH
               WHEN OTHER
                   MOVE 'Y' TO WS-SKIP-RECORD-FLG
           END-EVALUATE.
       
       PROCESS-ACCOUNT-CLOSURE.
      *    契約の有効性を確認後、口座再登録異動を作成
           PERFORM VALIDATE-CONTRACT.
           
           IF SKIP-RECORD
               PERFORM INCREMENT-ERROR-COUNT
               GOBACK
           END-IF.
           
           PERFORM RETRIEVE-ACCOUNT-INFO.
           
           IF SKIP-RECORD
               PERFORM INCREMENT-ERROR-COUNT
               GOBACK
           END-IF.
           
           MOVE '02' TO WS-CHANGE-TYPE-KBN.
           MOVE AC-POL-NO TO WS-OLD-VALUE-TXT.
           STRING AC-BANK-CD DELIMITED BY SIZE
                   AC-BRANCH-CD DELIMITED BY SIZE
                   AC-ACCOUNT-NO DELIMITED BY SIZE
               INTO WS-NEW-VALUE-TXT
           END-STRING.
           
           PERFORM WRITE-CHANGE-RECORD.
       
       PROCESS-NAME-MISMATCH.
      *    契約の有効性を確認後、払込方法変更異動を作成
           PERFORM VALIDATE-CONTRACT.
           
           IF SKIP-RECORD
               PERFORM INCREMENT-ERROR-COUNT
               GOBACK
           END-IF.
           
           PERFORM RETRIEVE-ACCOUNT-INFO.
           
           IF SKIP-RECORD
               PERFORM INCREMENT-ERROR-COUNT
               GOBACK
           END-IF.
           
           MOVE '01' TO WS-CHANGE-TYPE-KBN.
           MOVE CN-PAY-METHOD-KBN TO WS-OLD-VALUE-TXT.
           MOVE '01' TO WS-NEW-VALUE-TXT.
           
           PERFORM WRITE-CHANGE-RECORD.
       
       VALIDATE-CONTRACT.
      *    LFCNTF から契約情報を取得し、有効性を確認
           MOVE CL-POL-NO TO CN-POL-NO.
           
           READ LFCNTF-FILE INTO LFCNTF-REC
               KEY IS CN-POL-NO
               INVALID KEY
                   MOVE 'Y' TO WS-SKIP-RECORD-FLG
           END-READ.
           
           IF LFCNTF-ST NOT = '00' AND LFCNTF-ST NOT = '23'
               MOVE 8 TO RETURN-CODE
               STRING 'エラー：LFCNTF 読み込み失敗 ST=' 
                   LFCNTF-ST DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
               GOBACK
           END-IF.
           
           IF LFCNTF-ST = '23'
               MOVE 'Y' TO WS-SKIP-RECORD-FLG
           END-IF.
       
       RETRIEVE-ACCOUNT-INFO.
      *    LPACCF から口座情報を取得
           MOVE CL-POL-NO TO AC-POL-NO.
           
           READ LPACCF-FILE INTO LPACCF-REC
               KEY IS AC-POL-NO
               INVALID KEY
                   MOVE 'Y' TO WS-SKIP-RECORD-FLG
           END-READ.
           
           IF LPACCF-ST NOT = '00' AND LPACCF-ST NOT = '23'
               MOVE 8 TO RETURN-CODE
               STRING 'エラー：LPACCF 読み込み失敗 ST=' 
                   LPACCF-ST DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
               GOBACK
           END-IF.
           
           IF LPACCF-ST = '23'
               MOVE 'Y' TO WS-SKIP-RECORD-FLG
           END-IF.
       
       WRITE-CHANGE-RECORD.
      *    LFCHGF に契約異動レコードを書き込み
           ADD 1 TO WS-CHANGE-ID.
           
           MOVE WS-CHANGE-ID TO CG-CHANGE-ID.
           MOVE CL-POL-NO TO CG-POL-NO.
           MOVE WS-CHANGE-TYPE-KBN TO CG-CHANGE-TYPE-KBN.
           MOVE WS-CURRENT-DATE TO CG-APPLY-DATE.
           MOVE WS-OLD-VALUE-TXT TO CG-OLD-VALUE.
           MOVE WS-NEW-VALUE-TXT TO CG-NEW-VALUE.
           MOVE '00' TO CG-APPROVAL-STATUS-KBN.
           
           WRITE LFCHGF-REC.
           
           IF LFCHGF-ST NOT = '00'
               ADD 1 TO WS-ERROR-COUNT
               STRING 'エラー：LFCHGF 書き込み失敗 ST=' 
                   LFCHGF-ST ' 契約=' CL-POL-NO 
                   DELIMITED BY SIZE
                   INTO WS-STATUS-MSG
               DISPLAY WS-STATUS-MSG
           ELSE
               ADD 1 TO WS-CHANGE-COUNT
           END-IF.
       
       INCREMENT-ERROR-COUNT.
           ADD 1 TO WS-ERROR-COUNT.
       
       CLOSE-ALL-FILES.
           CLOSE LPCLMF-FILE.
           CLOSE LPACCF-FILE.
           CLOSE LFCNTF-FILE.
           CLOSE LFCHGF-FILE.
       
       DISPLAY-SUMMARY.
           DISPLAY '処理完了：請求件数=' WS-CLAIM-COUNT
                   ' 異動件数=' WS-CHANGE-COUNT
                   ' エラー件数=' WS-ERROR-COUNT.
