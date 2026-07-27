       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF290B.
      * 解約・失効候補作成バッチ
      *========================================================
      * 変更履歴
      * 版数  年月日    担当  概要
      * 1.0  20200901  契約システム課  初版作成
      *========================================================
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCNTF-FILE ASSIGN TO LS-LFCNTF
               ORGANIZATION IS INDEXED
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS FS-LFCNTF
               .
           SELECT LPCLMF-FILE ASSIGN TO LS-LPCLMF
               ORGANIZATION IS INDEXED
               RECORD KEY IS CL-CLAIM-ID
               FILE STATUS IS FS-LPCLMF
               .
           SELECT LPPAYF-FILE ASSIGN TO LS-LPPAYF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LPPAYF
               .
           SELECT LFCHGF-FILE ASSIGN TO LS-LFCHGF
               ORGANIZATION IS INDEXED
               RECORD KEY IS CG-CHANGE-ID
               FILE STATUS IS FS-LFCHGF
               .
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFCNTF-FILE.
       COPY LFCNTFC.
       
       FD  LPCLMF-FILE.
       COPY LPCLMFC.
       
       FD  LPPAYF-FILE.
       COPY LPPAYFC.
       
       FD  LFCHGF-FILE.
       COPY LFCHGFC.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-PARAMETERS.
           05  LS-LFCNTF              PIC X(100) VALUE SPACES.
           05  LS-LPCLMF              PIC X(100) VALUE SPACES.
           05  LS-LPPAYF              PIC X(100) VALUE SPACES.
           05  LS-LFCHGF              PIC X(100) VALUE SPACES.
       
       01  WS-FILE-STATUS.
           05  FS-LFCNTF              PIC XX VALUE SPACES.
           05  FS-LPCLMF              PIC XX VALUE SPACES.
           05  FS-LPPAYF              PIC XX VALUE SPACES.
           05  FS-LFCHGF              PIC XX VALUE SPACES.
       
       01  WS-CONTROL-FLAGS.
           05  WS-EOF-LFCNTF          PIC 9 VALUE 0.
           05  WS-EOF-LPPAYF          PIC 9 VALUE 0.
           05  WS-PROCESS-OK          PIC 9 VALUE 1.
       
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT        PIC 9(8) COMP VALUE 0.
           05  WS-CHANGE-COUNT        PIC 9(8) COMP VALUE 0.
           05  WS-ERROR-COUNT         PIC 9(8) COMP VALUE 0.
           05  WS-CLAIM-COUNT         PIC 9(8) COMP VALUE 0.
       
       01  WS-WORK-FIELDS.
           05  WS-CURRENT-POL         PIC X(18) VALUE SPACES.
           05  WS-GRACE-PERIOD        PIC 9(3) COMP VALUE 30.
           05  WS-BASE-DATE           PIC 9(8) COMP VALUE 0.
           05  WS-OVERDUE-THRESHOLD   PIC 9(2) COMP VALUE 3.
           05  WS-HAS-DELAYED-PAY     PIC 9 VALUE 0.
           05  WS-IS-MATURED          PIC 9 VALUE 0.
           05  WS-IS-APPROVAL-PENDING PIC 9 VALUE 0.
           05  WS-CHANGE-ID           PIC X(20) VALUE SPACES.
           05  WS-TIMESTAMP           PIC X(8) VALUE SPACES.
           05  WS-STRING-POINTER      PIC 9(4) COMP VALUE 0.
           05  WS-DUE-DATE-NUM        PIC 9(8) COMP VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           ACCEPT WS-BASE-DATE FROM DATE YYYYMMDD.
           
           PERFORM INITIALIZE-PROCESS.
           PERFORM OPEN-ALL-FILES.
           IF WS-PROCESS-OK = 0
               MOVE 8 TO RETURN-CODE
               DISPLAY '開始ファイルオープン失敗'
               GOBACK
           END-IF.
           
           PERFORM READ-CONTRACT-MASTER.
           PERFORM UNTIL WS-EOF-LFCNTF = 1
               PERFORM EVALUATE-CONTRACT
               PERFORM READ-CONTRACT-MASTER
           END-PERFORM.
           
           PERFORM CLOSE-ALL-FILES.
           
           IF WS-ERROR-COUNT = 0
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 8 TO RETURN-CODE
           END-IF.
           
           DISPLAY 'LF290B 処理完了 '
               '件数=' WS-RECORD-COUNT ' '
               '変更=' WS-CHANGE-COUNT ' '
               'エラー=' WS-ERROR-COUNT.
           
           GOBACK.
       
       INITIALIZE-PROCESS.
           MOVE 0 TO WS-RECORD-COUNT.
           MOVE 0 TO WS-CHANGE-COUNT.
           MOVE 0 TO WS-ERROR-COUNT.
           MOVE 0 TO WS-CLAIM-COUNT.
           MOVE 1 TO WS-PROCESS-OK.
       
       OPEN-ALL-FILES.
           ACCEPT LS-LFCNTF FROM ENVIRONMENT 'LFCNTF_PATH'.
           ACCEPT LS-LPCLMF FROM ENVIRONMENT 'LPCLMF_PATH'.
           ACCEPT LS-LPPAYF FROM ENVIRONMENT 'LPPAYF_PATH'.
           ACCEPT LS-LFCHGF FROM ENVIRONMENT 'LFCHGF_PATH'.
           
           OPEN INPUT LFCNTF-FILE.
           IF FS-LFCNTF NOT = '00' AND FS-LFCNTF NOT = '05'
               MOVE 0 TO WS-PROCESS-OK
               DISPLAY 'LFCNTF オープン失敗 ST=' FS-LFCNTF
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LPCLMF-FILE.
           IF FS-LPCLMF NOT = '00' AND FS-LPCLMF NOT = '05'
               MOVE 0 TO WS-PROCESS-OK
               DISPLAY 'LPCLMF オープン失敗 ST=' FS-LPCLMF
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LPPAYF-FILE.
           IF FS-LPPAYF NOT = '00' AND FS-LPPAYF NOT = '05'
               MOVE 0 TO WS-PROCESS-OK
               DISPLAY 'LPPAYF オープン失敗 ST=' FS-LPPAYF
               EXIT PARAGRAPH
           END-IF.
           
           OPEN EXTEND LFCHGF-FILE.
           IF FS-LFCHGF NOT = '00' AND FS-LFCHGF NOT = '05'
               MOVE 0 TO WS-PROCESS-OK
               DISPLAY 'LFCHGF オープン失敗 ST=' FS-LFCHGF
               EXIT PARAGRAPH
           END-IF.
       
       CLOSE-ALL-FILES.
           CLOSE LFCNTF-FILE.
           CLOSE LPCLMF-FILE.
           CLOSE LPPAYF-FILE.
           CLOSE LFCHGF-FILE.
       
       READ-CONTRACT-MASTER.
           READ LFCNTF-FILE
               AT END
                   MOVE 1 TO WS-EOF-LFCNTF
               NOT AT END
                   ADD 1 TO WS-RECORD-COUNT
           END-READ.
           
           IF FS-LFCNTF NOT = '00' AND FS-LFCNTF NOT = '10'
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LFCNTF 読込失敗 ST=' FS-LFCNTF
                   ' POL=' CN-POL-NO
           END-IF.
       
       EVALUATE-CONTRACT.
           MOVE CN-POL-NO TO WS-CURRENT-POL.
           MOVE 0 TO WS-IS-MATURED.
           MOVE 0 TO WS-IS-APPROVAL-PENDING.
           MOVE 0 TO WS-HAS-DELAYED-PAY.
           MOVE 0 TO WS-CLAIM-COUNT.
           
           PERFORM CHECK-CONTRACT-STATUS.
           IF WS-IS-MATURED = 1 OR WS-IS-APPROVAL-PENDING = 1
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM DETERMINE-GRACE-PERIOD.
           PERFORM COUNT-OVERDUE-CLAIMS.
           
           IF WS-CLAIM-COUNT >= WS-OVERDUE-THRESHOLD
               PERFORM CHECK-DELAYED-PAYMENTS
               IF WS-HAS-DELAYED-PAY = 0
                   PERFORM CREATE-LAPSE-RECORD
               END-IF
           END-IF.
       
       CHECK-CONTRACT-STATUS.
      *    満期日が本日以前の場合は失効済み
           IF CN-MATURITY-DATE NOT = 0 AND
              CN-MATURITY-DATE <= WS-BASE-DATE
               MOVE 1 TO WS-IS-MATURED
               EXIT PARAGRAPH
           END-IF.
           
      *    最終変更日から判定：解約承認待ち（簡略版）
           IF CN-LAST-CHANGE-DATE NOT = 0
               IF FUNCTION MOD(CN-LAST-CHANGE-DATE, 100) = 99
                   MOVE 1 TO WS-IS-APPROVAL-PENDING
               END-IF
           END-IF.
       
       DETERMINE-GRACE-PERIOD.
           EVALUATE CN-PAY-METHOD-KBN
               WHEN '1'
                   MOVE 30 TO WS-GRACE-PERIOD
               WHEN '2'
                   MOVE 60 TO WS-GRACE-PERIOD
               WHEN '3'
                   MOVE 45 TO WS-GRACE-PERIOD
               WHEN OTHER
                   MOVE 30 TO WS-GRACE-PERIOD
           END-EVALUATE.
       
       COUNT-OVERDUE-CLAIMS.
      *    LPCLMF 内で該当契約の未収請求をカウント
      *    簡略版：claim count based on due-ym と current date
           MOVE 0 TO WS-CLAIM-COUNT.
           
           IF CL-POL-NO = WS-CURRENT-POL
               IF CL-CLAIM-STATUS-KBN = '01'
                   ADD 1 TO WS-CLAIM-COUNT
               END-IF
           END-IF.
       
       CHECK-DELAYED-PAYMENTS.
      *    LPPAYF でこの契約の遅延入金を検査
      *    遅延：入金日 > 期日 かつ未消込（PY-MATCH-STATUS-KBN='00'）
           MOVE 0 TO WS-HAS-DELAYED-PAY.
           
           IF PY-POL-NO = WS-CURRENT-POL
               IF PY-MATCH-STATUS-KBN = '00'
                   COMPUTE WS-DUE-DATE-NUM =
                       FUNCTION NUMVAL(CN-NEXT-DUE-YM) * 100 + 1
                   IF PY-PAY-DATE > WS-DUE-DATE-NUM
                       MOVE 1 TO WS-HAS-DELAYED-PAY
                   END-IF
               END-IF
           END-IF.
       
       CREATE-LAPSE-RECORD.
           ACCEPT WS-TIMESTAMP FROM DATE YYYYMMDD.
           
           MOVE 1 TO WS-STRING-POINTER.
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               WS-CURRENT-POL(1:9) DELIMITED BY SIZE
               INTO WS-CHANGE-ID
               WITH POINTER WS-STRING-POINTER
           END-STRING.
           
           MOVE WS-CURRENT-POL TO CG-POL-NO.
           MOVE '02' TO CG-CHANGE-TYPE-KBN.
           MOVE WS-BASE-DATE TO CG-APPLY-DATE.
           MOVE CN-PLAN-CD TO CG-OLD-VALUE.
           MOVE '失効候補' TO CG-NEW-VALUE.
           MOVE '00' TO CG-APPROVAL-STATUS-KBN.
           
           WRITE LFCHGF-REC.
           
           IF FS-LFCHGF NOT = '00'
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LFCHGF 書込失敗 ST=' FS-LFCHGF
                   ' POL=' WS-CURRENT-POL
           ELSE
               ADD 1 TO WS-CHANGE-COUNT
           END-IF.
       
       END PROGRAM LF290B.
