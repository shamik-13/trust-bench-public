       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR290B.
      
      * 版数01 20211201 保険計理部 初版
      *   規程外計算監査リスト作成
      *   LFPRMFのBAND-KBNが年齢帯判定と一致しない契約を抽出
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF ASSIGN TO 'LFPRMF'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFPRMF.
           SELECT LFPOLF ASSIGN TO 'LFPOLF'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFPOLF.
           SELECT LFAGBF ASSIGN TO 'LFAGBF'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS AB-BAND-KBN
               FILE STATUS IS FS-LFAGBF.
           SELECT LRRPTF ASSIGN TO 'LRRPTF'
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS RP-REPORT-ID
               FILE STATUS IS FS-LRRPTF.
       
       DATA DIVISION.
       FILE SECTION.
       FD LFPRMF.
       COPY LFPRMFC.
       
       FD LFAGBF.
       COPY LFAGBFC.
       
       FD LFPOLF.
       COPY LFPOLFC.
       
       FD LRRPTF.
       COPY LRRPTFC.
       
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS.
           05 FS-LFPRMF PIC XX.
           05 FS-LFPOLF PIC XX.
           05 FS-LFAGBF PIC XX.
           05 FS-LRRPTF PIC XX.
       
       01 WS-PROCESS-FLAGS.
           05 WS-EOF-LFPRMF PIC X VALUE 'N'.
           05 WS-EOF-LFPOLF PIC X VALUE 'N'.
           05 WS-POLICY-FOUND PIC X VALUE 'N'.
           05 WS-BAND-VALID PIC X VALUE 'N'.
       
       01 WS-COUNTERS.
           05 WS-AUDIT-COUNT PIC 9(8) VALUE 0.
           05 WS-PROCESS-COUNT PIC 9(8) VALUE 0.
           05 WS-REPORT-ID PIC 9(8) VALUE 0.
           05 WS-POLICY-IDX PIC 9(5) VALUE 0.
           05 WS-POLICY-COUNT PIC 9(5) VALUE 0.
           05 WS-LOOP-IDX PIC 9(5).
       
       01 WS-WORK-FIELDS.
           05 WS-EXPECTED-BAND PIC XX.
           05 WS-ENTRY-AGE PIC 9(3).
           05 WS-LOOKUP-POL-NO PIC X(10).
       
       01 WS-POLICY-TABLE.
           05 WS-POLICY-TABLE-ITEM OCCURS 9999 TIMES.
               10 POL-T-POL-NO PIC X(10).
               10 POL-T-ENTRY-AGE PIC 9(3).
               10 POL-T-SEX-KBN PIC X.
               10 POL-T-STATUS-KBN PIC XX.
       
       PROCEDURE DIVISION.
       
       MAIN-PROC.
           PERFORM INITIALIZE-PROCESS.
           PERFORM LOAD-POLICY-TABLE.
           PERFORM PROCESS-PREMIUM-FILE.
           PERFORM FINALIZE-PROCESS.
           
           IF RETURN-CODE = 0 THEN
               DISPLAY '監査リスト作成完了 抽出件数='
                   WS-AUDIT-COUNT
           END-IF.
           GOBACK.
       
       INITIALIZE-PROCESS.
           MOVE 0 TO WS-AUDIT-COUNT.
           MOVE 0 TO WS-PROCESS-COUNT.
           MOVE 0 TO WS-REPORT-ID.
           MOVE 0 TO WS-POLICY-COUNT.
           
           OPEN INPUT LFPRMF.
           IF FS-LFPRMF NOT = '00' THEN
               DISPLAY 'LFPRMF オープン失敗 ST=' FS-LFPRMF
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           OPEN INPUT LFPOLF.
           IF FS-LFPOLF NOT = '00' THEN
               DISPLAY 'LFPOLF オープン失敗 ST=' FS-LFPOLF
               MOVE 8 TO RETURN-CODE
               CLOSE LFPRMF
               GOBACK
           END-IF.
           
           OPEN I-O LFAGBF.
           IF FS-LFAGBF NOT = '00' THEN
               DISPLAY 'LFAGBF オープン失敗 ST=' FS-LFAGBF
               MOVE 8 TO RETURN-CODE
               CLOSE LFPRMF LFPOLF
               GOBACK
           END-IF.
           
           OPEN OUTPUT LRRPTF.
           IF FS-LRRPTF NOT = '00' THEN
               DISPLAY 'LRRPTF オープン失敗 ST=' FS-LRRPTF
               MOVE 8 TO RETURN-CODE
               CLOSE LFPRMF LFPOLF LFAGBF
               GOBACK
           END-IF.
       
       LOAD-POLICY-TABLE.
           MOVE 'N' TO WS-EOF-LFPOLF.
           READ LFPOLF INTO LFPOLF-REC
               AT END MOVE 'Y' TO WS-EOF-LFPOLF
           END-READ.
           
           PERFORM UNTIL WS-EOF-LFPOLF = 'Y'
               ADD 1 TO WS-POLICY-COUNT
               IF WS-POLICY-COUNT > 9999 THEN
                   DISPLAY 'ポリシーテーブル満杯エラー'
                   MOVE 8 TO RETURN-CODE
                   CLOSE LFPRMF LFPOLF LFAGBF LRRPTF
                   GOBACK
               END-IF
               
               MOVE PO-POL-NO TO
                   POL-T-POL-NO(WS-POLICY-COUNT)
               MOVE PO-ENTRY-AGE-CNT TO
                   POL-T-ENTRY-AGE(WS-POLICY-COUNT)
               MOVE PO-SEX-KBN TO
                   POL-T-SEX-KBN(WS-POLICY-COUNT)
               MOVE PO-POL-STATUS-KBN TO
                   POL-T-STATUS-KBN(WS-POLICY-COUNT)
               
               READ LFPOLF INTO LFPOLF-REC
                   AT END MOVE 'Y' TO WS-EOF-LFPOLF
               END-READ
           END-PERFORM.
           
           CLOSE LFPOLF.
       
       PROCESS-PREMIUM-FILE.
           MOVE 'N' TO WS-EOF-LFPRMF.
           READ LFPRMF INTO LFPRMF-REC
               AT END MOVE 'Y' TO WS-EOF-LFPRMF
           END-READ.
           
           PERFORM UNTIL WS-EOF-LFPRMF = 'Y'
               PERFORM AUDIT-PREMIUM-RECORD
               READ LFPRMF INTO LFPRMF-REC
                   AT END MOVE 'Y' TO WS-EOF-LFPRMF
               END-READ
           END-PERFORM.
       
       AUDIT-PREMIUM-RECORD.
           ADD 1 TO WS-PROCESS-COUNT.
           
           MOVE PR-POL-NO TO WS-LOOKUP-POL-NO.
           MOVE 'N' TO WS-POLICY-FOUND.
           
           PERFORM VARYING WS-LOOP-IDX FROM 1 BY 1
               UNTIL WS-LOOP-IDX > WS-POLICY-COUNT
               IF POL-T-POL-NO(WS-LOOP-IDX) =
                   WS-LOOKUP-POL-NO THEN
                   MOVE 'Y' TO WS-POLICY-FOUND
                   MOVE WS-LOOP-IDX TO WS-POLICY-IDX
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
           IF WS-POLICY-FOUND = 'N' THEN
               EXIT PARAGRAPH
           END-IF.
           
           IF POL-T-STATUS-KBN(WS-POLICY-IDX) NOT = '01' THEN
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM VALIDATE-PREMIUM-AMOUNT.
           IF WS-AUDIT-COUNT > 0 THEN
               EXIT PARAGRAPH
           END-IF.
           
           MOVE POL-T-ENTRY-AGE(WS-POLICY-IDX) TO
               WS-ENTRY-AGE.
           PERFORM DETERMINE-AGE-BAND.
           
           PERFORM VALIDATE-BAND-EXISTS.
           IF WS-BAND-VALID = 'N' THEN
               PERFORM OUTPUT-AUDIT-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           IF PR-BAND-KBN NOT = WS-EXPECTED-BAND THEN
               PERFORM OUTPUT-AUDIT-RECORD
           END-IF.
       
       VALIDATE-PREMIUM-AMOUNT.
           IF PR-PRM-AMT = 0 THEN
               PERFORM OUTPUT-AUDIT-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           IF PR-PRM-AMT < 0 THEN
               PERFORM OUTPUT-AUDIT-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           IF PR-CALC-STATUS-KBN NOT = '0' THEN
               PERFORM OUTPUT-AUDIT-RECORD
           END-IF.
       
       DETERMINE-AGE-BAND.
           EVALUATE TRUE
               WHEN WS-ENTRY-AGE <= 29
                   MOVE 'A1' TO WS-EXPECTED-BAND
               WHEN WS-ENTRY-AGE <= 39
                   MOVE 'A2' TO WS-EXPECTED-BAND
               WHEN WS-ENTRY-AGE <= 49
                   MOVE 'A3' TO WS-EXPECTED-BAND
               WHEN WS-ENTRY-AGE <= 59
                   MOVE 'A4' TO WS-EXPECTED-BAND
               WHEN OTHER
                   MOVE 'A5' TO WS-EXPECTED-BAND
           END-EVALUATE.
       
       VALIDATE-BAND-EXISTS.
           MOVE WS-EXPECTED-BAND TO AB-BAND-KBN.
           READ LFAGBF INTO LFAGBF-REC
               KEY IS AB-BAND-KBN
           END-READ.
           
           IF FS-LFAGBF = '00' AND
              AB-MAINT-STATUS-KBN = '1' THEN
               MOVE 'Y' TO WS-BAND-VALID
           ELSE
               MOVE 'N' TO WS-BAND-VALID
           END-IF.
       
       OUTPUT-AUDIT-RECORD.
           ADD 1 TO WS-REPORT-ID.
           ADD 1 TO WS-AUDIT-COUNT.
           
           MOVE WS-REPORT-ID TO RP-REPORT-ID.
           MOVE PR-POL-NO TO RP-POL-NO.
           MOVE POL-T-ENTRY-AGE(WS-POLICY-IDX) TO
               RP-LINE-NO.
           MOVE PR-PRM-AMT TO RP-PRINT-AMT.
           MOVE '01' TO RP-OUTPUT-STATUS-KBN.
           
           WRITE LRRPTF-REC.
           IF FS-LRRPTF NOT = '00' THEN
               DISPLAY 'LRRPTF 書込エラー ST=' FS-LRRPTF
               MOVE 12 TO RETURN-CODE
           END-IF.
       
       FINALIZE-PROCESS.
           CLOSE LFPRMF.
           CLOSE LFAGBF.
           CLOSE LRRPTF.
           
           IF RETURN-CODE NOT = 0 THEN
               DISPLAY '処理中止 RC=' RETURN-CODE
           END-IF.
