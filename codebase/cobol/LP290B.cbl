       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP290B.
      *変更履歴
      * 版数  年月日    担当    概要
      * 1.0   20200401  収納システム課  初版：月次締め保険料請求収納バッチ
      *                         契約単位エラー隔離、請求生成、入金配賦統合
      *
       AUTHOR. みらい生命 システム部.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL.
           SELECT LPACCF ASSIGN TO "LPACCF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS AC-POL-NO.
           SELECT LPCLMF ASSIGN TO "LPCLMF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CL-CLAIM-ID.
           SELECT LPPAYF ASSIGN TO "LPPAYF"
               ORGANIZATION IS SEQUENTIAL.
           SELECT LFCNTF ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CN-POL-NO.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL.
           SELECT LRRPTF ASSIGN TO "LRRPTF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS RP-REPORT-ID.
           SELECT LFCHGF ASSIGN TO "LFCHGF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CG-CHANGE-ID.
       
       DATA DIVISION.
       FILE SECTION.
       FD LFPRMF.
       COPY LFPRMFC.
       
       FD LPACCF.
       COPY LPACCFC.
       
       FD LPCLMF.
       COPY LPCLMFC.
       
       FD LPPAYF.
       COPY LPPAYFC.
       
       FD LFCNTF.
       COPY LFCNTFC.
       
       FD LFPOLF.
       COPY LFPOLFC.
       
       FD LRRPTF.
       COPY LRRPTFC.
       
       FD LFCHGF.
       COPY LFCHGFC.
       
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS.
           05 WS-LFPRMF-STATUS      PIC XX VALUE SPACES.
           05 WS-LPACCF-STATUS      PIC XX VALUE SPACES.
           05 WS-LPCLMF-STATUS      PIC XX VALUE SPACES.
           05 WS-LPPAYF-STATUS      PIC XX VALUE SPACES.
           05 WS-LFCNTF-STATUS      PIC XX VALUE SPACES.
           05 WS-LFPOLF-STATUS      PIC XX VALUE SPACES.
           05 WS-LRRPTF-STATUS      PIC XX VALUE SPACES.
           05 WS-LFCHGF-STATUS      PIC XX VALUE SPACES.
       
       01 WS-COUNTERS.
           05 WS-PROCESSED-COUNT    PIC 9(7) VALUE 0.
           05 WS-SUCCESS-COUNT      PIC 9(7) VALUE 0.
           05 WS-ERROR-COUNT        PIC 9(7) VALUE 0.
           05 WS-CLAIM-SEQ          PIC 9(9) VALUE 0.
           05 WS-PAY-SEQ            PIC 9(9) VALUE 0.
           05 WS-RPT-SEQ            PIC 9(9) VALUE 0.
           05 WS-CHG-SEQ            PIC 9(9) VALUE 0.
       
       01 WS-WORKING-FIELDS.
           05 WS-CURRENT-POL-NO     PIC X(10) VALUE SPACES.
           05 WS-CURRENT-YM         PIC 9(6) VALUE 0.
           05 WS-CALC-AMT           PIC 9(11)V99 VALUE 0.
           05 WS-BALANCE-AMT        PIC 9(11)V99 VALUE 0.
           05 WS-RECEIPT-TOTAL      PIC 9(11)V99 VALUE 0.
           05 WS-UNCOLECTED-AMT     PIC 9(11)V99 VALUE 0.
           05 WS-TRANSFER-DAY       PIC 99 VALUE 0.
           05 WS-ACCOUNT-STATUS     PIC 9 VALUE 0.
           05 WS-CONTRACT-STATUS    PIC XX VALUE SPACES.
           05 WS-CLAIM-STATUS-FLAG  PIC 9 VALUE 0.
           05 WS-TRANSFER-STATUS-FLG PIC 9 VALUE 0.
           05 WS-ALLOCATION-STATUS  PIC 9 VALUE 0.
           05 WS-ERROR-FLAG         PIC 9 VALUE 0.
           05 WS-EOF-FLAG           PIC 9 VALUE 0.
           05 WS-PREV-POL-NO        PIC X(10) VALUE SPACES.
       
       01 WS-EDIT-FIELDS.
           05 WS-EDIT-AMT           PIC Z(10)9.99.
           05 WS-EDIT-DATE          PIC 9(8).
           05 WS-REPORT-MSG         PIC X(80) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM OPEN-FILES.
           IF RETURN-CODE NOT = 0
               GO TO MAIN-EXIT-ERROR
           END-IF.
      
           MOVE 0 TO WS-PROCESSED-COUNT.
           MOVE 0 TO WS-SUCCESS-COUNT.
           MOVE 0 TO WS-ERROR-COUNT.
           MOVE 0 TO WS-CLAIM-SEQ.
           MOVE 0 TO WS-PAY-SEQ.
           MOVE 0 TO WS-RPT-SEQ.
           MOVE 0 TO WS-CHG-SEQ.
           MOVE 0 TO WS-EOF-FLAG.
      
           READ LFPRMF
               AT END
                   MOVE 1 TO WS-EOF-FLAG
               NOT AT END
                   MOVE PR-POL-NO TO WS-CURRENT-POL-NO
           END-READ.
      
           PERFORM UNTIL WS-EOF-FLAG = 1
               PERFORM PROCESS-CONTRACT
               
               IF WS-EOF-FLAG NOT = 1
                   READ LFPRMF
                       AT END
                           MOVE 1 TO WS-EOF-FLAG
                       NOT AT END
                           CONTINUE
                   END-READ
               END-IF
           END-PERFORM.
      
           PERFORM CLOSE-FILES.
           
           MOVE 0 TO RETURN-CODE.
           GO TO MAIN-EXIT.
       
       MAIN-EXIT-ERROR.
           MOVE 8 TO RETURN-CODE.
       
       MAIN-EXIT.
           GOBACK.
       
       OPEN-FILES.
           OPEN INPUT LFPRMF.
           IF WS-LFPRMF-STATUS NOT = "00"
               DISPLAY "LFPRMF オープン失敗 ST=" WS-LFPRMF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN I-O LPACCF.
           IF WS-LPACCF-STATUS NOT = "00"
               DISPLAY "LPACCF オープン失敗 ST=" WS-LPACCF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN I-O LPCLMF.
           IF WS-LPCLMF-STATUS NOT = "00"
               DISPLAY "LPCLMF オープン失敗 ST=" WS-LPCLMF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN OUTPUT LPPAYF.
           IF WS-LPPAYF-STATUS NOT = "00"
               DISPLAY "LPPAYF オープン失敗 ST=" WS-LPPAYF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN I-O LFCNTF.
           IF WS-LFCNTF-STATUS NOT = "00"
               DISPLAY "LFCNTF オープン失敗 ST=" WS-LFCNTF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFPOLF.
           IF WS-LFPOLF-STATUS NOT = "00"
               DISPLAY "LFPOLF オープン失敗 ST=" WS-LFPOLF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN I-O LRRPTF.
           IF WS-LRRPTF-STATUS NOT = "00"
               DISPLAY "LRRPTF オープン失敗 ST=" WS-LRRPTF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN I-O LFCHGF.
           IF WS-LFCHGF-STATUS NOT = "00"
               DISPLAY "LFCHGF オープン失敗 ST=" WS-LFCHGF-STATUS
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           MOVE 0 TO RETURN-CODE.
       
       CLOSE-FILES.
           CLOSE LFPRMF LPACCF LPCLMF LPPAYF LFCNTF LFPOLF
                 LRRPTF LFCHGF.
       
       PROCESS-CONTRACT.
           MOVE 0 TO WS-ERROR-FLAG.
           MOVE WS-CURRENT-POL-NO TO WS-PREV-POL-NO.
           MOVE PR-POL-NO TO WS-CURRENT-POL-NO.
           MOVE PR-PRM-AMT TO WS-CALC-AMT.
           MOVE WS-CALC-AMT TO WS-BALANCE-AMT.
           MOVE 0 TO WS-RECEIPT-TOTAL.
           MOVE 0 TO WS-UNCOLECTED-AMT.
           
           ADD 1 TO WS-PROCESSED-COUNT.
      
           PERFORM GET-CONTRACT-RECORD.
           IF WS-ERROR-FLAG = 1
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
      
           IF CN-POL-NO NOT = WS-CURRENT-POL-NO
               DISPLAY "契約マスタ：契約 " WS-CURRENT-POL-NO
                   " 見つからない"
               MOVE 1 TO WS-ERROR-FLAG
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
           
           PERFORM GET-POLICY-RECORD.
           IF WS-ERROR-FLAG = 1
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
           
           IF PO-POL-STATUS-KBN NOT = "01"
               MOVE PO-POL-STATUS-KBN TO WS-REPORT-MSG
               DISPLAY "契約状態不正：" WS-CURRENT-POL-NO
                   " 状態=" WS-REPORT-MSG
               MOVE 1 TO WS-ERROR-FLAG
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
      
           PERFORM GET-ACCOUNT-RECORD.
           IF WS-ERROR-FLAG = 1
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
      
           PERFORM CREATE-CLAIM-RECORD.
           IF WS-ERROR-FLAG = 1
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
      
           PERFORM PROCESS-RECEIPTS.
           IF WS-ERROR-FLAG = 1
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
      
           PERFORM CREATE-REPORT-DETAIL.
           IF WS-ERROR-FLAG = 1
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
      
           PERFORM LINK-CHANGE-CANDIDATE.
           IF WS-ERROR-FLAG = 1
               ADD 1 TO WS-ERROR-COUNT
               GO TO PROCESS-CONTRACT-EXIT
           END-IF.
           
           ADD 1 TO WS-SUCCESS-COUNT.
       
       PROCESS-CONTRACT-EXIT.
           EXIT.
       
       GET-CONTRACT-RECORD.
           MOVE WS-CURRENT-POL-NO TO CN-POL-NO.
           READ LFCNTF
               AT END
                   DISPLAY "契約マスタ未検出:" WS-CURRENT-POL-NO
                   MOVE 1 TO WS-ERROR-FLAG
               NOT AT END
                   CONTINUE
           END-READ.
       
       GET-POLICY-RECORD.
           MOVE WS-CURRENT-POL-NO TO PO-POL-NO.
           READ LFPOLF
               AT END
                   DISPLAY "契約マスタ未検出:" WS-CURRENT-POL-NO
                   MOVE 1 TO WS-ERROR-FLAG
               NOT AT END
                   CONTINUE
           END-READ.
       
       GET-ACCOUNT-RECORD.
           MOVE WS-CURRENT-POL-NO TO AC-POL-NO.
           READ LPACCF
               AT END
                   DISPLAY "口座未登録:" WS-CURRENT-POL-NO
                   MOVE 1 TO WS-ERROR-FLAG
               NOT AT END
                   MOVE AC-TRANSFER-DAY TO WS-TRANSFER-DAY
                   MOVE AC-ACCOUNT-STATUS-KBN TO WS-ACCOUNT-STATUS
           END-READ.
           
           IF WS-ACCOUNT-STATUS NOT = 1
               DISPLAY "口座状態不正:" WS-CURRENT-POL-NO
               MOVE 1 TO WS-ERROR-FLAG
           END-IF.
       
       CREATE-CLAIM-RECORD.
           ADD 1 TO WS-CLAIM-SEQ.
           MOVE WS-CLAIM-SEQ TO CL-CLAIM-ID.
           MOVE WS-CURRENT-POL-NO TO CL-POL-NO.
           MOVE WS-CURRENT-YM TO CL-DUE-YM.
           MOVE WS-CALC-AMT TO CL-BILL-AMT.
           MOVE 0 TO CL-RECEIPT-AMT.
           MOVE "01" TO CL-CLAIM-STATUS-KBN.
           MOVE "0" TO CL-TRANSFER-RESULT-KBN.
           
           WRITE LPCLMF-REC
               INVALID KEY
                   DISPLAY "請求写込エラー:" WS-CLAIM-SEQ
                   MOVE 1 TO WS-ERROR-FLAG
           END-WRITE.
       
       PROCESS-RECEIPTS.
           ADD 1 TO WS-PAY-SEQ.
           MOVE WS-PAY-SEQ TO PY-PAY-ID.
           MOVE WS-CURRENT-POL-NO TO PY-POL-NO.
           MOVE WS-CURRENT-YM TO PY-DUE-YM.
           MOVE WS-BALANCE-AMT TO PY-PAY-AMT.
           MOVE FUNCTION CURRENT-DATE TO WS-EDIT-DATE.
           MOVE WS-EDIT-DATE TO PY-PAY-DATE.
           MOVE "01" TO PY-PAY-CHANNEL-KBN.
           MOVE "0" TO PY-MATCH-STATUS-KBN.
           
           WRITE LPPAYF-REC.
       
       CREATE-REPORT-DETAIL.
           ADD 1 TO WS-RPT-SEQ.
           MOVE WS-RPT-SEQ TO RP-REPORT-ID.
           MOVE WS-CURRENT-YM TO RP-REPORT-YM.
           MOVE "01" TO RP-REPORT-TYPE-KBN.
           MOVE WS-CURRENT-POL-NO TO RP-POL-NO.
           MOVE 1 TO RP-LINE-NO.
           MOVE WS-CALC-AMT TO RP-PRINT-AMT.
           MOVE "1" TO RP-OUTPUT-STATUS-KBN.
           
           WRITE LRRPTF-REC
               INVALID KEY
                   DISPLAY "帳票写込エラー:" WS-RPT-SEQ
                   MOVE 1 TO WS-ERROR-FLAG
           END-WRITE.
       
       LINK-CHANGE-CANDIDATE.
           ADD 1 TO WS-CHG-SEQ.
           MOVE WS-CHG-SEQ TO CG-CHANGE-ID.
           MOVE WS-CURRENT-POL-NO TO CG-POL-NO.
           MOVE "01" TO CG-CHANGE-TYPE-KBN.
           MOVE FUNCTION CURRENT-DATE TO WS-EDIT-DATE.
           MOVE WS-EDIT-DATE TO CG-APPLY-DATE.
           MOVE SPACES TO CG-OLD-VALUE.
           MOVE SPACES TO CG-NEW-VALUE.
           MOVE "0" TO CG-APPROVAL-STATUS-KBN.
           
           WRITE LFCHGF-REC
               INVALID KEY
                   DISPLAY "異動記録書込エラー:" WS-CHG-SEQ
                   MOVE 1 TO WS-ERROR-FLAG
           END-WRITE.
