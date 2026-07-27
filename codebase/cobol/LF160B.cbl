       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF160B.
      *┌─────────────────────────────────────────────────────────────┐
      *│ 版数   年月日     担当      概要                              │
      *│ 1.00  20210601   OKADA     初版 解約返戻金明細作成バッチ    │
      *└─────────────────────────────────────────────────────────────┘
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCVRF-FILE ASSIGN TO WS-LFCVRF-FNAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFCVRF-STATUS.
           SELECT LFPOLF2-FILE ASSIGN TO WS-LFPOLF2-FNAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               ACCESS IS RANDOM
               FILE STATUS IS WS-LFPOLF2-STATUS.
           SELECT LFREQF-FILE ASSIGN TO WS-LFREQF-FNAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS RQ-REQ-ID
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-LFREQF-STATUS.
           SELECT LFREPF-FILE ASSIGN TO WS-LFREPF-FNAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFREPF-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFCVRF-FILE.
       COPY LFCVRFC.
       
       FD  LFPOLF2-FILE.
       COPY LFPOLF2C.
       
       FD  LFREQF-FILE.
       COPY LFREQC.
       
       FD  LFREPF-FILE.
       COPY LFREPC.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-NAMES.
           05  WS-LFCVRF-FNAME    PIC X(256) VALUE
               '/data/LFCVRF'.
           05  WS-LFPOLF2-FNAME   PIC X(256) VALUE
               '/data/LFPOLF2'.
           05  WS-LFREQF-FNAME    PIC X(256) VALUE
               '/data/LFREQF'.
           05  WS-LFREPF-FNAME    PIC X(256) VALUE
               '/data/LFREPF'.
       
       01  WS-FILE-STATUS.
           05  WS-LFCVRF-STATUS   PIC XX VALUE '00'.
           05  WS-LFPOLF2-STATUS  PIC XX VALUE '00'.
           05  WS-LFREQF-STATUS   PIC XX VALUE '00'.
           05  WS-LFREPF-STATUS   PIC XX VALUE '00'.
       
       01  WS-COUNTERS.
           05  WS-CV-READ-COUNT   PIC 9(8) COMP VALUE 0.
           05  WS-REPORT-WRITE    PIC 9(8) COMP VALUE 0.
           05  WS-ERROR-COUNT     PIC 9(8) COMP VALUE 0.
           05  WS-DETAIL-LINENO   PIC 9(6) COMP VALUE 0.
       
       01  WS-REPORT-ID           PIC 9(12) COMP VALUE 0.
       
       01  WS-PROCESSING-FLAGS.
           05  WS-EOF-LFCVRF      PIC X VALUE 'N'.
           05  WS-POLICY-FOUND    PIC X VALUE 'N'.
           05  WS-RECEIPT-FOUND   PIC X VALUE 'N'.
       
       01  WS-TEMP-POL-NO         PIC X(12).
       01  WS-TEMP-REQ-ID         PIC X(12).
       01  WS-TEMP-CV-STATUS      PIC XX.
       
       PROCEDURE DIVISION.
           PERFORM INITIALIZE-PROCESS.
           
           PERFORM UNTIL WS-EOF-LFCVRF = 'Y'
               READ LFCVRF-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-LFCVRF
                   NOT AT END
                       PERFORM PROCESS-SURRENDER-RECORD
               END-READ
           END-PERFORM.
           
           PERFORM CLEANUP-PROCESS.
           
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       INITIALIZE-PROCESS.
      *    ファイルオープン
           OPEN INPUT LFCVRF-FILE.
           IF WS-LFCVRF-STATUS NOT = '00'
               DISPLAY
                   '◎LFCVRF オープン失敗 ST=' WS-LFCVRF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           OPEN INPUT LFPOLF2-FILE.
           IF WS-LFPOLF2-STATUS NOT = '00'
               DISPLAY
                   '◎LFPOLF2 オープン失敗 ST=' WS-LFPOLF2-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           OPEN INPUT LFREQF-FILE.
           IF WS-LFREQF-STATUS NOT = '00'
               DISPLAY
                   '◎LFREQF オープン失敗 ST=' WS-LFREQF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           OPEN OUTPUT LFREPF-FILE.
           IF WS-LFREPF-STATUS NOT = '00'
               DISPLAY
                   '◎LFREPF オープン失敗 ST=' WS-LFREPF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
       
       PROCESS-SURRENDER-RECORD.
      *    入力件数をカウント
           ADD 1 TO WS-CV-READ-COUNT.
           
      *    解約返戻金計算対象状態を確認 (01=計算対象のみ)
           MOVE CO-CALC-STATUS-KBN TO WS-TEMP-CV-STATUS.
           
           IF WS-TEMP-CV-STATUS NOT = '01'
      *        計算対象外または無効
               PERFORM CREATE-ERROR-DETAIL
               EXIT PARAGRAPH
           END-IF.
           
      *    契約情報をLFPOLF2から取得
           MOVE CO-POL-NO TO WS-TEMP-POL-NO.
           MOVE CO-POL-NO TO PO-POL-NO.
           
           READ LFPOLF2-FILE
               AT END
                   MOVE 'N' TO WS-POLICY-FOUND
               NOT AT END
                   MOVE 'Y' TO WS-POLICY-FOUND
           END-READ.
           
           IF WS-POLICY-FOUND NOT = 'Y'
      *        契約マスターが見つからない
               PERFORM CREATE-ERROR-DETAIL
               EXIT PARAGRAPH
           END-IF.
           
      *    受付情報をLFREQFから検索
           MOVE 'N' TO WS-RECEIPT-FOUND.
           CLOSE LFREQF-FILE.
           OPEN INPUT LFREQF-FILE.
           
           PERFORM UNTIL WS-RECEIPT-FOUND = 'Y'
               READ LFREQF-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF RQ-POL-NO = WS-TEMP-POL-NO
                           MOVE 'Y' TO WS-RECEIPT-FOUND
                       END-IF
               END-READ
           END-PERFORM.
           
           IF WS-RECEIPT-FOUND NOT = 'Y'
      *        受付情報が見つからない
               PERFORM CREATE-ERROR-DETAIL
               EXIT PARAGRAPH
           END-IF.
           
      *    正常な解約返戻金明細を作成
           PERFORM CREATE-POLICYHOLDER-DETAIL.
           PERFORM CREATE-INTERNAL-MEMO.
       
       CREATE-POLICYHOLDER-DETAIL.
      *    契約者向け明細行を出力
           ADD 1 TO WS-REPORT-ID.
           ADD 1 TO WS-DETAIL-LINENO.
           
           MOVE WS-REPORT-ID        TO RP-REPORT-ID.
           MOVE WS-DETAIL-LINENO    TO RP-LINE-NO.
           MOVE CO-POL-NO           TO RP-POL-NO.
           MOVE '01'                TO RP-PRINT-KBN.
           MOVE CO-CV-AMT           TO RP-PRINT-AMT.
           MOVE '00'                TO RP-ERROR-KBN.
           
           WRITE LFREPF-REC.
           IF WS-LFREPF-STATUS NOT = '00'
               DISPLAY
                   '◎LFREPF 書込失敗 ST=' WS-LFREPF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           ADD 1 TO WS-REPORT-WRITE.
       
       CREATE-INTERNAL-MEMO.
      *    社内控え行を出力 (返戻金予約額)
           ADD 1 TO WS-REPORT-ID.
           ADD 1 TO WS-DETAIL-LINENO.
           
           MOVE WS-REPORT-ID        TO RP-REPORT-ID.
           MOVE WS-DETAIL-LINENO    TO RP-LINE-NO.
           MOVE CO-POL-NO           TO RP-POL-NO.
           MOVE '02'                TO RP-PRINT-KBN.
           MOVE CO-RESERVE-AMT      TO RP-PRINT-AMT.
           MOVE '00'                TO RP-ERROR-KBN.
           
           WRITE LFREPF-REC.
           IF WS-LFREPF-STATUS NOT = '00'
               DISPLAY
                   '◎LFREPF 書込失敗 ST=' WS-LFREPF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           ADD 1 TO WS-REPORT-WRITE.
       
       CREATE-ERROR-DETAIL.
      *    エラー明細行を出力
           ADD 1 TO WS-REPORT-ID.
           ADD 1 TO WS-DETAIL-LINENO.
           ADD 1 TO WS-ERROR-COUNT.
           
           MOVE WS-REPORT-ID        TO RP-REPORT-ID.
           MOVE WS-DETAIL-LINENO    TO RP-LINE-NO.
           MOVE CO-POL-NO           TO RP-POL-NO.
           MOVE '09'                TO RP-PRINT-KBN.
           MOVE 0                   TO RP-PRINT-AMT.
           MOVE '99'                TO RP-ERROR-KBN.
           
           WRITE LFREPF-REC.
           IF WS-LFREPF-STATUS NOT = '00'
               DISPLAY
                   '◎LFREPF 書込失敗 ST=' WS-LFREPF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           ADD 1 TO WS-REPORT-WRITE.
       
       CLEANUP-PROCESS.
      *    ファイルをクローズ
           CLOSE LFCVRF-FILE.
           CLOSE LFPOLF2-FILE.
           CLOSE LFREQF-FILE.
           CLOSE LFREPF-FILE.
           
      *    終了メッセージを出力
           DISPLAY '◆LF160B 処理完了 '
               'CV読込=' WS-CV-READ-COUNT ' '
               'レポート出力=' WS-REPORT-WRITE ' '
               'エラー=' WS-ERROR-COUNT.
