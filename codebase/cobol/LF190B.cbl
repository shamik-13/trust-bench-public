       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF190B.
      *月次積立金集計バッチ LF190B
      *
      *変更履歴
      *版数   実施年月日  担当        概要
      *01.00  20240401   山田太郎    初版 月次積立金集計
      *01.01  20240415   田中花子    解約返戻金集計ロジック追加
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2-FILE ASSIGN TO "LFPOLF2"
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS FS-LFPOLF2.
           
           SELECT LFRSVF-FILE ASSIGN TO "LFRSVF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS FS-LFRSVF.
           
           SELECT LFCVRF-FILE ASSIGN TO "LFCVRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-LFCVRF.
           
           SELECT LFMTHF-FILE ASSIGN TO "LFMTHF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFMTHF.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2-FILE.
       COPY LFPOLF2C.
       
       FD  LFRSVF-FILE.
       COPY LFRSVC.
       
       FD  LFCVRF-FILE.
       COPY LFCVRFC.
       
       FD  LFMTHF-FILE.
       COPY LFMTHC.
       
       WORKING-STORAGE SECTION.
       01  WS-PROG-ID           PIC X(8) VALUE 'LF190B'.
       01  WS-VERSION           PIC X(6) VALUE '01.01'.
       01  WS-RUN-DATETIME.
           05 WS-RUN-DATE       PIC 9(8).
           05 WS-RUN-TIME       PIC 9(6).
       
       01  WS-FILE-STATUS.
           05 FS-LFPOLF2        PIC XX VALUE '00'.
           05 FS-LFRSVF         PIC XX VALUE '00'.
           05 FS-LFCVRF         PIC XX VALUE '00'.
           05 FS-LFMTHF         PIC XX VALUE '00'.
       
       01  WS-CONTROL-FLAGS.
           05 WS-EOF-LFPOLF2    PIC X VALUE 'N'.
           05 WS-EOF-LFCVRF     PIC X VALUE 'N'.
           05 WS-ERROR-FLAG     PIC X VALUE 'N'.
           05 WS-SUMMARY-YM     PIC 9(6).
       
       01  WS-COUNTERS.
           05 WS-POLF-COUNT     PIC 9(8) VALUE 0.
           05 WS-SUMMARY-COUNT  PIC 9(4) VALUE 0.
           05 WS-CV-COUNT       PIC 9(4) VALUE 0.
           05 WS-WRITE-COUNT    PIC 9(8) VALUE 0.
           05 WS-IDX            PIC 9(4).
           05 WS-JDX            PIC 9(4).
           05 WS-KDX            PIC 9(4).
       
       01  WS-MONTHLY-SUMMARY.
           05 WS-SUMMARY-TABLE OCCURS 200 TIMES
               INDEXED BY SUM-IDX.
               10 SUM-PRODUCT-CD       PIC X(4).
               10 SUM-CONTRACT-STATUS  PIC X(2).
               10 SUM-POLICY-COUNT     PIC 9(8) VALUE 0.
               10 SUM-RESERVE-AMOUNT   PIC S9(15)V99 VALUE 0.
               10 SUM-CV-TOTAL-AMOUNT  PIC S9(15)V99 VALUE 0.
       
       01  WS-CV-MAPPING.
           05 WS-CV-TABLE OCCURS 500 TIMES
               INDEXED BY CV-IDX.
               10 CV-POLICY-NO         PIC X(16).
               10 CV-RETURN-AMOUNT     PIC S9(15)V99.
       
       01  WS-SORT-WORKSPACE.
           05 WS-SORT-IDX             PIC 9(4).
           05 WS-SORT-JDX             PIC 9(4).
           05 WS-SORT-KEY1            PIC X(4).
           05 WS-SORT-KEY2            PIC X(2).
           05 WS-SORT-POL-CNT         PIC 9(8).
           05 WS-SORT-RES-AMT         PIC S9(15)V99.
           05 WS-SORT-CV-AMT          PIC S9(15)V99.
       
       01  WS-MATCHING-FLAG       PIC X VALUE 'N'.
       01  WS-CV-FOUND-FLAG       PIC X VALUE 'N'.
       
      *エラーメッセージ定数
       01  MSG-FILE-OPEN-ERROR    PIC X(50).
       01  MSG-FILE-READ-ERROR    PIC X(50).
       01  MSG-FILE-WRITE-ERROR   PIC X(50).
       01  MSG-PROCESS-COMPLETE   PIC X(50) 
           VALUE '月次集計処理完了'.
       01  MSG-PROCESS-ERROR      PIC X(50)
           VALUE '月次集計処理異常終了'.
       01  MSG-TABLE-OVERFLOW     PIC X(50)
           VALUE 'サマリーテーブルオーバーフロー'.
       01  MSG-CV-OVERFLOW        PIC X(50)
           VALUE 'CVマッピングテーブルオーバーフロー'.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-RUN-TIME FROM TIME.
           
           MOVE WS-RUN-DATE(1:6) TO WS-SUMMARY-YM.
           
           PERFORM OPEN-ALL-FILES.
           IF WS-ERROR-FLAG = 'Y'
               PERFORM CLOSE-ALL-FILES
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM LOAD-CV-DATA.
           IF WS-ERROR-FLAG = 'Y'
               PERFORM CLOSE-ALL-FILES
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM PROCESS-ALL-POLICIES.
           IF WS-ERROR-FLAG = 'Y'
               PERFORM CLOSE-ALL-FILES
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM SORT-SUMMARY.
           
           PERFORM WRITE-SUMMARY-OUTPUT.
           IF WS-ERROR-FLAG = 'Y'
               PERFORM CLOSE-ALL-FILES
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM CLOSE-ALL-FILES.
           
           DISPLAY MSG-PROCESS-COMPLETE.
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       OPEN-ALL-FILES.
           OPEN INPUT LFPOLF2-FILE.
           IF FS-LFPOLF2 NOT = '00'
               STRING 'LFPOLF2 オープン失敗 ST=' DELIMITED BY SIZE
                   INTO MSG-FILE-OPEN-ERROR
               DISPLAY MSG-FILE-OPEN-ERROR
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFRSVF-FILE.
           IF FS-LFRSVF NOT = '00'
               STRING 'LFRSVF オープン失敗 ST=' DELIMITED BY SIZE
                   INTO MSG-FILE-OPEN-ERROR
               DISPLAY MSG-FILE-OPEN-ERROR
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFCVRF-FILE.
           IF FS-LFCVRF NOT = '00'
               STRING 'LFCVRF オープン失敗 ST=' DELIMITED BY SIZE
                   INTO MSG-FILE-OPEN-ERROR
               DISPLAY MSG-FILE-OPEN-ERROR
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF.
           
           OPEN OUTPUT LFMTHF-FILE.
           IF FS-LFMTHF NOT = '00'
               STRING 'LFMTHF オープン失敗 ST=' DELIMITED BY SIZE
                   INTO MSG-FILE-OPEN-ERROR
               DISPLAY MSG-FILE-OPEN-ERROR
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.
       
       LOAD-CV-DATA.
           MOVE 'N' TO WS-EOF-LFCVRF.
           MOVE 0 TO WS-CV-COUNT.
           PERFORM UNTIL WS-EOF-LFCVRF = 'Y'
               READ LFCVRF-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-LFCVRF
                   NOT AT END
                       IF CO-CALC-STATUS-KBN = '01'
                           ADD 1 TO WS-CV-COUNT
                           IF WS-CV-COUNT > 500
                               DISPLAY MSG-CV-OVERFLOW
                               MOVE 'Y' TO WS-ERROR-FLAG
                               EXIT PARAGRAPH
                           END-IF
                           MOVE CO-POL-NO TO 
                               CV-POLICY-NO(WS-CV-COUNT)
                           MOVE CO-CV-AMT TO 
                               CV-RETURN-AMOUNT(WS-CV-COUNT)
                       END-IF
               END-READ
               IF FS-LFCVRF NOT = '00' AND FS-LFCVRF NOT = '10'
                   DISPLAY 'LFCVRF 読込エラー ST=' FS-LFCVRF
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.
       
       PROCESS-ALL-POLICIES.
           MOVE 'N' TO WS-EOF-LFPOLF2.
           MOVE 0 TO WS-POLF-COUNT.
           PERFORM UNTIL WS-EOF-LFPOLF2 = 'Y'
               READ LFPOLF2-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-LFPOLF2
                   NOT AT END
                       ADD 1 TO WS-POLF-COUNT
                       PERFORM GET-RESERVE-DATA
                       IF WS-ERROR-FLAG = 'Y'
                           EXIT PARAGRAPH
                       END-IF
               END-READ
               IF FS-LFPOLF2 NOT = '00' AND FS-LFPOLF2 NOT = '10'
                   DISPLAY 'LFPOLF2 読込エラー ST=' FS-LFPOLF2
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.
       
       GET-RESERVE-DATA.
           MOVE PO-POL-NO TO RS-POL-NO.
           READ LFRSVF-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   PERFORM UPDATE-SUMMARY-TOTALS
           END-READ.
       
       UPDATE-SUMMARY-TOTALS.
           MOVE 'N' TO WS-MATCHING-FLAG.
           MOVE 1 TO WS-IDX.
           PERFORM UNTIL WS-IDX > WS-SUMMARY-COUNT
               IF SUM-PRODUCT-CD(WS-IDX) = PO-PRODUCT-CD
                   AND SUM-CONTRACT-STATUS(WS-IDX) =
                       PO-CONTRACT-STATUS-KBN
                   MOVE 'Y' TO WS-MATCHING-FLAG
                   SET SUM-IDX TO WS-IDX
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-IDX
           END-PERFORM.
           
           IF WS-MATCHING-FLAG = 'N'
               ADD 1 TO WS-SUMMARY-COUNT
               IF WS-SUMMARY-COUNT > 200
                   DISPLAY MSG-TABLE-OVERFLOW
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               SET SUM-IDX TO WS-SUMMARY-COUNT
               MOVE PO-PRODUCT-CD TO 
                   SUM-PRODUCT-CD(SUM-IDX)
               MOVE PO-CONTRACT-STATUS-KBN TO
                   SUM-CONTRACT-STATUS(SUM-IDX)
               MOVE 0 TO SUM-POLICY-COUNT(SUM-IDX)
               MOVE 0 TO SUM-RESERVE-AMOUNT(SUM-IDX)
               MOVE 0 TO SUM-CV-TOTAL-AMOUNT(SUM-IDX)
           END-IF.
           
           ADD 1 TO SUM-POLICY-COUNT(SUM-IDX).
           ADD RS-RESERVE-AMT TO 
               SUM-RESERVE-AMOUNT(SUM-IDX).
           
           PERFORM FIND-CV-AMOUNT.
       
       FIND-CV-AMOUNT.
           MOVE 'N' TO WS-CV-FOUND-FLAG.
           MOVE 1 TO WS-JDX.
           PERFORM UNTIL WS-JDX > WS-CV-COUNT
               IF CV-POLICY-NO(WS-JDX) = PO-POL-NO
                   ADD CV-RETURN-AMOUNT(WS-JDX) TO 
                       SUM-CV-TOTAL-AMOUNT(SUM-IDX)
                   MOVE 'Y' TO WS-CV-FOUND-FLAG
               END-IF
               ADD 1 TO WS-JDX
           END-PERFORM.
       
       SORT-SUMMARY.
           MOVE 1 TO WS-SORT-IDX.
           PERFORM UNTIL WS-SORT-IDX >= WS-SUMMARY-COUNT
               MOVE 1 TO WS-SORT-JDX
               PERFORM UNTIL WS-SORT-JDX >
                   WS-SUMMARY-COUNT - WS-SORT-IDX
                   IF SUM-PRODUCT-CD(WS-SORT-JDX) >
                       SUM-PRODUCT-CD(WS-SORT-JDX + 1)
                       OR (SUM-PRODUCT-CD(WS-SORT-JDX) =
                           SUM-PRODUCT-CD(WS-SORT-JDX + 1)
                           AND SUM-CONTRACT-STATUS(WS-SORT-JDX) >
                           SUM-CONTRACT-STATUS(WS-SORT-JDX + 1))
                       PERFORM SWAP-SUMMARY-ENTRIES
                   END-IF
                   ADD 1 TO WS-SORT-JDX
               END-PERFORM
               ADD 1 TO WS-SORT-IDX
           END-PERFORM.
       
       SWAP-SUMMARY-ENTRIES.
           MOVE SUM-PRODUCT-CD(WS-SORT-JDX) TO WS-SORT-KEY1.
           MOVE SUM-CONTRACT-STATUS(WS-SORT-JDX) TO WS-SORT-KEY2.
           MOVE SUM-POLICY-COUNT(WS-SORT-JDX) TO WS-SORT-POL-CNT.
           MOVE SUM-RESERVE-AMOUNT(WS-SORT-JDX) TO WS-SORT-RES-AMT.
           MOVE SUM-CV-TOTAL-AMOUNT(WS-SORT-JDX) TO WS-SORT-CV-AMT.
           
           MOVE SUM-PRODUCT-CD(WS-SORT-JDX + 1) TO 
               SUM-PRODUCT-CD(WS-SORT-JDX).
           MOVE SUM-CONTRACT-STATUS(WS-SORT-JDX + 1) TO
               SUM-CONTRACT-STATUS(WS-SORT-JDX).
           MOVE SUM-POLICY-COUNT(WS-SORT-JDX + 1) TO
               SUM-POLICY-COUNT(WS-SORT-JDX).
           MOVE SUM-RESERVE-AMOUNT(WS-SORT-JDX + 1) TO
               SUM-RESERVE-AMOUNT(WS-SORT-JDX).
           MOVE SUM-CV-TOTAL-AMOUNT(WS-SORT-JDX + 1) TO
               SUM-CV-TOTAL-AMOUNT(WS-SORT-JDX).
           
           MOVE WS-SORT-KEY1 TO 
               SUM-PRODUCT-CD(WS-SORT-JDX + 1).
           MOVE WS-SORT-KEY2 TO
               SUM-CONTRACT-STATUS(WS-SORT-JDX + 1).
           MOVE WS-SORT-POL-CNT TO
               SUM-POLICY-COUNT(WS-SORT-JDX + 1).
           MOVE WS-SORT-RES-AMT TO
               SUM-RESERVE-AMOUNT(WS-SORT-JDX + 1).
           MOVE WS-SORT-CV-AMT TO
               SUM-CV-TOTAL-AMOUNT(WS-SORT-JDX + 1).
       
       WRITE-SUMMARY-OUTPUT.
           MOVE 1 TO WS-KDX.
           PERFORM UNTIL WS-KDX > WS-SUMMARY-COUNT
               MOVE WS-SUMMARY-YM TO MT-SUMMARY-YM
               MOVE SUM-PRODUCT-CD(WS-KDX) TO MT-PRODUCT-CD
               MOVE SUM-CONTRACT-STATUS(WS-KDX) TO 
                   MT-CONTRACT-STATUS-KBN
               MOVE SUM-POLICY-COUNT(WS-KDX) TO MT-POL-CNT
               MOVE SUM-RESERVE-AMOUNT(WS-KDX) TO 
                   MT-RESERVE-TOTAL-AMT
               MOVE SUM-CV-TOTAL-AMOUNT(WS-KDX) TO 
                   MT-CV-TOTAL-AMT
               
               WRITE LFMTHF-REC
               IF FS-LFMTHF NOT = '00'
                   DISPLAY 'LFMTHF 書込エラー ST=' FS-LFMTHF
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               
               ADD 1 TO WS-WRITE-COUNT
               ADD 1 TO WS-KDX
           END-PERFORM.
       
       CLOSE-ALL-FILES.
           IF FS-LFPOLF2 = '00'
               CLOSE LFPOLF2-FILE
           END-IF.
           IF FS-LFRSVF = '00'
               CLOSE LFRSVF-FILE
           END-IF.
           IF FS-LFCVRF = '00'
               CLOSE LFCVRF-FILE
           END-IF.
           IF FS-LFMTHF = '00'
               CLOSE LFMTHF-FILE
           END-IF.
