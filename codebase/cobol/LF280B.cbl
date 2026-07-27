       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF280B.
      * 料率改定対象契約抽出プログラム
      *
      * 変更履歴:
      * 版数  年月日    担当      概要
      * V1.0  20211101  保険計理部  初版作成
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS 有効状態 IS '01'.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF ASSIGN TO 'LFPOLF'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LFPOLF-ST.
           SELECT LFCNTF ASSIGN TO 'LFCNTF'
               ORGANIZATION IS INDEXED
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS LFCNTF-ST.
           SELECT LFPRMF ASSIGN TO 'LFPRMF'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LFPRMF-ST.
           SELECT LFAGBF ASSIGN TO 'LFAGBF'
               ORGANIZATION IS INDEXED
               RECORD KEY IS AB-BAND-KBN
               FILE STATUS IS LFAGBF-ST.
           SELECT LFRVSF ASSIGN TO 'LFRVSF'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LFRVSF-ST.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF.
       COPY LFPOLFC.
       
       FD  LFCNTF.
       COPY LFCNTFC.
       
       FD  LFPRMF.
       COPY LFPRMFC.
       
       FD  LFAGBF.
       COPY LFAGBFC.
       
       FD  LFRVSF.
       COPY LFRVSFC.
       
       WORKING-STORAGE SECTION.
       01  WS-PROCESS-CONTEXT.
           05  WS-NOTICE-YM       PIC 9(6).
           05  WS-NOTICE-SEQ      PIC 9(6) VALUE 0.
           05  WS-BAND-TEMP       PIC X(2).
       
       01  WS-FILE-STATUS-AREA.
           05  LFPOLF-ST          PIC XX.
           05  LFCNTF-ST          PIC XX.
           05  LFPRMF-ST          PIC XX.
           05  LFAGBF-ST          PIC XX.
           05  LFRVSF-ST          PIC XX.
       
       01  WS-CONTROL-FLAGS.
           05  WS-EOF-LFPOLF      PIC X VALUE 'N'.
           05  WS-EOF-LFPRMF      PIC X VALUE 'N'.
           05  WS-FOUND-PRM       PIC X VALUE 'N'.
           05  WS-BAND-VALID      PIC X VALUE 'N'.
       
       01  WS-COUNTERS.
           05  WS-RECS-READ       PIC 9(6) VALUE 0.
           05  WS-RECS-OUTPUT     PIC 9(6) VALUE 0.
           05  WS-ERRORS          PIC 9(6) VALUE 0.
       
       01  WS-CURRENT-VALUES.
           05  WS-TEMP-POL-NO     PIC 9(10).
           05  WS-TEMP-AGE        PIC 9(3).
           05  WS-TEMP-PRM-AMT    PIC 9(9)V99.
       
       PROCEDURE DIVISION.
           PERFORM 初期化処理.
           IF RETURN-CODE NOT = 0
               GOBACK
           END-IF.
           PERFORM メイン処理.
           PERFORM 終了処理.
           GOBACK.
       
       初期化処理.
           MOVE 0 TO RETURN-CODE.
           MOVE FUNCTION CURRENT-DATE(1:6) 
               TO WS-NOTICE-YM.
           MOVE 0 TO WS-NOTICE-SEQ.
           
           OPEN INPUT LFPOLF.
           IF LFPOLF-ST NOT = '00'
               DISPLAY '◆エラー LFPOLF オープン失敗 ST=' 
                   LFPOLF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFCNTF.
           IF LFCNTF-ST NOT = '00'
               DISPLAY '◆エラー LFCNTF オープン失敗 ST=' 
                   LFCNTF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFPRMF.
           IF LFPRMF-ST NOT = '00'
               DISPLAY '◆エラー LFPRMF オープン失敗 ST=' 
                   LFPRMF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFAGBF.
           IF LFAGBF-ST NOT = '00'
               DISPLAY '◆エラー LFAGBF オープン失敗 ST=' 
                   LFAGBF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN OUTPUT LFRVSF.
           IF LFRVSF-ST NOT = '00'
               DISPLAY '◆エラー LFRVSF オープン失敗 ST=' 
                   LFRVSF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
       
       メイン処理.
           MOVE 'N' TO WS-EOF-LFPOLF.
           PERFORM UNTIL WS-EOF-LFPOLF = 'Y'
               READ LFPOLF
                   AT END
                       MOVE 'Y' TO WS-EOF-LFPOLF
                   NOT AT END
                       ADD 1 TO WS-RECS-READ
                       PERFORM ポリシー処理
               END-READ
           END-PERFORM.
       
       ポリシー処理.
           IF PO-POL-STATUS-KBN NOT = '01'
               EXIT PARAGRAPH
           END-IF.
           
           MOVE PO-POL-NO TO WS-TEMP-POL-NO.
           MOVE PO-ENTRY-AGE-CNT TO WS-TEMP-AGE.
           MOVE 0 TO WS-TEMP-PRM-AMT.
           
           PERFORM 年齢帯判定.
           PERFORM バンド有効確認.
           IF WS-BAND-VALID NOT = 'Y'
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM 契約取得.
           IF LFCNTF-ST NOT = '00'
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM 保険料取得.
           IF LFPRMF-ST NOT = '00'
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM 出力レコード生成.
       
       年齢帯判定.
           EVALUATE TRUE
               WHEN WS-TEMP-AGE <= 29
                   MOVE 'A1' TO WS-BAND-TEMP
               WHEN WS-TEMP-AGE <= 39
                   MOVE 'A2' TO WS-BAND-TEMP
               WHEN WS-TEMP-AGE <= 49
                   MOVE 'A3' TO WS-BAND-TEMP
               WHEN WS-TEMP-AGE <= 59
                   MOVE 'A4' TO WS-BAND-TEMP
               WHEN OTHER
                   MOVE 'A5' TO WS-BAND-TEMP
           END-EVALUATE.
       
       バンド有効確認.
           MOVE 'N' TO WS-BAND-VALID.
           MOVE WS-BAND-TEMP TO AB-BAND-KBN.
           READ LFAGBF
               AT END
                   MOVE '23' TO LFAGBF-ST
               NOT AT END
                   IF AB-VALID-FROM-YM <= WS-NOTICE-YM AND
                      WS-NOTICE-YM <= AB-VALID-TO-YM AND
                      AB-MAINT-STATUS-KBN = '01'
                       MOVE 'Y' TO WS-BAND-VALID
                       MOVE '00' TO LFAGBF-ST
                   ELSE
                       MOVE '23' TO LFAGBF-ST
                   END-IF
           END-READ.
       
       契約取得.
           MOVE WS-TEMP-POL-NO TO CN-POL-NO.
           READ LFCNTF
               AT END
                   MOVE '23' TO LFCNTF-ST
               NOT AT END
                   MOVE '00' TO LFCNTF-ST
           END-READ.
       
       保険料取得.
           MOVE 'N' TO WS-EOF-LFPRMF.
           MOVE 'N' TO WS-FOUND-PRM.
           MOVE 0 TO WS-TEMP-PRM-AMT.
           
           PERFORM UNTIL WS-EOF-LFPRMF = 'Y'
               READ LFPRMF
                   AT END
                       MOVE 'Y' TO WS-EOF-LFPRMF
                       MOVE '23' TO LFPRMF-ST
                   NOT AT END
                       IF PR-POL-NO = WS-TEMP-POL-NO
                           MOVE PR-PRM-AMT TO WS-TEMP-PRM-AMT
                           MOVE 'Y' TO WS-FOUND-PRM
                           MOVE '00' TO LFPRMF-ST
                           MOVE 'Y' TO WS-EOF-LFPRMF
                       END-IF
               END-READ
           END-PERFORM.
           
           IF WS-FOUND-PRM NOT = 'Y'
               MOVE '23' TO LFPRMF-ST
           END-IF.
       
       出力レコード生成.
           ADD 1 TO WS-NOTICE-SEQ.
           MOVE WS-NOTICE-SEQ TO RV-NOTICE-ID.
           MOVE WS-TEMP-POL-NO TO RV-POL-NO.
           MOVE WS-NOTICE-YM TO RV-NOTICE-YM.
           MOVE '01' TO RV-NOTICE-TYPE-KBN.
           MOVE WS-TEMP-PRM-AMT TO RV-OLD-PRM-AMT.
           MOVE 0 TO RV-NEW-PRM-AMT.
           MOVE '01' TO RV-NOTICE-STATUS-KBN.
           
           WRITE LFRVSF-REC.
           IF LFRVSF-ST NOT = '00'
               ADD 1 TO WS-ERRORS
               DISPLAY '◆エラー LFRVSF 書込失敗 ST=' 
                   LFRVSF-ST ' POL=' WS-TEMP-POL-NO
           ELSE
               ADD 1 TO WS-RECS-OUTPUT
           END-IF.
       
       終了処理.
           CLOSE LFPOLF.
           CLOSE LFCNTF.
           CLOSE LFPRMF.
           CLOSE LFAGBF.
           CLOSE LFRVSF.
           
           DISPLAY '処理完了: 読込件数=' WS-RECS-READ 
               ' 出力件数=' WS-RECS-OUTPUT ' エラー件数=' 
               WS-ERRORS.
           
           IF WS-ERRORS > 0
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
