       IDENTIFICATION DIVISION.
       PROGRAM-ID. LV250B.
      *━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      * 変更履歴
      *━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      * 版  年月日      担当        概要
      * 1.0 2025/01/15 田中一郎    初版 契約照会索引を構築
      * 1.1 2025/02/28 鈴木花子    計算対象ステータス判定追加
      *━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      * 契約照会索引更新バッチ LV250B
      * 
      * 目的:
      *  契約マスタ LFPOLF2、異動履歴 LVCHGF、返戻金計算結果 LFCVRF
      *  から照会索引ファイル LFREPF を構築する。
      *  各契約について最新の状態サマリ行を出力する。
      *  返戻金計算済み有無と計算状態は転記のみ。
      *━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2-FILE ASSIGN TO "LFPOLF2"
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS LS-LFPOLF2-STATUS.
           
           SELECT LVCHGF-FILE ASSIGN TO "LVCHGF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CH-CHANGE-ID
               FILE STATUS IS LS-LVCHGF-STATUS.
           
           SELECT LFCVRF-FILE ASSIGN TO "LFCVRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LS-LFCVRF-STATUS.
           
           SELECT LFREPF-FILE ASSIGN TO "LFREPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LS-LFREPF-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2-FILE.
       COPY LFPOLF2C.
       
       FD  LVCHGF-FILE.
       COPY LVCHGC.
       
       FD  LFCVRF-FILE.
       COPY LFCVRFC.
       
       FD  LFREPF-FILE.
       COPY LFREPC.
       
       WORKING-STORAGE SECTION.
       01  LS-FILE-STATUS-FIELDS.
           05  LS-LFPOLF2-STATUS    PIC XX VALUE SPACES.
           05  LS-LVCHGF-STATUS     PIC XX VALUE SPACES.
           05  LS-LFCVRF-STATUS     PIC XX VALUE SPACES.
           05  LS-LFREPF-STATUS     PIC XX VALUE SPACES.
       
       01  LS-PROCESS-CONTROL.
           05  LS-EOF-LFPOLF2       PIC X VALUE 'N'.
           05  LS-EOF-LFCVRF        PIC X VALUE 'N'.
           05  LS-PROC-STATUS       PIC X VALUE 'N'.
           05  LS-REPORT-SEQ-NO     PIC 9(8) VALUE 0.
           05  LS-POLICY-PROCESS-CT PIC 9(8) VALUE 0.
           05  LS-ERROR-COUNT       PIC 9(6) VALUE 0.
       
       01  LS-CURRENT-POLICY.
           05  LS-CURR-POL-NO       PIC X(12) VALUE SPACES.
           05  LS-CURR-PRODUCT      PIC X(4) VALUE SPACES.
           05  LS-CURR-STATUS-KBN   PIC X(2) VALUE SPACES.
           05  LS-CURR-PAID-TO-DT   PIC 9(8) VALUE 0.
       
       01  LS-CV-CALC-STATE.
           05  LS-CV-FOUND-FLAG     PIC X VALUE 'N'.
           05  LS-CV-STATUS-KBN     PIC X(2) VALUE SPACES.
           05  LS-CV-CALC-STATUS    PIC X(2) VALUE SPACES.
           05  LS-CV-CALC-DATE      PIC 9(8) VALUE 0.
           05  LS-CV-RESERVE-AMT    PIC 9(13)V99 VALUE 0.
           05  LS-CV-SURR-CHG-AMT   PIC 9(13)V99 VALUE 0.
       
       01  LS-WORK-FIELDS.
           05  LS-TEMP-POL-NO       PIC X(12) VALUE SPACES.
           05  LS-TEMP-DATE         PIC 9(8) VALUE 0.
           05  LS-CALC-AMT          PIC 9(13)V99 VALUE 0.
       
       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           PERFORM 100-INITIALIZE.
           
           IF LS-PROC-STATUS = 'Y'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM 200-OPEN-ALL-FILES.
           
           IF LS-PROC-STATUS = 'Y'
               PERFORM 500-CLOSE-ALL-FILES
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM 300-PRE-READ-LFCVRF.
           PERFORM 400-PROCESS-POLICIES.
           
           IF LS-PROC-STATUS = 'Y'
               PERFORM 500-CLOSE-ALL-FILES
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM 500-CLOSE-ALL-FILES.
           
           IF LS-PROC-STATUS = 'Y'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       100-INITIALIZE.
           MOVE 'N' TO LS-EOF-LFPOLF2.
           MOVE 'N' TO LS-EOF-LFCVRF.
           MOVE 'N' TO LS-PROC-STATUS.
           MOVE 0 TO LS-REPORT-SEQ-NO.
           MOVE 0 TO LS-POLICY-PROCESS-CT.
           MOVE 0 TO LS-ERROR-COUNT.
       
       200-OPEN-ALL-FILES.
           OPEN INPUT LFPOLF2-FILE.
           IF LS-LFPOLF2-STATUS NOT = "00"
               DISPLAY '契約マスタ LFPOLF2 オープン失敗 ST='
                   LS-LFPOLF2-STATUS
               MOVE 'Y' TO LS-PROC-STATUS
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LVCHGF-FILE.
           IF LS-LVCHGF-STATUS NOT = "00"
               DISPLAY '異動履歴 LVCHGF オープン失敗 ST='
                   LS-LVCHGF-STATUS
               MOVE 'Y' TO LS-PROC-STATUS
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFCVRF-FILE.
           IF LS-LFCVRF-STATUS NOT = "00"
               DISPLAY '返戻金 LFCVRF オープン失敗 ST='
                   LS-LFCVRF-STATUS
               MOVE 'Y' TO LS-PROC-STATUS
               EXIT PARAGRAPH
           END-IF.
           
           OPEN OUTPUT LFREPF-FILE.
           IF LS-LFREPF-STATUS NOT = "00"
               DISPLAY '照会索引 LFREPF オープン失敗 ST='
                   LS-LFREPF-STATUS
               MOVE 'Y' TO LS-PROC-STATUS
               EXIT PARAGRAPH
           END-IF.
       
       300-PRE-READ-LFCVRF.
      *    LFCVRF(順編成)の全レコードを読込み、POL-NO でハッシュ化
      *    実装簡略: メモリ内テーブルは未実装のため、
      *    毎回の LFCVRF スキャンで対応
           MOVE 'N' TO LS-EOF-LFCVRF.
       
       400-PROCESS-POLICIES.
           MOVE 'N' TO LS-EOF-LFPOLF2.
           
           PERFORM UNTIL LS-EOF-LFPOLF2 = 'Y'
               READ LFPOLF2-FILE
                   AT END
                       MOVE 'Y' TO LS-EOF-LFPOLF2
                   NOT AT END
                       PERFORM 410-LOAD-POLICY-CONTEXT
                       PERFORM 420-FETCH-LATEST-CV-DATA
                       PERFORM 430-BUILD-REPORT-ROW
                       PERFORM 440-WRITE-REPORT-ROW
                       ADD 1 TO LS-POLICY-PROCESS-CT
               END-READ
               
               IF LS-LFPOLF2-STATUS NOT = "00" AND
                   LS-LFPOLF2-STATUS NOT = "10"
                   DISPLAY '契約マスタ読込失敗 ST='
                       LS-LFPOLF2-STATUS
                   MOVE 'Y' TO LS-PROC-STATUS
                   MOVE 'Y' TO LS-EOF-LFPOLF2
               END-IF
           END-PERFORM.
       
       410-LOAD-POLICY-CONTEXT.
           MOVE PO-POL-NO TO LS-CURR-POL-NO.
           MOVE PO-PRODUCT-CD TO LS-CURR-PRODUCT.
           MOVE PO-CONTRACT-STATUS-KBN TO LS-CURR-STATUS-KBN.
           MOVE PO-PAID-TO-DATE TO LS-CURR-PAID-TO-DT.
       
       420-FETCH-LATEST-CV-DATA.
           MOVE 'N' TO LS-CV-FOUND-FLAG.
           MOVE SPACES TO LS-CV-STATUS-KBN.
           MOVE SPACES TO LS-CV-CALC-STATUS.
           MOVE 0 TO LS-CV-CALC-DATE.
           MOVE 0 TO LS-CV-RESERVE-AMT.
           MOVE 0 TO LS-CV-SURR-CHG-AMT.
           
           PERFORM 421-SCAN-LFCVRF-FOR-POLICY.
       
       421-SCAN-LFCVRF-FOR-POLICY.
           MOVE 'N' TO LS-EOF-LFCVRF.
           
           PERFORM UNTIL LS-EOF-LFCVRF = 'Y'
               READ LFCVRF-FILE
                   AT END
                       MOVE 'Y' TO LS-EOF-LFCVRF
                   NOT AT END
                       IF CO-POL-NO = LS-CURR-POL-NO
                           PERFORM 421-VALIDATE-CV-STATUS
                           IF LS-CV-FOUND-FLAG = 'N'
                               MOVE 'Y' TO LS-CV-FOUND-FLAG
                               MOVE CO-CALC-STATUS-KBN TO
                                   LS-CV-CALC-STATUS
                               MOVE CO-RESERVE-AMT TO
                                   LS-CV-RESERVE-AMT
                               MOVE CO-SURR-CHARGE-AMT TO
                                   LS-CV-SURR-CHG-AMT
                           END-IF
                       END-IF
               END-READ
               
               IF LS-LFCVRF-STATUS NOT = "00" AND
                   LS-LFCVRF-STATUS NOT = "10"
                   DISPLAY '返戻金読込失敗 ST='
                       LS-LFCVRF-STATUS
                   MOVE 'Y' TO LS-PROC-STATUS
                   MOVE 'Y' TO LS-EOF-LFCVRF
               END-IF
           END-PERFORM.
       
       421-VALIDATE-CV-STATUS.
      *    返戻金計算対象状態の検証
      *    計算対象(01)= 計算対象
      *    計算除外(08)= 計算除外
      *    無効(09)    = 無効
           EVALUATE CO-CALC-STATUS-KBN
               WHEN '01'
                   MOVE '01' TO LS-CV-STATUS-KBN
               WHEN '08'
                   MOVE '08' TO LS-CV-STATUS-KBN
               WHEN '09'
                   MOVE '09' TO LS-CV-STATUS-KBN
               WHEN OTHER
                   ADD 1 TO LS-ERROR-COUNT
                   DISPLAY '返戻金ステータス不正: ' 
                       LS-CURR-POL-NO ' ST='
                       CO-CALC-STATUS-KBN
           END-EVALUATE.
       
       430-BUILD-REPORT-ROW.
           ADD 1 TO LS-REPORT-SEQ-NO.
           
           MOVE LS-REPORT-SEQ-NO TO RP-REPORT-ID.
           MOVE 1 TO RP-LINE-NO.
           MOVE LS-CURR-POL-NO TO RP-POL-NO.
           
      *    印刷区分: 計算対象のみ出力対象
           IF LS-CV-STATUS-KBN = '01'
               MOVE '1' TO RP-PRINT-KBN
               MOVE LS-CV-RESERVE-AMT TO RP-PRINT-AMT
           ELSE
               MOVE '0' TO RP-PRINT-KBN
               MOVE 0 TO RP-PRINT-AMT
           END-IF.
           
      *    エラー区分: 基本的にはクリア
           IF LS-CV-FOUND-FLAG = 'N'
               MOVE '1' TO RP-ERROR-KBN
           ELSE
               MOVE '0' TO RP-ERROR-KBN
           END-IF.
       
       440-WRITE-REPORT-ROW.
           WRITE LFREPF-REC.
           
           IF LS-LFREPF-STATUS NOT = "00"
               DISPLAY '照会索引書込失敗 POL='
                   LS-CURR-POL-NO ' ST='
                   LS-LFREPF-STATUS
               ADD 1 TO LS-ERROR-COUNT
               MOVE 'Y' TO LS-PROC-STATUS
           END-IF.
       
       500-CLOSE-ALL-FILES.
           CLOSE LFPOLF2-FILE.
           IF LS-LFPOLF2-STATUS NOT = "00" AND
               LS-LFPOLF2-STATUS NOT = "10"
               DISPLAY '契約マスタクローズ失敗 ST='
                   LS-LFPOLF2-STATUS
           END-IF.
           
           CLOSE LVCHGF-FILE.
           IF LS-LVCHGF-STATUS NOT = "00" AND
               LS-LVCHGF-STATUS NOT = "10"
               DISPLAY '異動履歴クローズ失敗 ST='
                   LS-LVCHGF-STATUS
           END-IF.
           
           CLOSE LFCVRF-FILE.
           IF LS-LFCVRF-STATUS NOT = "00" AND
               LS-LFCVRF-STATUS NOT = "10"
               DISPLAY '返戻金クローズ失敗 ST='
                   LS-LFCVRF-STATUS
           END-IF.
           
           CLOSE LFREPF-FILE.
           IF LS-LFREPF-STATUS NOT = "00" AND
               LS-LFREPF-STATUS NOT = "10"
               DISPLAY '照会索引クローズ失敗 ST='
                   LS-LFREPF-STATUS
               MOVE 'Y' TO LS-PROC-STATUS
           END-IF.
           
           IF LS-ERROR-COUNT > 0
               DISPLAY '処理終了 エラー件数 ' LS-ERROR-COUNT
           ELSE
               DISPLAY '処理終了 契約処理件数 ' 
                   LS-POLICY-PROCESS-CT
           END-IF.
