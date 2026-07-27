       IDENTIFICATION DIVISION.
       PROGRAM-ID. LV230B.
      *変更履歴
      *版数  年月日    担当      概要
      *1.00  20220517  架設      初版作成 — 解約返戻金計算完了受付から異動を抽出
      *                         し、契約状態を遷移させ LVCHGF へ登録
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFREQF ASSIGN TO LS-LFREQF-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS LFREQF-STATUS.
           SELECT LFCVRF ASSIGN TO LS-LFCVRF-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS LFCVRF-STATUS.
           SELECT LFPOLF2 ASSIGN TO LS-LFPOLF2-NAME
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS LFPOLF2-STATUS.
           SELECT LVCHGF ASSIGN TO LS-LVCHGF-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS LVCHGF-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LFREQF.
       COPY LFREQC.
      *
       FD  LFCVRF.
       COPY LFCVRFC.
      *
       FD  LFPOLF2.
       COPY LFPOLF2C.
      *
       FD  LVCHGF.
       COPY LVCHGC.
      *
       WORKING-STORAGE SECTION.
       01  LS-FILE-NAMES.
           05  LS-LFREQF-NAME    PIC X(50) VALUE
               'LFREQF'.
           05  LS-LFCVRF-NAME    PIC X(50) VALUE
               'LFCVRF'.
           05  LS-LFPOLF2-NAME   PIC X(50) VALUE
               'LFPOLF2'.
           05  LS-LVCHGF-NAME    PIC X(50) VALUE
               'LVCHGF'.
      *
       01  LS-FILE-STATUS-FIELDS.
           05  LFREQF-STATUS     PIC XX VALUE SPACES.
           05  LFCVRF-STATUS     PIC XX VALUE SPACES.
           05  LFPOLF2-STATUS    PIC XX VALUE SPACES.
           05  LVCHGF-STATUS     PIC XX VALUE SPACES.
      *
       01  LS-PROCESSING-FLAGS.
           05  LS-EOF-LFREQF     PIC 9 VALUE 0.
           05  LS-CV-FOUND       PIC 9 VALUE 0.
           05  LS-POL-FOUND      PIC 9 VALUE 0.
           05  LS-ERR-FLG        PIC 9 VALUE 0.
      *
       01  LS-WORKING-VARS.
           05  LS-CHANGE-ID      PIC 9(13) VALUE ZEROS.
           05  LS-CHANGE-ID-LAST PIC 9(13) VALUE ZEROS.
           05  LS-CURRENT-DATE   PIC 9(8) VALUE ZEROS.
           05  LS-NEW-STATUS     PIC X(2) VALUE SPACES.
           05  LS-OLD-STATUS     PIC X(2) VALUE SPACES.
           05  LS-CHANGE-TYPE    PIC X(2) VALUE SPACES.
           05  LS-CHANGE-CNT     PIC 9(7) VALUE 0.
           05  LS-REQST-CNT      PIC 9(7) VALUE 0.
           05  LS-MATCH-CNT      PIC 9(7) VALUE 0.
           05  LS-ERROR-CNT      PIC 9(7) VALUE 0.
      *
       01  LS-TEMP-STORAGE.
           05  LS-TEMP-POL-NO    PIC X(12) VALUE SPACES.
           05  LS-TEMP-CV-ID     PIC X(13) VALUE SPACES.
           05  LS-TEMP-REQID     PIC X(13) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
      *受付ファイルを順序処理し、解約返戻金計算完了分を抽出
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO LS-ERR-FLG.
           ACCEPT LS-CURRENT-DATE FROM DATE YYYYMMDD.
      *
           PERFORM OPEN-FILES.
           IF LS-ERR-FLG NOT = 0 THEN
               MOVE 12 TO RETURN-CODE
               PERFORM CLOSE-FILES
               GOBACK
           END-IF.
      *
           PERFORM READ-LFREQF-INIT.
           PERFORM UNTIL LS-EOF-LFREQF = 1
               PERFORM GET-NEXT-LFREQF
               IF LS-EOF-LFREQF = 1 THEN
                   EXIT PERFORM
               END-IF
               ADD 1 TO LS-REQST-CNT
               PERFORM MATCH-AND-WRITE
           END-PERFORM.
      *
           PERFORM CLOSE-FILES.
           IF LS-ERR-FLG = 1 THEN
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
           GOBACK.
      *
       OPEN-FILES.
      *全ファイルをオープンし、障害は記録する
           OPEN INPUT LFREQF.
           IF LFREQF-STATUS NOT = '00' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '口座要件F オープン失敗 ST='
                   LFREQF-STATUS
               EXIT PARAGRAPH
           END-IF.
      *
           OPEN INPUT LFCVRF.
           IF LFCVRF-STATUS NOT = '00' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '解約返戻計算F オープン失敗 ST='
                   LFCVRF-STATUS
               EXIT PARAGRAPH
           END-IF.
      *
           OPEN INPUT LFPOLF2.
           IF LFPOLF2-STATUS NOT = '00' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '契約F2 オープン失敗 ST='
                   LFPOLF2-STATUS
               EXIT PARAGRAPH
           END-IF.
      *
           OPEN OUTPUT LVCHGF.
           IF LVCHGF-STATUS NOT = '00' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '異動F オープン失敗 ST='
                   LVCHGF-STATUS
               EXIT PARAGRAPH
           END-IF.
      *
       READ-LFREQF-INIT.
           MOVE 0 TO LS-EOF-LFREQF.
      *
       GET-NEXT-LFREQF.
      *受付ファイルから次レコード取得
           READ LFREQF INTO LFREQF-REC
               AT END
                   MOVE 1 TO LS-EOF-LFREQF
               NOT AT END
                   MOVE 0 TO LS-EOF-LFREQF
           END-READ.
      *
           IF LFREQF-STATUS NOT = '00' AND
              LFREQF-STATUS NOT = '10' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '受付F 読込失敗 ST='
                   LFREQF-STATUS
               MOVE 1 TO LS-EOF-LFREQF
           END-IF.
      *
       MATCH-AND-WRITE.
      *受付に対応する解約返戻金計算レコードを検索
           MOVE RQ-POL-NO TO LS-TEMP-POL-NO.
           MOVE RQ-REQ-ID TO LS-TEMP-REQID.
           MOVE 0 TO LS-CV-FOUND.
           MOVE 0 TO LS-POL-FOUND.
      *
      *解約返戻金計算ファイルを順序スキャン(初期化後)
           CLOSE LFCVRF.
           OPEN INPUT LFCVRF.
           IF LFCVRF-STATUS NOT = '00' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '解約返戻計算F 再オープン失敗 ST='
                   LFCVRF-STATUS
               EXIT PARAGRAPH
           END-IF.
      *
           PERFORM SCAN-LFCVRF.
      *
           IF LS-CV-FOUND NOT = 1 THEN
               EXIT PARAGRAPH
           END-IF.
      *計算状態区分が計算対象(01)か確認
           IF CO-CALC-STATUS-KBN NOT = '01' THEN
               EXIT PARAGRAPH
           END-IF.
      *
      *契約ファイルで契約情報を確認
           MOVE LS-TEMP-POL-NO TO PO-POL-NO.
           READ LFPOLF2
               INVALID KEY
                   MOVE 0 TO LS-POL-FOUND
               NOT INVALID KEY
                   MOVE 1 TO LS-POL-FOUND
           END-READ.
      *
           IF LFPOLF2-STATUS NOT = '00' AND
              LFPOLF2-STATUS NOT = '23' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '契約F2 読込失敗 ST='
                   LFPOLF2-STATUS
               EXIT PARAGRAPH
           END-IF.
      *
           IF LS-POL-FOUND NOT = 1 THEN
               EXIT PARAGRAPH
           END-IF.
      *
      *新しい異動ＩＤを生成
           COMPUTE LS-CHANGE-ID = LS-CHANGE-ID-LAST + 1.
           MOVE LS-CHANGE-ID TO LS-CHANGE-ID-LAST.
      *
      *現在の契約状態を保存、新状態を決定
           MOVE PO-CONTRACT-STATUS-KBN TO LS-OLD-STATUS.
      *
           EVALUATE RQ-REQ-TYPE-KBN
               WHEN '01'
      *解約請求 → 解約済(03)へ遷移
                   MOVE '03' TO LS-NEW-STATUS
                   MOVE '01' TO LS-CHANGE-TYPE
               WHEN '02'
      *失効請求 → 失効済(09)へ遷移
                   MOVE '09' TO LS-NEW-STATUS
                   MOVE '02' TO LS-CHANGE-TYPE
               WHEN OTHER
      *その他は処理対象外
                   EXIT PARAGRAPH
           END-EVALUATE.
      *
      *新状態と現状態が異なる場合のみ異動を記録
           IF LS-NEW-STATUS = LS-OLD-STATUS THEN
               EXIT PARAGRAPH
           END-IF.
      *
      *異動レコードを構築して出力
           MOVE SPACES TO LVCHGF-REC.
           MOVE LS-CHANGE-ID TO CH-CHANGE-ID.
           MOVE LS-TEMP-POL-NO TO CH-POL-NO.
           MOVE LS-CURRENT-DATE TO CH-CHANGE-DATE.
           MOVE LS-CHANGE-TYPE TO CH-CHANGE-TYPE-KBN.
           MOVE LS-OLD-STATUS TO CH-BEFORE-STATUS-KBN.
           MOVE LS-NEW-STATUS TO CH-AFTER-STATUS-KBN.
           MOVE '01' TO CH-CHANGE-STATUS-KBN.
      *
           WRITE LVCHGF-REC.
           IF LVCHGF-STATUS NOT = '00' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '異動F 書込失敗 ST='
                   LVCHGF-STATUS
               ADD 1 TO LS-ERROR-CNT
               EXIT PARAGRAPH
           END-IF.
      *
           ADD 1 TO LS-CHANGE-CNT.
           ADD 1 TO LS-MATCH-CNT.
      *
       SCAN-LFCVRF.
      *解約返戻金計算ファイルを順序スキャンして対象を検索
           PERFORM UNTIL 1 = 0
               READ LFCVRF INTO LFCVRF-REC
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF CO-POL-NO = LS-TEMP-POL-NO THEN
                           MOVE 1 TO LS-CV-FOUND
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
      *
           IF LFCVRF-STATUS NOT = '00' AND
              LFCVRF-STATUS NOT = '10' THEN
               MOVE 1 TO LS-ERR-FLG
               DISPLAY '計算F スキャン失敗 ST='
                   LFCVRF-STATUS
           END-IF.
      *
       CLOSE-FILES.
      *全ファイルをクローズ
           CLOSE LFREQF.
           CLOSE LFCVRF.
           CLOSE LFPOLF2.
           CLOSE LVCHGF.
      *
           IF LFREQF-STATUS NOT = '00' THEN
               IF LFREQF-STATUS NOT = '07' THEN
                   MOVE 1 TO LS-ERR-FLG
               END-IF
           END-IF.
           IF LFCVRF-STATUS NOT = '00' THEN
               IF LFCVRF-STATUS NOT = '07' THEN
                   MOVE 1 TO LS-ERR-FLG
               END-IF
           END-IF.
           IF LFPOLF2-STATUS NOT = '00' THEN
               IF LFPOLF2-STATUS NOT = '07' THEN
                   MOVE 1 TO LS-ERR-FLG
               END-IF
           END-IF.
           IF LVCHGF-STATUS NOT = '00' THEN
               IF LVCHGF-STATUS NOT = '07' THEN
                   MOVE 1 TO LS-ERR-FLG
               END-IF
           END-IF.
