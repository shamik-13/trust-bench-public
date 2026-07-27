      *================================================================
      * KZ530B  延滞料GL仕訳作成
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ530B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  平成28.04.01  システム部 勘定系チーム  新規作成
      * 1.10  令和02.10.01  システム部 勘定系チーム  延滞利息GL科目判定追加
      * 1.20  令和06.04.01  システム部 勘定系チーム  計上対象抽出条件見直し
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLRF ASSIGN TO KZDLRF
               FILE STATUS IS WS-ST-KZDLRF.
           SELECT KZAUDF ASSIGN TO KZAUDF
               FILE STATUS IS WS-ST-KZAUDF.
           SELECT KZGLPF ASSIGN TO KZGLPF
               FILE STATUS IS WS-ST-KZGLPF.
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  KZDLRF.
       COPY KZDLRFC.
       FD  KZAUDF.
       COPY KZAUDCF.
       FD  KZGLPF.
       COPY KZGLPCF.
      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * 処理日付・ファイルステータス
      *----------------------------------------------------------------
       01  WS-PROC-DATE             PIC X(8)             VALUE SPACES.
       01  WS-ST-KZDLRF             PIC XX               VALUE SPACES.
       01  WS-ST-KZAUDF             PIC XX               VALUE SPACES.
       01  WS-ST-KZGLPF             PIC XX               VALUE SPACES.
      *----------------------------------------------------------------
      * スイッチ
      *----------------------------------------------------------------
       01  WS-SW-ERR                PIC X                VALUE '0'.
           88  ERR-DETECTED                              VALUE '1'.
       01  WS-SW-EOF-KZDLRF         PIC X                VALUE '0'.
           88  EOF-KZDLRF                                VALUE '1'.
       01  WS-SW-EOF-KZAUDF         PIC X                VALUE '0'.
           88  EOF-KZAUDF                                VALUE '1'.
       01  WS-FOUND-SW              PIC X                VALUE '0'.
           88  ENTRY-FOUND                               VALUE '1'.
      *----------------------------------------------------------------
      * カウンタ
      *----------------------------------------------------------------
       01  WS-CNT-DL-READ           PIC 9(7)             VALUE ZERO.
       01  WS-CNT-DL-SKIP           PIC 9(7)             VALUE ZERO.
       01  WS-CNT-GL-WRITE          PIC 9(7)             VALUE ZERO.
       01  WS-CNT-AD-WRITE          PIC 9(7)             VALUE ZERO.
       01  WS-CNT-AD-LOAD           PIC 9(7)             VALUE ZERO.
      *----------------------------------------------------------------
      * KZAUDFロードテーブル
      *----------------------------------------------------------------
       01  WS-AUD-TBL-MAX           PIC 9(5)             VALUE 10000.
       01  WS-AUD-TBL-CNT           PIC 9(5)             VALUE ZERO.
       01  WS-FOUND-IDX             PIC 9(5)             VALUE ZERO.
       01  WS-AUD-TABLE.
           05  WS-AUD-ENTRY OCCURS 10000 TIMES.
               10  WS-AT-ACCT      PIC X(10).
               10  WS-AT-DATE      PIC X(8).
               10  WS-AT-AMT       PIC S9(13)V99 COMP-3.
               10  WS-AT-NWSTS     PIC X(2).
      *----------------------------------------------------------------
      * KZ531S呼出しエリア
      *----------------------------------------------------------------
       01  WS-531S-AREA.
           05  KZ531S-PROD-TYPE    PIC X(4).
           05  KZ531S-DEBIT-CD     PIC X(4).
           05  KZ531S-CREDIT-CD    PIC X(4).
           05  KZ531S-RC           PIC S9(4)             COMP.
      *----------------------------------------------------------------
      * GL仕訳ワーク
      *----------------------------------------------------------------
       01  WS-PREV-AMT              PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-PREV-NWSTS            PIC X(2)             VALUE SPACES.
       01  WS-NET-AMT               PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-JRNL-TYPE             PIC X(2)             VALUE SPACES.
      *----------------------------------------------------------------
      * 口座番号分解ワーク
      *----------------------------------------------------------------
       01  WS-CURR-ACCT             PIC X(10)            VALUE SPACES.
       01  WS-PROD-TYPE             PIC X(4)             VALUE SPACES.
       01  WS-COST-CTR              PIC X(6)             VALUE SPACES.
      *----------------------------------------------------------------
      * 汎用ループカウンタ
      *----------------------------------------------------------------
       01  WS-IDX                   PIC 9(5)             VALUE ZERO.
       PROCEDURE DIVISION.
      *================================================================
       0000-MAIN.
      *================================================================
           PERFORM 1000-INIT
           IF ERR-DETECTED
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 2000-MAIN-LOOP
               UNTIL EOF-KZDLRF
                  OR ERR-DETECTED
           PERFORM 9000-TERM
           IF ERR-DETECTED
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
      *================================================================
       1000-INIT.
      *================================================================
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-PROC-DATE
           OPEN INPUT KZDLRF
           IF WS-ST-KZDLRF NOT = '00'
               DISPLAY 'KZDLRF オープン失敗 ST=' WS-ST-KZDLRF
               MOVE '1' TO WS-SW-ERR
               EXIT PARAGRAPH
           END-IF
           OPEN INPUT KZAUDF
           IF WS-ST-KZAUDF NOT = '00'
               DISPLAY 'KZAUDF オープン失敗 ST=' WS-ST-KZAUDF
               MOVE '1' TO WS-SW-ERR
               EXIT PARAGRAPH
           END-IF
           PERFORM 1100-LOAD-KZAUDF
           IF ERR-DETECTED
               EXIT PARAGRAPH
           END-IF
           CLOSE KZAUDF
           IF WS-ST-KZAUDF NOT = '00'
               DISPLAY 'KZAUDF クローズ失敗（初期化中）ST='
                       WS-ST-KZAUDF
               MOVE '1' TO WS-SW-ERR
               EXIT PARAGRAPH
           END-IF
           OPEN EXTEND KZAUDF
           IF WS-ST-KZAUDF NOT = '00'
               DISPLAY 'KZAUDF EXTENDオープン失敗 ST='
                       WS-ST-KZAUDF
               MOVE '1' TO WS-SW-ERR
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT KZGLPF
           IF WS-ST-KZGLPF NOT = '00'
               DISPLAY 'KZGLPF オープン失敗 ST=' WS-ST-KZGLPF
               MOVE '1' TO WS-SW-ERR
               EXIT PARAGRAPH
           END-IF
           READ KZDLRF
               AT END MOVE '1' TO WS-SW-EOF-KZDLRF
           END-READ
           IF WS-ST-KZDLRF NOT = '00' AND
              WS-ST-KZDLRF NOT = '10'
               DISPLAY 'KZDLRF 初回読込失敗 ST=' WS-ST-KZDLRF
               MOVE '1' TO WS-SW-ERR
           END-IF.
      *================================================================
       1100-LOAD-KZAUDF.
      *================================================================
      *    KZAUDFを全件走査し口座ごとの最新LCIをテーブルに格納する
      *================================================================
           MOVE '0' TO WS-SW-EOF-KZAUDF
           PERFORM UNTIL EOF-KZAUDF OR ERR-DETECTED
               READ KZAUDF
                   AT END MOVE '1' TO WS-SW-EOF-KZAUDF
               END-READ
               IF WS-ST-KZAUDF = '10'
                   MOVE '1' TO WS-SW-EOF-KZAUDF
               ELSE IF WS-ST-KZAUDF NOT = '00'
                   DISPLAY 'KZAUDF 読込失敗（ロード中）ST='
                           WS-ST-KZAUDF
                   MOVE '1' TO WS-SW-ERR
               ELSE IF NOT EOF-KZAUDF
                   IF AUD-EVENT-TYPE = 'LCI'
                       ADD 1 TO WS-CNT-AD-LOAD
                       PERFORM 1110-UPDATE-AUD-TBL
                   END-IF
               END-IF
           END-PERFORM.
      *================================================================
       1110-UPDATE-AUD-TBL.
      *================================================================
      *    テーブル内に同一口座があれば最新日付で更新、なければ新規追加
      *================================================================
           MOVE '0'  TO WS-FOUND-SW
           MOVE ZERO TO WS-FOUND-IDX
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-AUD-TBL-CNT
                  OR ENTRY-FOUND
               IF WS-AT-ACCT(WS-IDX) = AUD-ACCT-NO
                   MOVE '1'    TO WS-FOUND-SW
                   MOVE WS-IDX TO WS-FOUND-IDX
               END-IF
           END-PERFORM
           IF ENTRY-FOUND
               IF AUD-EVENT-DT > WS-AT-DATE(WS-FOUND-IDX)
                   MOVE AUD-EVENT-DT
                       TO WS-AT-DATE(WS-FOUND-IDX)
                   MOVE AUD-CHANGE-AMT
                       TO WS-AT-AMT(WS-FOUND-IDX)
                   MOVE AUD-NEW-STATUS
                       TO WS-AT-NWSTS(WS-FOUND-IDX)
               END-IF
           ELSE
               IF WS-AUD-TBL-CNT >= WS-AUD-TBL-MAX
                   DISPLAY 'KZAUDF テーブル容量超過 上限='
                           WS-AUD-TBL-MAX
                   MOVE '1' TO WS-SW-ERR
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-AUD-TBL-CNT
               MOVE AUD-ACCT-NO
                   TO WS-AT-ACCT(WS-AUD-TBL-CNT)
               MOVE AUD-EVENT-DT
                   TO WS-AT-DATE(WS-AUD-TBL-CNT)
               MOVE AUD-CHANGE-AMT
                   TO WS-AT-AMT(WS-AUD-TBL-CNT)
               MOVE AUD-NEW-STATUS
                   TO WS-AT-NWSTS(WS-AUD-TBL-CNT)
           END-IF.
      *================================================================
       2000-MAIN-LOOP.
      *================================================================
           ADD 1 TO WS-CNT-DL-READ
           IF DR-LATE-CHARGE-AMT = ZERO
               ADD 1 TO WS-CNT-DL-SKIP
           ELSE
               PERFORM 3000-PROC-RECORD
           END-IF
           IF NOT ERR-DETECTED
               PERFORM 2100-READ-KZDLRF
           END-IF.
       2100-READ-KZDLRF.
           READ KZDLRF
               AT END MOVE '1' TO WS-SW-EOF-KZDLRF
           END-READ
           IF WS-ST-KZDLRF NOT = '00' AND
              WS-ST-KZDLRF NOT = '10'
               DISPLAY 'KZDLRF 読込失敗 ST=' WS-ST-KZDLRF
               MOVE '1' TO WS-SW-ERR
           END-IF.
      *================================================================
       3000-PROC-RECORD.
      *================================================================
           MOVE DR-ACCT-NO TO WS-CURR-ACCT
           PERFORM 4000-FIND-PREV-LCI
           IF ENTRY-FOUND
               COMPUTE WS-NET-AMT =
                   DR-LATE-CHARGE-AMT - WS-PREV-AMT
           ELSE
               MOVE DR-LATE-CHARGE-AMT TO WS-NET-AMT
           END-IF
           IF WS-NET-AMT = ZERO
               ADD 1 TO WS-CNT-DL-SKIP
               EXIT PARAGRAPH
           END-IF
           IF WS-NET-AMT > ZERO
               MOVE 'LC' TO WS-JRNL-TYPE
           ELSE
               MOVE 'LR' TO WS-JRNL-TYPE
           END-IF
           PERFORM 5000-CALL-KZ531S
           IF ERR-DETECTED
               EXIT PARAGRAPH
           END-IF
           PERFORM 6000-WRITE-GL
           IF ERR-DETECTED
               EXIT PARAGRAPH
           END-IF
           PERFORM 7000-WRITE-AUDIT.
      *================================================================
       4000-FIND-PREV-LCI.
      *================================================================
      *    テーブルを線形検索し対象口座の最新LCI情報を返す
      *================================================================
           MOVE '0'    TO WS-FOUND-SW
           MOVE ZERO   TO WS-PREV-AMT
           MOVE SPACES TO WS-PREV-NWSTS
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-AUD-TBL-CNT
                  OR ENTRY-FOUND
               IF WS-AT-ACCT(WS-IDX) = WS-CURR-ACCT
                   MOVE '1'                  TO WS-FOUND-SW
                   MOVE WS-AT-AMT(WS-IDX)   TO WS-PREV-AMT
                   MOVE WS-AT-NWSTS(WS-IDX) TO WS-PREV-NWSTS
               END-IF
           END-PERFORM.
      *================================================================
       5000-CALL-KZ531S.
      *================================================================
      *    口座番号第5-8桁を商品種別コードとしてKZ531Sを呼び出す
      *================================================================
           MOVE WS-CURR-ACCT(5:4) TO WS-PROD-TYPE
           MOVE WS-PROD-TYPE      TO KZ531S-PROD-TYPE
           MOVE SPACES            TO KZ531S-DEBIT-CD
           MOVE SPACES            TO KZ531S-CREDIT-CD
           MOVE ZERO              TO KZ531S-RC
           CALL 'KZ531S'          USING WS-531S-AREA
           IF KZ531S-RC NOT = ZERO
               DISPLAY 'KZ531S 呼出し異常 RC=' KZ531S-RC
                       ' 商品種別=' WS-PROD-TYPE
               MOVE '1' TO WS-SW-ERR
               EXIT PARAGRAPH
           END-IF
           IF KZ531S-DEBIT-CD = SPACES OR
              KZ531S-CREDIT-CD = SPACES
               DISPLAY 'KZ531S 勘定科目コード未設定'
               DISPLAY '  商品種別=' WS-PROD-TYPE
               MOVE '1' TO WS-SW-ERR
           END-IF.
      *================================================================
       6000-WRITE-GL.
      *================================================================
           INITIALIZE KZGLPF-REC
           MOVE WS-CURR-ACCT        TO GP-ACCT-NO
           MOVE WS-PROC-DATE        TO GP-GL-DATE
           MOVE KZ531S-DEBIT-CD     TO GP-DEBIT-ACCT-CD
           MOVE KZ531S-CREDIT-CD    TO GP-CREDIT-ACCT-CD
           MOVE WS-NET-AMT          TO GP-JRNL-AMT
           MOVE WS-JRNL-TYPE        TO GP-JRNL-TYPE
           MOVE WS-CURR-ACCT(1:6)   TO WS-COST-CTR
           MOVE WS-COST-CTR         TO GP-COST-CENTER-CD
           WRITE KZGLPF-REC
           IF WS-ST-KZGLPF NOT = '00'
               DISPLAY 'KZGLPF 書込失敗 ST=' WS-ST-KZGLPF
                       ' 口座=' WS-CURR-ACCT
               MOVE '1' TO WS-SW-ERR
           ELSE
               ADD 1 TO WS-CNT-GL-WRITE
           END-IF.
      *================================================================
       7000-WRITE-AUDIT.
      *================================================================
           INITIALIZE KZAUDF-REC
           MOVE WS-CURR-ACCT    TO AUD-ACCT-NO
           MOVE WS-PROC-DATE    TO AUD-EVENT-DT
           MOVE 'LCI'           TO AUD-EVENT-TYPE
           IF ENTRY-FOUND
               MOVE WS-PREV-NWSTS TO AUD-OLD-STATUS
           ELSE
               MOVE '00'          TO AUD-OLD-STATUS
           END-IF
           MOVE DR-NEW-STATUS   TO AUD-NEW-STATUS
           MOVE WS-NET-AMT      TO AUD-CHANGE-AMT
           WRITE KZAUDF-REC
           IF WS-ST-KZAUDF NOT = '00'
               DISPLAY 'KZAUDF 書込失敗 ST=' WS-ST-KZAUDF
                       ' 口座=' WS-CURR-ACCT
               MOVE '1' TO WS-SW-ERR
           ELSE
               ADD 1 TO WS-CNT-AD-WRITE
           END-IF.
      *================================================================
       9000-TERM.
      *================================================================
           CLOSE KZDLRF
           IF WS-ST-KZDLRF NOT = '00'
               DISPLAY 'KZDLRF クローズ失敗 ST=' WS-ST-KZDLRF
               MOVE '1' TO WS-SW-ERR
           END-IF
           CLOSE KZAUDF
           IF WS-ST-KZAUDF NOT = '00'
               DISPLAY 'KZAUDF クローズ失敗 ST=' WS-ST-KZAUDF
               MOVE '1' TO WS-SW-ERR
           END-IF
           CLOSE KZGLPF
           IF WS-ST-KZGLPF NOT = '00'
               DISPLAY 'KZGLPF クローズ失敗 ST=' WS-ST-KZGLPF
               MOVE '1' TO WS-SW-ERR
           END-IF
           DISPLAY '*** KZ530B 処理終了 ***'
           DISPLAY '  KZDLRF 読込件数  ：' WS-CNT-DL-READ
           DISPLAY '  スキップ件数     ：' WS-CNT-DL-SKIP
           DISPLAY '  KZGLPF 書込件数  ：' WS-CNT-GL-WRITE
           DISPLAY '  KZAUDF 追記件数  ：' WS-CNT-AD-WRITE
           DISPLAY '  KZAUDF ロード件数：' WS-CNT-AD-LOAD.
