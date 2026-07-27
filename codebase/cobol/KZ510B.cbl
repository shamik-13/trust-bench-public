       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ510B.
      * 変更履歴
      * 版数  年月日(和暦)  担当              概要
      * 1.00  平成30.04.01  佐伯 亮 (B-118)  新規作成
      * 1.01  令和02.10.15  黒沢 紀子 (E-063) 延滞判定条件見直し
      * 1.02  令和05.06.20  黒沢 紀子 (E-063) 遅延損害金計算修正
       AUTHOR. KZB-KAIHATSU-G.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLQF
               ASSIGN TO "KZDLQF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-KZDLQF-ST.
           SELECT KZDLRF
               ASSIGN TO "KZDLRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-KZDLRF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZDLQF.
           COPY KZDLQFC.
       FD  KZDLRF.
           COPY KZDLRFC.

       WORKING-STORAGE SECTION.
      *--- ファイル状態コード ---
       01  WS-KZDLQF-ST         PIC XX VALUE SPACES.
       01  WS-KZDLRF-ST         PIC XX VALUE SPACES.
      *--- 処理件数カウンタ ---
       01  WS-CNT-READ          PIC 9(7) VALUE ZEROS.
       01  WS-CNT-WRITE         PIC 9(7) VALUE ZEROS.
      *--- EOFフラグ ---
       01  WS-EOF-FLG           PIC X    VALUE 'N'.
           88  EOF-KZDLQF                VALUE 'Y'.
      *--- 延滞日数算出ワーク ---
       01  WS-DATE-WORK.
           05  WS-DUE-INT       PIC 9(8)  VALUE ZEROS.
           05  WS-ASOF-INT      PIC 9(8)  VALUE ZEROS.
           05  WS-DAYS-SIGN     PIC S9(6) VALUE ZEROS.
           05  WS-DAYS-OVERDUE  PIC 9(6)  VALUE ZEROS.
      *--- 遅延損害金算出ワーク（円未満切捨て用中間値）---
       01  WS-CHARGE-WORK       PIC 9(15)V9(4) VALUE ZEROS.

       PROCEDURE DIVISION.
      *================================================================*
      * メイン制御                                                      *
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-MAIN-LOOP UNTIL EOF-KZDLQF
           PERFORM 9000-TERM
           MOVE 0 TO RETURN-CODE
           GOBACK.

      *================================================================*
      * 1000-INIT：ファイルオープン・初回読込                          *
      *================================================================*
       1000-INIT.
           OPEN INPUT KZDLQF
           IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLQF オープン失敗 ST='
                       WS-KZDLQF-ST
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           OPEN OUTPUT KZDLRF
           IF WS-KZDLRF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLRF オープン失敗 ST='
                       WS-KZDLRF-ST
               CLOSE KZDLQF
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 2100-READ-KZDLQF.

      *================================================================*
      * 2000-MAIN-LOOP：主処理ループ                                   *
      *================================================================*
       2000-MAIN-LOOP.
           PERFORM 3000-PROCESS-ACCOUNT
           PERFORM 2100-READ-KZDLQF.

      *================================================================*
      * 2100-READ-KZDLQF：入力ファイル読込                             *
      *================================================================*
       2100-READ-KZDLQF.
           READ KZDLQF
           EVALUATE WS-KZDLQF-ST
               WHEN '00'
                   ADD 1 TO WS-CNT-READ
               WHEN '10'
                   MOVE 'Y' TO WS-EOF-FLG
               WHEN OTHER
                   DISPLAY 'KZ510B KZDLQF 読取異常 ST='
                           WS-KZDLQF-ST
                   DISPLAY 'KZ510B 処理中止'
                   MOVE 8 TO RETURN-CODE
                   GOBACK
           END-EVALUATE.

      *================================================================*
      * 3000-PROCESS-ACCOUNT：口座単位処理                             *
      *================================================================*
       3000-PROCESS-ACCOUNT.
           PERFORM 3100-CALC-DAYS-OVERDUE
           PERFORM 3200-CALC-LATE-CHARGE
           PERFORM 3300-SET-AGING-BUCKET
           PERFORM 3400-SET-NEW-STATUS
           PERFORM 3500-MOVE-OUTPUT
           PERFORM 3600-WRITE-KZDLRF.

      *================================================================*
      * 3100-CALC-DAYS-OVERDUE：延滞日数算出                           *
      *   支払期日(DUE-DT)→判定基準日(ASOF-DT)の実経過暦日数          *
      *   ASOF <= DUE の場合は延滞日数ゼロ                             *
      *================================================================*
       3100-CALC-DAYS-OVERDUE.
           MOVE FUNCTION INTEGER-OF-DATE(DQ-DUE-DT)
               TO WS-DUE-INT
           MOVE FUNCTION INTEGER-OF-DATE(DQ-ASOF-DT)
               TO WS-ASOF-INT
           COMPUTE WS-DAYS-SIGN =
               WS-ASOF-INT - WS-DUE-INT
           IF WS-DAYS-SIGN > ZERO
               MOVE WS-DAYS-SIGN TO WS-DAYS-OVERDUE
           ELSE
               MOVE ZERO TO WS-DAYS-OVERDUE
           END-IF.

      *================================================================*
      * 3200-CALC-LATE-CHARGE：遅延損害金算出                          *
      *   計算式：延滞元本 × 0.146 × 延滞日数 ÷ 365                  *
      *   円未満は切捨て（ROUNDED 不使用）                             *
      *================================================================*
       3200-CALC-LATE-CHARGE.
           IF WS-DAYS-OVERDUE > ZERO
               COMPUTE WS-CHARGE-WORK =
                   DQ-OVERDUE-AMT * 0.146 *
                   WS-DAYS-OVERDUE / 365
               MOVE WS-CHARGE-WORK TO DR-LATE-CHARGE-AMT
           ELSE
               MOVE ZERO TO DR-LATE-CHARGE-AMT
           END-IF.

      *================================================================*
      * 3300-SET-AGING-BUCKET：経過区分設定                            *
      *   B1:1〜30日  B2:31〜60日  B3:61〜90日  B4:91日以上           *
      *================================================================*
       3300-SET-AGING-BUCKET.
           EVALUATE TRUE
               WHEN WS-DAYS-OVERDUE >= 91
                   MOVE 'B4' TO DR-AGING-BUCKET
               WHEN WS-DAYS-OVERDUE >= 61
                   MOVE 'B3' TO DR-AGING-BUCKET
               WHEN WS-DAYS-OVERDUE >= 31
                   MOVE 'B2' TO DR-AGING-BUCKET
               WHEN OTHER
                   MOVE 'B1' TO DR-AGING-BUCKET
           END-EVALUATE.

      *================================================================*
      * 3400-SET-NEW-STATUS：延滞ステータス遷移                        *
      *   延滞日数=0 → 00(正常)  延滞日数>0 → 10(延滞)               *
      *   本バッチでは上記2区分を設定する                              *
      *================================================================*
       3400-SET-NEW-STATUS.
           IF WS-DAYS-OVERDUE > ZERO
               MOVE '10' TO DR-NEW-STATUS
           ELSE
               MOVE '00' TO DR-NEW-STATUS
           END-IF.

      *================================================================*
      * 3500-MOVE-OUTPUT：出力レコード組立                             *
      *================================================================*
       3500-MOVE-OUTPUT.
           MOVE DQ-ACCT-NO      TO DR-ACCT-NO
           MOVE WS-DAYS-OVERDUE TO DR-DAYS-OVERDUE.

      *================================================================*
      * 3600-WRITE-KZDLRF：出力ファイル書込                            *
      *================================================================*
       3600-WRITE-KZDLRF.
           WRITE KZDLRF-REC
           IF WS-KZDLRF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLRF 書込異常 ST='
                       WS-KZDLRF-ST
               DISPLAY 'KZ510B 障害口座番号=' DQ-ACCT-NO
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           ADD 1 TO WS-CNT-WRITE.

      *================================================================*
      * 9000-TERM：ファイルクローズ・処理件数表示                      *
      *================================================================*
       9000-TERM.
           CLOSE KZDLQF
           IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLQF クローズ異常 ST='
                       WS-KZDLQF-ST
           END-IF
           CLOSE KZDLRF
           IF WS-KZDLRF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLRF クローズ異常 ST='
                       WS-KZDLRF-ST
           END-IF
           DISPLAY 'KZ510B 正常終了 入力件数=' WS-CNT-READ
           DISPLAY 'KZ510B           出力件数=' WS-CNT-WRITE.
