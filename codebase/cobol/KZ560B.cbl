       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------
      * プログラムID : KZ560B
      * システム名   : 信託銀行勘定系バッチサブシステム
      * 機能概要     : 期日スケジュール更新バッチ
      *                KZDLQF の各口座について銀行休日テーブルを用いて
      *                SCH-NEXT-DUE-DT を再計算し、KZRSCF に承認済リ
      *                スケジュール申請があれば優先適用する。
      *                KZSCHF を更新し、改定が発生した口座を SYSOUT に
      *                出力する。
      *----------------------------------------------------------------
      * 変更履歴
      * 版数  年月日    担当       概要
      * 1.00  20260401  山田T      新規作成
      * 1.01  20260512  佐藤K      休日テーブルサイズを 366 エントリに拡張
      * 1.02  20260619  鈴木H      リスケ承認ステータス判定ロジック修正
      *----------------------------------------------------------------
       PROGRAM-ID. KZ560B.
       AUTHOR.     BATCH-DEVELOPMENT-TEAM.
       DATE-WRITTEN. 20260401.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    --- 延滞ファイル（順編成） ---
           SELECT KZDLQF
               ASSIGN TO KZDLQF
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-KZDLQF-ST.
      *    --- リスケジュールファイル（順編成） ---
           SELECT KZRSCF
               ASSIGN TO KZRSCF
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-KZRSCF-ST.
      *    --- スケジュールファイル（VSAM KSDS） ---
           SELECT KZSCHF
               ASSIGN TO KZSCHF
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS SCH-ACCT-NO
               FILE STATUS  IS WS-KZSCHF-ST.
      *    --- 休日テーブル（SYSIN） ---
           SELECT SYSIN
               ASSIGN TO SYSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-SYSIN-ST.
      *    --- 制御レポート（SYSOUT） ---
           SELECT SYSOUT
               ASSIGN TO SYSOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-SYSOUT-ST.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  KZDLQF
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY KZDLQFC.
      *
       FD  KZRSCF
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY KZRSCCF.
      *
       FD  KZSCHF
           RECORDING MODE IS F.
           COPY KZSCHCF.
      *
       FD  SYSIN
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SYSIN-REC.
           05  SYSIN-HOLIDAY-DT          PIC X(08).
           05  SYSIN-FILLER              PIC X(72).
      *
       FD  SYSOUT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SYSOUT-REC                    PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * ファイルステータス
      *----------------------------------------------------------------
       01  WS-KZDLQF-ST                  PIC X(02) VALUE SPACES.
       01  WS-KZRSCF-ST                  PIC X(02) VALUE SPACES.
       01  WS-KZSCHF-ST                  PIC X(02) VALUE SPACES.
       01  WS-SYSIN-ST                   PIC X(02) VALUE SPACES.
       01  WS-SYSOUT-ST                  PIC X(02) VALUE SPACES.
      *----------------------------------------------------------------
      * 処理フラグ
      *----------------------------------------------------------------
       01  WS-FLAGS.
           05  WF-KZDLQF-EOF             PIC X(01) VALUE 'N'.
               88  KZDLQF-EOF            VALUE 'Y'.
           05  WF-KZRSCF-EOF             PIC X(01) VALUE 'N'.
               88  KZRSCF-EOF            VALUE 'Y'.
           05  WF-KZSCHF-FOUND           PIC X(01) VALUE 'N'.
               88  KZSCHF-FOUND          VALUE 'Y'.
           05  WF-HOLIDAY-LOADED         PIC X(01) VALUE 'N'.
               88  HOLIDAY-LOADED        VALUE 'Y'.
           05  WF-RSC-APPLIED            PIC X(01) VALUE 'N'.
               88  RSC-APPLIED           VALUE 'Y'.
           05  WF-DUE-REVISED            PIC X(01) VALUE 'N'.
               88  DUE-REVISED           VALUE 'Y'.
      *----------------------------------------------------------------
      * 計算量
      *----------------------------------------------------------------
       01  WS-COUNTERS.
           05  WC-REVISED-CNT            PIC S9(07) COMP-3 VALUE ZERO.
           05  WC-UNCHANGED-CNT          PIC S9(07) COMP-3 VALUE ZERO.
           05  WC-NEW-SCH-CNT            PIC S9(07) COMP-3 VALUE ZERO.
           05  WC-KZDLQF-READ            PIC S9(07) COMP-3 VALUE ZERO.
           05  WC-KZRSCF-READ            PIC S9(07) COMP-3 VALUE ZERO.
           05  WC-ERROR-CNT              PIC S9(07) COMP-3 VALUE ZERO.
           05  WC-HOLIDAY-CNT            PIC S9(04) COMP-3 VALUE ZERO.
      *----------------------------------------------------------------
      * 休日テーブル（最大 366 エントリ）
      *----------------------------------------------------------------
       01  WS-HOLIDAY-TABLE.
           05  WS-HOLIDAY-MAX            PIC S9(04) COMP-3 VALUE 366.
           05  WS-HOLIDAY-ENTRY
                   OCCURS 366 TIMES
                   INDEXED BY HT-IDX.
               10  WHT-DATE              PIC X(08).
      *----------------------------------------------------------------
      * 日付計算ワーク
      *----------------------------------------------------------------
       01  WS-DATE-WORK.
           05  WD-WORK-DATE              PIC X(08).
           05  WD-WORK-DATE-NUM          PIC 9(08).
           05  WD-CANDIDATE-DATE         PIC X(08).
           05  WD-CANDIDATE-NUM          PIC 9(08).
           05  WD-YEAR                   PIC 9(04).
           05  WD-MONTH                  PIC 9(02).
           05  WD-DAY                    PIC 9(02).
           05  WD-NEXT-YEAR              PIC 9(04).
           05  WD-NEXT-MONTH             PIC 9(02).
           05  WD-NEXT-DAY               PIC 9(02).
           05  WD-DAY-OF-WEEK            PIC 9(01).
           05  WD-HOLIDAY-SW             PIC X(01).
               88  WD-IS-HOLIDAY         VALUE 'Y'.
           05  WD-ADVANCE-CNT            PIC S9(04) COMP-3 VALUE ZERO.
           05  WD-MAX-ADVANCE            PIC S9(04) COMP-3 VALUE 31.
           05  WD-DAYS-IN-MONTH          PIC 9(02).
           05  WD-LEAP-YEAR-SW           PIC X(01).
               88  WD-IS-LEAP-YEAR       VALUE 'Y'.
      *----------------------------------------------------------------
      * RSCFバッファ
      *----------------------------------------------------------------
       01  WS-RSC-BUFFER.
           05  WB-RSC-ACCT-NO            PIC X(10).
           05  WB-RSC-NEW-DUE-DT         PIC X(08).
           05  WB-RSC-APPROVAL-STATUS    PIC X(02).
           05  WB-RSC-LOADED-SW          PIC X(01) VALUE 'N'.
               88  RSC-BUFFER-LOADED     VALUE 'Y'.
      *----------------------------------------------------------------
      * 出力レポート行
      *----------------------------------------------------------------
       01  WS-REPORT-LINE.
           05  WR-ACCT-NO                PIC X(10).
           05  FILLER                    PIC X(01) VALUE SPACE.
           05  WR-OLD-DUE-DT             PIC X(08).
           05  FILLER                    PIC X(01) VALUE SPACE.
           05  WR-NEW-DUE-DT             PIC X(08).
           05  FILLER                    PIC X(01) VALUE SPACE.
           05  WR-RSC-FLG                PIC X(04).
           05  FILLER                    PIC X(01) VALUE SPACE.
           05  WR-MSG                    PIC X(40).
           05  FILLER                    PIC X(59) VALUE SPACES.
      *
       01  WS-HEADER-LINE.
           05  FILLER PIC X(14) VALUE '口座番号  '.
           05  FILLER PIC X(01) VALUE SPACE.
           05  FILLER PIC X(11) VALUE '旧期日  '.
           05  FILLER PIC X(01) VALUE SPACE.
           05  FILLER PIC X(11) VALUE '新期日  '.
           05  FILLER PIC X(01) VALUE SPACE.
           05  FILLER PIC X(04) VALUE 'RSC '.
           05  FILLER PIC X(01) VALUE SPACE.
           05  FILLER PIC X(40) VALUE '備考'.
           05  FILLER PIC X(49) VALUE SPACES.
      *
       01  WS-TRAILER-LINE.
           05  FILLER        PIC X(26)
                             VALUE '★処理件数合計★  '.
           05  WT-REVISED    PIC ZZZ,ZZ9 VALUE ZERO.
           05  FILLER        PIC X(10) VALUE '件改定 '.
           05  WT-UNCHANGED  PIC ZZZ,ZZ9 VALUE ZERO.
           05  FILLER        PIC X(16) VALUE '件変更なし '.
           05  WT-NEW        PIC ZZZ,ZZ9 VALUE ZERO.
           05  FILLER        PIC X(27)
                             VALUE '件新規スケジュール'.
           05  FILLER        PIC X(33) VALUE SPACES.
      *
       01  WS-SYSOUT-BUFFER              PIC X(133).
      *
       01  WS-ABEND-MSG                  PIC X(80).
      *
       01  WS-KZSCHF-SAVE.
           05  WSS-SCH-ACCT-NO           PIC X(10).
           05  WSS-SCH-NEXT-DUE-DT       PIC X(08).
           05  WSS-SCH-ORIG-DUE-DT       PIC X(08).
           05  WSS-SCH-REVISED-DUE-DT    PIC X(08).
           05  WSS-SCH-RESCHEDULE-CNT    PIC S9(04) COMP-3.
      *
       01  WS-SEARCH-IDX                 PIC S9(04) COMP-3 VALUE ZERO.
       01  WS-COMPARE-DATE               PIC X(08).
       01  WS-NUMERIC-TEST               PIC 9(08).
       01  WS-ZERO-DATE                  PIC X(08) VALUE '00000000'.
      *----------------------------------------------------------------
      * 月別日数テーブル（通常年）
      *----------------------------------------------------------------
       01  WS-MONTH-DAYS-TABLE.
           05  WMD-DAYS OCCURS 12 TIMES
                   INDEXED BY MD-IDX     PIC 9(02).
      *
       PROCEDURE DIVISION.
      *================================================================
       0000-MAIN.
      *================================================================
           PERFORM 1000-INITIALIZE
           PERFORM 2000-LOAD-HOLIDAY-TABLE
           IF NOT HOLIDAY-LOADED
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 3000-MAIN-LOOP
               UNTIL KZDLQF-EOF
           PERFORM 9000-TERMINATE
           GOBACK.
      *================================================================
       1000-INITIALIZE.
      *================================================================
      *    月別日数テーブル初期化（2月は平年ベース）
           MOVE 31 TO WMD-DAYS(1)
           MOVE 28 TO WMD-DAYS(2)
           MOVE 31 TO WMD-DAYS(3)
           MOVE 30 TO WMD-DAYS(4)
           MOVE 31 TO WMD-DAYS(5)
           MOVE 30 TO WMD-DAYS(6)
           MOVE 31 TO WMD-DAYS(7)
           MOVE 31 TO WMD-DAYS(8)
           MOVE 30 TO WMD-DAYS(9)
           MOVE 31 TO WMD-DAYS(10)
           MOVE 30 TO WMD-DAYS(11)
           MOVE 31 TO WMD-DAYS(12)
      *    SYSOUT オープン
           OPEN OUTPUT SYSOUT
           IF WS-SYSOUT-ST NOT = '00'
               DISPLAY 'SYSOUT オープン失敗 ST=' WS-SYSOUT-ST
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
      *    ヘッダ行出力
           MOVE WS-HEADER-LINE TO SYSOUT-REC
           WRITE SYSOUT-REC
           IF WS-SYSOUT-ST NOT = '00'
               DISPLAY 'SYSOUT 書込失敗 ST=' WS-SYSOUT-ST
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
      *    KZDLQF オープン
           OPEN INPUT KZDLQF
           IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZDLQF オープン失敗 ST=' WS-KZDLQF-ST
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
      *    KZRSCF オープン
           OPEN INPUT KZRSCF
           IF WS-KZRSCF-ST NOT = '00'
               DISPLAY 'KZRSCF オープン失敗 ST=' WS-KZRSCF-ST
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
      *    KZSCHF I-O オープン
           OPEN I-O KZSCHF
           IF WS-KZSCHF-ST NOT = '00'
               DISPLAY 'KZSCHF オープン失敗 ST=' WS-KZSCHF-ST
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
      *    KZDLQF 先読み
           READ KZDLQF
           IF WS-KZDLQF-ST = '10'
               MOVE 'Y' TO WF-KZDLQF-EOF
           ELSE IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZDLQF 読込失敗 ST=' WS-KZDLQF-ST
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
      *    KZRSCF 先読み
           READ KZRSCF
           IF WS-KZRSCF-ST = '10'
               MOVE 'Y' TO WF-KZRSCF-EOF
           ELSE IF WS-KZRSCF-ST NOT = '00'
               DISPLAY 'KZRSCF 読込失敗 ST=' WS-KZRSCF-ST
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.
      *================================================================
       2000-LOAD-HOLIDAY-TABLE.
      *================================================================
           OPEN INPUT SYSIN
           IF WS-SYSIN-ST NOT = '00'
               DISPLAY 'SYSIN オープン失敗 ST=' WS-SYSIN-ST
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           MOVE 0 TO WC-HOLIDAY-CNT
           PERFORM UNTIL WS-SYSIN-ST = '10'
               READ SYSIN
               IF WS-SYSIN-ST = '00'
                   MOVE SYSIN-HOLIDAY-DT TO WS-NUMERIC-TEST
                   IF FUNCTION TEST-NUMVAL(SYSIN-HOLIDAY-DT) = 0
                       AND SYSIN-HOLIDAY-DT NOT = WS-ZERO-DATE
                       ADD 1 TO WC-HOLIDAY-CNT
                       IF WC-HOLIDAY-CNT > WS-HOLIDAY-MAX
                           DISPLAY 'SYSIN 休日エントリ数超過'
                                   ': 最大366'
                           MOVE 8 TO RETURN-CODE
                           CLOSE SYSIN
                           GOBACK
                       END-IF
                       MOVE SYSIN-HOLIDAY-DT
                           TO WHT-DATE(WC-HOLIDAY-CNT)
                   END-IF
               ELSE IF WS-SYSIN-ST NOT = '10'
                   DISPLAY 'SYSIN 読込失敗 ST=' WS-SYSIN-ST
                   MOVE 8 TO RETURN-CODE
                   CLOSE SYSIN
                   GOBACK
               END-IF
           END-PERFORM
           CLOSE SYSIN
           IF WS-SYSIN-ST NOT = '00'
               DISPLAY 'SYSIN クローズ失敗 ST=' WS-SYSIN-ST
           END-IF
           MOVE 'Y' TO WF-HOLIDAY-LOADED
           DISPLAY '休日テーブルロード完了'
           DISPLAY '件数=' WC-HOLIDAY-CNT.
      *================================================================
       3000-MAIN-LOOP.
      *================================================================
           ADD 1 TO WC-KZDLQF-READ
           MOVE 'N' TO WF-RSC-APPLIED
           MOVE 'N' TO WF-DUE-REVISED
      *    ---- RSCFのマッチング処理 ----
           PERFORM 3100-MATCH-RSCF
      *    ---- 期日計算 ----
           PERFORM 3200-COMPUTE-DUE-DATE
      *    ---- KZSCHF 検索・更新 ----
           PERFORM 3300-UPDATE-KZSCHF
      *    ---- 次レコード読込 ----
           READ KZDLQF
           IF WS-KZDLQF-ST = '10'
               MOVE 'Y' TO WF-KZDLQF-EOF
           ELSE IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZDLQF 読込失敗 ST=' WS-KZDLQF-ST
                       ' 口座=' DQ-ACCT-NO
               ADD 1 TO WC-ERROR-CNT
               MOVE 'Y' TO WF-KZDLQF-EOF
           END-IF.
      *================================================================
       3100-MATCH-RSCF.
      *================================================================
      *    KZRSCF は口座番号昇順ソート済を前提とするマージ処理
           PERFORM UNTIL KZRSCF-EOF
                      OR RSC-ACCT-NO >= DQ-ACCT-NO
               ADD 1 TO WC-KZRSCF-READ
               READ KZRSCF
               IF WS-KZRSCF-ST = '10'
                   MOVE 'Y' TO WF-KZRSCF-EOF
               ELSE IF WS-KZRSCF-ST NOT = '00'
                   DISPLAY 'KZRSCF 読込失敗 ST=' WS-KZRSCF-ST
                   ADD 1 TO WC-ERROR-CNT
                   MOVE 'Y' TO WF-KZRSCF-EOF
               END-IF
           END-PERFORM
           IF NOT KZRSCF-EOF
              AND RSC-ACCT-NO = DQ-ACCT-NO
              AND RSC-APPROVAL-STATUS = 'AP'
               MOVE RSC-ACCT-NO         TO WB-RSC-ACCT-NO
               MOVE RSC-NEW-DUE-DT      TO WB-RSC-NEW-DUE-DT
               MOVE RSC-APPROVAL-STATUS TO WB-RSC-APPROVAL-STATUS
               MOVE 'Y'                 TO WB-RSC-LOADED-SW
           ELSE
               MOVE SPACES TO WB-RSC-ACCT-NO
               MOVE SPACES TO WB-RSC-NEW-DUE-DT
               MOVE SPACES TO WB-RSC-APPROVAL-STATUS
               MOVE 'N'    TO WB-RSC-LOADED-SW
           END-IF.
      *================================================================
       3200-COMPUTE-DUE-DATE.
      *================================================================
      *    リスケ承認済の場合はそちらを優先
           IF RSC-BUFFER-LOADED
               MOVE WB-RSC-NEW-DUE-DT TO WD-WORK-DATE
               MOVE 'Y' TO WF-RSC-APPLIED
           ELSE
               MOVE DQ-DUE-DT TO WD-WORK-DATE
           END-IF
      *    入力日付数値チェック
           MOVE WD-WORK-DATE TO WS-NUMERIC-TEST
           IF FUNCTION TEST-NUMVAL(WD-WORK-DATE) NOT = 0
               DISPLAY '期日不正（非数値）口座=' DQ-ACCT-NO
                       ' 期日=' WD-WORK-DATE
               MOVE DQ-DUE-DT TO WD-WORK-DATE
           END-IF
      *    休日・週末ループ：翌営業日に繰り越す
           MOVE 0 TO WD-ADVANCE-CNT
           PERFORM UNTIL WD-ADVANCE-CNT > WD-MAX-ADVANCE
               PERFORM 3210-CHECK-HOLIDAY
               IF WD-IS-HOLIDAY
                   PERFORM 3220-ADVANCE-ONE-DAY
                   ADD 1 TO WD-ADVANCE-CNT
               ELSE
                   MOVE WD-ADVANCE-CNT TO WD-ADVANCE-CNT
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF WD-ADVANCE-CNT > WD-MAX-ADVANCE
               DISPLAY '休日繰越上限超過 口座=' DQ-ACCT-NO
                       ' 原期日=' WD-WORK-DATE
               ADD 1 TO WC-ERROR-CNT
           END-IF
           MOVE WD-WORK-DATE TO WD-CANDIDATE-DATE.
      *================================================================
       3210-CHECK-HOLIDAY.
      *================================================================
           MOVE 'N' TO WD-HOLIDAY-SW
      *    土曜（曜日=6）または日曜（曜日=0）チェック
           MOVE WD-WORK-DATE(1:4)  TO WD-YEAR
           MOVE WD-WORK-DATE(5:2)  TO WD-MONTH
           MOVE WD-WORK-DATE(7:2)  TO WD-DAY
           COMPUTE WD-DAY-OF-WEEK =
               FUNCTION MOD(
                   FUNCTION INTEGER-OF-DATE(
                       FUNCTION NUMVAL(WD-WORK-DATE)
                   ), 7)
           IF WD-DAY-OF-WEEK = 0 OR WD-DAY-OF-WEEK = 6
               MOVE 'Y' TO WD-HOLIDAY-SW
               EXIT PARAGRAPH
           END-IF
      *    休日テーブル検索
           MOVE 0 TO WS-SEARCH-IDX
           PERFORM VARYING HT-IDX FROM 1 BY 1
                   UNTIL HT-IDX > WC-HOLIDAY-CNT
               IF WHT-DATE(HT-IDX) = WD-WORK-DATE
                   MOVE 'Y' TO WD-HOLIDAY-SW
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      *================================================================
       3220-ADVANCE-ONE-DAY.
      *================================================================
           MOVE WD-WORK-DATE(1:4)  TO WD-YEAR
           MOVE WD-WORK-DATE(5:2)  TO WD-MONTH
           MOVE WD-WORK-DATE(7:2)  TO WD-DAY
      *    うるう年判定
           MOVE 'N' TO WD-LEAP-YEAR-SW
           IF FUNCTION MOD(WD-YEAR, 400) = 0
               MOVE 'Y' TO WD-LEAP-YEAR-SW
           ELSE IF FUNCTION MOD(WD-YEAR, 100) = 0
               MOVE 'N' TO WD-LEAP-YEAR-SW
           ELSE IF FUNCTION MOD(WD-YEAR, 4) = 0
               MOVE 'Y' TO WD-LEAP-YEAR-SW
           END-IF
           IF WD-IS-LEAP-YEAR
               MOVE 29 TO WMD-DAYS(2)
           ELSE
               MOVE 28 TO WMD-DAYS(2)
           END-IF
           MOVE WMD-DAYS(WD-MONTH) TO WD-DAYS-IN-MONTH
           ADD 1 TO WD-DAY
           IF WD-DAY > WD-DAYS-IN-MONTH
               MOVE 1 TO WD-DAY
               ADD 1 TO WD-MONTH
               IF WD-MONTH > 12
                   MOVE 1  TO WD-MONTH
                   ADD 1   TO WD-YEAR
               END-IF
           END-IF
           STRING WD-YEAR  DELIMITED SIZE
                  WD-MONTH DELIMITED SIZE
                  WD-DAY   DELIMITED SIZE
               INTO WD-WORK-DATE
           MOVE WMD-DAYS(2) TO WMD-DAYS(2).
      *================================================================
       3300-UPDATE-KZSCHF.
      *================================================================
           MOVE DQ-ACCT-NO TO SCH-ACCT-NO
           READ KZSCHF
           EVALUATE WS-KZSCHF-ST
               WHEN '00'
      *            既存レコード更新
                   MOVE 'Y' TO WF-KZSCHF-FOUND
                   MOVE SCH-NEXT-DUE-DT TO WSS-SCH-NEXT-DUE-DT
                   PERFORM 3310-UPDATE-EXISTING
               WHEN '23'
      *            新規レコード書込
                   MOVE 'N' TO WF-KZSCHF-FOUND
                   PERFORM 3320-INSERT-NEW
               WHEN OTHER
                   DISPLAY 'KZSCHF 読込失敗 ST=' WS-KZSCHF-ST
                           ' 口座=' DQ-ACCT-NO
                   ADD 1 TO WC-ERROR-CNT
           END-EVALUATE.
      *================================================================
       3310-UPDATE-EXISTING.
      *================================================================
      *    変更前の期日を保存して比較
           IF SCH-NEXT-DUE-DT = WD-CANDIDATE-DATE
      *        変更なし
               ADD 1 TO WC-UNCHANGED-CNT
           ELSE
      *        変更あり → 更新
               MOVE WD-CANDIDATE-DATE TO SCH-NEXT-DUE-DT
               IF RSC-APPLIED
                   MOVE WD-CANDIDATE-DATE TO SCH-REVISED-DUE-DT
                   ADD 1 TO SCH-RESCHEDULE-CNT
               END-IF
               REWRITE KZSCHF-REC
               IF WS-KZSCHF-ST NOT = '00'
                   DISPLAY 'KZSCHF 更新失敗 ST=' WS-KZSCHF-ST
                           ' 口座=' DQ-ACCT-NO
                   ADD 1 TO WC-ERROR-CNT
               ELSE
                   ADD 1 TO WC-REVISED-CNT
                   MOVE 'Y' TO WF-DUE-REVISED
                   PERFORM 3400-WRITE-REPORT-LINE
               END-IF
           END-IF.
      *================================================================
       3320-INSERT-NEW.
      *================================================================
           MOVE DQ-ACCT-NO        TO SCH-ACCT-NO
           MOVE DQ-DUE-DT         TO SCH-ORIG-DUE-DT
           MOVE WD-CANDIDATE-DATE TO SCH-NEXT-DUE-DT
           IF RSC-APPLIED
               MOVE WD-CANDIDATE-DATE TO SCH-REVISED-DUE-DT
               MOVE 1 TO SCH-RESCHEDULE-CNT
           ELSE
               MOVE ZEROS TO SCH-REVISED-DUE-DT
               MOVE 0     TO SCH-RESCHEDULE-CNT
           END-IF
           WRITE KZSCHF-REC
           IF WS-KZSCHF-ST NOT = '00'
               DISPLAY 'KZSCHF 書込失敗 ST=' WS-KZSCHF-ST
                       ' 口座=' DQ-ACCT-NO
               ADD 1 TO WC-ERROR-CNT
           ELSE
               ADD 1 TO WC-NEW-SCH-CNT
               MOVE 'Y' TO WF-DUE-REVISED
               PERFORM 3400-WRITE-REPORT-LINE
           END-IF.
      *================================================================
       3400-WRITE-REPORT-LINE.
      *================================================================
           MOVE SPACES TO WS-REPORT-LINE
           MOVE DQ-ACCT-NO TO WR-ACCT-NO
           MOVE WSS-SCH-NEXT-DUE-DT TO WR-OLD-DUE-DT
           MOVE WD-CANDIDATE-DATE   TO WR-NEW-DUE-DT
           IF RSC-APPLIED
               MOVE 'RSC ' TO WR-RSC-FLG
               MOVE 'リスケ承認適用' TO WR-MSG
           ELSE
               MOVE '    ' TO WR-RSC-FLG
               MOVE '休日調整期日更新' TO WR-MSG
           END-IF
           MOVE WS-REPORT-LINE TO SYSOUT-REC
           WRITE SYSOUT-REC
           IF WS-SYSOUT-ST NOT = '00'
               DISPLAY 'SYSOUT 書込失敗 ST=' WS-SYSOUT-ST
           END-IF.
      *================================================================
       9000-TERMINATE.
      *================================================================
      *    トレーラ行出力
           MOVE WC-REVISED-CNT   TO WT-REVISED
           MOVE WC-UNCHANGED-CNT TO WT-UNCHANGED
           MOVE WC-NEW-SCH-CNT   TO WT-NEW
           MOVE WS-TRAILER-LINE TO SYSOUT-REC
           WRITE SYSOUT-REC
           IF WS-SYSOUT-ST NOT = '00'
               DISPLAY 'SYSOUT トレーラ書込失敗'
               DISPLAY 'ST=' WS-SYSOUT-ST
           END-IF
      *    ファイルクローズ
           CLOSE KZDLQF
           IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZDLQF クローズ失敗 ST=' WS-KZDLQF-ST
           END-IF
           CLOSE KZRSCF
           IF WS-KZRSCF-ST NOT = '00'
               DISPLAY 'KZRSCF クローズ失敗 ST=' WS-KZRSCF-ST
           END-IF
           CLOSE KZSCHF
           IF WS-KZSCHF-ST NOT = '00'
               DISPLAY 'KZSCHF クローズ失敗 ST=' WS-KZSCHF-ST
           END-IF
           CLOSE SYSOUT
           IF WS-SYSOUT-ST NOT = '00'
               DISPLAY 'SYSOUT クローズ失敗 ST=' WS-SYSOUT-ST
           END-IF
      *    処理統計ログ
           DISPLAY '処理終了 KZDLQF読込=' WC-KZDLQF-READ
           DISPLAY '        改定件数  =' WC-REVISED-CNT
           DISPLAY '        変更なし  =' WC-UNCHANGED-CNT
           DISPLAY '        新規SCH   =' WC-NEW-SCH-CNT
           DISPLAY '        エラー    =' WC-ERROR-CNT
      *    リターンコード設定
           IF WC-ERROR-CNT > ZERO
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
