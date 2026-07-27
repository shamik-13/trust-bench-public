       IDENTIFICATION DIVISION.
      *================================================================*
      * 変更履歴                                                        *
      * 版数  年月日      担当           概要                           *
      * V1.0  20260601   KZ開発グループ  新規作成                       *
      * V1.1  20260612   KZ開発グループ  据置期間ロジック修正            *
      * V1.2  20260619   KZ開発グループ  ステータス遷移条件修正          *
      *================================================================*
       PROGRAM-ID.    KZ510B.
      *----------------------------------------------------------------*
      * プログラム名   : KZ510B                                        *
      * 機能概要       : 延滞判定・遅延損害金バッチ                    *
      * 入力ファイル   : KZDLQF（延滞口座ファイル）                   *
      * 出力ファイル   : KZDLRF（延滞判定結果ファイル）                *
      * 処理概要       : 延滞口座を順読みし、経過日数・遅延損害金・    *
      *                  エージングバケット・新ステータスを算出して    *
      *                  KZDLRF へ1レコード出力する。                  *
      * 遅延損害金     : 延滞元本 × 年14.6% × 課金日数 ÷ 365        *
      *                  円未満切捨て（ROUNDED 不使用）                *
      * 据置期間       : 課金対象は支払期日超過後10日を超えた日数      *
      *                  ステータス遷移は据置期間の影響を受けない      *
      * 作成者         : KZ開発グループ                                *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LINUX.
       OBJECT-COMPUTER. LINUX.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLQF
               ASSIGN       TO KZDLQF
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-KZDLQF-ST.
           SELECT KZDLRF
               ASSIGN       TO KZDLRF
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
      *----------------------------------------------------------------*
      * ファイルステータスフィールド                                    *
      *----------------------------------------------------------------*
       01  WS-KZDLQF-ST        PIC X(02) VALUE SPACES.
       01  WS-KZDLRF-ST        PIC X(02) VALUE SPACES.
      *----------------------------------------------------------------*
      * ファイルオープン状態管理フラグ                                  *
      *----------------------------------------------------------------*
       01  WS-FILE-OPEN-GRP.
           05  WS-KZDLQF-OPEN  PIC X(01) VALUE '0'.
               88  KZDLQF-IS-OPEN  VALUE '1'.
           05  WS-KZDLRF-OPEN  PIC X(01) VALUE '0'.
               88  KZDLRF-IS-OPEN  VALUE '1'.
      *----------------------------------------------------------------*
      * EOF フラグ                                                      *
      *----------------------------------------------------------------*
       01  WS-KZDLQF-EOF-FLG   PIC X(01) VALUE '0'.
           88  KZDLQF-IS-EOF    VALUE '1'.
      *----------------------------------------------------------------*
      * 処理カウンタ                                                    *
      *----------------------------------------------------------------*
       01  WS-READ-CNT         PIC 9(07) COMP-3 VALUE ZERO.
       01  WS-WRITE-CNT        PIC 9(07) COMP-3 VALUE ZERO.
      *----------------------------------------------------------------*
      * 日付計算ワーク                                                  *
      *----------------------------------------------------------------*
       01  WS-DUE-INT          PIC 9(08) COMP   VALUE ZERO.
       01  WS-ASOF-INT         PIC 9(08) COMP   VALUE ZERO.
       01  WS-DAYS-OVR         PIC S9(06) COMP  VALUE ZERO.
       01  WS-CHARGEABLE-DAYS  PIC 9(06)  COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * 遅延損害金計算ワーク                                            *
      *----------------------------------------------------------------*
       01  WS-LATE-CHG-WORK    PIC 9(15)V99 COMP-3 VALUE ZERO.
      *----------------------------------------------------------------*
      * 計算定数定義                                                    *
      *----------------------------------------------------------------*
       01  WS-CALC-CONSTS.
           05  WS-ANNUAL-RATE  PIC 9V9(04) COMP-3 VALUE 0.1460.
           05  WS-DAYS-YEAR    PIC 9(03)   COMP-3 VALUE 365.
           05  WS-GRACE-DAYS   PIC 9(02)   COMP-3 VALUE 10.
      *----------------------------------------------------------------*
      * エージングバケット定数                                          *
      *----------------------------------------------------------------*
       01  WS-BUCKET-CONSTS.
           05  WS-BKT-B1       PIC X(02) VALUE 'B1'.
           05  WS-BKT-B2       PIC X(02) VALUE 'B2'.
           05  WS-BKT-B3       PIC X(02) VALUE 'B3'.
           05  WS-BKT-B4       PIC X(02) VALUE 'B4'.
      *----------------------------------------------------------------*
      * 延滞ステータス定数                                              *
      *----------------------------------------------------------------*
       01  WS-STATUS-CONSTS.
           05  WS-STS-NORMAL   PIC X(02) VALUE '00'.
           05  WS-STS-ENTTAI   PIC X(02) VALUE '10'.
           05  WS-STS-KAISHU   PIC X(02) VALUE '30'.
       PROCEDURE DIVISION.
      *================================================================*
      * メイン制御                                                      *
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS UNTIL KZDLQF-IS-EOF
           PERFORM 3000-TERMINATION
           GOBACK
           .
      *================================================================*
      * 初期処理                                                        *
      *================================================================*
       1000-INIT.
           MOVE ZERO TO RETURN-CODE
           OPEN INPUT KZDLQF
           IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLQF オープン失敗 ST='
                       WS-KZDLQF-ST
               MOVE 8 TO RETURN-CODE
               PERFORM 9900-ABEND
           END-IF
           MOVE '1' TO WS-KZDLQF-OPEN
           OPEN OUTPUT KZDLRF
           IF WS-KZDLRF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLRF オープン失敗 ST='
                       WS-KZDLRF-ST
               MOVE 8 TO RETURN-CODE
               PERFORM 9900-ABEND
           END-IF
           MOVE '1' TO WS-KZDLRF-OPEN
           PERFORM 1100-FIRST-READ
           .
      *----------------------------------------------------------------*
      * 先行読込（プライミングリード）                                   *
      *----------------------------------------------------------------*
       1100-FIRST-READ.
           READ KZDLQF
               AT END
                   MOVE '1' TO WS-KZDLQF-EOF-FLG
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
           IF WS-KZDLQF-ST NOT = '00'
              AND WS-KZDLQF-ST NOT = '10'
               DISPLAY 'KZ510B KZDLQF 初回読込エラー ST='
                       WS-KZDLQF-ST
               MOVE 8 TO RETURN-CODE
               PERFORM 9900-ABEND
           END-IF
           .
      *================================================================*
      * 主処理ループ                                                    *
      *================================================================*
       2000-PROCESS.
           PERFORM 2100-CALC-DAYS-OVERDUE
           PERFORM 2200-SET-AGING-BUCKET
           PERFORM 2300-CALC-LATE-CHARGE
           PERFORM 2400-SET-NEW-STATUS
           PERFORM 2500-WRITE-RESULT
           PERFORM 2900-READ-NEXT
           .
      *----------------------------------------------------------------*
      * 経過日数算出                                                    *
      * FUNCTION INTEGER-OF-DATE（YYYYMMDD形式）差分で実暦日数を算出   *
      * ASOF <= DUE の場合は経過日数 0 とする                          *
      *----------------------------------------------------------------*
       2100-CALC-DAYS-OVERDUE.
           COMPUTE WS-DUE-INT  =
               FUNCTION INTEGER-OF-DATE(DQ-DUE-DT)
           COMPUTE WS-ASOF-INT =
               FUNCTION INTEGER-OF-DATE(DQ-ASOF-DT)
           COMPUTE WS-DAYS-OVR =
               WS-ASOF-INT - WS-DUE-INT
           IF WS-DAYS-OVR < ZERO
               MOVE ZERO TO WS-DAYS-OVR
           END-IF
           MOVE WS-DAYS-OVR TO DR-DAYS-OVERDUE
           .
      *----------------------------------------------------------------*
      * エージングバケット判定                                          *
      * 1–30日:B1  31–60日:B2  61–90日:B3  91日以上:B4               *
      * 経過日数 0 以下は正常口座のためバケット空欄                     *
      *----------------------------------------------------------------*
       2200-SET-AGING-BUCKET.
           EVALUATE TRUE
               WHEN WS-DAYS-OVR <= ZERO
                   MOVE SPACES    TO DR-AGING-BUCKET
               WHEN WS-DAYS-OVR <= 30
                   MOVE WS-BKT-B1 TO DR-AGING-BUCKET
               WHEN WS-DAYS-OVR <= 60
                   MOVE WS-BKT-B2 TO DR-AGING-BUCKET
               WHEN WS-DAYS-OVR <= 90
                   MOVE WS-BKT-B3 TO DR-AGING-BUCKET
               WHEN OTHER
                   MOVE WS-BKT-B4 TO DR-AGING-BUCKET
           END-EVALUATE
           .
      *----------------------------------------------------------------*
      * 遅延損害金算出                                                  *
      * 課金日数 = MAX(0, 経過日数 − 据置10日)                         *
      * 遅延損害金 = 延滞元本 × 0.146 × 課金日数 ÷ 365                *
      * 円未満切捨て（ROUNDED 不使用、端数は自動切捨て）               *
      * 経過日数が据置期間以内の場合は遅延損害金 0 円                  *
      *----------------------------------------------------------------*
       2300-CALC-LATE-CHARGE.
           IF WS-DAYS-OVR > WS-GRACE-DAYS
               COMPUTE WS-CHARGEABLE-DAYS =
                   WS-DAYS-OVR - WS-GRACE-DAYS
               COMPUTE WS-LATE-CHG-WORK =
                   DQ-OVERDUE-AMT * WS-ANNUAL-RATE
                   * WS-CHARGEABLE-DAYS / WS-DAYS-YEAR
               MOVE WS-LATE-CHG-WORK TO DR-LATE-CHARGE-AMT
           ELSE
               MOVE ZERO TO WS-CHARGEABLE-DAYS
               MOVE ZERO TO DR-LATE-CHARGE-AMT
           END-IF
           .
      *----------------------------------------------------------------*
      * 新ステータス判定（D-1502）                                      *
      * 経過日数 0 以下  → 00（正常）                                  *
      * 経過日数 1–60日  → 10（延滞）  ※B1（1-30日）B2（31-60日）    *
      * 経過日数 61日以上 → 30（回収） ※B3到達でステータス遷移        *
      * 判定は据置期間の影響を受けない（経過日数そのままを使用）       *
      *----------------------------------------------------------------*
       2400-SET-NEW-STATUS.
           EVALUATE TRUE
               WHEN WS-DAYS-OVR <= ZERO
                   MOVE WS-STS-NORMAL TO DR-NEW-STATUS
               WHEN WS-DAYS-OVR <= 60
                   MOVE WS-STS-ENTTAI TO DR-NEW-STATUS
               WHEN OTHER
                   MOVE WS-STS-KAISHU TO DR-NEW-STATUS
           END-EVALUATE
           .
      *----------------------------------------------------------------*
      * 結果レコード出力                                                *
      *----------------------------------------------------------------*
       2500-WRITE-RESULT.
           MOVE DQ-ACCT-NO TO DR-ACCT-NO
           WRITE KZDLRF-REC
           IF WS-KZDLRF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLRF 書込エラー ST='
                       WS-KZDLRF-ST
               DISPLAY 'KZ510B 対象口座番号=' DQ-ACCT-NO
               MOVE 8 TO RETURN-CODE
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-WRITE-CNT
           .
      *----------------------------------------------------------------*
      * 次レコード読込                                                  *
      *----------------------------------------------------------------*
       2900-READ-NEXT.
           READ KZDLQF
               AT END
                   MOVE '1' TO WS-KZDLQF-EOF-FLG
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
           IF WS-KZDLQF-ST NOT = '00'
              AND WS-KZDLQF-ST NOT = '10'
               DISPLAY 'KZ510B KZDLQF 読込エラー ST='
                       WS-KZDLQF-ST
               DISPLAY 'KZ510B 対象口座番号=' DQ-ACCT-NO
               MOVE 8 TO RETURN-CODE
               PERFORM 9900-ABEND
           END-IF
           .
      *================================================================*
      * 終了処理                                                        *
      *================================================================*
       3000-TERMINATION.
           CLOSE KZDLQF
           IF WS-KZDLQF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLQF クローズ失敗 ST='
                       WS-KZDLQF-ST
               MOVE 8 TO RETURN-CODE
               PERFORM 9900-ABEND
           END-IF
           MOVE '0' TO WS-KZDLQF-OPEN
           CLOSE KZDLRF
           IF WS-KZDLRF-ST NOT = '00'
               DISPLAY 'KZ510B KZDLRF クローズ失敗 ST='
                       WS-KZDLRF-ST
               MOVE 8 TO RETURN-CODE
               PERFORM 9900-ABEND
           END-IF
           MOVE '0' TO WS-KZDLRF-OPEN
           DISPLAY 'KZ510B 処理正常終了'
           DISPLAY 'KZ510B 読込件数 =' WS-READ-CNT
           DISPLAY 'KZ510B 書込件数 =' WS-WRITE-CNT
           MOVE ZERO TO RETURN-CODE
           .
      *================================================================*
      * 異常終了処理                                                    *
      * オープン済ファイルのみクローズしてから GOBACK する              *
      *================================================================*
       9900-ABEND.
           DISPLAY 'KZ510B 異常終了 RC=' RETURN-CODE
           IF KZDLQF-IS-OPEN
               CLOSE KZDLQF
           END-IF
           IF KZDLRF-IS-OPEN
               CLOSE KZDLRF
           END-IF
           GOBACK
           .
