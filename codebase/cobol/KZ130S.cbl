       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ130S.
      *
      *変更履歴
      *版数  年月日      担当                         概要
      *01    平成30年04月 システム部 勘定系チーム     初版作成
      *02    令和02年10月 システム部 勘定系チーム     手数料端数対応
      *03    令和05年06月 システム部 勘定系チーム     保守資料反映
      *
      *端数処理ユーティリティ
      *円未満を切り捨て、手数料を整数円へ正規化する。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-AMT-IN              PIC S9(15)V9(06) COMP-3.
           05  WS-AMT-YEN             PIC S9(15)        COMP-3.
           05  WS-VALID-SW            PIC X VALUE "N".
               88  WS-VALID-AMT       VALUE "Y".
               88  WS-INVALID-AMT     VALUE "N".
      *
       LINKAGE SECTION.
           COPY LK-RND-PARM.
      *
       PROCEDURE DIVISION USING LK-RND-PARM.
      *
       0000-MAIN SECTION.
       0000-MAIN-START.
           PERFORM 1000-VALIDATE-PARM
           IF WS-VALID-AMT
              PERFORM 2000-FLOOR-YEN
           ELSE
              MOVE ZERO TO LK-AMT-FLOORED
           END-IF
           GOBACK
           .
      *
       1000-VALIDATE-PARM SECTION.
       1000-VALIDATE-PARM-START.
           SET WS-INVALID-AMT TO TRUE
           IF LK-AMT-RAW IS NUMERIC
              IF LK-AMT-RAW >= ZERO
                 SET WS-VALID-AMT TO TRUE
              END-IF
           END-IF
           .
      *
       2000-FLOOR-YEN SECTION.
       2000-FLOOR-YEN-START.
           MOVE LK-AMT-RAW TO WS-AMT-IN
           COMPUTE WS-AMT-YEN = FUNCTION INTEGER(WS-AMT-IN)
           MOVE WS-AMT-YEN TO LK-AMT-FLOORED
           .
