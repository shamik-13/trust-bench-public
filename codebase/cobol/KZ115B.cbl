       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ115B.
      *変更履歴
      *版数  年月日       担当                         概要
      *1.00  令和06年04月01日 システム部 勘定系チーム 初版作成
      *1.01  令和07年10月15日 システム部 勘定系チーム 年間手数料対応
      *1.02  令和07年12月09日 システム部 勘定系チーム 端数処理見直し
       AUTHOR. システム部 勘定系チーム.
       DATE-WRITTEN. 2024-04-01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-VALID-SW       PIC X VALUE SPACE.
           88  WS-INPUT-VALID      VALUE 'Y'.
           88  WS-INPUT-INVALID    VALUE 'N'.

       LINKAGE SECTION.
           COPY LK-CAP-PARM.

       PROCEDURE DIVISION USING LK-CAP-PARM.

       0000-MAIN SECTION.
       0000-MAIN-ENTRY.
           PERFORM 1000-VALIDATE-PARM
           PERFORM 2000-APPLY-CURRENT-RULE
           GOBACK
           .

       1000-VALIDATE-PARM SECTION.
       1000-VALIDATE-PARM-ENTRY.
           IF LK-FEE-YTD IS NUMERIC
               SET WS-INPUT-VALID TO TRUE
           ELSE
               SET WS-INPUT-INVALID TO TRUE
           END-IF
           .

       2000-APPLY-CURRENT-RULE SECTION.
       2000-APPLY-CURRENT-RULE-ENTRY.
           IF WS-INPUT-VALID
               MOVE LK-FEE-YTD TO LK-FEE-CAPPED
           ELSE
               MOVE ZERO TO LK-FEE-CAPPED
           END-IF
           MOVE 'N' TO LK-CAP-FLAG
           .
