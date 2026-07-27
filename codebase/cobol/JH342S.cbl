       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH342S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  H28.04.01    システム部 情報系チーム  新規作成
      * 1.01  R02.10.15    システム部 情報系チーム  分布帳票編集項目追加
      * 1.02  R06.03.22    システム部 情報系チーム  帳票出力判定見直し

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-COMP-RATE       PIC 9(07)V9(06) VALUE ZERO.
           05  WS-AVG-BAL         PIC S9(15)V9(06) VALUE ZERO.
           05  WS-ABS-RATE        PIC 9(07)V9(06) VALUE ZERO.
           05  WS-ABS-AVG         PIC 9(15)V9(06) VALUE ZERO.
           05  WS-EDIT-RATE       PIC 9(03)V9(02) VALUE ZERO.
           05  WS-EDIT-AVG        PIC S9(13)V9(02) VALUE ZERO.
           05  WS-HARD-ERR        PIC X VALUE SPACE.
           05  WS-ZERO-DIV        PIC X VALUE SPACE.

       01  WS-CONST.
           05  WK-ON              PIC X VALUE '1'.
           05  WK-OFF             PIC X VALUE '0'.
           05  WK-BLANK           PIC X VALUE SPACE.
           05  WK-SIGN-PLUS       PIC X VALUE '+'.
           05  WK-SIGN-MINUS      PIC X VALUE '-'.
           05  WK-SIGN-ZERO       PIC X VALUE '0'.
           05  WK-OVF-YES         PIC X VALUE '1'.
           05  WK-OVF-NO          PIC X VALUE '0'.
           05  WK-MSG-NORMAL      PIC X(08) VALUE '00000000'.
           05  WK-MSG-PARM        PIC X(08) VALUE 'JH342001'.
           05  WK-MSG-MODE        PIC X(08) VALUE 'JH342002'.
           05  WK-MSG-OVF         PIC X(08) VALUE 'JH342003'.

       LINKAGE SECTION.
       01  LK-JH342S-PARM.
           05  LK-IN-AREA.
               10  LK-GROUP-COUNT     PIC 9(09).
               10  LK-BALANCE-SUM     PIC S9(15)V99 COMP-3.
               10  LK-TOTAL-COUNT     PIC 9(09).
               10  LK-PREV-BAL-SUM    PIC S9(15)V99 COMP-3.
               10  LK-ROUND-MODE      PIC X.
           05  LK-OUT-AREA.
               10  LK-COMP-RATE       PIC 9(03)V99.
               10  LK-AVG-BALANCE     PIC S9(13)V99.
               10  LK-DIFF-SIGN       PIC X.
               10  LK-OVERFLOW-FLAG   PIC X.
               10  LK-RETURN-CD       PIC 9(02).
               10  LK-MESSAGE-CD      PIC X(08).

       PROCEDURE DIVISION USING LK-JH342S-PARM.

       0000-MAIN.
      * 判定前状態: 上位から受領した集計値をそのまま検査する。
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE
           IF WS-HARD-ERR = WK-OFF
               PERFORM 3000-CALCULATE
               PERFORM 4000-SET-OUTPUT
           END-IF
           PERFORM 9000-FINISH
           GOBACK.

       1000-INITIALIZE.
           MOVE WK-OFF       TO WS-HARD-ERR
           MOVE WK-OFF       TO WS-ZERO-DIV
           MOVE ZERO         TO WS-COMP-RATE
           MOVE ZERO         TO WS-AVG-BAL
           MOVE ZERO         TO WS-ABS-RATE
           MOVE ZERO         TO WS-ABS-AVG
           MOVE ZERO         TO WS-EDIT-RATE
           MOVE ZERO         TO WS-EDIT-AVG
           MOVE ZERO         TO LK-COMP-RATE
           MOVE ZERO         TO LK-AVG-BALANCE
           MOVE WK-SIGN-ZERO TO LK-DIFF-SIGN
           MOVE WK-OVF-NO    TO LK-OVERFLOW-FLAG
           MOVE ZERO         TO LK-RETURN-CD
           MOVE WK-MSG-NORMAL TO LK-MESSAGE-CD
           MOVE 0            TO RETURN-CODE.

       2000-VALIDATE.
           IF LK-ROUND-MODE NOT = '0'
              AND LK-ROUND-MODE NOT = '1'
               MOVE WK-ON       TO WS-HARD-ERR
               MOVE 8           TO LK-RETURN-CD
               MOVE WK-MSG-MODE TO LK-MESSAGE-CD
               DISPLAY '丸め区分不正 CD=' LK-ROUND-MODE
           END-IF

           IF LK-GROUP-COUNT > LK-TOTAL-COUNT
              AND LK-TOTAL-COUNT NOT = ZERO
               MOVE WK-ON       TO WS-HARD-ERR
               MOVE 8           TO LK-RETURN-CD
               MOVE WK-MSG-PARM TO LK-MESSAGE-CD
               DISPLAY '構成比件数不整合'
           END-IF

           IF LK-TOTAL-COUNT = ZERO
              OR LK-GROUP-COUNT = ZERO
               MOVE WK-ON TO WS-ZERO-DIV
           END-IF.

       3000-CALCULATE.
           IF WS-ZERO-DIV = WK-ON
               MOVE ZERO TO WS-COMP-RATE
               MOVE ZERO TO WS-AVG-BAL
           ELSE
               IF LK-ROUND-MODE = '0'
                   COMPUTE WS-COMP-RATE ROUNDED =
                       LK-GROUP-COUNT * 100 / LK-TOTAL-COUNT
                   COMPUTE WS-AVG-BAL ROUNDED =
                       LK-BALANCE-SUM / LK-GROUP-COUNT
               ELSE
                   COMPUTE WS-COMP-RATE =
                       LK-GROUP-COUNT * 100 / LK-TOTAL-COUNT
                   COMPUTE WS-AVG-BAL =
                       LK-BALANCE-SUM / LK-GROUP-COUNT
               END-IF
           END-IF

           IF LK-BALANCE-SUM > LK-PREV-BAL-SUM
               MOVE WK-SIGN-PLUS TO LK-DIFF-SIGN
           ELSE
               IF LK-BALANCE-SUM < LK-PREV-BAL-SUM
                   MOVE WK-SIGN-MINUS TO LK-DIFF-SIGN
               ELSE
                   MOVE WK-SIGN-ZERO TO LK-DIFF-SIGN
               END-IF
           END-IF.

       4000-SET-OUTPUT.
           MOVE WS-COMP-RATE TO WS-ABS-RATE
           IF WS-AVG-BAL < ZERO
               COMPUTE WS-ABS-AVG = WS-AVG-BAL * -1
           ELSE
               MOVE WS-AVG-BAL TO WS-ABS-AVG
           END-IF

           IF WS-ABS-RATE > 999.99
              OR WS-ABS-AVG > 9999999999999.99
               MOVE WK-OVF-YES  TO LK-OVERFLOW-FLAG
               MOVE WK-MSG-OVF  TO LK-MESSAGE-CD
               DISPLAY '帳票編集桁あふれ'
           ELSE
               MOVE WK-OVF-NO TO LK-OVERFLOW-FLAG
               MOVE WK-MSG-NORMAL TO LK-MESSAGE-CD
           END-IF

           IF WS-ABS-RATE > 999.99
               MOVE 999.99 TO WS-EDIT-RATE
           ELSE
               MOVE WS-COMP-RATE TO WS-EDIT-RATE
           END-IF

           IF WS-ABS-AVG > 9999999999999.99
               IF WS-AVG-BAL < ZERO
                   MOVE -9999999999999.99 TO WS-EDIT-AVG
               ELSE
                   MOVE 9999999999999.99 TO WS-EDIT-AVG
               END-IF
           ELSE
               MOVE WS-AVG-BAL TO WS-EDIT-AVG
           END-IF

           MOVE WS-EDIT-RATE TO LK-COMP-RATE
           MOVE WS-EDIT-AVG  TO LK-AVG-BALANCE.

       9000-FINISH.
           IF LK-RETURN-CD = ZERO
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 8 TO RETURN-CODE
           END-IF.
