       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH100S.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当   概要
      * 1.00  20240115  共通   初版作成
      * 1.01  20240520  共通   年累計逆転判定を追加
      * 1.02  20241108  共通   桁上限判定と理由文言を整理
      ******************************************************************
      * DWH共通金額編集サブ
      * 手数料金額と年累計を外部表示用小数二桁へ標準化する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-WORK-AREA.
           05  WK-FEE-2             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WK-YTD-2             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WK-PRV-2             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WK-FEE-ABS           PIC 9(15)V99  COMP-3 VALUE ZERO.
           05  WK-YTD-ABS           PIC 9(15)V99  COMP-3 VALUE ZERO.
           05  WK-PRV-ABS           PIC 9(15)V99  COMP-3 VALUE ZERO.
           05  WK-HARD-ERR          PIC X VALUE SPACE.
               88  WK-HARD-ERR-ON         VALUE '1'.
           05  WK-EDIT-WARN         PIC X VALUE SPACE.
               88  WK-EDIT-WARN-ON        VALUE '1'.

       01  WK-LIMITS.
           05  WK-MAX-AMT           PIC 9(13)V99 COMP-3
                                      VALUE 9999999999999.99.

       01  WK-EDIT-FIELDS.
           05  WK-FEE-DISP          PIC -ZZZZZZZZZZZZ9.99.
           05  WK-YTD-DISP          PIC -ZZZZZZZZZZZZ9.99.
           05  WK-PRV-DISP          PIC -ZZZZZZZZZZZZ9.99.

       LINKAGE SECTION.
       01  JH315S-PARM.
           05  JH315S-IN-AREA.
               10  JH315S-FEE-AMT   PIC S9(13)V9(4) COMP-3.
               10  JH315S-YTD-AMT   PIC S9(15)V9(4) COMP-3.
               10  JH315S-PRV-YTD   PIC S9(15)V9(4) COMP-3.
               10  JH315S-SCALE     PIC 9.
           05  JH315S-OUT-AREA.
               10  JH315S-FEE-TEXT  PIC X(18).
               10  JH315S-YTD-TEXT  PIC X(18).
               10  JH315S-PRV-TEXT  PIC X(18).
               10  JH315S-STATUS    PIC XX.
               10  JH315S-ZERO-FLG  PIC X.
               10  JH315S-MINUS-FLG PIC X.
               10  JH315S-OVER-FLG  PIC X.
               10  JH315S-REV-FLG   PIC X.
               10  JH315S-REASON    PIC X(40).

       PROCEDURE DIVISION USING JH315S-PARM.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-INPUT
           IF WK-HARD-ERR-ON
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 3000-EDIT-AMOUNT
           PERFORM 4000-SET-RESULT
           MOVE 0 TO RETURN-CODE
           GOBACK
           .

       1000-INIT.
           MOVE SPACES TO JH315S-FEE-TEXT
                          JH315S-YTD-TEXT
                          JH315S-PRV-TEXT
                          JH315S-STATUS
                          JH315S-ZERO-FLG
                          JH315S-MINUS-FLG
                          JH315S-OVER-FLG
                          JH315S-REV-FLG
                          JH315S-REASON
                          WK-HARD-ERR
                          WK-EDIT-WARN
           MOVE '00' TO JH315S-STATUS
           MOVE '0'  TO JH315S-ZERO-FLG
                        JH315S-MINUS-FLG
                        JH315S-OVER-FLG
                        JH315S-REV-FLG
           .

       2000-CHECK-INPUT.
           IF JH315S-SCALE > 4
              MOVE '1' TO WK-HARD-ERR
              MOVE '99' TO JH315S-STATUS
              MOVE '小数桁指定不正' TO JH315S-REASON
              DISPLAY 'JH315S 小数桁指定不正 SCALE=' JH315S-SCALE
           END-IF
           .

       3000-EDIT-AMOUNT.
           COMPUTE WK-FEE-2 ROUNDED = JH315S-FEE-AMT
           COMPUTE WK-YTD-2 ROUNDED = JH315S-YTD-AMT
           COMPUTE WK-PRV-2 ROUNDED = JH315S-PRV-YTD

           COMPUTE WK-FEE-ABS = FUNCTION ABS(WK-FEE-2)
           COMPUTE WK-YTD-ABS = FUNCTION ABS(WK-YTD-2)
           COMPUTE WK-PRV-ABS = FUNCTION ABS(WK-PRV-2)

           IF WK-FEE-2 = ZERO AND WK-YTD-2 = ZERO
              MOVE '1' TO JH315S-ZERO-FLG
              MOVE '1' TO WK-EDIT-WARN
              MOVE '10' TO JH315S-STATUS
              MOVE '金額ゼロ' TO JH315S-REASON
           END-IF

           IF WK-FEE-2 < ZERO OR WK-YTD-2 < ZERO
              MOVE '1' TO JH315S-MINUS-FLG
              MOVE '1' TO WK-EDIT-WARN
              MOVE '20' TO JH315S-STATUS
              MOVE '負値検出' TO JH315S-REASON
           END-IF

           IF WK-FEE-ABS > WK-MAX-AMT OR WK-YTD-ABS > WK-MAX-AMT
              MOVE '1' TO JH315S-OVER-FLG
              MOVE '1' TO WK-EDIT-WARN
              MOVE '30' TO JH315S-STATUS
              MOVE '桁上限超過' TO JH315S-REASON
           END-IF

           IF WK-YTD-2 < WK-PRV-2
              MOVE '1' TO JH315S-REV-FLG
              MOVE '1' TO WK-EDIT-WARN
              MOVE '40' TO JH315S-STATUS
              MOVE '年累計逆転' TO JH315S-REASON
           END-IF

           IF WK-EDIT-WARN NOT = '1'
              MOVE '正常' TO JH315S-REASON
           END-IF
           .

       4000-SET-RESULT.
           MOVE WK-FEE-2 TO WK-FEE-DISP
           MOVE WK-YTD-2 TO WK-YTD-DISP
           MOVE WK-PRV-2 TO WK-PRV-DISP
           MOVE WK-FEE-DISP TO JH315S-FEE-TEXT
           MOVE WK-YTD-DISP TO JH315S-YTD-TEXT
           MOVE WK-PRV-DISP TO JH315S-PRV-TEXT
           .
