       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB290S.
       AUTHOR. 大原 修.
      *===============================================================*
      * 残高スライド元金定額算定サブルーチン                         *
      * リボ残高より旧残高スライド表で月次元金額を算定する。         *
      *===============================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-ABS-BAL             PIC S9(13) VALUE ZERO.
           05  WS-ERR-MSG             PIC X(60) VALUE SPACE.

       01  WS-SLIDE-TABLE.
           05  WS-TBL-ENTRY OCCURS 3 TIMES
               INDEXED BY WS-SL-IDX.
               10  WS-TBL-UPPER       PIC S9(13).
               10  WS-TBL-PRIN        PIC S9(09).
               10  WS-TBL-TIER        PIC X(02).

       LINKAGE SECTION.
           COPY LK-SLIDE-PARM.

       PROCEDURE DIVISION USING LK-SLIDE-PARM.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-PARM
           IF LK-SL-RET = '00'
              PERFORM 3000-CALC-PRINCIPAL
           END-IF
           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           MOVE ZERO  TO LK-SL-PRIN-AMT
           MOVE SPACE TO LK-SL-TIER
           MOVE '00'  TO LK-SL-RET
           MOVE ZERO  TO RETURN-CODE

           MOVE 200000  TO WS-TBL-UPPER (1)
           MOVE 3000    TO WS-TBL-PRIN  (1)
           MOVE 'T1'    TO WS-TBL-TIER  (1)

           MOVE 500000  TO WS-TBL-UPPER (2)
           MOVE 6000    TO WS-TBL-PRIN  (2)
           MOVE 'T2'    TO WS-TBL-TIER  (2)

           MOVE 9999999999999 TO WS-TBL-UPPER (3)
           MOVE 10000         TO WS-TBL-PRIN  (3)
           MOVE 'T3'          TO WS-TBL-TIER  (3)
           .

       2000-CHECK-PARM.
           IF LK-SL-REV-BAL NOT NUMERIC
              MOVE '91' TO LK-SL-RET
              MOVE 'リボ残高数値不正' TO WS-ERR-MSG
              DISPLAY 'CB290S パラメータ不正 '
                      WS-ERR-MSG
              MOVE 8 TO RETURN-CODE
           ELSE
              IF LK-SL-REV-BAL < ZERO
                 MOVE '92' TO LK-SL-RET
                 MOVE 'リボ残高負値不正' TO WS-ERR-MSG
                 DISPLAY 'CB290S パラメータ不正 '
                         WS-ERR-MSG
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
           .

       3000-CALC-PRINCIPAL.
           MOVE LK-SL-REV-BAL TO WS-ABS-BAL

           SET WS-SL-IDX TO 1
           SEARCH WS-TBL-ENTRY
              AT END
                 MOVE '93' TO LK-SL-RET
                 MOVE '残高スライド表該当なし' TO WS-ERR-MSG
                 DISPLAY 'CB290S 算定異常 '
                         WS-ERR-MSG
                 MOVE 8 TO RETURN-CODE
              WHEN WS-ABS-BAL <= WS-TBL-UPPER (WS-SL-IDX)
                 MOVE WS-TBL-PRIN (WS-SL-IDX) TO LK-SL-PRIN-AMT
                 MOVE WS-TBL-TIER (WS-SL-IDX) TO LK-SL-TIER
           END-SEARCH
           .

       9000-END.
           IF LK-SL-RET = '00'
              MOVE 0 TO RETURN-CODE
           END-IF
           .
