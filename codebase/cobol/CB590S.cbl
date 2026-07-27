       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB590S.
      ******************************************************************
      * 海外利用事務手数料算定サブルーチン
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-PGM-ID              PIC X(08) VALUE 'CB590S'.
           05  WS-CURRENCY            PIC X(03) VALUE SPACE.
           05  WS-FEE-CALC            PIC S9(15)V9(04) COMP-3
                                      VALUE ZERO.
           05  WS-FEE-YEN             PIC S9(15) COMP-3
                                      VALUE ZERO.
           05  WS-OLD-FEE-RATE        PIC 9V9(04) COMP-3
                                      VALUE 0.0160.
           05  WS-MSG-SALE            PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.

       LINKAGE SECTION.
           COPY LK-FXFEE-PARM.

       PROCEDURE DIVISION USING LK-FXFEE-PARM.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-PARM
           IF LK-FX-RET = '00'
              PERFORM 3000-CALC-FEE
           END-IF
           PERFORM 9000-END
           GOBACK
           .

       1000-INIT SECTION.
       1000-START.
           MOVE 0                  TO RETURN-CODE
           MOVE '00'               TO LK-FX-RET
           MOVE ZERO               TO LK-FX-FEE-AMT
           MOVE FUNCTION UPPER-CASE(LK-FX-CURRENCY)
                                      TO WS-CURRENCY
           .

       2000-CHECK-PARM SECTION.
       2000-START.
           IF WS-CURRENCY = SPACE
              MOVE '91'            TO LK-FX-RET
              MOVE 8               TO RETURN-CODE
              DISPLAY WS-PGM-ID
                      ' 通貨コード未設定'
           END-IF

           IF LK-FX-RET = '00'
              IF LK-FX-SALE-AMT < ZERO
                 MOVE '92'         TO LK-FX-RET
                 MOVE 8            TO RETURN-CODE
                 MOVE LK-FX-SALE-AMT
                                      TO WS-MSG-SALE
                 DISPLAY WS-PGM-ID
                         ' 売上金額不正 AMT='
                         WS-MSG-SALE
              END-IF
           END-IF
           .

       3000-CALC-FEE SECTION.
       3000-START.
           IF WS-CURRENCY = 'JPY'
              MOVE ZERO            TO LK-FX-FEE-AMT
           ELSE
              COMPUTE WS-FEE-CALC =
                      LK-FX-SALE-AMT * WS-OLD-FEE-RATE
              COMPUTE WS-FEE-YEN =
                      FUNCTION INTEGER(WS-FEE-CALC)
              MOVE WS-FEE-YEN      TO LK-FX-FEE-AMT
           END-IF
           MOVE '00'               TO LK-FX-RET
           .

       9000-END SECTION.
       9000-START.
           IF LK-FX-RET = '00'
              MOVE 0               TO RETURN-CODE
           END-IF
           .
