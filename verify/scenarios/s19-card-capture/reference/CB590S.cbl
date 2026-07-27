       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB590S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONST.
           05  WS-BASE-CURRENCY        PIC X(03) VALUE 'JPY'.
           05  WS-NORMAL-RET           PIC X(02) VALUE '00'.
           05  WS-PARM-ERR-RET         PIC X(02) VALUE '12'.
           05  WS-FX-FEE-RATE          PIC 9V9999 VALUE 0.0220.
       01  WS-CALC.
           05  WS-FEE-WORK             PIC S9(15)V9999 COMP-3
                                       VALUE ZERO.
           05  WS-FEE-YEN              PIC S9(15) COMP-3
                                       VALUE ZERO.
       01  WS-MSG.
           05  WS-DISP-ST              PIC X(02) VALUE SPACE.

       LINKAGE SECTION.
           COPY LK-FXFEE-PARM.

       PROCEDURE DIVISION USING LK-FXFEE-PARM.

       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-PARM
           IF RETURN-CODE = 0
              PERFORM 3000-CALC-FEE
           END-IF
           GOBACK
           .

       1000-INIT SECTION.
       1000-START.
           MOVE ZERO TO LK-FX-FEE-AMT
           MOVE SPACE TO LK-FX-RET
           MOVE ZERO TO WS-FEE-WORK
           MOVE ZERO TO WS-FEE-YEN
           .

       2000-CHECK-PARM SECTION.
       2000-START.
           IF LK-FX-CURRENCY = SPACE
              MOVE WS-PARM-ERR-RET TO LK-FX-RET
              MOVE 8 TO RETURN-CODE
              MOVE WS-PARM-ERR-RET TO WS-DISP-ST
              DISPLAY 'FXFEE CURRENCY NOT SET ST=' WS-DISP-ST
           END-IF

           IF RETURN-CODE = 0
              IF LK-FX-SALE-AMT < ZERO
                 MOVE WS-PARM-ERR-RET TO LK-FX-RET
                 MOVE 8 TO RETURN-CODE
                 MOVE WS-PARM-ERR-RET TO WS-DISP-ST
                 DISPLAY 'FXFEE SALE AMOUNT ERROR ST=' WS-DISP-ST
              END-IF
           END-IF
           .

       3000-CALC-FEE SECTION.
       3000-START.
           IF LK-FX-CURRENCY = WS-BASE-CURRENCY
              MOVE ZERO TO LK-FX-FEE-AMT
           ELSE
              COMPUTE WS-FEE-WORK =
                      LK-FX-SALE-AMT * WS-FX-FEE-RATE
              COMPUTE WS-FEE-YEN =
                      FUNCTION INTEGER-PART(WS-FEE-WORK)
              MOVE WS-FEE-YEN TO LK-FX-FEE-AMT
           END-IF

           MOVE WS-NORMAL-RET TO LK-FX-RET
           MOVE 0 TO RETURN-CODE
           .
