       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG935S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NET-WORK                 PIC S9(14) COMP-3.
       01  WS-ABEND-SW                 PIC X(01) VALUE SPACE.
           88  WS-ABEND                VALUE '1'.
           88  WS-NORMAL               VALUE '0'.

       LINKAGE SECTION.
       COPY LK-NET-PARM.

       PROCEDURE DIVISION USING LK-NET-PARM.
       0000-MAIN SECTION.
       0000-MAIN-START.
           SET WS-NORMAL TO TRUE
           MOVE SPACE TO LK-NET-RET
           MOVE ZERO  TO WS-NET-WORK

           COMPUTE WS-NET-WORK =
                   LK-NET-RECV-AMT - LK-NET-PAY-AMT
               ON SIZE ERROR
                   SET WS-ABEND TO TRUE
           END-COMPUTE

           IF WS-NORMAL
               COMPUTE LK-NET-AMT = WS-NET-WORK
                   ON SIZE ERROR
                       SET WS-ABEND TO TRUE
               END-COMPUTE
           END-IF

           IF WS-ABEND
               MOVE '90' TO LK-NET-RET
               DISPLAY 'ＴＧ９３５Ｓ 交換尻算定桁あふれ'
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE '00' TO LK-NET-RET
               MOVE 0 TO RETURN-CODE
           END-IF

           GOBACK.
