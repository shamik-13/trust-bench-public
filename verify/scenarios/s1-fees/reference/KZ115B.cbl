       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ115B.
       AUTHOR. BENCHMARK-DATA.
       DATE-WRITTEN. 2026-06-17.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       LINKAGE SECTION.
           COPY LK-CAP-PARM.

       PROCEDURE DIVISION USING LK-CAP-PARM.
       0000-MAIN.
           PERFORM 1000-VALIDATE-PARM
           IF LK-CAP-FLAG NOT = 'E'
               PERFORM 2000-APPLY-CAP
           END-IF
           GOBACK
           .

       1000-VALIDATE-PARM.
           IF LK-FEE-YTD < ZERO
               MOVE ZERO TO LK-FEE-CAPPED
               MOVE 'E'  TO LK-CAP-FLAG
           END-IF
           .

       2000-APPLY-CAP.
           IF LK-FEE-YTD > 50000.00
               MOVE 50000.00   TO LK-FEE-CAPPED
               MOVE 'Y'        TO LK-CAP-FLAG
           ELSE
               MOVE LK-FEE-YTD TO LK-FEE-CAPPED
               MOVE 'N'        TO LK-CAP-FLAG
           END-IF
           .
