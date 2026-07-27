       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ125S.
       AUTHOR. BATCH-SYSTEM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDGRPF
               ASSIGN TO "KZDGRPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DG-GROUP-CODE
               FILE STATUS IS WS-DGRP-STAT.

       DATA DIVISION.
       FILE SECTION.

       FD  KZDGRPF.
       COPY KZDGRPC.

       WORKING-STORAGE SECTION.

       01  WS-DGRP-STAT              PIC XX.
           88  DGRP-OK               VALUE "00".
           88  DGRP-NOT-FOUND        VALUE "23".

       01  WS-WORK-AREA.
           05  WS-OPENED-SW          PIC X VALUE "N".
               88  WS-OPENED         VALUE "Y".
               88  WS-CLOSED         VALUE "N".
           05  WS-EFFECTIVE-GRP      PIC X(04).
               88  GRP-STANDARD      VALUE "STD0".
               88  GRP-GOLD          VALUE "GLD1".
               88  GRP-PLATINUM      VALUE "PLT2".
               88  GRP-EXEMPT        VALUE "EXMP".
           05  WS-RATE               PIC S9(01)V9(04) COMP-3
                                      VALUE ZERO.
           05  WS-CALC-FEE           PIC S9(13)V99 COMP-3
                                      VALUE ZERO.
           05  WS-USE-FALLBACK-SW    PIC X VALUE "N".
               88  WS-USE-FALLBACK   VALUE "Y".

       01  WS-CONSTANTS.
           05  WS-RATE-STANDARD      PIC S9(01)V9(04) COMP-3
                                      VALUE 0.0150.
           05  WS-RATE-ZERO          PIC S9(01)V9(04) COMP-3
                                      VALUE 0.0000.
           05  RC-NORMAL             PIC S9(04) COMP VALUE 0.
           05  RC-PARM-ERROR         PIC S9(04) COMP VALUE 10.
           05  RC-FILE-ERROR         PIC S9(04) COMP VALUE 20.
           05  RC-NOT-FOUND          PIC S9(04) COMP VALUE 23.

       COPY LK-RND-PARM.

       LINKAGE SECTION.
       COPY LK-RATE-PARM.

       PROCEDURE DIVISION USING LK-RATE-PARM.

       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-PARM

           IF LK-RET-CD = RC-NORMAL
               PERFORM 3000-OPEN-FILE
           END-IF

           IF LK-RET-CD = RC-NORMAL
               PERFORM 4000-LOOKUP-RATE
           END-IF

           IF LK-RET-CD = RC-NORMAL
               PERFORM 5000-CALCULATE-FEE
           END-IF

           PERFORM 9000-CLOSE-FILE
           GOBACK
           .

       1000-INITIALIZE SECTION.
       1000-START.
           MOVE RC-NORMAL TO LK-RET-CD
           MOVE ZERO      TO LK-FEE-AMT
           MOVE ZERO      TO LK-AMT-RAW
           MOVE ZERO      TO LK-AMT-FLOORED
           MOVE ZERO      TO WS-RATE
           MOVE ZERO      TO WS-CALC-FEE
           MOVE SPACES    TO WS-EFFECTIVE-GRP
           MOVE "N"       TO WS-USE-FALLBACK-SW
           SET WS-CLOSED  TO TRUE
           .

       2000-VALIDATE-PARM SECTION.
       2000-START.
           EVALUATE LK-GRP-CODE
               WHEN "STD0"
                   MOVE "STD0" TO WS-EFFECTIVE-GRP
               WHEN "GLD1"
                   MOVE "GLD1" TO WS-EFFECTIVE-GRP
               WHEN "PLT2"
                   MOVE "PLT2" TO WS-EFFECTIVE-GRP
               WHEN "EXMP"
                   MOVE "EXMP" TO WS-EFFECTIVE-GRP
               WHEN "PREM"
                   MOVE "STD0" TO WS-EFFECTIVE-GRP
               WHEN OTHER
                   MOVE RC-PARM-ERROR TO LK-RET-CD
           END-EVALUATE

           IF LK-RET-CD = RC-NORMAL
              AND LK-OVER-AMT < ZERO
               MOVE RC-PARM-ERROR TO LK-RET-CD
           END-IF
           .

       3000-OPEN-FILE SECTION.
       3000-START.
           OPEN INPUT KZDGRPF

           IF DGRP-OK
               SET WS-OPENED TO TRUE
           ELSE
               MOVE RC-FILE-ERROR TO LK-RET-CD
           END-IF
           .

       4000-LOOKUP-RATE SECTION.
       4000-START.
           MOVE WS-EFFECTIVE-GRP TO DG-GROUP-CODE

           READ KZDGRPF KEY IS DG-GROUP-CODE
               INVALID KEY
                   IF DGRP-NOT-FOUND
                      AND GRP-STANDARD
                       SET WS-USE-FALLBACK TO TRUE
                   ELSE
                       IF DGRP-NOT-FOUND
                           MOVE RC-NOT-FOUND TO LK-RET-CD
                       ELSE
                           MOVE RC-FILE-ERROR TO LK-RET-CD
                       END-IF
                   END-IF
               NOT INVALID KEY
                   MOVE DG-OL-FEE-RATE TO WS-RATE
           END-READ

           IF LK-RET-CD = RC-NORMAL
               IF WS-USE-FALLBACK
                   MOVE WS-RATE-STANDARD TO WS-RATE
               ELSE
                   IF DG-EXEMPT-FLAG = "Y"
                      OR DG-EXEMPT-FLAG = "1"
                      OR WS-RATE = WS-RATE-ZERO
                       MOVE WS-RATE-ZERO TO WS-RATE
                   END-IF
               END-IF
           END-IF
           .

       5000-CALCULATE-FEE SECTION.
       5000-START.
           IF WS-RATE = WS-RATE-ZERO
               MOVE ZERO TO LK-FEE-AMT
           ELSE
               COMPUTE WS-CALC-FEE = LK-OVER-AMT * WS-RATE
               MOVE WS-CALC-FEE TO LK-AMT-RAW
               CALL "KZ130S" USING LK-RND-PARM
               MOVE LK-AMT-FLOORED TO LK-FEE-AMT
           END-IF
           .

       9000-CLOSE-FILE SECTION.
       9000-START.
           IF WS-OPENED
               CLOSE KZDGRPF
               SET WS-CLOSED TO TRUE
           END-IF
           .

       END PROGRAM KZ125S.
