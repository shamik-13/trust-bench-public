       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ125S.
      ******************************************************************
      * 変更履歴                                                       *
      * 版数  年月日      担当                         概要            *
      * 1.00  平成30年04月 システム部 勘定系チーム      新規作成        *
      * 1.01  令和02年10月 システム部 勘定系チーム      料率取得見直し  *
      * 1.02  令和05年06月 システム部 勘定系チーム      適用確認        *
      ******************************************************************
       AUTHOR. BATCH-SYSTEM.
      ******************************************************************
      * RATE ASSIGNMENT SUBPROGRAM                                    *
      ******************************************************************

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
               FILE STATUS IS WS-DGRP-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  KZDGRPF.
           COPY KZDGRPC.

       WORKING-STORAGE SECTION.

       01  WS-DGRP-STATUS              PIC XX.
           88  DGRP-OK                 VALUE "00".
           88  DGRP-NOT-FOUND          VALUE "23".

       01  WS-NORMAL-GRP-CODE          PIC X(04).
       01  WS-EFFECTIVE-RATE           PIC 9V9999.
       01  WS-CALC-RATE                PIC 9V9999.
       01  WS-WORK-FEE                 PIC S9(13)V9(06) COMP-3.
       01  WS-OVER-AMT                 PIC S9(13)V99 COMP-3.
       01  WS-FILE-OPEN-SW             PIC X VALUE "N".
           88  FILE-OPENED             VALUE "Y".
           88  FILE-CLOSED             VALUE "N".

       01  WS-STD-GRP-CODE             PIC X(04) VALUE "STD0".
       01  WS-STANDARD-RATE            PIC 9V9999 VALUE 0.0150.

       01  WS-RET-NORMAL               PIC X(02) VALUE "00".
       01  WS-RET-INPUT-ERR            PIC X(02) VALUE "10".
       01  WS-RET-FILE-ERR             PIC X(02) VALUE "20".

           COPY LK-RND-PARM.

       LINKAGE SECTION.
           COPY LK-RATE-PARM.

       PROCEDURE DIVISION USING LK-RATE-PARM.

       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-INPUT

           IF LK-RET-CD = WS-RET-NORMAL
               PERFORM 3000-GET-RATE
           END-IF

           IF LK-RET-CD = WS-RET-NORMAL
               PERFORM 4000-CALCULATE-FEE
           END-IF

           PERFORM 9000-FINALIZE
           GOBACK
           .

       1000-INITIALIZE SECTION.
       1000-START.
           MOVE WS-RET-NORMAL TO LK-RET-CD
           MOVE ZERO          TO LK-FEE-AMT
           MOVE ZERO          TO WS-EFFECTIVE-RATE
           MOVE ZERO          TO WS-CALC-RATE
           MOVE ZERO          TO WS-WORK-FEE
           MOVE LK-OVER-AMT   TO WS-OVER-AMT
           SET FILE-CLOSED    TO TRUE
           .

       2000-VALIDATE-INPUT SECTION.
       2000-START.
           EVALUATE LK-GRP-CODE
               WHEN "STD0"
               WHEN "GLD1"
               WHEN "PLT2"
               WHEN "EXMP"
                   MOVE LK-GRP-CODE TO WS-NORMAL-GRP-CODE
               WHEN "PREM"
                   MOVE LK-GRP-CODE TO WS-NORMAL-GRP-CODE
               WHEN OTHER
                   MOVE WS-RET-INPUT-ERR TO LK-RET-CD
           END-EVALUATE

           IF LK-RET-CD = WS-RET-NORMAL
              AND WS-OVER-AMT < ZERO
               MOVE WS-RET-INPUT-ERR TO LK-RET-CD
           END-IF
           .

       3000-GET-RATE SECTION.
       3000-START.
           OPEN INPUT KZDGRPF

           IF DGRP-OK
               SET FILE-OPENED TO TRUE
               MOVE WS-NORMAL-GRP-CODE TO DG-GROUP-CODE
               READ KZDGRPF KEY IS DG-GROUP-CODE
                   INVALID KEY
                       PERFORM 3100-HANDLE-MISSING-RATE
                   NOT INVALID KEY
                       PERFORM 3200-APPLY-FOUND-RATE
               END-READ
           ELSE
               MOVE WS-RET-FILE-ERR TO LK-RET-CD
           END-IF
           .

       3100-HANDLE-MISSING-RATE SECTION.
       3100-START.
           IF DGRP-NOT-FOUND
              AND WS-NORMAL-GRP-CODE = WS-STD-GRP-CODE
               MOVE WS-STANDARD-RATE TO WS-EFFECTIVE-RATE
           ELSE
               MOVE WS-RET-FILE-ERR TO LK-RET-CD
           END-IF
           .

       3200-APPLY-FOUND-RATE SECTION.
       3200-START.
           IF DG-EXEMPT-FLAG = "1"
               MOVE ZERO TO WS-EFFECTIVE-RATE
           ELSE
               MOVE DG-OL-FEE-RATE-OLD TO WS-EFFECTIVE-RATE
           END-IF
           .

       4000-CALCULATE-FEE SECTION.
       4000-START.
           IF WS-EFFECTIVE-RATE = ZERO
               MOVE ZERO TO LK-FEE-AMT
           ELSE
               MOVE WS-EFFECTIVE-RATE TO WS-CALC-RATE
               COMPUTE WS-WORK-FEE ROUNDED =
                   WS-OVER-AMT * WS-CALC-RATE
               MOVE WS-WORK-FEE TO LK-AMT-RAW
               CALL "KZ130S" USING LK-RND-PARM
               MOVE LK-AMT-FLOORED TO LK-FEE-AMT
           END-IF
           .

       9000-FINALIZE SECTION.
       9000-START.
           IF FILE-OPENED
               CLOSE KZDGRPF
               SET FILE-CLOSED TO TRUE
           END-IF
           .

       END PROGRAM KZ125S.
