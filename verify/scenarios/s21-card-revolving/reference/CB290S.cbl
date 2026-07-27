       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB290S.
       AUTHOR. 大原 修.
      *
      * 残高スライド元金定額算定サブルーチン。
      * 改定後の残高スライド表により元金定額と階層を返却する。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONST.
           05  WS-RET-OK              PIC X(02) VALUE '00'.
           05  WS-RET-DATA-ERR        PIC X(02) VALUE '90'.
           05  WS-TIER-1              PIC X(02) VALUE 'T1'.
           05  WS-TIER-2              PIC X(02) VALUE 'T2'.
           05  WS-TIER-3              PIC X(02) VALUE 'T3'.
           05  WS-TIER-4              PIC X(02) VALUE 'T4'.
           05  WS-BAL-LIMIT-1         PIC 9(11) VALUE 100000.
           05  WS-BAL-LIMIT-2         PIC 9(11) VALUE 300000.
           05  WS-BAL-LIMIT-3         PIC 9(11) VALUE 500000.
           05  WS-PRIN-1              PIC 9(09) VALUE 5000.
           05  WS-PRIN-2              PIC 9(09) VALUE 10000.
           05  WS-PRIN-3              PIC 9(09) VALUE 15000.
           05  WS-PRIN-4              PIC 9(09) VALUE 20000.
      *
       LINKAGE SECTION.
           COPY LK-SLIDE-PARM.
      *
       PROCEDURE DIVISION USING LK-SLIDE-PARM.
      *
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-VALIDATE
           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF
      *
           PERFORM 2000-SET-PRINCIPAL
      *
           MOVE WS-RET-OK TO LK-SL-RET
           MOVE 0 TO RETURN-CODE
           GOBACK
           .
      *
       1000-VALIDATE SECTION.
       1000-START.
           IF LK-SL-REV-BAL < ZERO
              MOVE ZERO TO LK-SL-PRIN-AMT
              MOVE SPACES TO LK-SL-TIER
              MOVE WS-RET-DATA-ERR TO LK-SL-RET
              DISPLAY '残高スライド算定 入力残高不正'
              MOVE 8 TO RETURN-CODE
           END-IF
           .
      *
       2000-SET-PRINCIPAL SECTION.
       2000-START.
           EVALUATE TRUE
              WHEN LK-SL-REV-BAL <= WS-BAL-LIMIT-1
                 MOVE WS-PRIN-1 TO LK-SL-PRIN-AMT
                 MOVE WS-TIER-1 TO LK-SL-TIER
              WHEN LK-SL-REV-BAL <= WS-BAL-LIMIT-2
                 MOVE WS-PRIN-2 TO LK-SL-PRIN-AMT
                 MOVE WS-TIER-2 TO LK-SL-TIER
              WHEN LK-SL-REV-BAL <= WS-BAL-LIMIT-3
                 MOVE WS-PRIN-3 TO LK-SL-PRIN-AMT
                 MOVE WS-TIER-3 TO LK-SL-TIER
              WHEN OTHER
                 MOVE WS-PRIN-4 TO LK-SL-PRIN-AMT
                 MOVE WS-TIER-4 TO LK-SL-TIER
           END-EVALUATE
           .
