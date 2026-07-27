       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR220S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05 WS-NEED-AMT             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-DEFICIT-AMT          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-RATIO                PIC S9(07)V99 COMP-3 VALUE ZERO.
           05 WS-ABS-NEED             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-ABS-DEFICIT          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-ERR-FLG              PIC X VALUE SPACE.
              88 WS-HARD-ERR               VALUE '1'.
       01  WS-MSG.
           05 WS-DSP-ST               PIC X(02) VALUE SPACE.
           05 WS-DSP-ORG              PIC X(06) VALUE SPACE.

       LINKAGE SECTION.
       01  LK-CR220S-PARM.
           05 LK-ORG-CD               PIC X(06).
           05 LK-AVAILABLE-AMT        PIC S9(13)V99 COMP-3.
           05 LK-RESERVED-AMT         PIC S9(13)V99 COMP-3.
           05 LK-TODAY-SETTLE-AMT     PIC S9(13)V99 COMP-3.
           05 LK-TRANSFER-PLAN-AMT    PIC S9(13)V99 COMP-3.
           05 LK-WARN-THRESHOLD-AMT   PIC S9(13)V99 COMP-3.
           05 LK-CRIT-THRESHOLD-AMT   PIC S9(13)V99 COMP-3.
           05 LK-SHORTAGE-AMT         PIC S9(13)V99 COMP-3.
           05 LK-SHORTAGE-RATIO       PIC S9(05)V99 COMP-3.
           05 LK-WARNING-KBN          PIC X.
              88 LK-WARN-NONE              VALUE '0'.
              88 LK-WARN-INFO              VALUE '1'.
              88 LK-WARN-CAUTION           VALUE '2'.
              88 LK-WARN-CRITICAL          VALUE '3'.
           05 LK-REASON-CD            PIC X(04).
           05 LK-STATUS-CD            PIC X(02).

       PROCEDURE DIVISION USING LK-CR220S-PARM.
       MAIN-RTN.
           PERFORM 010-INIT
           PERFORM 020-CHECK-PARM
           IF WS-HARD-ERR
              PERFORM 900-ABEND
           ELSE
              PERFORM 100-CALC-LIQUIDITY
              PERFORM 200-SET-WARNING
              MOVE 0 TO RETURN-CODE
              GOBACK
           END-IF
           .

       010-INIT.
           MOVE ZERO  TO WS-NEED-AMT
           MOVE ZERO  TO WS-DEFICIT-AMT
           MOVE ZERO  TO WS-RATIO
           MOVE ZERO  TO WS-ABS-NEED
           MOVE ZERO  TO WS-ABS-DEFICIT
           MOVE SPACE TO WS-ERR-FLG
           MOVE ZERO  TO LK-SHORTAGE-AMT
           MOVE ZERO  TO LK-SHORTAGE-RATIO
           MOVE '0'   TO LK-WARNING-KBN
           MOVE '0000' TO LK-REASON-CD
           MOVE '00'  TO LK-STATUS-CD
           .

       020-CHECK-PARM.
           IF LK-ORG-CD = SPACE
              MOVE '1' TO WS-ERR-FLG
              MOVE 'E1' TO LK-STATUS-CD
              MOVE '9001' TO LK-REASON-CD
              DISPLAY 'CR220S 組織コード未設定'
           END-IF

           IF LK-WARN-THRESHOLD-AMT < ZERO
              MOVE '1' TO WS-ERR-FLG
              MOVE 'E2' TO LK-STATUS-CD
              MOVE '9002' TO LK-REASON-CD
              DISPLAY 'CR220S 警告しきい値不正'
           END-IF

           IF LK-CRIT-THRESHOLD-AMT < ZERO
              MOVE '1' TO WS-ERR-FLG
              MOVE 'E3' TO LK-STATUS-CD
              MOVE '9003' TO LK-REASON-CD
              DISPLAY 'CR220S 重大しきい値不正'
           END-IF

           IF LK-CRIT-THRESHOLD-AMT > LK-WARN-THRESHOLD-AMT
              MOVE '1' TO WS-ERR-FLG
              MOVE 'E4' TO LK-STATUS-CD
              MOVE '9004' TO LK-REASON-CD
              DISPLAY 'CR220S しきい値大小不正'
           END-IF
           .

       100-CALC-LIQUIDITY.
           COMPUTE WS-NEED-AMT =
                   LK-RESERVED-AMT
                 + LK-TODAY-SETTLE-AMT
                 + LK-TRANSFER-PLAN-AMT

           COMPUTE WS-DEFICIT-AMT =
                   WS-NEED-AMT - LK-AVAILABLE-AMT

           IF WS-DEFICIT-AMT > ZERO
              MOVE WS-DEFICIT-AMT TO LK-SHORTAGE-AMT
           ELSE
              MOVE ZERO TO LK-SHORTAGE-AMT
           END-IF

           IF WS-NEED-AMT < ZERO
              COMPUTE WS-ABS-NEED = WS-NEED-AMT * -1
           ELSE
              MOVE WS-NEED-AMT TO WS-ABS-NEED
           END-IF

           IF LK-SHORTAGE-AMT > ZERO AND WS-ABS-NEED > ZERO
              COMPUTE WS-RATIO ROUNDED =
                      (LK-SHORTAGE-AMT * 100) / WS-ABS-NEED
              MOVE WS-RATIO TO LK-SHORTAGE-RATIO
           ELSE
              MOVE ZERO TO LK-SHORTAGE-RATIO
           END-IF
           .

       200-SET-WARNING.
           EVALUATE TRUE
              WHEN LK-SHORTAGE-AMT <= ZERO
                 MOVE '0' TO LK-WARNING-KBN
                 MOVE '0000' TO LK-REASON-CD
              WHEN LK-SHORTAGE-AMT >= LK-WARN-THRESHOLD-AMT
                 MOVE '3' TO LK-WARNING-KBN
                 MOVE '2100' TO LK-REASON-CD
              WHEN LK-SHORTAGE-AMT >= LK-CRIT-THRESHOLD-AMT
                 MOVE '2' TO LK-WARNING-KBN
                 MOVE '1200' TO LK-REASON-CD
              WHEN OTHER
                 MOVE '1' TO LK-WARNING-KBN
                 MOVE '1100' TO LK-REASON-CD
           END-EVALUATE

           IF LK-WARNING-KBN NOT = '0'
              MOVE LK-STATUS-CD TO WS-DSP-ST
              MOVE LK-ORG-CD    TO WS-DSP-ORG
              DISPLAY 'CR220S 流動性警告 ORG=' WS-DSP-ORG
                      ' 区分=' LK-WARNING-KBN
                      ' 理由=' LK-REASON-CD
           END-IF
           .

       900-ABEND.
           MOVE 8 TO RETURN-CODE
           DISPLAY 'CR220S 異常終了 ST=' LK-STATUS-CD
                   ' 理由=' LK-REASON-CD
           GOBACK
           .
