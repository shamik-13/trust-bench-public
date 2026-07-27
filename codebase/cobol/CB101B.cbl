       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB101B.
      *================================================================*
      * 為替レート取込                                                 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDRTEXF
               ASSIGN       TO "CDRTEXF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS FX-CURRENCY-CD
               FILE STATUS  IS WS-CDRTEXF-ST.

           SELECT CDEXCPF
               ASSIGN       TO "CDEXCPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS WS-CDEXCPF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDRTEXF.
           COPY CDRTEXC.

       FD  CDEXCPF.
           COPY CDEXCPC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CDRTEXF-ST          PIC XX VALUE SPACES.
           05 WS-CDEXCPF-ST          PIC XX VALUE SPACES.

       01  WS-CONTROL.
           05 WS-ABEND-SW            PIC X VALUE 'N'.
              88 WS-ABEND                 VALUE 'Y'.
           05 WS-END-SW              PIC X VALUE 'N'.
              88 WS-END                   VALUE 'Y'.
           05 WS-REC-IDX             PIC 9(03) COMP VALUE ZERO.
           05 WS-REQ-IDX             PIC 9(03) COMP VALUE ZERO.
           05 WS-SEEN-IDX            PIC 9(03) COMP VALUE ZERO.
           05 WS-PRIOR-IDX           PIC 9(03) COMP VALUE ZERO.
           05 WS-SEEN-CNT            PIC 9(03) COMP VALUE ZERO.
           05 WS-EXCEPTION-SEQ       PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-PROCESS-DT          PIC 9(08) VALUE 20241008.
           05 WS-APPLY-STATUS        PIC X VALUE SPACE.
           05 WS-DUP-SW              PIC X VALUE 'N'.
              88 WS-DUPLICATE             VALUE 'Y'.
           05 WS-FOUND-SW            PIC X VALUE 'N'.
              88 WS-FOUND                 VALUE 'Y'.
           05 WS-VALID-SW            PIC X VALUE 'Y'.
              88 WS-VALID                 VALUE 'Y'.
              88 WS-INVALID               VALUE 'N'.
           05 WS-CHANGE-RATE         PIC S9(03)V9(06) COMP-3
                                      VALUE ZERO.
           05 WS-ABS-CHANGE          PIC 9(03)V9(06) COMP-3
                                      VALUE ZERO.
           05 WS-PRIOR-RATE          PIC 9(09)V9(06) COMP-3
                                      VALUE ZERO.

       01  WS-COUNTERS.
           05 WS-IN-CNT              PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-OK-CNT              PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-NG-CNT              PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-EX-CNT              PIC 9(07) COMP-3 VALUE ZERO.

       01  WS-REQUIRED-CURRENCY.
           05 WS-REQ-CUR OCCURS 8 TIMES PIC X(03) VALUE SPACES.

       01  WS-SEEN-TABLE.
           05 WS-SEEN-CUR OCCURS 32 TIMES PIC X(03) VALUE SPACES.

       01  WS-PRIOR-TABLE.
           05 WS-PRIOR-ROW OCCURS 8 TIMES.
              10 WS-PRIOR-CUR        PIC X(03) VALUE SPACES.
              10 WS-PRIOR-TTM        PIC 9(09)V9(06) VALUE ZERO.

       01  WS-RECEIVED-TABLE.
           05 WS-RECEIVED-ROW OCCURS 10 TIMES.
              10 WS-R-CUR            PIC X(03) VALUE SPACES.
              10 WS-R-DT             PIC 9(08) VALUE ZERO.
              10 WS-R-TTM            PIC 9(09)V9(06) VALUE ZERO.
              10 WS-R-SRC            PIC X(08) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INITIALIZE
           PERFORM 1000-OPEN-FILES
           IF NOT WS-ABEND
               PERFORM 2000-PROCESS-RECEIVED
               PERFORM 3000-CHECK-MISSING
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-ABEND
               MOVE 12 TO RETURN-CODE
           ELSE
               DISPLAY 'CB101B 正常終了 入力=' WS-IN-CNT
               DISPLAY '正常=' WS-OK-CNT
               DISPLAY '例外=' WS-EX-CNT
           END-IF
           GOBACK.

       0100-INITIALIZE.
           MOVE 'USD' TO WS-REQ-CUR(1)
           MOVE 'EUR' TO WS-REQ-CUR(2)
           MOVE 'GBP' TO WS-REQ-CUR(3)
           MOVE 'AUD' TO WS-REQ-CUR(4)
           MOVE 'CAD' TO WS-REQ-CUR(5)
           MOVE 'CHF' TO WS-REQ-CUR(6)
           MOVE 'HKD' TO WS-REQ-CUR(7)
           MOVE 'SGD' TO WS-REQ-CUR(8)

           MOVE 'USD'      TO WS-PRIOR-CUR(1)
           MOVE 149.250000 TO WS-PRIOR-TTM(1)
           MOVE 'EUR'      TO WS-PRIOR-CUR(2)
           MOVE 161.340000 TO WS-PRIOR-TTM(2)
           MOVE 'GBP'      TO WS-PRIOR-CUR(3)
           MOVE 190.870000 TO WS-PRIOR-TTM(3)
           MOVE 'AUD'      TO WS-PRIOR-CUR(4)
           MOVE 098.760000 TO WS-PRIOR-TTM(4)
           MOVE 'CAD'      TO WS-PRIOR-CUR(5)
           MOVE 110.420000 TO WS-PRIOR-TTM(5)
           MOVE 'CHF'      TO WS-PRIOR-CUR(6)
           MOVE 171.550000 TO WS-PRIOR-TTM(6)
           MOVE 'HKD'      TO WS-PRIOR-CUR(7)
           MOVE 019.120000 TO WS-PRIOR-TTM(7)
           MOVE 'SGD'      TO WS-PRIOR-CUR(8)
           MOVE 109.880000 TO WS-PRIOR-TTM(8)

           MOVE 'USD'      TO WS-R-CUR(1)
           MOVE 20241008   TO WS-R-DT(1)
           MOVE 149.610000 TO WS-R-TTM(1)
           MOVE 'JPTS    ' TO WS-R-SRC(1)
           MOVE 'EUR'      TO WS-R-CUR(2)
           MOVE 20241008   TO WS-R-DT(2)
           MOVE 162.010000 TO WS-R-TTM(2)
           MOVE 'JPTS    ' TO WS-R-SRC(2)
           MOVE 'GBP'      TO WS-R-CUR(3)
           MOVE 20241007   TO WS-R-DT(3)
           MOVE 191.120000 TO WS-R-TTM(3)
           MOVE 'JPTS    ' TO WS-R-SRC(3)
           MOVE 'AUD'      TO WS-R-CUR(4)
           MOVE 20241008   TO WS-R-DT(4)
           MOVE 000.000000 TO WS-R-TTM(4)
           MOVE 'JPTS    ' TO WS-R-SRC(4)
           MOVE 'CAD'      TO WS-R-CUR(5)
           MOVE 20241008   TO WS-R-DT(5)
           MOVE 116.180000 TO WS-R-TTM(5)
           MOVE 'JPTS    ' TO WS-R-SRC(5)
           MOVE 'CHF'      TO WS-R-CUR(6)
           MOVE 20241008   TO WS-R-DT(6)
           MOVE 172.010000 TO WS-R-TTM(6)
           MOVE 'JPTS    ' TO WS-R-SRC(6)
           MOVE 'HKD'      TO WS-R-CUR(7)
           MOVE 20241008   TO WS-R-DT(7)
           MOVE 019.160000 TO WS-R-TTM(7)
           MOVE 'JPTS    ' TO WS-R-SRC(7)
           MOVE 'USD'      TO WS-R-CUR(8)
           MOVE 20241008   TO WS-R-DT(8)
           MOVE 149.620000 TO WS-R-TTM(8)
           MOVE 'JPTS    ' TO WS-R-SRC(8)
           MOVE 'NZD'      TO WS-R-CUR(9)
           MOVE 20241008   TO WS-R-DT(9)
           MOVE 091.330000 TO WS-R-TTM(9)
           MOVE 'JPTS    ' TO WS-R-SRC(9)
           MOVE 'JPY'      TO WS-R-CUR(10)
           MOVE 20241008   TO WS-R-DT(10)
           MOVE 001.000000 TO WS-R-TTM(10)
           MOVE 'JPTS    ' TO WS-R-SRC(10).

       1000-OPEN-FILES.
           OPEN OUTPUT CDRTEXF
           IF WS-CDRTEXF-ST NOT = '00'
               DISPLAY 'CDRTEXF オープン失敗 ST='
                       WS-CDRTEXF-ST
               SET WS-ABEND TO TRUE
           END-IF
           IF NOT WS-ABEND
               OPEN OUTPUT CDEXCPF
               IF WS-CDEXCPF-ST NOT = '00'
                   DISPLAY 'CDEXCPF オープン失敗 ST='
                           WS-CDEXCPF-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.

       2000-PROCESS-RECEIVED.
           PERFORM VARYING WS-REC-IDX FROM 1 BY 1
                   UNTIL WS-REC-IDX > 10 OR WS-ABEND
               ADD 1 TO WS-IN-CNT
               MOVE 'Y' TO WS-VALID-SW
               MOVE 'A' TO WS-APPLY-STATUS
               PERFORM 2100-CHECK-DUPLICATE
               IF WS-DUPLICATE
                   MOVE 'N' TO WS-VALID-SW
                   MOVE 'N' TO WS-APPLY-STATUS
                   PERFORM 8100-WRITE-EXCEPTION
               ELSE
                   PERFORM 2200-VALIDATE-RECORD
               END-IF
               IF WS-VALID AND NOT WS-ABEND
                   PERFORM 2300-WRITE-RATE
               ELSE
                   IF NOT WS-ABEND
                       ADD 1 TO WS-NG-CNT
                   END-IF
               END-IF
           END-PERFORM.

       2100-CHECK-DUPLICATE.
           MOVE 'N' TO WS-DUP-SW
           PERFORM VARYING WS-SEEN-IDX FROM 1 BY 1
                   UNTIL WS-SEEN-IDX > WS-SEEN-CNT
                      OR WS-DUPLICATE
               IF WS-SEEN-CUR(WS-SEEN-IDX) = WS-R-CUR(WS-REC-IDX)
                   SET WS-DUPLICATE TO TRUE
               END-IF
           END-PERFORM
           IF NOT WS-DUPLICATE
               ADD 1 TO WS-SEEN-CNT
               MOVE WS-R-CUR(WS-REC-IDX)
                 TO WS-SEEN-CUR(WS-SEEN-CNT)
           END-IF.

       2200-VALIDATE-RECORD.
           IF WS-R-DT(WS-REC-IDX) NOT = WS-PROCESS-DT
               MOVE 'N' TO WS-VALID-SW
               MOVE 'N' TO WS-APPLY-STATUS
               PERFORM 8100-WRITE-EXCEPTION
           END-IF
           IF WS-VALID AND WS-R-TTM(WS-REC-IDX) = ZERO
               MOVE 'N' TO WS-VALID-SW
               MOVE 'N' TO WS-APPLY-STATUS
               PERFORM 8100-WRITE-EXCEPTION
           END-IF
           IF WS-VALID
               PERFORM 2210-CHECK-REQUIRED-CURRENCY
               IF NOT WS-FOUND
                   MOVE 'N' TO WS-VALID-SW
                   MOVE 'N' TO WS-APPLY-STATUS
                   PERFORM 8100-WRITE-EXCEPTION
               END-IF
           END-IF
           IF WS-VALID
               PERFORM 2220-CHECK-DAILY-CHANGE
               IF WS-ABS-CHANGE > 0.030000
                   MOVE 'H' TO WS-APPLY-STATUS
                   PERFORM 8100-WRITE-EXCEPTION
               END-IF
           END-IF.

       2210-CHECK-REQUIRED-CURRENCY.
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING WS-REQ-IDX FROM 1 BY 1
                   UNTIL WS-REQ-IDX > 8 OR WS-FOUND
               IF WS-REQ-CUR(WS-REQ-IDX) = WS-R-CUR(WS-REC-IDX)
                   SET WS-FOUND TO TRUE
               END-IF
           END-PERFORM.

       2220-CHECK-DAILY-CHANGE.
           MOVE ZERO TO WS-PRIOR-RATE
           MOVE ZERO TO WS-CHANGE-RATE
           MOVE ZERO TO WS-ABS-CHANGE
           PERFORM VARYING WS-PRIOR-IDX FROM 1 BY 1
                   UNTIL WS-PRIOR-IDX > 8
               IF WS-PRIOR-CUR(WS-PRIOR-IDX) = WS-R-CUR(WS-REC-IDX)
                   MOVE WS-PRIOR-TTM(WS-PRIOR-IDX)
                     TO WS-PRIOR-RATE
                   MOVE 9 TO WS-PRIOR-IDX
               END-IF
           END-PERFORM
           IF WS-PRIOR-RATE > ZERO
               COMPUTE WS-CHANGE-RATE ROUNDED =
                   (WS-R-TTM(WS-REC-IDX) - WS-PRIOR-RATE)
                   / WS-PRIOR-RATE
               IF WS-CHANGE-RATE < ZERO
                   COMPUTE WS-ABS-CHANGE =
                       WS-CHANGE-RATE * -1
               ELSE
                   MOVE WS-CHANGE-RATE TO WS-ABS-CHANGE
               END-IF
           END-IF.

       2300-WRITE-RATE.
           MOVE WS-R-CUR(WS-REC-IDX) TO FX-CURRENCY-CD
           MOVE WS-R-DT(WS-REC-IDX)  TO FX-RATE-DT
           MOVE WS-R-TTM(WS-REC-IDX) TO FX-TTM-RATE
           MOVE WS-R-SRC(WS-REC-IDX) TO FX-RATE-SOURCE
           MOVE WS-APPLY-STATUS      TO FX-APPLY-STATUS
           WRITE CDRTEXF-REC
           IF WS-CDRTEXF-ST = '00'
               ADD 1 TO WS-OK-CNT
           ELSE
               DISPLAY 'CDRTEXF 書込失敗 通貨=' FX-CURRENCY-CD
               DISPLAY 'ST=' WS-CDRTEXF-ST
               SET WS-ABEND TO TRUE
           END-IF.

       3000-CHECK-MISSING.
           PERFORM VARYING WS-REQ-IDX FROM 1 BY 1
                   UNTIL WS-REQ-IDX > 8 OR WS-ABEND
               MOVE 'N' TO WS-FOUND-SW
               PERFORM VARYING WS-SEEN-IDX FROM 1 BY 1
                       UNTIL WS-SEEN-IDX > WS-SEEN-CNT
                          OR WS-FOUND
                   IF WS-SEEN-CUR(WS-SEEN-IDX)
                      = WS-REQ-CUR(WS-REQ-IDX)
                       SET WS-FOUND TO TRUE
                   END-IF
               END-PERFORM
               IF NOT WS-FOUND
                   PERFORM 8200-WRITE-MISSING-EXCEPTION
               END-IF
           END-PERFORM.

       8100-WRITE-EXCEPTION.
           ADD 1 TO WS-EXCEPTION-SEQ
           MOVE WS-EXCEPTION-SEQ TO EX-EXCEPTION-ID
           MOVE ZERO             TO EX-SALE-ID
           MOVE SPACES           TO EX-CARD-NO
           EVALUATE TRUE
               WHEN WS-DUPLICATE
                   MOVE 'FXDUP' TO EX-REASON-CD
               WHEN WS-R-DT(WS-REC-IDX) NOT = WS-PROCESS-DT
                   MOVE 'FXDAT' TO EX-REASON-CD
               WHEN WS-R-TTM(WS-REC-IDX) = ZERO
                   MOVE 'FXZER' TO EX-REASON-CD
               WHEN NOT WS-FOUND
                   MOVE 'FXCUR' TO EX-REASON-CD
               WHEN WS-ABS-CHANGE > 0.030000
                   MOVE 'FXJMP' TO EX-REASON-CD
               WHEN OTHER
                   MOVE 'FXCHK' TO EX-REASON-CD
           END-EVALUATE
           MOVE 'CB101B'       TO EX-DETECTED-PGM
           MOVE WS-PROCESS-DT  TO EX-EXCEPTION-DT
           MOVE '未対応'       TO EX-ACTION-STATUS
           WRITE CDEXCPF-REC
           IF WS-CDEXCPF-ST = '00'
               ADD 1 TO WS-EX-CNT
           ELSE
               DISPLAY 'CDEXCPF 書込失敗 ST=' WS-CDEXCPF-ST
               SET WS-ABEND TO TRUE
           END-IF.

       8200-WRITE-MISSING-EXCEPTION.
           ADD 1 TO WS-EXCEPTION-SEQ
           MOVE WS-EXCEPTION-SEQ TO EX-EXCEPTION-ID
           MOVE ZERO             TO EX-SALE-ID
           MOVE SPACES           TO EX-CARD-NO
           MOVE 'FXMIS'          TO EX-REASON-CD
           MOVE 'CB101B'         TO EX-DETECTED-PGM
           MOVE WS-PROCESS-DT    TO EX-EXCEPTION-DT
           MOVE '未対応'         TO EX-ACTION-STATUS
           WRITE CDEXCPF-REC
           IF WS-CDEXCPF-ST = '00'
               ADD 1 TO WS-EX-CNT
               DISPLAY '必須通貨欠落 通貨='
                       WS-REQ-CUR(WS-REQ-IDX)
           ELSE
               DISPLAY 'CDEXCPF 欠落通貨書込失敗 ST='
                       WS-CDEXCPF-ST
               SET WS-ABEND TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           IF WS-CDRTEXF-ST NOT = SPACES
               CLOSE CDRTEXF
               IF WS-CDRTEXF-ST NOT = '00'
                   DISPLAY 'CDRTEXF クローズ失敗 ST='
                           WS-CDRTEXF-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF
           IF WS-CDEXCPF-ST NOT = SPACES
               CLOSE CDEXCPF
               IF WS-CDEXCPF-ST NOT = '00'
                   DISPLAY 'CDEXCPF クローズ失敗 ST='
                           WS-CDEXCPF-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.
