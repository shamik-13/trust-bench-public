       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB118B.
      ******************************************************************
      * 売上日次集計
      * CDCAPFの確定明細を日付、加盟店、通貨で集計する。
      * 返品とチャージバック申立額は同一キーへ控除集計する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCAPF ASSIGN TO "CDCAPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CDCAPF-ST.
           SELECT CDRTNF ASSIGN TO "CDRTNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CDRTNF-ST.
           SELECT CDCBKPF ASSIGN TO "CDCBKPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CBK-CHARGEBACK-ID
               FILE STATUS IS WS-CDCBKPF-ST.
           SELECT CDSUMF ASSIGN TO "CDSUMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SM-SUMMARY-KEY
               FILE STATUS IS WS-CDSUMF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDCAPF.
           COPY CDCAPFC.
       FD  CDRTNF.
           COPY CDRTNC.
       FD  CDCBKPF.
           COPY CDCBKPC.
       FD  CDSUMF.
           COPY CDSUMC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CDCAPF-ST          PIC XX VALUE SPACES.
           05 WS-CDRTNF-ST          PIC XX VALUE SPACES.
           05 WS-CDCBKPF-ST         PIC XX VALUE SPACES.
           05 WS-CDSUMF-ST          PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-CDCAPF         PIC X VALUE 'N'.
              88 EOF-CDCAPF              VALUE 'Y'.
           05 WS-EOF-CDRTNF         PIC X VALUE 'N'.
              88 EOF-CDRTNF              VALUE 'Y'.
           05 WS-EOF-CDCBKPF        PIC X VALUE 'N'.
              88 EOF-CDCBKPF             VALUE 'Y'.
           05 WS-HARD-ERROR         PIC X VALUE 'N'.
              88 HARD-ERROR              VALUE 'Y'.

       01  WS-CONSTANTS.
           05 WS-BASE-CCY           PIC X(03) VALUE 'JPY'.
           05 WS-CAP-CNF            PIC X VALUE 'C'.
           05 WS-CAP-SKP            PIC X VALUE 'S'.
           05 WS-CAP-HLD            PIC X VALUE 'H'.
           05 WS-RTN-OK             PIC X VALUE 'A'.
           05 WS-CBK-ACTIVE         PIC X VALUE 'O'.
           05 WS-CBK-PENDING        PIC X VALUE 'P'.
           05 WS-STAT-NORMAL        PIC X VALUE '0'.
           05 WS-STAT-HOLD          PIC X VALUE 'H'.

       01  WS-COUNTERS.
           05 WS-READ-CAP           PIC 9(9) VALUE ZERO.
           05 WS-READ-RTN           PIC 9(9) VALUE ZERO.
           05 WS-READ-CBK           PIC 9(9) VALUE ZERO.
           05 WS-WRITE-SUM          PIC 9(9) VALUE ZERO.
           05 WS-CNT-CNF            PIC 9(9) VALUE ZERO.
           05 WS-CNT-SKP            PIC 9(9) VALUE ZERO.
           05 WS-CNT-HLD            PIC 9(9) VALUE ZERO.
           05 WS-CNT-OTH            PIC 9(9) VALUE ZERO.
           05 WS-IDX                PIC 9(5) VALUE ZERO.
           05 WS-IDY                PIC 9(5) VALUE ZERO.
           05 WS-SALE-MAX           PIC 9(5) VALUE 5000.
           05 WS-SUM-MAX            PIC 9(5) VALUE 2000.
           05 WS-SALE-CNT           PIC 9(5) VALUE ZERO.
           05 WS-SUM-CNT            PIC 9(5) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-SALE-AMT           PIC S9(13)V99 VALUE ZERO.
           05 WS-FEE-AMT            PIC S9(13)V99 VALUE ZERO.
           05 WS-RETURN-AMT         PIC S9(13)V99 VALUE ZERO.
           05 WS-CLAIM-AMT          PIC S9(13)V99 VALUE ZERO.
           05 WS-DETAIL-NET         PIC S9(15)V99 VALUE ZERO.
           05 WS-SUM-NET            PIC S9(15)V99 VALUE ZERO.
           05 WS-DIFF-AMT           PIC S9(15)V99 VALUE ZERO.

       01  WS-KEY-WORK.
           05 WS-WORK-DT            PIC X(08) VALUE SPACES.
           05 WS-WORK-MERCHANT      PIC X(15) VALUE SPACES.
           05 WS-WORK-CCY           PIC X(03) VALUE SPACES.
           05 WS-WORK-KEY           PIC X(26) VALUE SPACES.
           05 WS-FOUND              PIC X VALUE 'N'.
              88 FOUND-KEY               VALUE 'Y'.
           05 WS-FOUND-SALE         PIC X VALUE 'N'.
              88 FOUND-SALE              VALUE 'Y'.

       01  WS-SALE-TABLE.
           05 WS-SALE-ENTRY OCCURS 5000 TIMES.
              10 T-SALE-ID          PIC X(20).
              10 T-SALE-DT          PIC X(08).
              10 T-MERCHANT         PIC X(15).
              10 T-CCY              PIC X(03).
              10 T-SALE-AMT         PIC S9(13)V99.
              10 T-FEE-AMT          PIC S9(13)V99.

       01  WS-SUM-TABLE.
           05 WS-SUM-ENTRY OCCURS 2000 TIMES.
              10 T-SUM-KEY          PIC X(26).
              10 T-SUM-DT           PIC X(08).
              10 T-SUM-MERCHANT     PIC X(15).
              10 T-SUM-CCY          PIC X(03).
              10 T-SUM-SALE-CNT     PIC 9(9).
              10 T-SUM-SALE-AMT     PIC S9(13)V99.
              10 T-SUM-RETURN-AMT   PIC S9(13)V99.
              10 T-SUM-STATUS       PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
               PERFORM 2000-LOAD-CAPTURE
               PERFORM 3000-APPLY-RETURN
               PERFORM 4000-APPLY-CHARGEBACK
               PERFORM 5000-CHECK-SUMMARY
               PERFORM 6000-WRITE-SUMMARY
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY 'CB610B NORMAL END'
               DISPLAY 'CAPTURE=' WS-CNT-CNF
                       ' SKIP=' WS-CNT-SKP
                       ' HOLD=' WS-CNT-HLD
                       ' OTHER=' WS-CNT-OTH
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDCAPF CDRTNF CDCBKPF
                OUTPUT CDSUMF
           IF WS-CDCAPF-ST NOT = '00'
               DISPLAY 'CDCAPF OPEN ERROR ST=' WS-CDCAPF-ST
               SET HARD-ERROR TO TRUE
           END-IF
           IF WS-CDRTNF-ST NOT = '00'
               DISPLAY 'CDRTNF OPEN ERROR ST=' WS-CDRTNF-ST
               SET HARD-ERROR TO TRUE
           END-IF
           IF WS-CDCBKPF-ST NOT = '00'
               DISPLAY 'CDCBKPF OPEN ERROR ST=' WS-CDCBKPF-ST
               SET HARD-ERROR TO TRUE
           END-IF
           IF WS-CDSUMF-ST NOT = '00'
               DISPLAY 'CDSUMF OPEN ERROR ST=' WS-CDSUMF-ST
               SET HARD-ERROR TO TRUE
           END-IF.

       2000-LOAD-CAPTURE.
           PERFORM UNTIL EOF-CDCAPF OR HARD-ERROR
               READ CDCAPF
                   AT END
                       SET EOF-CDCAPF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ-CAP
                       EVALUATE BC-CAP-STATUS
                           WHEN WS-CAP-CNF
                               ADD 1 TO WS-CNT-CNF
                               PERFORM 2100-ADD-CAPTURE
                           WHEN WS-CAP-SKP
                               ADD 1 TO WS-CNT-SKP
                           WHEN WS-CAP-HLD
                               ADD 1 TO WS-CNT-HLD
                           WHEN OTHER
                               ADD 1 TO WS-CNT-OTH
                               DISPLAY 'CAP STATUS ERROR SALE='
                                       BC-SALE-ID
                       END-EVALUATE
               END-READ
               IF WS-CDCAPF-ST NOT = '00'
                  AND WS-CDCAPF-ST NOT = '10'
                   DISPLAY 'CDCAPF READ ERROR ST=' WS-CDCAPF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM.

       2100-ADD-CAPTURE.
           IF WS-SALE-CNT >= WS-SALE-MAX
               DISPLAY 'SALE TABLE OVERFLOW SALE=' BC-SALE-ID
               SET HARD-ERROR TO TRUE
           ELSE
               MOVE SPACES TO WS-WORK-MERCHANT
               MOVE BC-SALE-ID(1:8) TO WS-WORK-DT
               MOVE BC-PROGRAM-ID TO WS-WORK-MERCHANT
               MOVE BC-CURRENCY-CD TO WS-WORK-CCY
               MOVE BC-BILLED-AMT TO WS-SALE-AMT
               IF BC-CURRENCY-CD = WS-BASE-CCY
                   MOVE ZERO TO WS-FEE-AMT
               ELSE
                   MOVE BC-FEE-AMT TO WS-FEE-AMT
               END-IF
               ADD 1 TO WS-SALE-CNT
               MOVE BC-SALE-ID TO T-SALE-ID(WS-SALE-CNT)
               MOVE WS-WORK-DT TO T-SALE-DT(WS-SALE-CNT)
               MOVE WS-WORK-MERCHANT TO T-MERCHANT(WS-SALE-CNT)
               MOVE WS-WORK-CCY TO T-CCY(WS-SALE-CNT)
               MOVE WS-SALE-AMT TO T-SALE-AMT(WS-SALE-CNT)
               MOVE WS-FEE-AMT TO T-FEE-AMT(WS-SALE-CNT)
               ADD WS-SALE-AMT WS-FEE-AMT TO WS-DETAIL-NET
               PERFORM 2200-BUILD-KEY
               PERFORM 2300-FIND-OR-ADD-SUM
               IF NOT HARD-ERROR
                   ADD 1 TO T-SUM-SALE-CNT(WS-IDX)
                   ADD WS-SALE-AMT WS-FEE-AMT
                       TO T-SUM-SALE-AMT(WS-IDX)
               END-IF
           END-IF.

       2200-BUILD-KEY.
           MOVE SPACES TO WS-WORK-KEY
           STRING WS-WORK-DT       DELIMITED BY SIZE
                  WS-WORK-MERCHANT DELIMITED BY SIZE
                  WS-WORK-CCY      DELIMITED BY SIZE
                  INTO WS-WORK-KEY
           END-STRING.

       2300-FIND-OR-ADD-SUM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-SUM-CNT OR FOUND-KEY
               IF T-SUM-KEY(WS-IDX) = WS-WORK-KEY
                   SET FOUND-KEY TO TRUE
               END-IF
           END-PERFORM
           IF NOT FOUND-KEY
               IF WS-SUM-CNT >= WS-SUM-MAX
                   DISPLAY 'SUMMARY TABLE OVERFLOW KEY=' WS-WORK-KEY
                   SET HARD-ERROR TO TRUE
               ELSE
                   ADD 1 TO WS-SUM-CNT
                   MOVE WS-SUM-CNT TO WS-IDX
                   MOVE WS-WORK-KEY TO T-SUM-KEY(WS-IDX)
                   MOVE WS-WORK-DT TO T-SUM-DT(WS-IDX)
                   MOVE WS-WORK-MERCHANT TO T-SUM-MERCHANT(WS-IDX)
                   MOVE WS-WORK-CCY TO T-SUM-CCY(WS-IDX)
                   MOVE ZERO TO T-SUM-SALE-CNT(WS-IDX)
                   MOVE ZERO TO T-SUM-SALE-AMT(WS-IDX)
                   MOVE ZERO TO T-SUM-RETURN-AMT(WS-IDX)
                   MOVE WS-STAT-NORMAL TO T-SUM-STATUS(WS-IDX)
               END-IF
           END-IF.

       3000-APPLY-RETURN.
           PERFORM UNTIL EOF-CDRTNF OR HARD-ERROR
               READ CDRTNF
                   AT END
                       SET EOF-CDRTNF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ-RTN
                       IF RT-APPROVAL-STATUS = WS-RTN-OK
                           PERFORM 3100-FIND-SALE-FOR-RTN
                       END-IF
               END-READ
               IF WS-CDRTNF-ST NOT = '00'
                  AND WS-CDRTNF-ST NOT = '10'
                   DISPLAY 'CDRTNF READ ERROR ST=' WS-CDRTNF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM.

       3100-FIND-SALE-FOR-RTN.
           MOVE 'N' TO WS-FOUND-SALE
           PERFORM VARYING WS-IDY FROM 1 BY 1
               UNTIL WS-IDY > WS-SALE-CNT OR FOUND-SALE
               IF T-SALE-ID(WS-IDY) = RT-SALE-ID
                   SET FOUND-SALE TO TRUE
               END-IF
           END-PERFORM
           IF FOUND-SALE
               MOVE T-SALE-DT(WS-IDY) TO WS-WORK-DT
               MOVE T-MERCHANT(WS-IDY) TO WS-WORK-MERCHANT
               MOVE T-CCY(WS-IDY) TO WS-WORK-CCY
               MOVE RT-RETURN-AMT TO WS-RETURN-AMT
               ADD WS-RETURN-AMT TO WS-DETAIL-NET
               PERFORM 2200-BUILD-KEY
               PERFORM 2300-FIND-OR-ADD-SUM
               IF NOT HARD-ERROR
                   ADD WS-RETURN-AMT TO T-SUM-RETURN-AMT(WS-IDX)
               END-IF
           ELSE
               DISPLAY 'RETURN SALE NOT FOUND RTN=' RT-RETURN-ID
           END-IF.

       4000-APPLY-CHARGEBACK.
           PERFORM UNTIL EOF-CDCBKPF OR HARD-ERROR
               READ CDCBKPF NEXT RECORD
                   AT END
                       SET EOF-CDCBKPF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ-CBK
                       IF CBK-CASE-STATUS = WS-CBK-ACTIVE
                          OR CBK-CASE-STATUS = WS-CBK-PENDING
                           PERFORM 4100-APPLY-CBK-ONE
                       END-IF
               END-READ
               IF WS-CDCBKPF-ST NOT = '00'
                  AND WS-CDCBKPF-ST NOT = '10'
                   DISPLAY 'CDCBKPF READ ERROR ST=' WS-CDCBKPF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM.

       4100-APPLY-CBK-ONE.
           MOVE 'N' TO WS-FOUND-SALE
           PERFORM VARYING WS-IDY FROM 1 BY 1
               UNTIL WS-IDY > WS-SALE-CNT OR FOUND-SALE
               IF T-SALE-ID(WS-IDY) = CBK-SALE-ID
                   SET FOUND-SALE TO TRUE
               END-IF
           END-PERFORM
           IF FOUND-SALE
               MOVE SPACES TO WS-WORK-MERCHANT
               MOVE T-SALE-DT(WS-IDY) TO WS-WORK-DT
               MOVE CBK-MERCHANT-CODE TO WS-WORK-MERCHANT
               MOVE T-CCY(WS-IDY) TO WS-WORK-CCY
               MOVE CBK-CLAIM-AMT TO WS-CLAIM-AMT
               ADD WS-CLAIM-AMT TO WS-DETAIL-NET
               PERFORM 2200-BUILD-KEY
               PERFORM 2300-FIND-OR-ADD-SUM
               IF NOT HARD-ERROR
                   ADD WS-CLAIM-AMT TO T-SUM-RETURN-AMT(WS-IDX)
               END-IF
           ELSE
               DISPLAY 'CHARGEBACK SALE NOT FOUND CBK='
                       CBK-CHARGEBACK-ID
           END-IF.

       5000-CHECK-SUMMARY.
           MOVE ZERO TO WS-SUM-NET
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-SUM-CNT
               COMPUTE WS-SUM-NET = WS-SUM-NET
                   + T-SUM-SALE-AMT(WS-IDX)
                   + T-SUM-RETURN-AMT(WS-IDX)
           END-PERFORM
           COMPUTE WS-DIFF-AMT = WS-DETAIL-NET - WS-SUM-NET
           IF WS-DIFF-AMT NOT = ZERO
               DISPLAY 'SUMMARY DIFFERENCE=' WS-DIFF-AMT
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-SUM-CNT
                   MOVE WS-STAT-HOLD TO T-SUM-STATUS(WS-IDX)
               END-PERFORM
           END-IF.

       6000-WRITE-SUMMARY.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-SUM-CNT OR HARD-ERROR
               MOVE SPACES TO CDSUMF-REC
               MOVE T-SUM-KEY(WS-IDX) TO SM-SUMMARY-KEY
               MOVE T-SUM-DT(WS-IDX) TO SM-SUMMARY-DT
               MOVE T-SUM-MERCHANT(WS-IDX) TO SM-MERCHANT-CODE
               MOVE T-SUM-CCY(WS-IDX) TO SM-CURRENCY-CD
               MOVE T-SUM-SALE-CNT(WS-IDX) TO SM-SALE-COUNT
               MOVE T-SUM-SALE-AMT(WS-IDX) TO SM-SALE-AMT
               MOVE T-SUM-RETURN-AMT(WS-IDX) TO SM-RETURN-AMT
               IF T-SUM-STATUS(WS-IDX) = WS-STAT-HOLD
                   MOVE ZERO TO SM-SALE-COUNT
                   DISPLAY 'SUMMARY HOLD KEY=' T-SUM-KEY(WS-IDX)
               END-IF
               WRITE CDSUMF-REC
               IF WS-CDSUMF-ST = '00'
                   ADD 1 TO WS-WRITE-SUM
               ELSE
                   DISPLAY 'CDSUMF WRITE ERROR ST=' WS-CDSUMF-ST
                   DISPLAY 'SUMMARY KEY=' T-SUM-KEY(WS-IDX)
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM.

       9000-CLOSE-FILES.
           CLOSE CDCAPF CDRTNF CDCBKPF CDSUMF
           IF WS-CDCAPF-ST NOT = '00'
               DISPLAY 'CDCAPF CLOSE ST=' WS-CDCAPF-ST
           END-IF
           IF WS-CDRTNF-ST NOT = '00'
               DISPLAY 'CDRTNF CLOSE ST=' WS-CDRTNF-ST
           END-IF
           IF WS-CDCBKPF-ST NOT = '00'
               DISPLAY 'CDCBKPF CLOSE ST=' WS-CDCBKPF-ST
           END-IF
           IF WS-CDSUMF-ST NOT = '00'
               DISPLAY 'CDSUMF CLOSE ST=' WS-CDSUMF-ST
           END-IF.
