       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB330B.
      *
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20240115  ＣＤ運用  初版作成
      * 1.10  20240520  ＣＤ運用  手数料内訳判定追加
      * 1.20  20241108  ＣＤ運用  最低支払額再計算呼出追加
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDBALF ASSIGN TO "CDBALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-BL-STATUS.
           SELECT CDBILLF ASSIGN TO "CDBILLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-BI-STATUS.
           SELECT CDSALESF ASSIGN TO "CDSALESF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SL-STATUS.
           SELECT CDFEEF ASSIGN TO "CDFEEF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CDBALF.
           COPY CDBALFC.
       FD  CDBILLF.
           COPY CDBILLFC.
       FD  CDSALESF.
           COPY CDSALEC.
       FD  CDFEEF.
           COPY CDFEEC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-BL-STATUS             PIC X(02) VALUE SPACES.
           05 WS-BI-STATUS             PIC X(02) VALUE SPACES.
           05 WS-SL-STATUS             PIC X(02) VALUE SPACES.
           05 WS-FE-STATUS             PIC X(02) VALUE SPACES.

       01  WS-FLAGS.
           05 WS-END-BL                PIC X VALUE "N".
           05 WS-END-BI                PIC X VALUE "N".
           05 WS-END-SL                PIC X VALUE "N".
           05 WS-END-FE                PIC X VALUE "N".
           05 WS-HARD-ERROR            PIC X VALUE "N".
           05 WS-BAL-FOUND             PIC X VALUE "N".

       01  WS-COUNTERS.
           05 WS-BL-CNT                PIC 9(7) VALUE ZERO.
           05 WS-BI-CNT                PIC 9(7) VALUE ZERO.
           05 WS-SL-CNT                PIC 9(7) VALUE ZERO.
           05 WS-FE-CNT                PIC 9(7) VALUE ZERO.
           05 WS-MATCH-CNT             PIC 9(7) VALUE ZERO.
           05 WS-DIFF-CNT              PIC 9(7) VALUE ZERO.
           05 WS-SKIP-CNT              PIC 9(7) VALUE ZERO.
           05 WS-ERR-CNT               PIC 9(7) VALUE ZERO.

       01  WS-SALES-TABLE.
           05 WS-SALES-NUM             PIC 9(5) VALUE ZERO.
           05 WS-SALES-ROW OCCURS 20000 TIMES.
              10 TB-SL-CARD-NO         PIC X(16).
              10 TB-SL-CYCLE-DT        PIC 9(08).
              10 TB-SL-SALES-AMT       PIC S9(11)V99 COMP-3.
              10 TB-SL-TAX-AMT         PIC S9(09)V99 COMP-3.
              10 TB-SL-STATUS          PIC X(02).

       01  WS-FEE-TABLE.
           05 WS-FEE-NUM               PIC 9(5) VALUE ZERO.
           05 WS-FEE-ROW OCCURS 10000 TIMES.
              10 TB-FE-CARD-NO         PIC X(16).
              10 TB-FE-CYCLE-CD        PIC X(08).
              10 TB-FE-AMT             PIC S9(09)V99 COMP-3.
              10 TB-FE-TYPE            PIC X(02).
              10 TB-FE-STATUS          PIC X(02).

       01  WS-BAL-TABLE.
           05 WS-BAL-NUM               PIC 9(5) VALUE ZERO.
           05 WS-BAL-ROW OCCURS 20000 TIMES.
              10 TB-BL-CARD-NO         PIC X(16).
              10 TB-BL-CYCLE-DT        PIC 9(08).
              10 TB-BL-CLOSING-AMT     PIC S9(11)V99 COMP-3.
              10 TB-BL-REV-AMT         PIC S9(11)V99 COMP-3.
              10 TB-BL-NEW-AMT         PIC S9(11)V99 COMP-3.
              10 TB-BL-CASH-AMT        PIC S9(11)V99 COMP-3.

       01  WS-SUBSCRIPTS.
           05 SX                       PIC 9(5) COMP VALUE ZERO.
           05 SX-BAL                   PIC 9(5) COMP VALUE ZERO.

       01  WS-CALC.
           05 WS-CYCLE-CHAR            PIC X(08) VALUE SPACES.
           05 WS-CLOSING-AMT           PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-REV-AMT               PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-NEW-AMT               PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-CASH-AMT              PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-SALES-SUM             PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-TAX-SUM               PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-FEE-SUM               PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-LATE-FEE-SUM          PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-ANNUAL-FEE-SUM        PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-OTHER-FEE-SUM         PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-RECALC-BILL-AMT       PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-DIFF-AMT              PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-MIN-DIFF-AMT          PIC S9(11)V99 COMP-3
                                       VALUE ZERO.
           05 WS-CARD-STATUS           PIC X(02) VALUE "01".
           05 WS-REASON                PIC X(40) VALUE SPACES.

       01  WS-DISPLAY-AMTS.
           05 DSP-BILL-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 DSP-CALC-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 DSP-DIFF-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 DSP-MIN-AMT              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.

       01  WS-CONSTANTS.
           05 CN-YES                   PIC X VALUE "Y".
           05 CN-NO                    PIC X VALUE "N".
           05 CN-BILL-CONFIRMED        PIC X VALUE "C".
           05 CN-BILL-HOLD             PIC X VALUE "H".
           05 CN-BILL-SKIP             PIC X VALUE "S".
           05 CN-POSTED                PIC X(02) VALUE "01".
           05 CN-CAPTURED              PIC X(02) VALUE "01".
           05 CN-FEE-LATE              PIC X(02) VALUE "01".
           05 CN-FEE-ANNUAL            PIC X(02) VALUE "02".

           COPY LK-MINPAY-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WS-HARD-ERROR = CN-NO
               PERFORM 2000-LOAD-BALANCE
               PERFORM 2100-LOAD-SALES
               PERFORM 2200-LOAD-FEE
               PERFORM 3000-CHECK-BILLS
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-HARD-ERROR = CN-YES
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CB330B 正常終了"
               DISPLAY "請求件数=" WS-BI-CNT
               DISPLAY "一致件数=" WS-MATCH-CNT
               DISPLAY "差異件数=" WS-DIFF-CNT
               DISPLAY "対象外件数=" WS-SKIP-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDBALF
           IF WS-BL-STATUS NOT = "00"
               DISPLAY "CDBALF OPEN ST="
               DISPLAY WS-BL-STATUS
               MOVE CN-YES TO WS-HARD-ERROR
           END-IF

           OPEN INPUT CDBILLF
           IF WS-BI-STATUS NOT = "00"
               DISPLAY "CDBILLF OPEN ST="
               DISPLAY WS-BI-STATUS
               MOVE CN-YES TO WS-HARD-ERROR
           END-IF

           OPEN INPUT CDSALESF
           IF WS-SL-STATUS NOT = "00"
               DISPLAY "CDSALESF OPEN ST="
               DISPLAY WS-SL-STATUS
               MOVE CN-YES TO WS-HARD-ERROR
           END-IF

           OPEN INPUT CDFEEF
           IF WS-FE-STATUS NOT = "00"
               DISPLAY "CDFEEF OPEN ST="
               DISPLAY WS-FE-STATUS
               MOVE CN-YES TO WS-HARD-ERROR
           END-IF.

       2000-LOAD-BALANCE.
           PERFORM UNTIL WS-END-BL = CN-YES
               READ CDBALF
                   AT END
                       MOVE CN-YES TO WS-END-BL
                   NOT AT END
                       ADD 1 TO WS-BL-CNT
                       IF WS-BAL-NUM >= 20000
                           DISPLAY "BAL TABLE FULL CNT="
                           DISPLAY WS-BL-CNT
                           MOVE CN-YES TO WS-HARD-ERROR
                           MOVE CN-YES TO WS-END-BL
                       ELSE
                           ADD 1 TO WS-BAL-NUM
                           MOVE BL-CARD-NO
                             TO TB-BL-CARD-NO(WS-BAL-NUM)
                           MOVE BL-CYCLE-DT
                             TO TB-BL-CYCLE-DT(WS-BAL-NUM)
                           MOVE BL-CLOSING-BAL-AMT
                             TO TB-BL-CLOSING-AMT(WS-BAL-NUM)
                           MOVE BL-REVOLVING-BAL-AMT
                             TO TB-BL-REV-AMT(WS-BAL-NUM)
                           MOVE BL-NEW-CHARGE-AMT
                             TO TB-BL-NEW-AMT(WS-BAL-NUM)
                           MOVE BL-CASH-ADV-AMT
                             TO TB-BL-CASH-AMT(WS-BAL-NUM)
                       END-IF
               END-READ
               IF WS-BL-STATUS NOT = "00"
                  AND WS-BL-STATUS NOT = "10"
                   DISPLAY "CDBALF READ ST="
                   DISPLAY WS-BL-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
                   MOVE CN-YES TO WS-END-BL
               END-IF
           END-PERFORM.

       2100-LOAD-SALES.
           PERFORM UNTIL WS-END-SL = CN-YES
               READ CDSALESF
                   AT END
                       MOVE CN-YES TO WS-END-SL
                   NOT AT END
                       ADD 1 TO WS-SL-CNT
                       IF WS-SALES-NUM >= 20000
                           DISPLAY "SALES TABLE FULL CNT="
                           DISPLAY WS-SL-CNT
                           MOVE CN-YES TO WS-HARD-ERROR
                           MOVE CN-YES TO WS-END-SL
                       ELSE
                           ADD 1 TO WS-SALES-NUM
                           MOVE SL-CARD-NO
                             TO TB-SL-CARD-NO(WS-SALES-NUM)
                           MOVE SL-POSTING-DT
                             TO TB-SL-CYCLE-DT(WS-SALES-NUM)
                           MOVE SL-SALES-AMT
                             TO TB-SL-SALES-AMT(WS-SALES-NUM)
                           MOVE SL-TAX-AMT
                             TO TB-SL-TAX-AMT(WS-SALES-NUM)
                           MOVE SL-CAPTURE-STATUS
                             TO TB-SL-STATUS(WS-SALES-NUM)
                       END-IF
               END-READ
               IF WS-SL-STATUS NOT = "00"
                  AND WS-SL-STATUS NOT = "10"
                   DISPLAY "CDSALESF READ ST="
                   DISPLAY WS-SL-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
                   MOVE CN-YES TO WS-END-SL
               END-IF
           END-PERFORM.

       2200-LOAD-FEE.
           PERFORM UNTIL WS-END-FE = CN-YES
               READ CDFEEF
                   AT END
                       MOVE CN-YES TO WS-END-FE
                   NOT AT END
                       ADD 1 TO WS-FE-CNT
                       IF WS-FEE-NUM >= 10000
                           DISPLAY "FEE TABLE FULL CNT="
                           DISPLAY WS-FE-CNT
                           MOVE CN-YES TO WS-HARD-ERROR
                           MOVE CN-YES TO WS-END-FE
                       ELSE
                           ADD 1 TO WS-FEE-NUM
                           MOVE FE-CARD-NO
                             TO TB-FE-CARD-NO(WS-FEE-NUM)
                           MOVE FE-BILL-CYCLE-CD
                             TO TB-FE-CYCLE-CD(WS-FEE-NUM)
                           MOVE FE-FEE-AMT
                             TO TB-FE-AMT(WS-FEE-NUM)
                           MOVE FE-FEE-TYPE
                             TO TB-FE-TYPE(WS-FEE-NUM)
                           MOVE FE-POST-STATUS
                             TO TB-FE-STATUS(WS-FEE-NUM)
                       END-IF
               END-READ
               IF WS-FE-STATUS NOT = "00"
                  AND WS-FE-STATUS NOT = "10"
                   DISPLAY "CDFEEF READ ST="
                   DISPLAY WS-FE-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
                   MOVE CN-YES TO WS-END-FE
               END-IF
           END-PERFORM.

       3000-CHECK-BILLS.
           PERFORM UNTIL WS-END-BI = CN-YES
                    OR WS-HARD-ERROR = CN-YES
               READ CDBILLF
                   AT END
                       MOVE CN-YES TO WS-END-BI
                   NOT AT END
                       ADD 1 TO WS-BI-CNT
                       PERFORM 3100-CHECK-ONE-BILL
               END-READ
               IF WS-BI-STATUS NOT = "00"
                  AND WS-BI-STATUS NOT = "10"
                   DISPLAY "CDBILLF READ ST="
                   DISPLAY WS-BI-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
                   MOVE CN-YES TO WS-END-BI
               END-IF
           END-PERFORM.

       3100-CHECK-ONE-BILL.
           EVALUATE BI-BILL-STATUS(1:1)
               WHEN CN-BILL-SKIP
               WHEN CN-BILL-HOLD
                   ADD 1 TO WS-SKIP-CNT
               WHEN CN-BILL-CONFIRMED
                   PERFORM 3200-RECALCULATE
                   PERFORM 3300-CALL-MINPAY
                   PERFORM 3400-COMPARE-BILL
               WHEN OTHER
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY "請求状態不正 CARD="
                   DISPLAY BI-CARD-NO
                   DISPLAY "請求状態="
                   DISPLAY BI-BILL-STATUS
           END-EVALUATE.

       3200-RECALCULATE.
           MOVE ZERO TO WS-CLOSING-AMT
           MOVE ZERO TO WS-REV-AMT
           MOVE ZERO TO WS-NEW-AMT
           MOVE ZERO TO WS-CASH-AMT
           MOVE ZERO TO WS-SALES-SUM
           MOVE ZERO TO WS-TAX-SUM
           MOVE ZERO TO WS-FEE-SUM
           MOVE ZERO TO WS-LATE-FEE-SUM
           MOVE ZERO TO WS-ANNUAL-FEE-SUM
           MOVE ZERO TO WS-OTHER-FEE-SUM
           MOVE ZERO TO WS-RECALC-BILL-AMT
           MOVE CN-NO TO WS-BAL-FOUND
           MOVE BI-CYCLE-DT TO WS-CYCLE-CHAR

           PERFORM VARYING SX FROM 1 BY 1
             UNTIL SX > WS-BAL-NUM
               IF TB-BL-CARD-NO(SX) = BI-CARD-NO
                  AND TB-BL-CYCLE-DT(SX) = BI-CYCLE-DT
                   MOVE CN-YES TO WS-BAL-FOUND
                   MOVE SX TO SX-BAL
               END-IF
           END-PERFORM

           IF WS-BAL-FOUND = CN-YES
               MOVE TB-BL-CLOSING-AMT(SX-BAL) TO WS-CLOSING-AMT
               MOVE TB-BL-REV-AMT(SX-BAL) TO WS-REV-AMT
               MOVE TB-BL-NEW-AMT(SX-BAL) TO WS-NEW-AMT
               MOVE TB-BL-CASH-AMT(SX-BAL) TO WS-CASH-AMT
           ELSE
               ADD 1 TO WS-ERR-CNT
               DISPLAY "残高未検出 CARD="
               DISPLAY BI-CARD-NO
               DISPLAY "サイクル日="
               DISPLAY BI-CYCLE-DT
           END-IF

           PERFORM VARYING SX FROM 1 BY 1
             UNTIL SX > WS-SALES-NUM
               IF TB-SL-CARD-NO(SX) = BI-CARD-NO
                  AND TB-SL-CYCLE-DT(SX) <= BI-CYCLE-DT
                  AND TB-SL-STATUS(SX) = CN-CAPTURED
                   ADD TB-SL-SALES-AMT(SX) TO WS-SALES-SUM
                   ADD TB-SL-TAX-AMT(SX) TO WS-TAX-SUM
               END-IF
           END-PERFORM

           PERFORM VARYING SX FROM 1 BY 1
             UNTIL SX > WS-FEE-NUM
               IF TB-FE-CARD-NO(SX) = BI-CARD-NO
                  AND TB-FE-CYCLE-CD(SX) = WS-CYCLE-CHAR
                  AND TB-FE-STATUS(SX) = CN-POSTED
                   ADD TB-FE-AMT(SX) TO WS-FEE-SUM
                   EVALUATE TB-FE-TYPE(SX)
                       WHEN CN-FEE-LATE
                           ADD TB-FE-AMT(SX)
                             TO WS-LATE-FEE-SUM
                       WHEN CN-FEE-ANNUAL
                           ADD TB-FE-AMT(SX)
                             TO WS-ANNUAL-FEE-SUM
                       WHEN OTHER
                           ADD TB-FE-AMT(SX)
                             TO WS-OTHER-FEE-SUM
                   END-EVALUATE
               END-IF
           END-PERFORM

           COMPUTE WS-RECALC-BILL-AMT =
                   WS-CLOSING-AMT + WS-SALES-SUM
                 + WS-TAX-SUM + WS-FEE-SUM.

       3300-CALL-MINPAY.
           MOVE WS-RECALC-BILL-AMT TO LK-MP-CLOSING-AMT
           MOVE WS-CARD-STATUS TO LK-MP-CARD-STATUS
           MOVE ZERO TO LK-MP-MIN-PAY
           MOVE ZERO TO LK-MP-RET
           CALL "CB910S" USING LK-MINPAY-PARM
           IF LK-MP-RET NOT = ZERO
               ADD 1 TO WS-ERR-CNT
               DISPLAY "最低支払額算定エラー CARD="
               DISPLAY BI-CARD-NO
               DISPLAY "RC="
               DISPLAY LK-MP-RET
           END-IF.

       3400-COMPARE-BILL.
           COMPUTE WS-DIFF-AMT =
                   WS-RECALC-BILL-AMT - BI-BILL-AMT
           COMPUTE WS-MIN-DIFF-AMT =
                   LK-MP-MIN-PAY - BI-MIN-PAY-AMT

           IF WS-DIFF-AMT = ZERO
              AND WS-MIN-DIFF-AMT = ZERO
               ADD 1 TO WS-MATCH-CNT
           ELSE
               ADD 1 TO WS-DIFF-CNT
               PERFORM 3500-BUILD-REASON
               MOVE BI-BILL-AMT TO DSP-BILL-AMT
               MOVE WS-RECALC-BILL-AMT TO DSP-CALC-AMT
               MOVE WS-DIFF-AMT TO DSP-DIFF-AMT
               MOVE WS-MIN-DIFF-AMT TO DSP-MIN-AMT
               DISPLAY "請求差異 CARD="
               DISPLAY BI-CARD-NO
               DISPLAY "請求額="
               DISPLAY DSP-BILL-AMT
               DISPLAY "再計算額="
               DISPLAY DSP-CALC-AMT
               DISPLAY "差異額="
               DISPLAY DSP-DIFF-AMT
               DISPLAY "最低支払差異="
               DISPLAY DSP-MIN-AMT
               DISPLAY "理由="
               DISPLAY WS-REASON
           END-IF.

       3500-BUILD-REASON.
           MOVE SPACES TO WS-REASON
           IF WS-BAL-FOUND NOT = CN-YES
               MOVE "前残高なし" TO WS-REASON
           ELSE
               IF WS-FEE-SUM NOT = ZERO
                   IF WS-LATE-FEE-SUM NOT = ZERO
                       MOVE "遅延手数料差異" TO WS-REASON
                   ELSE
                       IF WS-ANNUAL-FEE-SUM NOT = ZERO
                           MOVE "年会費差異" TO WS-REASON
                       ELSE
                           MOVE "その他手数料差異" TO WS-REASON
                       END-IF
                   END-IF
               ELSE
                   IF WS-SALES-SUM NOT = ZERO
                      OR WS-TAX-SUM NOT = ZERO
                       MOVE "売上税額差異" TO WS-REASON
                   ELSE
                       IF WS-MIN-DIFF-AMT NOT = ZERO
                           MOVE "最低支払額差異" TO WS-REASON
                       ELSE
                           MOVE "残高内訳差異" TO WS-REASON
                       END-IF
                   END-IF
               END-IF
           END-IF.

       9000-CLOSE-FILES.
           IF WS-BL-STATUS NOT = SPACES
               CLOSE CDBALF
               IF WS-BL-STATUS NOT = "00"
                   DISPLAY "CDBALF CLOSE ST="
                   DISPLAY WS-BL-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
               END-IF
           END-IF

           IF WS-BI-STATUS NOT = SPACES
               CLOSE CDBILLF
               IF WS-BI-STATUS NOT = "00"
                   DISPLAY "CDBILLF CLOSE ST="
                   DISPLAY WS-BI-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
               END-IF
           END-IF

           IF WS-SL-STATUS NOT = SPACES
               CLOSE CDSALESF
               IF WS-SL-STATUS NOT = "00"
                   DISPLAY "CDSALESF CLOSE ST="
                   DISPLAY WS-SL-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
               END-IF
           END-IF

           IF WS-FE-STATUS NOT = SPACES
               CLOSE CDFEEF
               IF WS-FE-STATUS NOT = "00"
                   DISPLAY "CDFEEF CLOSE ST="
                   DISPLAY WS-FE-STATUS
                   MOVE CN-YES TO WS-HARD-ERROR
               END-IF
           END-IF.
