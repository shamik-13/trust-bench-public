       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ620B.
       AUTHOR. TRUST-BANK-BATCH.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXIF
               ASSIGN TO "KZTXIF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZTXIF-ST.
           SELECT KZTXRF
               ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZTXRF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZTXIF.
       COPY KZTXIFC.
      *
       FD  KZTXRF.
       COPY KZTXRFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-KZTXIF-ST             PIC X(02) VALUE SPACE.
               88  KZTXIF-OK                     VALUE "00".
               88  KZTXIF-EOF                    VALUE "10".
           05  WS-KZTXRF-ST             PIC X(02) VALUE SPACE.
               88  KZTXRF-OK                     VALUE "00".
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                PIC X     VALUE "N".
               88  EOF-YES                        VALUE "Y".
               88  EOF-NO                         VALUE "N".
           05  WS-ABEND-SW              PIC X     VALUE "N".
               88  ABEND-YES                      VALUE "Y".
               88  ABEND-NO                       VALUE "N".
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT              PIC 9(09) VALUE ZERO.
           05  WS-WRITE-CNT             PIC 9(09) VALUE ZERO.
      *
       01  WS-TOTALS.
           05  WS-NATIONAL-TOTAL        PIC 9(15) VALUE ZERO.
           05  WS-LOCAL-TOTAL           PIC 9(15) VALUE ZERO.
           05  WS-TAX-TOTAL             PIC 9(15) VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-NATIONAL-RATE         PIC 9V9(05) VALUE ZERO.
           05  WS-LOCAL-RATE            PIC 9V9(05) VALUE ZERO.
           05  WS-CALC-NATIONAL         PIC 9(15)V9(06) VALUE ZERO.
           05  WS-CALC-LOCAL            PIC 9(15)V9(06) VALUE ZERO.
           05  WS-NATIONAL-TAX          PIC 9(15) VALUE ZERO.
           05  WS-LOCAL-TAX             PIC 9(15) VALUE ZERO.
           05  WS-TOTAL-TAX             PIC 9(15) VALUE ZERO.
           05  WS-NET-AMT               PIC 9(15) VALUE ZERO.
      *
       01  WS-CONSTANTS.
           05  CT-ACCT-GENERAL          PIC X(02) VALUE "01".
           05  CT-ACCT-CORP             PIC X(02) VALUE "02".
           05  CT-ACCT-NISA             PIC X(02) VALUE "03".
           05  CT-RATE-NATIONAL         PIC 9V9(05) VALUE 0.15315.
           05  CT-RATE-LOCAL            PIC 9V9(05) VALUE 0.05000.
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           DISPLAY "KZ620B 源泉徴収税計算バッチ 開始"
           PERFORM OPEN-RTN
           IF ABEND-NO
               PERFORM READ-RTN
               PERFORM UNTIL EOF-YES OR ABEND-YES
                   PERFORM PROCESS-RTN
                   IF ABEND-NO
                       PERFORM READ-RTN
                   END-IF
               END-PERFORM
           END-IF
           PERFORM CLOSE-RTN
           IF ABEND-YES
               MOVE 8 TO RETURN-CODE
               DISPLAY "KZ620B 異常終了 RC=08"
           ELSE
               PERFORM END-MESSAGE-RTN
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
      *
       OPEN-RTN.
           OPEN INPUT KZTXIF
           IF NOT KZTXIF-OK
               DISPLAY "KZTXIF オープン失敗 ST=" WS-KZTXIF-ST
               SET ABEND-YES TO TRUE
           END-IF
           IF ABEND-NO
               OPEN OUTPUT KZTXRF
               IF NOT KZTXRF-OK
                   DISPLAY "KZTXRF オープン失敗 ST=" WS-KZTXRF-ST
                   SET ABEND-YES TO TRUE
               END-IF
           END-IF.
      *
       READ-RTN.
           READ KZTXIF
               AT END
                   SET EOF-YES TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
           IF NOT KZTXIF-OK AND NOT KZTXIF-EOF
               DISPLAY "KZTXIF 読込失敗 ST=" WS-KZTXIF-ST
               SET ABEND-YES TO TRUE
           END-IF.
      *
       PROCESS-RTN.
           PERFORM VALIDATE-INPUT-RTN
           IF ABEND-NO
               PERFORM SELECT-RATE-RTN
               PERFORM CALCULATE-TAX-RTN
               PERFORM WRITE-RTN
           END-IF.
      *
       VALIDATE-INPUT-RTN.
           IF TI-ACCT-NO = SPACE
               DISPLAY "KZTXIF 口座番号不正"
               SET ABEND-YES TO TRUE
           END-IF
           IF ABEND-NO
               IF TI-ACCT-TYPE NOT = CT-ACCT-GENERAL
                  AND TI-ACCT-TYPE NOT = CT-ACCT-CORP
                  AND TI-ACCT-TYPE NOT = CT-ACCT-NISA
                   DISPLAY "KZTXIF 口座区分不正"
                   DISPLAY "口座=" TI-ACCT-NO
                   DISPLAY "口座区分=" TI-ACCT-TYPE
                   SET ABEND-YES TO TRUE
               END-IF
           END-IF
           IF ABEND-NO
               IF TI-INT-AMT < ZERO
                   DISPLAY "KZTXIF 利息金額不正"
                   DISPLAY "口座=" TI-ACCT-NO
                   SET ABEND-YES TO TRUE
               END-IF
           END-IF.
      *
       SELECT-RATE-RTN.
           MOVE ZERO TO WS-NATIONAL-RATE
           MOVE ZERO TO WS-LOCAL-RATE
           EVALUATE TI-ACCT-TYPE
               WHEN CT-ACCT-GENERAL
                   MOVE CT-RATE-NATIONAL TO WS-NATIONAL-RATE
                   MOVE CT-RATE-LOCAL    TO WS-LOCAL-RATE
               WHEN CT-ACCT-CORP
                   MOVE CT-RATE-NATIONAL TO WS-NATIONAL-RATE
               WHEN CT-ACCT-NISA
                   CONTINUE
           END-EVALUATE.
      *
       CALCULATE-TAX-RTN.
           MOVE ZERO TO WS-CALC-NATIONAL
           MOVE ZERO TO WS-CALC-LOCAL
           MOVE ZERO TO WS-NATIONAL-TAX
           MOVE ZERO TO WS-LOCAL-TAX
           MOVE ZERO TO WS-TOTAL-TAX
           MOVE ZERO TO WS-NET-AMT
      *
           COMPUTE WS-CALC-NATIONAL =
                   TI-INT-AMT * WS-NATIONAL-RATE
           COMPUTE WS-CALC-LOCAL =
                   TI-INT-AMT * WS-LOCAL-RATE
           MOVE WS-CALC-NATIONAL TO WS-NATIONAL-TAX
           MOVE WS-CALC-LOCAL    TO WS-LOCAL-TAX
           COMPUTE WS-TOTAL-TAX = WS-NATIONAL-TAX + WS-LOCAL-TAX
           COMPUTE WS-NET-AMT = TI-INT-AMT - WS-TOTAL-TAX.
      *
       WRITE-RTN.
           INITIALIZE KZTXRF-REC
           MOVE TI-ACCT-NO        TO TR-ACCT-NO
           MOVE TI-ACCT-TYPE      TO TR-ACCT-TYPE
           MOVE TI-INT-AMT        TO TR-GROSS-INT-AMT
           MOVE WS-NATIONAL-TAX   TO TR-NATIONAL-TAX-AMT
           MOVE WS-LOCAL-TAX      TO TR-LOCAL-TAX-AMT
           MOVE WS-TOTAL-TAX      TO TR-TOTAL-TAX-AMT
           MOVE WS-NET-AMT        TO TR-NET-AMT
      *
           WRITE KZTXRF-REC
           IF KZTXRF-OK
               ADD 1 TO WS-WRITE-CNT
               ADD WS-NATIONAL-TAX TO WS-NATIONAL-TOTAL
               ADD WS-LOCAL-TAX    TO WS-LOCAL-TOTAL
               ADD WS-TOTAL-TAX    TO WS-TAX-TOTAL
           ELSE
               DISPLAY "KZTXRF 書込失敗 ST=" WS-KZTXRF-ST
               DISPLAY "口座=" TI-ACCT-NO
               SET ABEND-YES TO TRUE
           END-IF.
      *
       CLOSE-RTN.
           CLOSE KZTXIF
           IF NOT KZTXIF-OK
              AND NOT KZTXIF-EOF
               DISPLAY "KZTXIF クローズ失敗 ST=" WS-KZTXIF-ST
               SET ABEND-YES TO TRUE
           END-IF
           CLOSE KZTXRF
           IF NOT KZTXRF-OK
               DISPLAY "KZTXRF クローズ失敗 ST=" WS-KZTXRF-ST
               SET ABEND-YES TO TRUE
           END-IF.
      *
       END-MESSAGE-RTN.
           DISPLAY "KZ620B 正常終了"
           DISPLAY "読込件数=" WS-READ-CNT
           DISPLAY "書込件数=" WS-WRITE-CNT
           DISPLAY "国税合計=" WS-NATIONAL-TOTAL
           DISPLAY "地方税合計=" WS-LOCAL-TOTAL
           DISPLAY "税額合計=" WS-TAX-TOTAL.
