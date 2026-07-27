       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB105B.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAUTHF3
               ASSIGN TO "CDAUTHF3"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AU-AUTH-NO
               FILE STATUS IS WS-ST-AUTH.

           SELECT CDREVF
               ASSIGN TO "CDREVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RV-CARD-NO
               FILE STATUS IS WS-ST-REV.

           SELECT CDCAPTF
               ASSIGN TO "CDCAPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-CAPT.

           SELECT CDRBALF
               ASSIGN TO "CDRBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-RBAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CDAUTHF3.
           COPY CDAUTHF3C.

       FD  CDREVF.
           COPY CDREVFC.

       FD  CDCAPTF.
           COPY CDCAPTC.

       FD  CDRBALF.
           COPY CDRBALFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-AUTH                 PIC XX.
           05 WS-ST-REV                  PIC XX.
           05 WS-ST-CAPT                 PIC XX.
           05 WS-ST-RBAL                 PIC XX.

       01  WS-SWITCHES.
           05 WS-EOF-AUTH                PIC X VALUE "N".
              88 AUTH-END                VALUE "Y".
           05 WS-HARD-ERR                PIC X VALUE "N".
              88 HARD-ERROR             VALUE "Y".
           05 WS-REV-FOUND-SW            PIC X VALUE "N".
              88 REV-FOUND              VALUE "Y".
           05 WS-CAPT-DUP-SW             PIC X VALUE "N".
              88 CAPT-DUP               VALUE "Y".
           05 WS-RBAL-FOUND-SW           PIC X VALUE "N".
              88 RBAL-FOUND             VALUE "Y".

       01  WS-CONSTANTS.
           05 WS-REV-RATE                PIC 9V9999 VALUE 0.0125.
           05 WS-ST-OK                   PIC XX VALUE "00".
           05 WS-ST-EOF                  PIC XX VALUE "10".
           05 WS-ST-NOT-FOUND            PIC XX VALUE "23".
           05 WS-REV-OK                  PIC XX VALUE "01".
           05 WS-REV-SUSPEND             PIC XX VALUE "02".
           05 WS-REV-CANCEL              PIC XX VALUE "03".
           05 WS-RSLD-CONF               PIC X  VALUE "C".
           05 WS-RSLD-SKIP               PIC X  VALUE "S".
           05 WS-AUTH-APPROVED           PIC X  VALUE "1".
           05 WS-AUTH-CANCEL             PIC X  VALUE "9".
           05 WS-REV-USE                 PIC X  VALUE "1".
           05 WS-CAPT-CONF               PIC X  VALUE "1".
           05 WS-MAX-CAPT                PIC 9(5) VALUE 20000.
           05 WS-MAX-RBAL                PIC 9(5) VALUE 20000.

       01  WS-COUNTERS.
           05 WS-CNT-AUTH-IN             PIC 9(9) VALUE ZERO.
           05 WS-CNT-CAPT-OUT            PIC 9(9) VALUE ZERO.
           05 WS-CNT-RBAL-OUT            PIC 9(9) VALUE ZERO.
           05 WS-CNT-REV-SALES           PIC 9(9) VALUE ZERO.
           05 WS-CNT-SKIP                PIC 9(9) VALUE ZERO.
           05 WS-CNT-ERR                 PIC 9(9) VALUE ZERO.
           05 WS-CNT-DUP                 PIC 9(9) VALUE ZERO.
           05 WS-CNT-CANCEL              PIC 9(9) VALUE ZERO.

       01  WS-WORK.
           05 WS-IDX                     PIC 9(5) COMP.
           05 WS-RBAL-IDX                PIC 9(5) COMP.
           05 WS-CAPTURE-ID-WK           PIC X(20).
           05 WS-SALES-DT-WK             PIC 9(8).
           05 WS-CYCLE-DT-WK             PIC 9(8).
           05 WS-CAPTURE-AMT-WK          PIC S9(13) COMP-3.
           05 WS-FEE-AMT                 PIC S9(13) COMP-3.
           05 WS-PRIN-AMT                PIC S9(13) COMP-3.
           05 WS-PAY-AMT                 PIC S9(13) COMP-3.
           05 WS-RSLD-STATUS             PIC X.
           05 WS-SLIDE-TIER              PIC XX.
           05 WS-YYMM                    PIC 9(6).
           05 WS-DD                      PIC 99.

       01  WS-CAPTURE-TABLE.
           05 WS-CAPT-COUNT              PIC 9(5) COMP VALUE ZERO.
           05 WS-CAPT-ENTRY OCCURS 20000 TIMES.
              10 WS-T-CAPTURE-ID         PIC X(20).

       01  WS-RBAL-TABLE.
           05 WS-RBAL-COUNT              PIC 9(5) COMP VALUE ZERO.
           05 WS-RBAL-ENTRY OCCURS 20000 TIMES.
              10 WS-T-RB-CARD-NO         PIC X(19).
              10 WS-T-RB-CYCLE-DT        PIC 9(8).
              10 WS-T-RB-REV-BAL-AMT     PIC S9(13) COMP-3.
              10 WS-T-RB-FEE-AMT         PIC S9(13) COMP-3.
              10 WS-T-RB-NEW-REV-AMT     PIC S9(13) COMP-3.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
               PERFORM 2000-PROCESS UNTIL AUTH-END OR HARD-ERROR
           END-IF
           IF NOT HARD-ERROR
               PERFORM 5000-WRITE-RBAL
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE ZERO TO RETURN-CODE
               DISPLAY "CB105B END IN=" WS-CNT-AUTH-IN
               DISPLAY "CAPT=" WS-CNT-CAPT-OUT
                       " RBAL=" WS-CNT-RBAL-OUT
                       " SKIP=" WS-CNT-SKIP
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDAUTHF3
           IF WS-ST-AUTH NOT = WS-ST-OK
               DISPLAY "OPEN CDAUTHF3 ERROR ST=" WS-ST-AUTH
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT CDREVF
               IF WS-ST-REV NOT = WS-ST-OK
                   DISPLAY "OPEN CDREVF ERROR ST=" WS-ST-REV
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT CDCAPTF
               IF WS-ST-CAPT NOT = WS-ST-OK
                   DISPLAY "OPEN CDCAPTF ERROR ST=" WS-ST-CAPT
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT CDRBALF
               IF WS-ST-RBAL NOT = WS-ST-OK
                   DISPLAY "OPEN CDRBALF ERROR ST=" WS-ST-RBAL
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               PERFORM 1100-READ-AUTH
           END-IF.

       1100-READ-AUTH.
           READ CDAUTHF3
               AT END
                   SET AUTH-END TO TRUE
               NOT AT END
                   ADD 1 TO WS-CNT-AUTH-IN
           END-READ
           IF WS-ST-AUTH NOT = WS-ST-OK
              AND WS-ST-AUTH NOT = WS-ST-EOF
               DISPLAY "READ CDAUTHF3 ERROR ST=" WS-ST-AUTH
               DISPLAY "AUTH-NO=" AU-AUTH-NO
               SET HARD-ERROR TO TRUE
           END-IF.

       2000-PROCESS.
           PERFORM 2100-EDIT-AUTH
           IF NOT HARD-ERROR
               PERFORM 1100-READ-AUTH
           END-IF.

       2100-EDIT-AUTH.
           MOVE ZERO TO WS-PRIN-AMT WS-FEE-AMT WS-PAY-AMT
           MOVE SPACES TO WS-SLIDE-TIER
           MOVE WS-RSLD-SKIP TO WS-RSLD-STATUS

           IF AU-AUTH-NO = SPACE
               DISPLAY "AUTH-NO IS BLANK"
               ADD 1 TO WS-CNT-ERR
               ADD 1 TO WS-CNT-SKIP
           ELSE
               IF AU-CARD-NO = SPACE
                   DISPLAY "CARD-NO IS BLANK AUTH=" AU-AUTH-NO
                   ADD 1 TO WS-CNT-ERR
                   ADD 1 TO WS-CNT-SKIP
               ELSE
                   IF AU-MERCHANT-ID = SPACE
                       DISPLAY "MERCHANT IS BLANK AUTH=" AU-AUTH-NO
                       ADD 1 TO WS-CNT-ERR
                       ADD 1 TO WS-CNT-SKIP
                   ELSE
                       IF AU-AUTH-AMT <= ZERO
                           DISPLAY "AMOUNT ERROR AUTH=" AU-AUTH-NO
                           ADD 1 TO WS-CNT-ERR
                           ADD 1 TO WS-CNT-SKIP
                       ELSE
                           PERFORM 2200-CHECK-AUTH-STATUS
                       END-IF
                   END-IF
               END-IF
           END-IF.

       2200-CHECK-AUTH-STATUS.
           IF AU-AUTH-STATUS = WS-AUTH-CANCEL
               DISPLAY "AUTH CANCEL SKIP AUTH=" AU-AUTH-NO
               ADD 1 TO WS-CNT-CANCEL
               ADD 1 TO WS-CNT-SKIP
           ELSE
               IF AU-AUTH-STATUS NOT = WS-AUTH-APPROVED
                   DISPLAY "AUTH STATUS ERROR AUTH=" AU-AUTH-NO
                   DISPLAY "STATUS=" AU-AUTH-STATUS
                   ADD 1 TO WS-CNT-ERR
                   ADD 1 TO WS-CNT-SKIP
               ELSE
                   PERFORM 2300-MAKE-CAPTURE-DATA
               END-IF
           END-IF.

       2300-MAKE-CAPTURE-DATA.
           MOVE AU-AUTH-DT TO WS-SALES-DT-WK
           MOVE AU-AUTH-AMT TO WS-CAPTURE-AMT-WK
           MOVE SPACE TO WS-CAPTURE-ID-WK
           STRING AU-AUTH-NO DELIMITED BY SIZE
                  AU-MERCHANT-ID DELIMITED BY SIZE
                  INTO WS-CAPTURE-ID-WK
           END-STRING

           PERFORM 2400-CHECK-DUP-CAPTURE
           IF CAPT-DUP
               DISPLAY "CAPTURE DUP AUTH=" AU-AUTH-NO
               DISPLAY "CAPTURE-ID=" WS-CAPTURE-ID-WK
               ADD 1 TO WS-CNT-DUP
               ADD 1 TO WS-CNT-SKIP
           ELSE
               PERFORM 2500-KEEP-CAPTURE-ID
               IF NOT HARD-ERROR
                   IF AU-REV-USE-FLG = WS-REV-USE
                       PERFORM 3000-PROCESS-REV
                   ELSE
                       PERFORM 4000-WRITE-CAPTURE
                   END-IF
               END-IF
           END-IF.

       2400-CHECK-DUP-CAPTURE.
           MOVE "N" TO WS-CAPT-DUP-SW
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-CAPT-COUNT OR CAPT-DUP
               IF WS-T-CAPTURE-ID(WS-IDX) = WS-CAPTURE-ID-WK
                   SET CAPT-DUP TO TRUE
               END-IF
           END-PERFORM.

       2500-KEEP-CAPTURE-ID.
           IF WS-CAPT-COUNT >= WS-MAX-CAPT
               DISPLAY "CAPTURE TABLE FULL COUNT=" WS-CAPT-COUNT
               SET HARD-ERROR TO TRUE
           ELSE
               ADD 1 TO WS-CAPT-COUNT
               MOVE WS-CAPTURE-ID-WK
                 TO WS-T-CAPTURE-ID(WS-CAPT-COUNT)
           END-IF.

       3000-PROCESS-REV.
           MOVE "N" TO WS-REV-FOUND-SW
           MOVE AU-CARD-NO TO RV-CARD-NO
           READ CDREVF KEY IS RV-CARD-NO
               INVALID KEY
                   MOVE "N" TO WS-REV-FOUND-SW
               NOT INVALID KEY
                   SET REV-FOUND TO TRUE
           END-READ

           IF WS-ST-REV NOT = WS-ST-OK
              AND WS-ST-REV NOT = WS-ST-NOT-FOUND
               DISPLAY "READ CDREVF ERROR ST=" WS-ST-REV
               DISPLAY "CARD=" AU-CARD-NO
               SET HARD-ERROR TO TRUE
           ELSE
               IF NOT REV-FOUND
                   DISPLAY "REV CONTRACT NOT FOUND AUTH=" AU-AUTH-NO
                   DISPLAY "CARD=" AU-CARD-NO
                   ADD 1 TO WS-CNT-SKIP
               ELSE
                   PERFORM 3100-CHECK-REV-STATUS
               END-IF
           END-IF.

       3100-CHECK-REV-STATUS.
           IF RV-REV-STATUS = WS-REV-OK
               MOVE WS-RSLD-CONF TO WS-RSLD-STATUS
               PERFORM 3200-CALC-CYCLE
               PERFORM 3300-CALC-FEE
               PERFORM 3400-ADD-RBAL
               IF NOT HARD-ERROR
                   PERFORM 4000-WRITE-CAPTURE
                   ADD 1 TO WS-CNT-REV-SALES
               END-IF
           ELSE
               IF RV-REV-STATUS = WS-REV-SUSPEND
                  OR RV-REV-STATUS = WS-REV-CANCEL
                   MOVE WS-RSLD-SKIP TO WS-RSLD-STATUS
                   MOVE ZERO TO WS-PRIN-AMT WS-FEE-AMT WS-PAY-AMT
                   DISPLAY "REV STATUS SKIP AUTH=" AU-AUTH-NO
                   DISPLAY "STATUS=" RV-REV-STATUS
                   ADD 1 TO WS-CNT-SKIP
               ELSE
                   DISPLAY "REV STATUS ERROR AUTH=" AU-AUTH-NO
                   DISPLAY "STATUS=" RV-REV-STATUS
                   ADD 1 TO WS-CNT-ERR
                   ADD 1 TO WS-CNT-SKIP
               END-IF
           END-IF.

       3200-CALC-CYCLE.
           DIVIDE AU-AUTH-DT BY 100 GIVING WS-YYMM
               REMAINDER WS-DD
           IF WS-DD <= 15
               COMPUTE WS-CYCLE-DT-WK = WS-YYMM * 100 + 15
           ELSE
               COMPUTE WS-CYCLE-DT-WK = WS-YYMM * 100 + 99
           END-IF.

       3300-CALC-FEE.
      *    リボ手数料 = 売上額 × 月利(0.0125) を切り捨て。
      *    元金の按分は当バッチでは行わない(残高更新は CB290S)。
           COMPUTE WS-FEE-AMT =
               FUNCTION INTEGER-PART (AU-AUTH-AMT * WS-REV-RATE).

       3400-ADD-RBAL.
           MOVE "N" TO WS-RBAL-FOUND-SW
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-RBAL-COUNT OR RBAL-FOUND
               IF WS-T-RB-CARD-NO(WS-IDX) = AU-CARD-NO
                  AND WS-T-RB-CYCLE-DT(WS-IDX) = WS-CYCLE-DT-WK
                   SET RBAL-FOUND TO TRUE
                   MOVE WS-IDX TO WS-RBAL-IDX
               END-IF
           END-PERFORM

           IF RBAL-FOUND
               ADD AU-AUTH-AMT TO
                   WS-T-RB-NEW-REV-AMT(WS-RBAL-IDX)
               ADD WS-FEE-AMT TO
                   WS-T-RB-FEE-AMT(WS-RBAL-IDX)
           ELSE
               IF WS-RBAL-COUNT >= WS-MAX-RBAL
                   DISPLAY "RBAL TABLE FULL COUNT=" WS-RBAL-COUNT
                   SET HARD-ERROR TO TRUE
               ELSE
                   ADD 1 TO WS-RBAL-COUNT
                   MOVE AU-CARD-NO
                     TO WS-T-RB-CARD-NO(WS-RBAL-COUNT)
                   MOVE WS-CYCLE-DT-WK
                     TO WS-T-RB-CYCLE-DT(WS-RBAL-COUNT)
                   MOVE ZERO
                     TO WS-T-RB-REV-BAL-AMT(WS-RBAL-COUNT)
                   MOVE WS-FEE-AMT
                     TO WS-T-RB-FEE-AMT(WS-RBAL-COUNT)
                   MOVE AU-AUTH-AMT
                     TO WS-T-RB-NEW-REV-AMT(WS-RBAL-COUNT)
               END-IF
           END-IF.

       4000-WRITE-CAPTURE.
           MOVE SPACES TO CDCAPTF-REC
           MOVE WS-CAPTURE-ID-WK  TO CP-CAPTURE-ID
           MOVE AU-AUTH-NO        TO CP-AUTH-NO
           MOVE AU-CARD-NO        TO CP-CARD-NO
           MOVE WS-SALES-DT-WK    TO CP-SALES-DT
           MOVE WS-CAPTURE-AMT-WK TO CP-CAPTURE-AMT
           MOVE AU-MERCHANT-ID    TO CP-MERCHANT-ID
           MOVE WS-CAPT-CONF      TO CP-CAPTURE-STATUS

           WRITE CDCAPTF-REC
           IF WS-ST-CAPT NOT = WS-ST-OK
               DISPLAY "WRITE CDCAPTF ERROR ST=" WS-ST-CAPT
               DISPLAY "CAPTURE-ID=" CP-CAPTURE-ID
               SET HARD-ERROR TO TRUE
           ELSE
               ADD 1 TO WS-CNT-CAPT-OUT
           END-IF.

       5000-WRITE-RBAL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-RBAL-COUNT OR HARD-ERROR
               MOVE SPACES TO CDRBALF-REC
               MOVE WS-T-RB-CARD-NO(WS-IDX)
                 TO RB-CARD-NO
               MOVE WS-T-RB-CYCLE-DT(WS-IDX)
                 TO RB-CYCLE-DT
               MOVE WS-T-RB-REV-BAL-AMT(WS-IDX)
                 TO RB-REV-BAL-AMT
               MOVE WS-T-RB-FEE-AMT(WS-IDX)
                 TO RB-CARRIED-FEE-AMT
               MOVE WS-T-RB-NEW-REV-AMT(WS-IDX)
                 TO RB-NEW-REV-AMT

               WRITE CDRBALF-REC
               IF WS-ST-RBAL NOT = WS-ST-OK
                   DISPLAY "WRITE CDRBALF ERROR ST=" WS-ST-RBAL
                   DISPLAY "CARD=" RB-CARD-NO
                   SET HARD-ERROR TO TRUE
               ELSE
                   ADD 1 TO WS-CNT-RBAL-OUT
               END-IF
           END-PERFORM.

       9000-CLOSE-FILES.
           CLOSE CDAUTHF3
           CLOSE CDREVF
           CLOSE CDCAPTF
           CLOSE CDRBALF.
