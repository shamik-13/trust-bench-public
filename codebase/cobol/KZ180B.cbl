       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ180B.
      ******************************************************************
      * プログラム : KZ180B 残高整合性検査バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS FS-KZACCTF.
           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZTRANF.
           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZFEEHF.
           SELECT KZINTAF ASSIGN TO "KZINTAF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZINTAF.
           SELECT KZBALER ASSIGN TO "KZBALER"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZBALER.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC.
       FD  KZTRANF.
           COPY KZTRANC.
       FD  KZFEEHF.
           COPY KZFEEHC.
       FD  KZINTAF.
           COPY KZINTAFC.
       FD  KZBALER.
           COPY KZBALERC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-KZACCTF              PIC XX VALUE SPACES.
           05 FS-KZTRANF              PIC XX VALUE SPACES.
           05 FS-KZFEEHF              PIC XX VALUE SPACES.
           05 FS-KZINTAF              PIC XX VALUE SPACES.
           05 FS-KZBALER              PIC XX VALUE SPACES.

       01  EOF-SWITCHES.
           05 AC-EOF-SW               PIC X VALUE 'N'.
              88 AC-EOF                     VALUE 'Y'.
           05 TR-EOF-SW               PIC X VALUE 'N'.
              88 TR-EOF                     VALUE 'Y'.
           05 FE-EOF-SW               PIC X VALUE 'N'.
              88 FE-EOF                     VALUE 'Y'.
           05 IA-EOF-SW               PIC X VALUE 'N'.
              88 IA-EOF                     VALUE 'Y'.

       01  WORK-AREA.
           05 WS-TODAY                PIC 9(08) VALUE ZERO.
           05 WS-EFFECT-GROUP         PIC X(04) VALUE SPACES.
           05 WS-RATE                 PIC 9V9(04) VALUE ZERO.
           05 WS-TRAN-SUM             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-FEE-SUM              PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-INT-SUM              PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-BASE-BAL             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-CALC-BAL             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-DIFF-AMT             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-ABS-DIFF             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-TRAN-COUNT           PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-FEE-COUNT            PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-INT-COUNT            PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-LAST-TRAN-CD         PIC X(04) VALUE SPACES.
           05 WS-SOURCE-PGM           PIC X(08) VALUE SPACES.
           05 WS-HARD-ERROR-SW        PIC X VALUE 'N'.
              88 HARD-ERROR                 VALUE 'Y'.

       01  COUNTER-AREA.
           05 CNT-ACCT-READ           PIC 9(09) COMP-3 VALUE ZERO.
           05 CNT-ERROR-WRITE         PIC 9(09) COMP-3 VALUE ZERO.
           05 CNT-KZ181S-SKIP         PIC 9(09) COMP-3 VALUE ZERO.
           05 CNT-TRAN-ORPHAN         PIC 9(09) COMP-3 VALUE ZERO.
           05 CNT-FEE-ORPHAN          PIC 9(09) COMP-3 VALUE ZERO.
           05 CNT-INT-ORPHAN          PIC 9(09) COMP-3 VALUE ZERO.

       01  LK-KZ181S-PARM.
           05 LK-KZ181S-KYC-STATUS    PIC X(02).
           05 LK-KZ181S-TRAN-CD       PIC X(04).
           05 LK-KZ181S-SA-GAKU       PIC S9(13)V99 COMP-3.
           05 LK-KZ181S-ERROR-CD      PIC X(06).
           05 LK-KZ181S-SEVERITY      PIC 9(02).
           05 LK-KZ181S-OUTPUT-SW     PIC X.
              88 LK-KZ181S-OUTPUT-OK        VALUE 'Y'.
              88 LK-KZ181S-OUTPUT-NG        VALUE 'N'.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
              PERFORM 2000-PROCESS UNTIL AC-EOF OR HARD-ERROR
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN INPUT KZACCTF
                      KZTRANF
                      KZFEEHF
                      KZINTAF
                OUTPUT KZBALER
           IF FS-KZACCTF NOT = '00'
              DISPLAY 'KZACCTF OPEN ERROR ST=' FS-KZACCTF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZTRANF NOT = '00'
              DISPLAY 'KZTRANF OPEN ERROR ST=' FS-KZTRANF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZFEEHF NOT = '00'
              DISPLAY 'KZFEEHF OPEN ERROR ST=' FS-KZFEEHF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZINTAF NOT = '00'
              DISPLAY 'KZINTAF OPEN ERROR ST=' FS-KZINTAF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZBALER NOT = '00'
              DISPLAY 'KZBALER OPEN ERROR ST=' FS-KZBALER
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              PERFORM 1100-READ-ACCT
              PERFORM 1200-READ-TRAN
              PERFORM 1300-READ-FEE
              PERFORM 1400-READ-INT
           END-IF.

       1100-READ-ACCT.
           READ KZACCTF
              AT END
                 SET AC-EOF TO TRUE
              NOT AT END
                 ADD 1 TO CNT-ACCT-READ
           END-READ
           IF FS-KZACCTF NOT = '00' AND FS-KZACCTF NOT = '10'
              DISPLAY 'KZACCTF READ ERROR ST=' FS-KZACCTF
              SET HARD-ERROR TO TRUE
           END-IF.

       1200-READ-TRAN.
           READ KZTRANF
              AT END
                 SET TR-EOF TO TRUE
           END-READ
           IF FS-KZTRANF NOT = '00' AND FS-KZTRANF NOT = '10'
              DISPLAY 'KZTRANF READ ERROR ST=' FS-KZTRANF
              SET HARD-ERROR TO TRUE
           END-IF.

       1300-READ-FEE.
           READ KZFEEHF
              AT END
                 SET FE-EOF TO TRUE
           END-READ
           IF FS-KZFEEHF NOT = '00' AND FS-KZFEEHF NOT = '10'
              DISPLAY 'KZFEEHF READ ERROR ST=' FS-KZFEEHF
              SET HARD-ERROR TO TRUE
           END-IF.

       1400-READ-INT.
           READ KZINTAF
              AT END
                 SET IA-EOF TO TRUE
           END-READ
           IF FS-KZINTAF NOT = '00' AND FS-KZINTAF NOT = '10'
              DISPLAY 'KZINTAF READ ERROR ST=' FS-KZINTAF
              SET HARD-ERROR TO TRUE
           END-IF.

       2000-PROCESS.
           PERFORM 2100-CLEAR-ACCOUNT-WORK
           PERFORM 2200-SET-GROUP-RATE
           PERFORM 2300-COLLECT-TRAN
           PERFORM 2400-COLLECT-FEE
           PERFORM 2500-COLLECT-INT
           IF NOT HARD-ERROR
              PERFORM 2600-CHECK-BALANCE
              PERFORM 1100-READ-ACCT
           END-IF.

       2100-CLEAR-ACCOUNT-WORK.
           MOVE ZERO TO WS-TRAN-SUM
                        WS-FEE-SUM
                        WS-INT-SUM
                        WS-BASE-BAL
                        WS-CALC-BAL
                        WS-DIFF-AMT
                        WS-ABS-DIFF
                        WS-TRAN-COUNT
                        WS-FEE-COUNT
                        WS-INT-COUNT
           MOVE SPACES TO WS-LAST-TRAN-CD
                          WS-SOURCE-PGM.

       2200-SET-GROUP-RATE.
           EVALUATE AC-GROUP-CODE
              WHEN 'STD0'
                 MOVE 'STD0' TO WS-EFFECT-GROUP
                 MOVE 0.0150 TO WS-RATE
              WHEN 'GLD1'
                 MOVE 'GLD1' TO WS-EFFECT-GROUP
                 MOVE 0.0100 TO WS-RATE
              WHEN 'PLT2'
                 MOVE 'PLT2' TO WS-EFFECT-GROUP
                 MOVE 0.0080 TO WS-RATE
              WHEN 'EXMP'
                 MOVE 'EXMP' TO WS-EFFECT-GROUP
                 MOVE 0.0000 TO WS-RATE
              WHEN 'PREM'
                 MOVE 'PREM' TO WS-EFFECT-GROUP
                 MOVE 0.0080 TO WS-RATE
              WHEN OTHER
                 MOVE 'STD0' TO WS-EFFECT-GROUP
                 MOVE 0.0150 TO WS-RATE
                 DISPLAY 'BAD GROUP TO STD0 ACCT='
                         AC-ACCT-ID
           END-EVALUATE.

       2300-COLLECT-TRAN.
           PERFORM UNTIL TR-EOF OR TR-ACCT-ID >= AC-ACCT-ID
              ADD 1 TO CNT-TRAN-ORPHAN
              DISPLAY 'ORPHAN TRAN ACCT='
                      TR-ACCT-ID
              PERFORM 1200-READ-TRAN
           END-PERFORM
           PERFORM UNTIL TR-EOF OR TR-ACCT-ID NOT = AC-ACCT-ID
              EVALUATE TR-TRAN-CD
                 WHEN 'SALE'
                 WHEN 'CASH'
                    ADD TR-TRAN-AMT TO WS-TRAN-SUM
                 WHEN 'PYMT'
                 WHEN 'REVR'
                    SUBTRACT TR-TRAN-AMT FROM WS-TRAN-SUM
                 WHEN OTHER
                    DISPLAY 'BAD TRAN CD ACCT='
                            AC-ACCT-ID
                            ' CD='
                            TR-TRAN-CD
                    ADD TR-TRAN-AMT TO WS-TRAN-SUM
              END-EVALUATE
              MOVE TR-TRAN-CD TO WS-LAST-TRAN-CD
              ADD 1 TO WS-TRAN-COUNT
              PERFORM 1200-READ-TRAN
           END-PERFORM.

       2400-COLLECT-FEE.
           PERFORM UNTIL FE-EOF OR FE-ACCT-ID >= AC-ACCT-ID
              ADD 1 TO CNT-FEE-ORPHAN
              DISPLAY 'ORPHAN FEE ACCT='
                      FE-ACCT-ID
              PERFORM 1300-READ-FEE
           END-PERFORM
           PERFORM UNTIL FE-EOF OR FE-ACCT-ID NOT = AC-ACCT-ID
              IF FE-EXEMPT-FLAG = 'Y' OR WS-EFFECT-GROUP = 'EXMP'
                 CONTINUE
              ELSE
                 IF FE-CAP-FLAG = 'Y'
                    IF FE-FEE-YTD < AC-OVER-AMT
                       ADD FE-FEE-AMT TO WS-FEE-SUM
                    ELSE
                       DISPLAY 'FEE CAP REACHED ACCT='
                               AC-ACCT-ID
                    END-IF
                 ELSE
                    ADD FE-FEE-AMT TO WS-FEE-SUM
                 END-IF
              END-IF
              ADD 1 TO WS-FEE-COUNT
              PERFORM 1300-READ-FEE
           END-PERFORM.

       2500-COLLECT-INT.
           PERFORM UNTIL IA-EOF OR IA-ACCT-ID >= AC-ACCT-ID
              ADD 1 TO CNT-INT-ORPHAN
              DISPLAY 'ORPHAN INT ACCT='
                      IA-ACCT-ID
              PERFORM 1400-READ-INT
           END-PERFORM
           PERFORM UNTIL IA-EOF OR IA-ACCT-ID NOT = AC-ACCT-ID
              IF WS-INT-COUNT = ZERO
                 MOVE IA-CYCLE-BAL TO WS-BASE-BAL
              END-IF
              IF WS-EFFECT-GROUP = 'EXMP'
                 IF IA-INT-AMT NOT = ZERO
                    DISPLAY 'EXEMPT INT EXISTS ACCT='
                            AC-ACCT-ID
                 END-IF
              ELSE
                 ADD IA-INT-AMT TO WS-INT-SUM
              END-IF
              ADD 1 TO WS-INT-COUNT
              PERFORM 1400-READ-INT
           END-PERFORM.

       2600-CHECK-BALANCE.
           IF WS-INT-COUNT = ZERO
              MOVE AC-CYCLE-BAL TO WS-BASE-BAL
           END-IF
           COMPUTE WS-CALC-BAL ROUNDED =
                   WS-BASE-BAL + WS-TRAN-SUM - WS-FEE-SUM + WS-INT-SUM
           COMPUTE WS-DIFF-AMT = AC-CYCLE-BAL - WS-CALC-BAL
           IF WS-DIFF-AMT NOT = ZERO
              PERFORM 2700-SELECT-SOURCE
              PERFORM 2800-CALL-KZ181S
              IF NOT HARD-ERROR
                 IF LK-KZ181S-OUTPUT-OK
                    PERFORM 2900-WRITE-ERROR
                 ELSE
                    ADD 1 TO CNT-KZ181S-SKIP
                    DISPLAY 'KZ181S SKIP ACCT='
                            AC-ACCT-ID
                            ' ERR='
                            LK-KZ181S-ERROR-CD
                            ' SEV='
                            LK-KZ181S-SEVERITY
                 END-IF
              END-IF
           END-IF.

       2700-SELECT-SOURCE.
           COMPUTE WS-ABS-DIFF = FUNCTION ABS(WS-DIFF-AMT)
           EVALUATE TRUE
              WHEN WS-FEE-COUNT > ZERO
                   AND WS-ABS-DIFF = WS-FEE-SUM
                 MOVE 'KZ150B' TO WS-SOURCE-PGM
              WHEN WS-INT-COUNT > ZERO
                   AND WS-ABS-DIFF = WS-INT-SUM
                 MOVE 'KZ160B' TO WS-SOURCE-PGM
              WHEN WS-TRAN-COUNT > ZERO
                 MOVE 'KZ110B' TO WS-SOURCE-PGM
              WHEN OTHER
                 MOVE 'KZ120S' TO WS-SOURCE-PGM
           END-EVALUATE.

       2800-CALL-KZ181S.
           MOVE AC-KYC-STATUS TO LK-KZ181S-KYC-STATUS
           MOVE WS-LAST-TRAN-CD TO LK-KZ181S-TRAN-CD
           MOVE WS-DIFF-AMT TO LK-KZ181S-SA-GAKU
           MOVE SPACES TO LK-KZ181S-ERROR-CD
           MOVE ZERO TO LK-KZ181S-SEVERITY
           MOVE 'N' TO LK-KZ181S-OUTPUT-SW
           CALL 'KZ181S' USING LK-KZ181S-PARM
              ON EXCEPTION
                 DISPLAY 'KZ181S CALL ERROR ACCT='
                         AC-ACCT-ID
                 MOVE 'S18199' TO LK-KZ181S-ERROR-CD
                 MOVE 12 TO LK-KZ181S-SEVERITY
                 SET HARD-ERROR TO TRUE
           END-CALL.

       2900-WRITE-ERROR.
           INITIALIZE KZBALER-REC
           MOVE AC-ACCT-ID TO BE-ACCT-ID
           MOVE WS-TODAY TO BE-EDIT-DT
           MOVE LK-KZ181S-ERROR-CD TO BE-ERROR-CD
           MOVE WS-SOURCE-PGM TO BE-SOURCE-PGM
           MOVE WS-DIFF-AMT TO BE-DIFF-AMT
           WRITE KZBALER-REC
           IF FS-KZBALER = '00'
              ADD 1 TO CNT-ERROR-WRITE
           ELSE
              DISPLAY 'KZBALER WRITE ERROR ST='
                      FS-KZBALER
                      ' ACCT='
                      AC-ACCT-ID
              SET HARD-ERROR TO TRUE
           END-IF.

       9000-FINALIZE.
           CLOSE KZACCTF KZTRANF KZFEEHF KZINTAF KZBALER
           IF FS-KZACCTF NOT = '00'
              DISPLAY 'KZACCTF CLOSE ST=' FS-KZACCTF
           END-IF
           IF FS-KZTRANF NOT = '00'
              DISPLAY 'KZTRANF CLOSE ST=' FS-KZTRANF
           END-IF
           IF FS-KZFEEHF NOT = '00'
              DISPLAY 'KZFEEHF CLOSE ST=' FS-KZFEEHF
           END-IF
           IF FS-KZINTAF NOT = '00'
              DISPLAY 'KZINTAF CLOSE ST=' FS-KZINTAF
           END-IF
           IF FS-KZBALER NOT = '00'
              DISPLAY 'KZBALER CLOSE ST=' FS-KZBALER
           END-IF
           DISPLAY 'KZ180B ACCT READ=' CNT-ACCT-READ
           DISPLAY 'KZ180B ERROR WRITE=' CNT-ERROR-WRITE
           DISPLAY 'KZ180B KZ181S SKIP=' CNT-KZ181S-SKIP
           DISPLAY 'KZ180B TRAN ORPHAN=' CNT-TRAN-ORPHAN
           DISPLAY 'KZ180B FEE ORPHAN=' CNT-FEE-ORPHAN
           DISPLAY 'KZ180B INT ORPHAN=' CNT-INT-ORPHAN
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
              DISPLAY 'KZ180B ABEND'
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY 'KZ180B NORMAL END'
           END-IF.
