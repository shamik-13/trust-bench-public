       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ450B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  令和03.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和04.10.03  システム部 勘定系チーム  利率改定対応
      * 1.02  令和06.01.15  システム部 勘定系チーム  端数処理見直し

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDBALF ASSIGN TO "KZDBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZDBALF.
           SELECT KZIRMF ASSIGN TO "KZIRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RM-RATE-KEY
               FILE STATUS IS FS-KZIRMF.
           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZCYRF.
           SELECT KZCTLF ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS FS-KZCTLF.
           SELECT KZODBF ASSIGN TO "KZODBF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZODBF.
           SELECT KZINTF ASSIGN TO "KZINTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZINTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZDBALF.
           COPY KZDBALFC.
       FD  KZIRMF.
           COPY KZIRMFC.
       FD  KZCYRF.
           COPY KZCYRFC.
       FD  KZCTLF.
           COPY KZCTLFC.
       FD  KZODBF.
           COPY KZODBFC.
       FD  KZINTF.
           COPY KZINTFC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                 PIC X(08) VALUE "KZ450B".
       01  WS-CTL-KEY                PIC X(16) VALUE "KZ450B".
       01  WS-ABEND-FLAG             PIC X(01) VALUE "N".
           88  ABEND-OCCURRED                  VALUE "Y".
       01  WS-EOF-DB                 PIC X(01) VALUE "N".
           88  EOF-DB                          VALUE "Y".
       01  WS-EOF-CR                 PIC X(01) VALUE "N".
           88  EOF-CR                          VALUE "Y".

       01  WS-FILE-STATUS.
           05  FS-KZDBALF            PIC X(02) VALUE SPACES.
           05  FS-KZIRMF             PIC X(02) VALUE SPACES.
           05  FS-KZCYRF             PIC X(02) VALUE SPACES.
           05  FS-KZCTLF             PIC X(02) VALUE SPACES.
           05  FS-KZODBF             PIC X(02) VALUE SPACES.
           05  FS-KZINTF             PIC X(02) VALUE SPACES.

       01  WS-COUNTERS.
           05  CNT-DB-READ           PIC 9(09) VALUE ZERO.
           05  CNT-NEG-SELECT        PIC 9(09) VALUE ZERO.
           05  CNT-OD-WRITE          PIC 9(09) VALUE ZERO.
           05  CNT-IN-WRITE          PIC 9(09) VALUE ZERO.
           05  CNT-SKIP              PIC 9(09) VALUE ZERO.
           05  CNT-CYCLE             PIC 9(04) VALUE ZERO.

       01  WS-WORK.
           05  WS-PRODUCT-CD         PIC X(03) VALUE SPACES.
           05  WS-RATE-KEY           PIC X(20) VALUE SPACES.
           05  WS-CYCLE-ID           PIC X(10) VALUE SPACES.
           05  WS-CYCLE-NOMINAL-DT   PIC 9(08) VALUE ZERO.
           05  WS-CYCLE-NOMINAL-X REDEFINES WS-CYCLE-NOMINAL-DT
                                      PIC X(08).
           05  WS-CYCLE-FOUND        PIC X(01) VALUE "N".
           05  WS-MONTH              PIC 9(02) VALUE ZERO.
           05  WS-FIXED-DAYS         PIC 9(03) VALUE ZERO.
           05  WS-DAY-DENOM          PIC 9(03) VALUE 360.
           05  WS-NEG-AVG-BAL        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-ABS-BAL            PIC 9(13)V99 COMP-3 VALUE ZERO.
           05  WS-OD-RATE            PIC 9(03)V9(6) COMP-3 VALUE ZERO.
           05  WS-INT-WORK           PIC 9(15)V9(9) COMP-3 VALUE ZERO.
           05  WS-INT-AMT            PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-IX                 PIC 9(04) COMP VALUE ZERO.

       01  WS-CYCLE-TABLE.
           05  WS-CYCLE-ENTRY OCCURS 500 TIMES.
               10  T-CYCLE-ID        PIC X(10) VALUE SPACES.
               10  T-NOMINAL-DT      PIC 9(08) VALUE ZERO.
               10  T-ROLLED-FLAG     PIC X(01) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT ABEND-OCCURRED
               PERFORM 2000-LOAD-CONTROL
           END-IF
           IF NOT ABEND-OCCURRED
               PERFORM 3000-LOAD-CYCLES
           END-IF
           IF NOT ABEND-OCCURRED
               PERFORM 4000-PROCESS-DBALF
           END-IF
           IF NOT ABEND-OCCURRED
               PERFORM 8000-END-CONTROL
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF ABEND-OCCURRED
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE ZERO TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZDBALF KZIRMF KZCYRF
           OPEN I-O KZCTLF
           OPEN OUTPUT KZODBF KZINTF
           IF FS-KZDBALF NOT = "00"
               PERFORM 9900-SET-ABEND
           END-IF
           IF FS-KZIRMF NOT = "00"
               PERFORM 9900-SET-ABEND
           END-IF
           IF FS-KZCYRF NOT = "00"
               PERFORM 9900-SET-ABEND
           END-IF
           IF FS-KZCTLF NOT = "00"
               PERFORM 9900-SET-ABEND
           END-IF
           IF FS-KZODBF NOT = "00"
               PERFORM 9900-SET-ABEND
           END-IF
           IF FS-KZINTF NOT = "00"
               PERFORM 9900-SET-ABEND
           END-IF.

       2000-LOAD-CONTROL.
           MOVE WS-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
               INVALID KEY
                   PERFORM 9900-SET-ABEND
           END-READ
           IF FS-KZCTLF NOT = "00" AND NOT ABEND-OCCURRED
               PERFORM 9900-SET-ABEND
           END-IF
           IF NOT ABEND-OCCURRED
               IF CL-RUN-STATUS = "R" OR CL-RUN-STATUS = "N"
                   MOVE "R" TO CL-RUN-STATUS
                   MOVE WS-PGM-ID TO CL-LAST-PGM-ID
                   MOVE SPACE TO CL-RESTART-POINT
                   REWRITE KZCTLF-REC
                   IF FS-KZCTLF NOT = "00"
                       PERFORM 9900-SET-ABEND
                   END-IF
               ELSE
                   PERFORM 9900-SET-ABEND
               END-IF
           END-IF.

       3000-LOAD-CYCLES.
           PERFORM UNTIL EOF-CR OR ABEND-OCCURRED
               READ KZCYRF
                   AT END
                       SET EOF-CR TO TRUE
                   NOT AT END
                       IF FS-KZCYRF = "00"
                           PERFORM 3100-STORE-CYCLE
                       ELSE
                           PERFORM 9900-SET-ABEND
                       END-IF
               END-READ
           END-PERFORM.

       3100-STORE-CYCLE.
           IF CR-CYCLE-ID = SPACE
               PERFORM 9900-SET-ABEND
           ELSE
               IF CR-NOMINAL-DT < 19000101
                   OR CR-NOMINAL-DT > 20991231
                   PERFORM 9900-SET-ABEND
               ELSE
                   IF CR-ROLLED-FLAG NOT = "Y"
                       AND CR-ROLLED-FLAG NOT = "N"
                       PERFORM 9900-SET-ABEND
                   ELSE
                       ADD 1 TO CNT-CYCLE
                       IF CNT-CYCLE > 500
                           PERFORM 9900-SET-ABEND
                       ELSE
                           MOVE CR-CYCLE-ID TO T-CYCLE-ID(CNT-CYCLE)
                           MOVE CR-NOMINAL-DT TO T-NOMINAL-DT(CNT-CYCLE)
                           MOVE CR-ROLLED-FLAG
                               TO T-ROLLED-FLAG(CNT-CYCLE)
                       END-IF
                   END-IF
               END-IF
           END-IF.

       4000-PROCESS-DBALF.
           PERFORM 4100-READ-DBALF
           PERFORM UNTIL EOF-DB OR ABEND-OCCURRED
               ADD 1 TO CNT-DB-READ
               IF DB-AVG-DAILY-BAL < ZERO
                   PERFORM 5000-PROCESS-NEGATIVE
               END-IF
               PERFORM 4100-READ-DBALF
           END-PERFORM.

       4100-READ-DBALF.
           READ KZDBALF
               AT END
                   SET EOF-DB TO TRUE
               NOT AT END
                   IF FS-KZDBALF NOT = "00"
                       PERFORM 9900-SET-ABEND
                   END-IF
           END-READ.

       5000-PROCESS-NEGATIVE.
           ADD 1 TO CNT-NEG-SELECT
           MOVE DB-CYCLE-ID TO WS-CYCLE-ID
           PERFORM 5100-FIND-CYCLE
           IF WS-CYCLE-FOUND NOT = "Y"
               ADD 1 TO CNT-SKIP
           ELSE
               PERFORM 5200-VALIDATE-ACCOUNT
               IF NOT ABEND-OCCURRED
                   PERFORM 5300-READ-RATE
               END-IF
               IF NOT ABEND-OCCURRED
                   PERFORM 5400-DECIDE-FIXED-DAYS
               END-IF
               IF NOT ABEND-OCCURRED
                   PERFORM 5500-CALCULATE-INTEREST
                   PERFORM 5600-WRITE-DETAIL
                   PERFORM 5700-WRITE-INTEREST
                   PERFORM 5800-UPDATE-RESTART
               END-IF
           END-IF.

       5100-FIND-CYCLE.
           MOVE "N" TO WS-CYCLE-FOUND
           MOVE ZERO TO WS-CYCLE-NOMINAL-DT
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > CNT-CYCLE OR WS-CYCLE-FOUND = "Y"
               IF T-CYCLE-ID(WS-IX) = WS-CYCLE-ID
                   MOVE "Y" TO WS-CYCLE-FOUND
                   MOVE T-NOMINAL-DT(WS-IX) TO WS-CYCLE-NOMINAL-DT
               END-IF
           END-PERFORM.

       5200-VALIDATE-ACCOUNT.
           IF DB-ACCT-NO = SPACE
               PERFORM 9900-SET-ABEND
           END-IF
           IF DB-PERIOD-START-DT < 19000101
               PERFORM 9900-SET-ABEND
           END-IF
           IF DB-PERIOD-START-DT > WS-CYCLE-NOMINAL-DT
               PERFORM 9900-SET-ABEND
           END-IF.

       5300-READ-RATE.
           MOVE DB-ACCT-NO(1:3) TO WS-PRODUCT-CD
           MOVE SPACES TO WS-RATE-KEY
           STRING WS-PRODUCT-CD DELIMITED BY SIZE
                  WS-CYCLE-NOMINAL-X DELIMITED BY SIZE
                  INTO WS-RATE-KEY
           END-STRING
           MOVE WS-RATE-KEY TO RM-RATE-KEY
           READ KZIRMF KEY IS RM-RATE-KEY
               INVALID KEY
                   PERFORM 9900-SET-ABEND
           END-READ
           IF FS-KZIRMF NOT = "00" AND NOT ABEND-OCCURRED
               PERFORM 9900-SET-ABEND
           END-IF
           IF NOT ABEND-OCCURRED
               IF RM-APPROVAL-STAT NOT = "A"
                   PERFORM 9900-SET-ABEND
               END-IF
               IF RM-OD-RATE <= ZERO
                   PERFORM 9900-SET-ABEND
               END-IF
               MOVE RM-OD-RATE TO WS-OD-RATE
           END-IF.

       5400-DECIDE-FIXED-DAYS.
           MOVE WS-CYCLE-NOMINAL-X(5:2) TO WS-MONTH
           MOVE 360 TO WS-DAY-DENOM
           EVALUATE WS-PRODUCT-CD
               WHEN "TRS"
                   EVALUATE WS-MONTH
                       WHEN 02
                           MOVE 28 TO WS-FIXED-DAYS
                       WHEN 04
                       WHEN 06
                       WHEN 09
                       WHEN 11
                           MOVE 30 TO WS-FIXED-DAYS
                       WHEN OTHER
                           MOVE 31 TO WS-FIXED-DAYS
                   END-EVALUATE
               WHEN "FND"
                   MOVE 30 TO WS-FIXED-DAYS
               WHEN "BND"
                   EVALUATE WS-MONTH
                       WHEN 02
                           MOVE 28 TO WS-FIXED-DAYS
                       WHEN OTHER
                           MOVE 30 TO WS-FIXED-DAYS
                   END-EVALUATE
               WHEN "CST"
                   EVALUATE WS-MONTH
                       WHEN 01
                       WHEN 03
                       WHEN 05
                       WHEN 07
                       WHEN 08
                       WHEN 10
                       WHEN 12
                           MOVE 31 TO WS-FIXED-DAYS
                       WHEN 02
                           MOVE 28 TO WS-FIXED-DAYS
                       WHEN OTHER
                           MOVE 30 TO WS-FIXED-DAYS
                   END-EVALUATE
               WHEN OTHER
                   PERFORM 9900-SET-ABEND
           END-EVALUATE.

       5500-CALCULATE-INTEREST.
           COMPUTE WS-NEG-AVG-BAL = DB-AVG-DAILY-BAL
           COMPUTE WS-ABS-BAL = WS-NEG-AVG-BAL * -1
           COMPUTE WS-INT-WORK ROUNDED =
               WS-ABS-BAL * WS-OD-RATE * WS-FIXED-DAYS
               / WS-DAY-DENOM / 100
           COMPUTE WS-INT-AMT ROUNDED = WS-INT-WORK.

       5600-WRITE-DETAIL.
           INITIALIZE KZODBF-REC
           MOVE DB-ACCT-NO TO OD-ACCT-NO
           MOVE DB-CYCLE-ID TO OD-CYCLE-ID
           MOVE DB-PERIOD-START-DT TO OD-OD-START-DT
           MOVE WS-CYCLE-NOMINAL-DT TO OD-OD-END-DT
           MOVE WS-ABS-BAL TO OD-NEG-AVG-BAL
           MOVE WS-OD-RATE TO OD-OD-RATE
           MOVE WS-INT-AMT TO OD-OD-INT-AMT
           WRITE KZODBF-REC
           IF FS-KZODBF = "00"
               ADD 1 TO CNT-OD-WRITE
           ELSE
               PERFORM 9900-SET-ABEND
           END-IF.

       5700-WRITE-INTEREST.
           INITIALIZE KZINTF-REC
           MOVE DB-ACCT-NO TO IN-ACCT-NO
           MOVE DB-CYCLE-ID TO IN-CYCLE-ID
           MOVE WS-CYCLE-NOMINAL-DT TO IN-ACCRUAL-DT
           MOVE WS-FIXED-DAYS TO IN-ACCR-DAYS
           MOVE WS-INT-AMT TO IN-INT-AMT
           WRITE KZINTF-REC
           IF FS-KZINTF = "00"
               ADD 1 TO CNT-IN-WRITE
           ELSE
               PERFORM 9900-SET-ABEND
           END-IF.

       5800-UPDATE-RESTART.
           MOVE WS-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
               INVALID KEY
                   PERFORM 9900-SET-ABEND
           END-READ
           IF FS-KZCTLF = "00"
               MOVE DB-ACCT-NO TO CL-RESTART-POINT
               MOVE WS-PGM-ID TO CL-LAST-PGM-ID
               REWRITE KZCTLF-REC
               IF FS-KZCTLF NOT = "00"
                   PERFORM 9900-SET-ABEND
               END-IF
           ELSE
               IF NOT ABEND-OCCURRED
                   PERFORM 9900-SET-ABEND
               END-IF
           END-IF.

       8000-END-CONTROL.
           MOVE WS-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
               INVALID KEY
                   PERFORM 9900-SET-ABEND
           END-READ
           IF FS-KZCTLF = "00"
               MOVE "N" TO CL-RUN-STATUS
               MOVE SPACE TO CL-RESTART-POINT
               MOVE WS-PGM-ID TO CL-LAST-PGM-ID
               REWRITE KZCTLF-REC
               IF FS-KZCTLF NOT = "00"
                   PERFORM 9900-SET-ABEND
               END-IF
           ELSE
               IF NOT ABEND-OCCURRED
                   PERFORM 9900-SET-ABEND
               END-IF
           END-IF.

       9000-CLOSE-FILES.
           CLOSE KZDBALF
           CLOSE KZIRMF
           CLOSE KZCYRF
           CLOSE KZCTLF
           CLOSE KZODBF
           CLOSE KZINTF.

       9900-SET-ABEND.
           MOVE "Y" TO WS-ABEND-FLAG.
