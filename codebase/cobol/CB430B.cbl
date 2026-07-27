       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB430B.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDRSLDF ASSIGN TO "CDRSLDF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RS-STATUS.
           SELECT CDCAPTF ASSIGN TO "CDCAPTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CP-STATUS.
           SELECT CDMEMF ASSIGN TO "CDMEMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MM-CARD-NO
               FILE STATUS IS WS-MM-STATUS.
           SELECT CDSTMTF2 ASSIGN TO "CDSTMTF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ST-CARD-NO
               FILE STATUS IS WS-ST-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CDRSLDF.
       COPY CDRSLDFC.

       FD  CDCAPTF.
       COPY CDCAPTC.

       FD  CDMEMF.
       COPY CDMEMC.

       FD  CDSTMTF2.
       COPY CDSTMTF2C.

       WORKING-STORAGE SECTION.
       01  WS-RS-STATUS          PIC X(02) VALUE SPACES.
       01  WS-CP-STATUS          PIC X(02) VALUE SPACES.
       01  WS-MM-STATUS          PIC X(02) VALUE SPACES.
       01  WS-ST-STATUS          PIC X(02) VALUE SPACES.

       01  WS-RS-EOF             PIC X VALUE "N".
       01  WS-CP-EOF             PIC X VALUE "N".
       01  WS-MM-EOF             PIC X VALUE "N".
       01  WS-ABEND-FLG          PIC X VALUE "N".
       01  WS-MEMBER-FOUND       PIC X VALUE "N".

       01  WS-CUR-CARD-NO        PIC X(16) VALUE SPACES.
       01  WS-CUR-CYCLE-DT       PIC 9(08) VALUE ZERO.
       01  WS-CP-CARD-NO         PIC X(16) VALUE HIGH-VALUES.
       01  WS-CP-CYCLE-DT        PIC 9(08) VALUE 99999999.

       01  WS-SUM-PRIN-AMT       PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-SUM-FEE-AMT        PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-SUM-PAY-AMT        PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-SUM-CAP-AMT        PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-ANNUAL-FEE-AMT     PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-CALC-FEE-AMT       PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-BILL-AMT           PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-MIN-PAY-AMT        PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-WORK-FEE           PIC S9(13)V9999 COMP-3 VALUE ZERO.

       01  WS-DATE-INT           PIC S9(09) COMP VALUE ZERO.
       01  WS-DUE-DATE-INT       PIC S9(09) COMP VALUE ZERO.
       01  WS-DUE-DATE           PIC 9(08) VALUE ZERO.
       01  WS-DELINQ-DAYS        PIC 9(10) VALUE ZERO.
       01  WS-RS-COUNT           PIC 9(09) COMP VALUE ZERO.
       01  WS-CP-COUNT           PIC 9(09) COMP VALUE ZERO.
       01  WS-ST-COUNT           PIC 9(09) COMP VALUE ZERO.
       01  WS-ERR-COUNT          PIC 9(09) COMP VALUE ZERO.
       01  WS-MEMBER-SUB         PIC 9(05) COMP VALUE ZERO.
       01  WS-CYCLE-MMDD         PIC 9(04) VALUE ZERO.
       01  WS-JOIN-MMDD          PIC 9(04) VALUE ZERO.

       01  WS-MEMBER-CNT         PIC 9(05) COMP VALUE ZERO.
       01  WS-MEMBER-TABLE.
           05 WS-MEMBER-ITEM OCCURS 20000 TIMES.
              10 TB-MM-CARD-NO   PIC X(16).
              10 TB-MM-STATUS    PIC X(02).
              10 TB-MM-JOIN-DT   PIC 9(08).
              10 TB-MM-FEE-CD    PIC X(02).
              10 TB-MM-LAST-DT   PIC 9(08).

       01  WS-MEMBER-STATUS      PIC X(02) VALUE SPACES.
       01  WS-MEMBER-JOIN-DT     PIC 9(08) VALUE ZERO.
       01  WS-MEMBER-FEE-CD      PIC X(02) VALUE SPACES.
       01  WS-MEMBER-LAST-DT     PIC 9(08) VALUE ZERO.

       01  WS-STAT-CONFIRM       PIC X VALUE "C".
       01  WS-STAT-SKIP          PIC X VALUE "S".
       01  WS-MEMBER-ACTIVE      PIC X(02) VALUE "01".
       01  WS-DISP-CNT           PIC Z,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WS-ABEND-FLG = "N"
              PERFORM 2000-LOAD-MEMBER
              PERFORM 3000-READ-RS
              PERFORM 3100-READ-CP
              PERFORM 4000-PROCESS-ALL
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-ABEND-FLG = "Y"
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE WS-ST-COUNT TO WS-DISP-CNT
              DISPLAY "CB430B NORMAL END COUNT=" WS-DISP-CNT
              MOVE ZERO TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDRSLDF
           IF WS-RS-STATUS NOT = "00"
              DISPLAY "CDRSLDF OPEN ERROR ST=" WS-RS-STATUS
              MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN INPUT CDCAPTF
           IF WS-CP-STATUS NOT = "00"
              DISPLAY "CDCAPTF OPEN ERROR ST=" WS-CP-STATUS
              MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN INPUT CDMEMF
           IF WS-MM-STATUS NOT = "00"
              DISPLAY "CDMEMF OPEN ERROR ST=" WS-MM-STATUS
              MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN OUTPUT CDSTMTF2
           IF WS-ST-STATUS NOT = "00"
              DISPLAY "CDSTMTF2 OPEN ERROR ST=" WS-ST-STATUS
              MOVE "Y" TO WS-ABEND-FLG
           END-IF.

       2000-LOAD-MEMBER.
           PERFORM UNTIL WS-MM-EOF = "Y" OR WS-ABEND-FLG = "Y"
              READ CDMEMF
                 AT END
                    MOVE "Y" TO WS-MM-EOF
                 NOT AT END
                    IF WS-MEMBER-CNT >= 20000
                       DISPLAY "MEMBER TABLE OVERFLOW"
                       MOVE "Y" TO WS-ABEND-FLG
                    ELSE
                       ADD 1 TO WS-MEMBER-CNT
                       MOVE WS-MEMBER-CNT TO WS-MEMBER-SUB
                       MOVE MM-CARD-NO
                         TO TB-MM-CARD-NO(WS-MEMBER-SUB)
                       MOVE MM-MEMBER-STATUS
                         TO TB-MM-STATUS(WS-MEMBER-SUB)
                       MOVE MM-JOIN-DT
                         TO TB-MM-JOIN-DT(WS-MEMBER-SUB)
                       MOVE MM-ANNUAL-FEE-CD
                         TO TB-MM-FEE-CD(WS-MEMBER-SUB)
                       MOVE MM-LAST-STATUS-DT
                         TO TB-MM-LAST-DT(WS-MEMBER-SUB)
                    END-IF
              END-READ
              IF WS-MM-STATUS NOT = "00" AND WS-MM-STATUS NOT = "10"
                 DISPLAY "CDMEMF READ ERROR ST=" WS-MM-STATUS
                 MOVE "Y" TO WS-ABEND-FLG
              END-IF
           END-PERFORM.

       3000-READ-RS.
           READ CDRSLDF
              AT END
                 MOVE "Y" TO WS-RS-EOF
              NOT AT END
                 ADD 1 TO WS-RS-COUNT
                 IF RS-CARD-NO = SPACES OR RS-CYCLE-DT = ZERO
                    DISPLAY "INVALID CDRSLDF KEY"
                    ADD 1 TO WS-ERR-COUNT
                 END-IF
           END-READ
           IF WS-RS-STATUS NOT = "00" AND WS-RS-STATUS NOT = "10"
              DISPLAY "CDRSLDF READ ERROR ST=" WS-RS-STATUS
              MOVE "Y" TO WS-ABEND-FLG
           END-IF.

       3100-READ-CP.
           READ CDCAPTF
              AT END
                 MOVE "Y" TO WS-CP-EOF
                 MOVE HIGH-VALUES TO WS-CP-CARD-NO
                 MOVE 99999999 TO WS-CP-CYCLE-DT
              NOT AT END
                 ADD 1 TO WS-CP-COUNT
                 MOVE CP-CARD-NO TO WS-CP-CARD-NO
                 MOVE CP-SALES-DT TO WS-CP-CYCLE-DT
                 IF CP-CARD-NO = SPACES OR CP-SALES-DT = ZERO
                    DISPLAY "INVALID CDCAPTF KEY"
                    ADD 1 TO WS-ERR-COUNT
                 END-IF
           END-READ
           IF WS-CP-STATUS NOT = "00" AND WS-CP-STATUS NOT = "10"
              DISPLAY "CDCAPTF READ ERROR ST=" WS-CP-STATUS
              MOVE "Y" TO WS-ABEND-FLG
           END-IF.

       4000-PROCESS-ALL.
           PERFORM UNTIL WS-RS-EOF = "Y" OR WS-ABEND-FLG = "Y"
              MOVE RS-CARD-NO TO WS-CUR-CARD-NO
              MOVE RS-CYCLE-DT TO WS-CUR-CYCLE-DT
              PERFORM 4100-CLEAR-GROUP
              PERFORM 4200-COLLECT-RS
              PERFORM 4300-COLLECT-CP
              PERFORM 5000-FIND-MEMBER
              PERFORM 6000-BUILD-STMT
              PERFORM 7000-WRITE-STMT
           END-PERFORM.

       4100-CLEAR-GROUP.
           MOVE ZERO TO WS-SUM-PRIN-AMT
           MOVE ZERO TO WS-SUM-FEE-AMT
           MOVE ZERO TO WS-SUM-PAY-AMT
           MOVE ZERO TO WS-SUM-CAP-AMT
           MOVE ZERO TO WS-ANNUAL-FEE-AMT
           MOVE ZERO TO WS-CALC-FEE-AMT
           MOVE ZERO TO WS-BILL-AMT
           MOVE ZERO TO WS-MIN-PAY-AMT.

       4200-COLLECT-RS.
           PERFORM UNTIL WS-RS-EOF = "Y"
              OR RS-CARD-NO NOT = WS-CUR-CARD-NO
              OR RS-CYCLE-DT NOT = WS-CUR-CYCLE-DT
              IF RS-RSLD-STATUS = WS-STAT-CONFIRM
                 PERFORM 4210-ADD-RS
              ELSE
                 IF RS-RSLD-STATUS NOT = WS-STAT-SKIP
                    DISPLAY "INVALID RSLD STATUS"
                    ADD 1 TO WS-ERR-COUNT
                 END-IF
              END-IF
              PERFORM 3000-READ-RS
           END-PERFORM.

       4210-ADD-RS.
           IF RS-SLIDE-TIER NOT = "T1"
              AND RS-SLIDE-TIER NOT = "T2"
              AND RS-SLIDE-TIER NOT = "T3"
              AND RS-SLIDE-TIER NOT = "T4"
              DISPLAY "INVALID SLIDE TIER"
              ADD 1 TO WS-ERR-COUNT
           END-IF
           COMPUTE WS-WORK-FEE ROUNDED = RS-PRIN-AMT * 0.0125
           MOVE WS-WORK-FEE TO WS-CALC-FEE-AMT
           IF WS-CALC-FEE-AMT NOT = RS-FEE-AMT
              DISPLAY "RSLD FEE MISMATCH"
              ADD 1 TO WS-ERR-COUNT
           END-IF
           COMPUTE WS-BILL-AMT = RS-PRIN-AMT + RS-FEE-AMT
           IF RS-PAY-AMT NOT = WS-BILL-AMT
              DISPLAY "RSLD PAY AMOUNT MISMATCH"
              ADD 1 TO WS-ERR-COUNT
           END-IF
           ADD RS-PRIN-AMT TO WS-SUM-PRIN-AMT
           ADD RS-FEE-AMT TO WS-SUM-FEE-AMT
           ADD RS-PAY-AMT TO WS-SUM-PAY-AMT.

       4300-COLLECT-CP.
           PERFORM UNTIL WS-CP-EOF = "Y"
              OR WS-CP-CARD-NO > WS-CUR-CARD-NO
              OR (WS-CP-CARD-NO = WS-CUR-CARD-NO
              AND WS-CP-CYCLE-DT > WS-CUR-CYCLE-DT)
              IF WS-CP-CARD-NO = WS-CUR-CARD-NO
                 AND WS-CP-CYCLE-DT = WS-CUR-CYCLE-DT
                 IF CP-CAPTURE-STATUS = WS-STAT-CONFIRM
                    ADD CP-CAPTURE-AMT TO WS-SUM-CAP-AMT
                 ELSE
                    IF CP-CAPTURE-STATUS NOT = WS-STAT-SKIP
                       DISPLAY "INVALID CAPTURE STATUS"
                       ADD 1 TO WS-ERR-COUNT
                    END-IF
                 END-IF
              ELSE
                 DISPLAY "CAPTURE WITHOUT RSLD"
                 ADD 1 TO WS-ERR-COUNT
              END-IF
              PERFORM 3100-READ-CP
           END-PERFORM.

       5000-FIND-MEMBER.
           MOVE "N" TO WS-MEMBER-FOUND
           MOVE SPACES TO WS-MEMBER-STATUS
           MOVE SPACES TO WS-MEMBER-FEE-CD
           MOVE ZERO TO WS-MEMBER-JOIN-DT
           MOVE ZERO TO WS-MEMBER-LAST-DT
           PERFORM VARYING WS-MEMBER-SUB FROM 1 BY 1
              UNTIL WS-MEMBER-SUB > WS-MEMBER-CNT
                 OR WS-MEMBER-FOUND = "Y"
              IF TB-MM-CARD-NO(WS-MEMBER-SUB) = WS-CUR-CARD-NO
                 MOVE "Y" TO WS-MEMBER-FOUND
                 MOVE TB-MM-STATUS(WS-MEMBER-SUB)
                   TO WS-MEMBER-STATUS
                 MOVE TB-MM-JOIN-DT(WS-MEMBER-SUB)
                   TO WS-MEMBER-JOIN-DT
                 MOVE TB-MM-FEE-CD(WS-MEMBER-SUB)
                   TO WS-MEMBER-FEE-CD
                 MOVE TB-MM-LAST-DT(WS-MEMBER-SUB)
                   TO WS-MEMBER-LAST-DT
              END-IF
           END-PERFORM
           IF WS-MEMBER-FOUND = "N"
              DISPLAY "MEMBER NOT FOUND"
              ADD 1 TO WS-ERR-COUNT
           END-IF.

       6000-BUILD-STMT.
           INITIALIZE CDSTMTF2-REC
           MOVE WS-CUR-CARD-NO TO ST-CARD-NO
           MOVE WS-CUR-CYCLE-DT TO ST-CYCLE-DT
           IF WS-MEMBER-FOUND = "Y"
              AND WS-MEMBER-STATUS = WS-MEMBER-ACTIVE
              PERFORM 6100-CALC-ANNUAL-FEE
              COMPUTE WS-BILL-AMT =
                 WS-SUM-CAP-AMT + WS-SUM-PAY-AMT
                 + WS-ANNUAL-FEE-AMT
              PERFORM 6200-CALC-MIN-PAY
              MOVE WS-BILL-AMT TO ST-BILL-AMT
              MOVE WS-MIN-PAY-AMT TO ST-MIN-PAY-AMT
              MOVE WS-STAT-CONFIRM TO ST-STMT-STATUS
           ELSE
              MOVE ZERO TO ST-BILL-AMT
              MOVE ZERO TO ST-MIN-PAY-AMT
              MOVE WS-STAT-SKIP TO ST-STMT-STATUS
           END-IF
           PERFORM 6300-CALC-DUE-DATE
           MOVE WS-DUE-DATE TO ST-DUE-DT
           PERFORM 6400-CALC-DELINQ.

       6100-CALC-ANNUAL-FEE.
           MOVE ZERO TO WS-ANNUAL-FEE-AMT
           COMPUTE WS-CYCLE-MMDD = FUNCTION MOD(WS-CUR-CYCLE-DT 10000)
           COMPUTE WS-JOIN-MMDD = FUNCTION MOD(WS-MEMBER-JOIN-DT 10000)
           EVALUATE WS-MEMBER-FEE-CD
              WHEN "A1"
                 IF WS-CYCLE-MMDD = WS-JOIN-MMDD
                    MOVE 11000 TO WS-ANNUAL-FEE-AMT
                 END-IF
              WHEN "A2"
                 IF WS-CYCLE-MMDD = WS-JOIN-MMDD
                    MOVE 33000 TO WS-ANNUAL-FEE-AMT
                 END-IF
              WHEN "F0"
                 CONTINUE
              WHEN OTHER
                 DISPLAY "INVALID ANNUAL FEE CODE"
                 ADD 1 TO WS-ERR-COUNT
           END-EVALUATE.

       6200-CALC-MIN-PAY.
      *    請求額に対する最低支払額(目安)は請求額の5%とする。
      *    元金定額(残高スライド)の確定は当バッチでは行わない。
           IF WS-BILL-AMT <= ZERO
              MOVE ZERO TO WS-MIN-PAY-AMT
           ELSE
              COMPUTE WS-MIN-PAY-AMT ROUNDED =
                 WS-BILL-AMT / 20
           END-IF
           IF WS-MIN-PAY-AMT > WS-BILL-AMT
              MOVE WS-BILL-AMT TO WS-MIN-PAY-AMT
           END-IF.

       6300-CALC-DUE-DATE.
           COMPUTE WS-DATE-INT =
              FUNCTION INTEGER-OF-DATE(WS-CUR-CYCLE-DT)
           ADD 27 TO WS-DATE-INT GIVING WS-DUE-DATE-INT
           COMPUTE WS-DUE-DATE =
              FUNCTION DATE-OF-INTEGER(WS-DUE-DATE-INT).

       6400-CALC-DELINQ.
           IF WS-MEMBER-LAST-DT > ZERO
              AND WS-MEMBER-LAST-DT < WS-CUR-CYCLE-DT
              COMPUTE WS-DATE-INT =
                 FUNCTION INTEGER-OF-DATE(WS-CUR-CYCLE-DT)
              COMPUTE WS-DUE-DATE-INT =
                 FUNCTION INTEGER-OF-DATE(WS-MEMBER-LAST-DT)
              COMPUTE WS-DELINQ-DAYS =
                 WS-DATE-INT - WS-DUE-DATE-INT
           ELSE
              MOVE ZERO TO WS-DELINQ-DAYS
           END-IF
           MOVE WS-DELINQ-DAYS TO ST-DELINQ-DAYS.

       7000-WRITE-STMT.
           WRITE CDSTMTF2-REC
           IF WS-ST-STATUS = "00"
              ADD 1 TO WS-ST-COUNT
           ELSE
              DISPLAY "CDSTMTF2 WRITE ERROR ST=" WS-ST-STATUS
              MOVE "Y" TO WS-ABEND-FLG
           END-IF.

       9000-CLOSE-FILES.
           IF WS-RS-STATUS NOT = SPACES
              CLOSE CDRSLDF
           END-IF
           IF WS-CP-STATUS NOT = SPACES
              CLOSE CDCAPTF
           END-IF
           IF WS-MM-STATUS NOT = SPACES
              CLOSE CDMEMF
           END-IF
           IF WS-ST-STATUS NOT = SPACES
              CLOSE CDSTMTF2
           END-IF
           IF WS-ERR-COUNT > ZERO
              MOVE WS-ERR-COUNT TO WS-DISP-CNT
              DISPLAY "VALIDATION WARNING COUNT=" WS-DISP-CNT
           END-IF.
