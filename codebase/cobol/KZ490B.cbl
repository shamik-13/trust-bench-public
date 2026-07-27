       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ490B.
      *
      * CHANGE HISTORY
      * 01.00 20240401 KZB001  INITIAL VERSION
      * 01.01 20240915 KZB002  CYCLE PRINT DATE SUPPORT
      * 01.02 20250220 KZB003  SUPPRESS ZERO NOTICE
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZINTF ASSIGN TO "KZINTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZINTF.
           SELECT KZTAXF ASSIGN TO "KZTAXF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZTAXF.
           SELECT KZUNIF ASSIGN TO "KZUNIF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS UI-ACCT-NO
               FILE STATUS IS FS-KZUNIF.
           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZCYRF.
           SELECT KZSTMF ASSIGN TO "KZSTMF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZSTMF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZINTF.
           COPY KZINTFC.
       FD  KZTAXF.
           COPY KZTAXFC.
       FD  KZUNIF.
           COPY KZUNIFC.
       FD  KZCYRF.
           COPY KZCYRFC.
       FD  KZSTMF.
           COPY KZSTMFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZINTF              PIC XX VALUE SPACES.
           05 FS-KZTAXF              PIC XX VALUE SPACES.
           05 FS-KZUNIF              PIC XX VALUE SPACES.
           05 FS-KZCYRF              PIC XX VALUE SPACES.
           05 FS-KZSTMF              PIC XX VALUE SPACES.

       01  EOF-AREA.
           05 EOF-KZINTF             PIC X VALUE "N".
              88 END-KZINTF                VALUE "Y".
           05 EOF-KZTAXF             PIC X VALUE "N".
              88 END-KZTAXF                VALUE "Y".
           05 EOF-KZCYRF             PIC X VALUE "N".
              88 END-KZCYRF                VALUE "Y".
           05 ABEND-SW               PIC X VALUE "N".
              88 ABEND-ON                  VALUE "Y".

       01  WORK-AREA.
           05 WS-LINE-NO             PIC 9(04) VALUE 0.
           05 WS-TAX-AMT             PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-NET-AMT             PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-CARRY-AMT           PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-STMT-DT             PIC 9(08) VALUE 0.
           05 WS-HAS-TAX             PIC X VALUE "N".
              88 HAS-TAX                   VALUE "Y".
           05 WS-HAS-UNPAID          PIC X VALUE "N".
              88 HAS-UNPAID                VALUE "Y".
           05 WS-PRINT-KBN           PIC X VALUE SPACE.
           05 WS-CYCLE-FOUND         PIC X VALUE "N".
              88 CYCLE-FOUND               VALUE "Y".

       01  COUNT-AREA.
           05 CNT-IN-READ            PIC 9(09) VALUE 0.
           05 CNT-TX-READ            PIC 9(09) VALUE 0.
           05 CNT-CR-READ            PIC 9(09) VALUE 0.
           05 CNT-ST-WRITE           PIC 9(09) VALUE 0.
           05 CNT-ST-NOPRINT         PIC 9(09) VALUE 0.

       01  CYCLE-TABLE.
           05 CY-IDX                 PIC 9(04) COMP VALUE 0.
           05 CY-MAX                 PIC 9(04) COMP VALUE 0.
           05 CY-ENT OCCURS 1000 TIMES.
              10 CY-CYCLE-ID         PIC X(20).
              10 CY-NOMINAL-DT       PIC 9(08).
              10 CY-RESOLVED-DT      PIC 9(08).
              10 CY-ROLLED-FLAG      PIC X.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NOT ABEND-ON
              PERFORM 2000-LOAD-CYCLE
           END-IF
           IF NOT ABEND-ON
              PERFORM 3000-PROCESS
           END-IF
           PERFORM 9000-CLOSE
           IF ABEND-ON
              MOVE 8 TO RETURN-CODE
           ELSE
              DISPLAY "KZ490B OK IN=" CNT-IN-READ
                      " OUT=" CNT-ST-WRITE
                      " NOPRINT=" CNT-ST-NOPRINT
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN INPUT KZINTF
           IF FS-KZINTF NOT = "00"
              DISPLAY "KZINTF OPEN ERROR ST=" FS-KZINTF
              SET ABEND-ON TO TRUE
           END-IF

           OPEN INPUT KZTAXF
           IF FS-KZTAXF NOT = "00"
              DISPLAY "KZTAXF OPEN ERROR ST=" FS-KZTAXF
              SET ABEND-ON TO TRUE
           END-IF

           OPEN INPUT KZUNIF
           IF FS-KZUNIF NOT = "00"
              DISPLAY "KZUNIF OPEN ERROR ST=" FS-KZUNIF
              SET ABEND-ON TO TRUE
           END-IF

           OPEN INPUT KZCYRF
           IF FS-KZCYRF NOT = "00"
              DISPLAY "KZCYRF OPEN ERROR ST=" FS-KZCYRF
              SET ABEND-ON TO TRUE
           END-IF

           OPEN OUTPUT KZSTMF
           IF FS-KZSTMF NOT = "00"
              DISPLAY "KZSTMF OPEN ERROR ST=" FS-KZSTMF
              SET ABEND-ON TO TRUE
           END-IF.

       2000-LOAD-CYCLE.
           PERFORM UNTIL END-KZCYRF OR ABEND-ON
              READ KZCYRF
                 AT END
                    SET END-KZCYRF TO TRUE
                 NOT AT END
                    IF FS-KZCYRF NOT = "00"
                       DISPLAY "KZCYRF READ ERROR ST=" FS-KZCYRF
                       SET ABEND-ON TO TRUE
                    ELSE
                       ADD 1 TO CNT-CR-READ
                       IF CY-MAX >= 1000
                          DISPLAY "KZCYRF TOO MANY RECORDS"
                          SET ABEND-ON TO TRUE
                       ELSE
                          ADD 1 TO CY-MAX
                          MOVE CR-CYCLE-ID TO CY-CYCLE-ID(CY-MAX)
                          MOVE CR-NOMINAL-DT TO CY-NOMINAL-DT(CY-MAX)
                          MOVE CR-RESOLVED-DT TO CY-RESOLVED-DT(CY-MAX)
                          MOVE CR-ROLLED-FLAG TO CY-ROLLED-FLAG(CY-MAX)
                          IF CR-ROLLED-FLAG NOT = "Y"
                             AND CR-ROLLED-FLAG NOT = "N"
                             DISPLAY "KZCYRF BAD ROLL FLAG "
                                     CR-CYCLE-ID
                             SET ABEND-ON TO TRUE
                          END-IF
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           IF NOT ABEND-ON AND CY-MAX = 0
              DISPLAY "KZCYRF NO CYCLE DATA"
              SET ABEND-ON TO TRUE
           END-IF.

       3000-PROCESS.
           PERFORM 3100-READ-TAX
           PERFORM UNTIL END-KZINTF OR ABEND-ON
              READ KZINTF
                 AT END
                    SET END-KZINTF TO TRUE
                 NOT AT END
                    IF FS-KZINTF NOT = "00"
                       DISPLAY "KZINTF READ ERROR ST=" FS-KZINTF
                       SET ABEND-ON TO TRUE
                    ELSE
                       ADD 1 TO CNT-IN-READ
                       PERFORM 3200-VALIDATE-INT
                       IF NOT ABEND-ON
                          PERFORM 3300-SYNC-TAX
                       END-IF
                       IF NOT ABEND-ON
                          PERFORM 3400-READ-UNPAID
                       END-IF
                       IF NOT ABEND-ON
                          PERFORM 3500-DECIDE-STMT
                       END-IF
                       IF NOT ABEND-ON
                          PERFORM 3600-WRITE-STMT
                       END-IF
                    END-IF
              END-READ
           END-PERFORM.

       3100-READ-TAX.
           IF NOT END-KZTAXF
              READ KZTAXF
                 AT END
                    SET END-KZTAXF TO TRUE
                 NOT AT END
                    IF FS-KZTAXF NOT = "00"
                       DISPLAY "KZTAXF READ ERROR ST=" FS-KZTAXF
                       SET ABEND-ON TO TRUE
                    ELSE
                       ADD 1 TO CNT-TX-READ
                    END-IF
              END-READ
           END-IF.

       3200-VALIDATE-INT.
           IF IN-ACCT-NO = SPACES
              DISPLAY "KZINTF ACCT REQUIRED"
              SET ABEND-ON TO TRUE
           END-IF
           IF IN-CYCLE-ID = SPACES
              DISPLAY "KZINTF CYCLE REQUIRED ACCT=" IN-ACCT-NO
              SET ABEND-ON TO TRUE
           END-IF
           IF IN-ACCRUAL-DT = 0
              DISPLAY "KZINTF BAD DATE ACCT=" IN-ACCT-NO
              SET ABEND-ON TO TRUE
           END-IF
           IF IN-ACCR-DAYS < 0
              DISPLAY "KZINTF BAD DAYS ACCT=" IN-ACCT-NO
              SET ABEND-ON TO TRUE
           END-IF
           IF IN-INT-AMT < 0
              DISPLAY "KZINTF BAD INT AMT ACCT=" IN-ACCT-NO
              SET ABEND-ON TO TRUE
           END-IF.

       3300-SYNC-TAX.
           MOVE "N" TO WS-HAS-TAX
           PERFORM UNTIL END-KZTAXF OR HAS-TAX OR ABEND-ON
              IF TX-ACCT-NO < IN-ACCT-NO
                 OR (TX-ACCT-NO = IN-ACCT-NO
                     AND TX-CYCLE-ID < IN-CYCLE-ID)
                 PERFORM 3100-READ-TAX
              ELSE
                 IF TX-ACCT-NO = IN-ACCT-NO
                    AND TX-CYCLE-ID = IN-CYCLE-ID
                    MOVE "Y" TO WS-HAS-TAX
                 ELSE
                    EXIT PERFORM
                 END-IF
              END-IF
           END-PERFORM
           IF HAS-TAX
              IF TX-GROSS-INT-AMT NOT = IN-INT-AMT
                 DISPLAY "KZTAXF GROSS MISMATCH ACCT="
                         IN-ACCT-NO
              END-IF
              COMPUTE WS-TAX-AMT =
                      TX-NATIONAL-TAX-AMT + TX-LOCAL-TAX-AMT
              MOVE TX-NET-INT-AMT TO WS-NET-AMT
              IF WS-TAX-AMT < 0 OR WS-NET-AMT < 0
                 DISPLAY "KZTAXF BAD TAX ACCT=" IN-ACCT-NO
                 SET ABEND-ON TO TRUE
              END-IF
           ELSE
              MOVE 0 TO WS-TAX-AMT
              MOVE IN-INT-AMT TO WS-NET-AMT
           END-IF.

       3400-READ-UNPAID.
           MOVE "N" TO WS-HAS-UNPAID
           MOVE 0 TO WS-CARRY-AMT
           MOVE IN-ACCT-NO TO UI-ACCT-NO
           READ KZUNIF
              INVALID KEY
                 IF FS-KZUNIF = "23"
                    CONTINUE
                 ELSE
                    DISPLAY "KZUNIF READ ERROR ST=" FS-KZUNIF
                            " ACCT=" IN-ACCT-NO
                    SET ABEND-ON TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF FS-KZUNIF NOT = "00"
                    DISPLAY "KZUNIF READ ERROR ST=" FS-KZUNIF
                            " ACCT=" IN-ACCT-NO
                    SET ABEND-ON TO TRUE
                 ELSE
                    IF UI-CYCLE-ID = IN-CYCLE-ID
                       MOVE "Y" TO WS-HAS-UNPAID
                       MOVE UI-UNPAID-INT-AMT TO WS-CARRY-AMT
                       IF UI-UNPAID-INT-AMT < 0
                          DISPLAY "KZUNIF BAD UNPAID ACCT="
                                  IN-ACCT-NO
                          SET ABEND-ON TO TRUE
                       END-IF
                    END-IF
                 END-IF
           END-READ.

       3500-DECIDE-STMT.
           MOVE 0 TO WS-STMT-DT
           MOVE "N" TO WS-CYCLE-FOUND
           PERFORM VARYING CY-IDX FROM 1 BY 1
                   UNTIL CY-IDX > CY-MAX OR CYCLE-FOUND
              IF CY-CYCLE-ID(CY-IDX) = IN-CYCLE-ID
                 MOVE "Y" TO WS-CYCLE-FOUND
                 IF CY-ROLLED-FLAG(CY-IDX) = "Y"
                    MOVE CY-RESOLVED-DT(CY-IDX) TO WS-STMT-DT
                 ELSE
                    MOVE CY-NOMINAL-DT(CY-IDX) TO WS-STMT-DT
                 END-IF
              END-IF
           END-PERFORM

           IF NOT CYCLE-FOUND
              DISPLAY "KZCYRF NO TARGET CYCLE ACCT="
                      IN-ACCT-NO
              DISPLAY "CYCLE=" IN-CYCLE-ID
              SET ABEND-ON TO TRUE
           END-IF

           IF NOT ABEND-ON
              IF WS-NET-AMT = 0 AND WS-TAX-AMT = 0
                 AND WS-CARRY-AMT = 0
                 MOVE "1" TO WS-PRINT-KBN
                 ADD 1 TO CNT-ST-NOPRINT
              ELSE
                 MOVE "0" TO WS-PRINT-KBN
              END-IF
           END-IF.

       3600-WRITE-STMT.
           ADD 1 TO WS-LINE-NO
           INITIALIZE KZSTMF-REC
           MOVE IN-ACCT-NO TO ST-ACCT-NO
           MOVE IN-CYCLE-ID TO ST-CYCLE-ID
           MOVE WS-LINE-NO TO ST-LINE-NO
           MOVE WS-STMT-DT TO ST-STATEMENT-DT
           MOVE IN-INT-AMT TO ST-INT-AMT
           MOVE WS-TAX-AMT TO ST-TAX-AMT
           MOVE WS-NET-AMT TO ST-NET-INT-AMT
           MOVE WS-PRINT-KBN TO ST-PRINT-KBN
           WRITE KZSTMF-REC
           IF FS-KZSTMF NOT = "00"
              DISPLAY "KZSTMF WRITE ERROR ST=" FS-KZSTMF
                      " ACCT=" IN-ACCT-NO
              SET ABEND-ON TO TRUE
           ELSE
              ADD 1 TO CNT-ST-WRITE
           END-IF.

       9000-CLOSE.
           CLOSE KZINTF
           IF FS-KZINTF NOT = "00"
              DISPLAY "KZINTF CLOSE ERROR ST=" FS-KZINTF
              SET ABEND-ON TO TRUE
           END-IF

           CLOSE KZTAXF
           IF FS-KZTAXF NOT = "00"
              DISPLAY "KZTAXF CLOSE ERROR ST=" FS-KZTAXF
              SET ABEND-ON TO TRUE
           END-IF

           CLOSE KZUNIF
           IF FS-KZUNIF NOT = "00"
              DISPLAY "KZUNIF CLOSE ERROR ST=" FS-KZUNIF
              SET ABEND-ON TO TRUE
           END-IF

           CLOSE KZCYRF
           IF FS-KZCYRF NOT = "00"
              DISPLAY "KZCYRF CLOSE ERROR ST=" FS-KZCYRF
              SET ABEND-ON TO TRUE
           END-IF

           CLOSE KZSTMF
           IF FS-KZSTMF NOT = "00"
              DISPLAY "KZSTMF CLOSE ERROR ST=" FS-KZSTMF
              SET ABEND-ON TO TRUE
           END-IF.
