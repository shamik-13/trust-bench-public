       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT290S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCINSF ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IN-INS-ID
               FILE STATUS IS WS-CCINSF-STS.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCDTLF-STS.

       DATA DIVISION.
       FILE SECTION.
       FD  CCINSF.
           COPY CCINSC.
       FD  CCDTLF.
           COPY CCDTLC.

       WORKING-STORAGE SECTION.
       01  WS-CCINSF-STS              PIC XX VALUE SPACE.
       01  WS-CCDTLF-STS              PIC XX VALUE SPACE.

       01  WS-EOF-SW.
           05 WS-CCINSF-EOF           PIC X VALUE "N".
              88 CCINSF-END                 VALUE "Y".
           05 WS-CCDTLF-EOF           PIC X VALUE "N".
              88 CCDTLF-END                 VALUE "Y".

       01  WS-WORK.
           05 WS-HARD-ERR             PIC X VALUE "N".
              88 HARD-ERROR                 VALUE "Y".
           05 WS-CCINSF-OPENED        PIC X VALUE "N".
              88 CCINSF-OPENED              VALUE "Y".
           05 WS-CCDTLF-OPENED        PIC X VALUE "N".
              88 CCDTLF-OPENED              VALUE "Y".
           05 WS-IN-CNT               PIC 9(9) VALUE ZERO.
           05 WS-DL-CNT               PIC 9(9) VALUE ZERO.
           05 WS-IN-SKIP-CNT          PIC 9(9) VALUE ZERO.
           05 WS-DL-SKIP-CNT          PIC 9(9) VALUE ZERO.

       01  WS-COMMON-KBN.
           05 WS-APPROVED-KBN         PIC X VALUE "1".
           05 WS-INSTR-NORMAL-KBN     PIC X VALUE "1".

       LINKAGE SECTION.
       01  LK-CT290S-PARM.
           05 LK-ORG-CD               PIC X(6).
           05 LK-FROM-DT              PIC 9(8).
           05 LK-TO-DT                PIC 9(8).
           05 LK-INS-TOTAL-AMT        PIC S9(15)V99 COMP-3.
           05 LK-DTL-TOTAL-AMT        PIC S9(15)V99 COMP-3.
           05 LK-INS-COUNT            PIC 9(9).
           05 LK-DTL-COUNT            PIC 9(9).
           05 LK-RESP-CD              PIC 9(2).
           05 LK-RESP-REASON          PIC X(40).

       PROCEDURE DIVISION USING LK-CT290S-PARM.

       0000-MAIN.
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
              PERFORM 2000-PROCESS
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE ZERO TO LK-INS-TOTAL-AMT
                        LK-DTL-TOTAL-AMT
                        LK-INS-COUNT
                        LK-DTL-COUNT
                        LK-RESP-CD
                        WS-IN-CNT
                        WS-DL-CNT
                        WS-IN-SKIP-CNT
                        WS-DL-SKIP-CNT
           MOVE SPACE TO LK-RESP-REASON
                         WS-CCINSF-STS
                         WS-CCDTLF-STS
           MOVE "N" TO WS-HARD-ERR
                       WS-CCINSF-EOF
                       WS-CCDTLF-EOF
                       WS-CCINSF-OPENED
                       WS-CCDTLF-OPENED

           IF LK-ORG-CD = SPACE
              MOVE 8 TO RETURN-CODE
              MOVE 12 TO LK-RESP-CD
              MOVE "ORG-CD REQUIRED" TO LK-RESP-REASON
              MOVE "Y" TO WS-HARD-ERR
              DISPLAY "CT290S ORG-CD REQUIRED"
           END-IF

           IF NOT HARD-ERROR
              IF LK-FROM-DT = ZERO OR LK-TO-DT = ZERO
                 MOVE 8 TO RETURN-CODE
                 MOVE 13 TO LK-RESP-CD
                 MOVE "DATE REQUIRED" TO LK-RESP-REASON
                 MOVE "Y" TO WS-HARD-ERR
                 DISPLAY "CT290S DATE REQUIRED"
              END-IF
           END-IF

           IF NOT HARD-ERROR
              IF LK-FROM-DT > LK-TO-DT
                 MOVE 8 TO RETURN-CODE
                 MOVE 14 TO LK-RESP-CD
                 MOVE "DATE RANGE ERROR" TO LK-RESP-REASON
                 MOVE "Y" TO WS-HARD-ERR
                 DISPLAY "CT290S DATE RANGE ERROR"
              END-IF
           END-IF.

       2000-PROCESS.
           OPEN INPUT CCINSF
           IF WS-CCINSF-STS = "00"
              MOVE "Y" TO WS-CCINSF-OPENED
           ELSE
              MOVE 12 TO RETURN-CODE
              MOVE 21 TO LK-RESP-CD
              STRING "CCINSF OPEN ERROR ST="
                     WS-CCINSF-STS
                DELIMITED BY SIZE INTO LK-RESP-REASON
              END-STRING
              MOVE "Y" TO WS-HARD-ERR
              DISPLAY "CT290S CCINSF OPEN ERROR ST="
                      WS-CCINSF-STS
           END-IF

           IF NOT HARD-ERROR
              OPEN INPUT CCDTLF
              IF WS-CCDTLF-STS = "00"
                 MOVE "Y" TO WS-CCDTLF-OPENED
              ELSE
                 MOVE 12 TO RETURN-CODE
                 MOVE 22 TO LK-RESP-CD
                 STRING "CCDTLF OPEN ERROR ST="
                        WS-CCDTLF-STS
                   DELIMITED BY SIZE INTO LK-RESP-REASON
                 END-STRING
                 MOVE "Y" TO WS-HARD-ERR
                 DISPLAY "CT290S CCDTLF OPEN ERROR ST="
                         WS-CCDTLF-STS
              END-IF
           END-IF

           IF NOT HARD-ERROR
              PERFORM 2100-READ-CCINSF
              PERFORM UNTIL CCINSF-END OR HARD-ERROR
                 PERFORM 2200-EDIT-CCINSF
                 PERFORM 2100-READ-CCINSF
              END-PERFORM
           END-IF

           IF NOT HARD-ERROR
              PERFORM 3100-READ-CCDTLF
              PERFORM UNTIL CCDTLF-END OR HARD-ERROR
                 PERFORM 3200-EDIT-CCDTLF
                 PERFORM 3100-READ-CCDTLF
              END-PERFORM
           END-IF.

       2100-READ-CCINSF.
           READ CCINSF
              AT END
                 MOVE "Y" TO WS-CCINSF-EOF
              NOT AT END
                 IF WS-CCINSF-STS NOT = "00"
                    MOVE 12 TO RETURN-CODE
                    MOVE 31 TO LK-RESP-CD
                    STRING "CCINSF READ ERROR ST="
                           WS-CCINSF-STS
                      DELIMITED BY SIZE INTO LK-RESP-REASON
                    END-STRING
                    MOVE "Y" TO WS-HARD-ERR
                    DISPLAY "CT290S CCINSF READ ERROR ST="
                            WS-CCINSF-STS
                 END-IF
           END-READ.

       2200-EDIT-CCINSF.
           IF IN-ORG-CD = LK-ORG-CD
              AND IN-RECV-DT >= LK-FROM-DT
              AND IN-RECV-DT <= LK-TO-DT
              IF IN-INSTR-STATUS-KBN = WS-APPROVED-KBN
                 AND IN-INSTR-KBN = WS-INSTR-NORMAL-KBN
                 ADD IN-INSTR-AMT TO LK-INS-TOTAL-AMT
                 ADD 1 TO LK-INS-COUNT
                 ADD 1 TO WS-IN-CNT
              ELSE
                 ADD 1 TO WS-IN-SKIP-CNT
              END-IF
           END-IF.

       3100-READ-CCDTLF.
           READ CCDTLF
              AT END
                 MOVE "Y" TO WS-CCDTLF-EOF
              NOT AT END
                 IF WS-CCDTLF-STS NOT = "00"
                    MOVE 12 TO RETURN-CODE
                    MOVE 41 TO LK-RESP-CD
                    STRING "CCDTLF READ ERROR ST="
                           WS-CCDTLF-STS
                      DELIMITED BY SIZE INTO LK-RESP-REASON
                    END-STRING
                    MOVE "Y" TO WS-HARD-ERR
                    DISPLAY "CT290S CCDTLF READ ERROR ST="
                            WS-CCDTLF-STS
                 END-IF
           END-READ.

       3200-EDIT-CCDTLF.
           IF DL-ORG-CD = LK-ORG-CD
              AND DL-VALUE-DT >= LK-FROM-DT
              AND DL-VALUE-DT <= LK-TO-DT
              IF DL-DETAIL-STATUS-KBN = WS-APPROVED-KBN
                 ADD DL-DETAIL-AMT TO LK-DTL-TOTAL-AMT
                 ADD 1 TO LK-DTL-COUNT
                 ADD 1 TO WS-DL-CNT
              ELSE
                 ADD 1 TO WS-DL-SKIP-CNT
              END-IF
           END-IF.

       9000-FINAL.
           IF CCINSF-OPENED
              CLOSE CCINSF
              IF WS-CCINSF-STS NOT = "00"
                 AND NOT HARD-ERROR
                 MOVE 12 TO RETURN-CODE
                 MOVE 51 TO LK-RESP-CD
                 STRING "CCINSF CLOSE ERROR ST="
                        WS-CCINSF-STS
                   DELIMITED BY SIZE INTO LK-RESP-REASON
                 END-STRING
                 MOVE "Y" TO WS-HARD-ERR
                 DISPLAY "CT290S CCINSF CLOSE ERROR ST="
                         WS-CCINSF-STS
              END-IF
           END-IF

           IF CCDTLF-OPENED
              CLOSE CCDTLF
              IF WS-CCDTLF-STS NOT = "00"
                 AND NOT HARD-ERROR
                 MOVE 12 TO RETURN-CODE
                 MOVE 52 TO LK-RESP-CD
                 STRING "CCDTLF CLOSE ERROR ST="
                        WS-CCDTLF-STS
                   DELIMITED BY SIZE INTO LK-RESP-REASON
                 END-STRING
                 MOVE "Y" TO WS-HARD-ERR
                 DISPLAY "CT290S CCDTLF CLOSE ERROR ST="
                         WS-CCDTLF-STS
              END-IF
           END-IF

           IF NOT HARD-ERROR
              MOVE 0 TO RETURN-CODE
              MOVE 0 TO LK-RESP-CD
              IF LK-INS-COUNT = ZERO AND LK-DTL-COUNT = ZERO
                 MOVE "NORMAL END ZERO COUNT" TO LK-RESP-REASON
              ELSE
                 MOVE "NORMAL END" TO LK-RESP-REASON
              END-IF
              DISPLAY "CT290S NORMAL END INS-COUNT="
                      LK-INS-COUNT
                      " DTL-COUNT="
                      LK-DTL-COUNT
           END-IF.
