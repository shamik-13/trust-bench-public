       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB170B.
       AUTHOR. TRUST-BATCH.
      *
      * 入金取消反映バッチ
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCANF ASSIGN TO "CDCANF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CAN-CANCEL-ID
               FILE STATUS IS FS-CDCANF.

           SELECT CDAPPF ASSIGN TO "CDAPPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDAPPF.

           SELECT CDOSF ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS FS-CDOSF.

           SELECT CDHISTF ASSIGN TO "CDHISTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HIS-CARD-NO
               FILE STATUS IS FS-CDHISTF.

           SELECT CDEXCPF2 ASSIGN TO "CDEXCPF2"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDEXCPF2.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  CDCANF.
           COPY CDCANC.
      *
       FD  CDAPPF.
           COPY CDAPPFC.
      *
       FD  CDOSF.
           COPY CDOSFC.
      *
       FD  CDHISTF.
           COPY CDHISTC.
      *
       FD  CDEXCPF2.
           COPY CDEXCPF2C.
      *
       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDCANF        PIC XX VALUE SPACE.
           05 FS-CDAPPF        PIC XX VALUE SPACE.
           05 FS-CDOSF         PIC XX VALUE SPACE.
           05 FS-CDHISTF       PIC XX VALUE SPACE.
           05 FS-CDEXCPF2      PIC XX VALUE SPACE.
      *
       01  SW-AREA.
           05 SW-ABEND         PIC X VALUE "N".
              88 SW-ABEND-ON         VALUE "Y".
           05 SW-CAN-EOF       PIC X VALUE "N".
              88 CAN-EOF             VALUE "Y".
           05 SW-APP-EOF       PIC X VALUE "N".
              88 APP-EOF             VALUE "Y".
           05 SW-APP-FOUND     PIC X VALUE "N".
              88 APP-FOUND           VALUE "Y".
           05 SW-OS-FOUND      PIC X VALUE "N".
              88 OS-FOUND            VALUE "Y".
      *
       01  CNST-AREA.
           05 CNST-PGM-ID      PIC X(08) VALUE "CB170B".
           05 CNST-EVT-CANCEL  PIC X(02) VALUE "CR".
           05 CNST-EXP-NOAPP   PIC X(04) VALUE "E101".
           05 CNST-EXP-CARD    PIC X(04) VALUE "E102".
           05 CNST-EXP-STAT    PIC X(04) VALUE "E103".
           05 CNST-EXP-AMT     PIC X(04) VALUE "E104".
           05 CNST-EXP-LATE    PIC X(04) VALUE "E105".
           05 CNST-EXP-OS      PIC X(04) VALUE "E106".
           05 CNST-EXP-DUP     PIC X(04) VALUE "E107".
           05 CNST-CANCEL-LIMIT PIC S9(9) COMP VALUE 30.
      *
       01  DATE-AREA.
           05 WS-CURRENT-DATE  PIC X(21).
           05 WS-TODAY         PIC 9(08).
           05 WS-TODAY-INT     PIC S9(9) COMP.
           05 WS-CANCEL-INT    PIC S9(9) COMP.
           05 WS-ELAPSED-DAYS  PIC S9(9) COMP.
      *
       01  COUNT-AREA.
           05 CNT-CAN-READ     PIC 9(09) VALUE 0.
           05 CNT-APP-READ     PIC 9(09) VALUE 0.
           05 CNT-HIST-WRITE   PIC 9(09) VALUE 0.
           05 CNT-EXP-WRITE    PIC 9(09) VALUE 0.
           05 CNT-SKIP         PIC 9(09) VALUE 0.
           05 CNT-APP-IDX      PIC 9(05) VALUE 0.
           05 CNT-APP-MAX      PIC 9(05) VALUE 20000.
           05 WK-SUB           PIC 9(05) VALUE 0.
           05 WK-EVENT-SEQ     PIC 9(09) VALUE 0.
           05 WK-EXP-SEQ       PIC 9(09) VALUE 0.
      *
       01  AMT-AREA.
           05 WK-APPLIED-TOTAL PIC S9(13)V99 COMP-3 VALUE 0.
           05 WK-RESTORE-AMT   PIC S9(13)V99 COMP-3 VALUE 0.
      *
       01  APP-TABLE.
           05 APP-ENTRY OCCURS 20000 TIMES.
              10 T-AP-PAY-ID   PIC X(20).
              10 T-AP-CARD-NO  PIC X(19).
              10 T-AP-FEE-AMT  PIC S9(13)V99 COMP-3.
              10 T-AP-INT-AMT  PIC S9(13)V99 COMP-3.
              10 T-AP-PRIN-AMT PIC S9(13)V99 COMP-3.
              10 T-AP-REMAIN-AMT PIC S9(13)V99 COMP-3.
              10 T-AP-STATUS   PIC X.
              10 T-AP-PROGRAM-ID PIC X(08).
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF NOT SW-ABEND-ON
              PERFORM 2000-LOAD-APP
           END-IF
           IF NOT SW-ABEND-ON
              PERFORM 3000-PROCESS-CANCEL UNTIL CAN-EOF
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.
      *
       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-TODAY
           COMPUTE WS-TODAY-INT =
               FUNCTION INTEGER-OF-DATE(WS-TODAY)

           OPEN INPUT CDCANF CDAPPF CDOSF
                OUTPUT CDHISTF CDEXCPF2

           IF FS-CDCANF NOT = "00"
              DISPLAY "CDCANF オープン失敗 ST=" FS-CDCANF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDAPPF NOT = "00"
              DISPLAY "CDAPPF オープン失敗 ST=" FS-CDAPPF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDOSF NOT = "00"
              DISPLAY "CDOSF オープン失敗 ST=" FS-CDOSF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDHISTF NOT = "00"
              DISPLAY "CDHISTF オープン失敗 ST=" FS-CDHISTF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDEXCPF2 NOT = "00"
              DISPLAY "CDEXCPF2 オープン失敗 ST=" FS-CDEXCPF2
              PERFORM 9100-SET-ABEND
           END-IF

           IF NOT SW-ABEND-ON
              PERFORM 3100-READ-CANCEL
           END-IF.
      *
       2000-LOAD-APP.
           PERFORM UNTIL APP-EOF OR SW-ABEND-ON
              READ CDAPPF
                 AT END
                    SET APP-EOF TO TRUE
                 NOT AT END
                    IF FS-CDAPPF = "00"
                       ADD 1 TO CNT-APP-READ
                       IF CNT-APP-READ > CNT-APP-MAX
                          DISPLAY "CDAPPF 件数上限超過"
                          PERFORM 9100-SET-ABEND
                       ELSE
                          ADD 1 TO CNT-APP-IDX
                          MOVE AP-PAY-ID
                            TO T-AP-PAY-ID(CNT-APP-IDX)
                          MOVE AP-CARD-NO
                            TO T-AP-CARD-NO(CNT-APP-IDX)
                          MOVE AP-APPLIED-FEE-AMT
                            TO T-AP-FEE-AMT(CNT-APP-IDX)
                          MOVE AP-APPLIED-INT-AMT
                            TO T-AP-INT-AMT(CNT-APP-IDX)
                          MOVE AP-APPLIED-PRIN-AMT
                            TO T-AP-PRIN-AMT(CNT-APP-IDX)
                          MOVE AP-REMAIN-AMT
                            TO T-AP-REMAIN-AMT(CNT-APP-IDX)
                          MOVE AP-APP-STATUS
                            TO T-AP-STATUS(CNT-APP-IDX)
                          MOVE AP-PROGRAM-ID
                            TO T-AP-PROGRAM-ID(CNT-APP-IDX)
                       END-IF
                    ELSE
                       DISPLAY "CDAPPF 読込失敗 ST=" FS-CDAPPF
                       PERFORM 9100-SET-ABEND
                    END-IF
              END-READ
           END-PERFORM.
      *
       3000-PROCESS-CANCEL.
           ADD 1 TO CNT-CAN-READ
           MOVE "N" TO SW-APP-FOUND
           MOVE 0 TO WK-SUB
           PERFORM 3200-FIND-APP
              VARYING WK-SUB FROM 1 BY 1
              UNTIL WK-SUB > CNT-APP-IDX OR APP-FOUND

           IF NOT APP-FOUND
              MOVE CAN-CANCEL-AMT TO WK-RESTORE-AMT
              PERFORM 7200-WRITE-EXCEPTION-NOAPP
           ELSE
              PERFORM 3300-VALIDATE-CANCEL
           END-IF

           PERFORM 3100-READ-CANCEL.
      *
       3100-READ-CANCEL.
           READ CDCANF
              AT END
                 SET CAN-EOF TO TRUE
              NOT AT END
                 IF FS-CDCANF NOT = "00"
                    DISPLAY "CDCANF 読込失敗 ST=" FS-CDCANF
                    PERFORM 9100-SET-ABEND
                    SET CAN-EOF TO TRUE
                 END-IF
           END-READ.
      *
       3200-FIND-APP.
           IF CAN-PAY-ID = T-AP-PAY-ID(WK-SUB)
              SET APP-FOUND TO TRUE
           END-IF.
      *
       3300-VALIDATE-CANCEL.
           COMPUTE WK-APPLIED-TOTAL =
               T-AP-FEE-AMT(WK-SUB)
             + T-AP-INT-AMT(WK-SUB)
             + T-AP-PRIN-AMT(WK-SUB)
           MOVE CAN-CANCEL-AMT TO WK-RESTORE-AMT

           IF CAN-CARD-NO NOT = T-AP-CARD-NO(WK-SUB)
              PERFORM 7210-WRITE-EXCEPTION-CARD
           ELSE
              IF T-AP-STATUS(WK-SUB) = "O"
                 ADD 1 TO CNT-SKIP
              ELSE
                 IF T-AP-STATUS(WK-SUB) = "S"
                    PERFORM 7220-WRITE-EXCEPTION-STAT
                 ELSE
                    IF T-AP-STATUS(WK-SUB) NOT = "F"
                       AND T-AP-STATUS(WK-SUB) NOT = "P"
                       PERFORM 7220-WRITE-EXCEPTION-STAT
                    ELSE
                       IF CAN-CANCEL-AMT <= 0
                          PERFORM 7230-WRITE-EXCEPTION-AMT
                       ELSE
                          IF CAN-CANCEL-AMT > WK-APPLIED-TOTAL
                             PERFORM 7230-WRITE-EXCEPTION-AMT
                          ELSE
                             PERFORM 3400-CHECK-PERIOD
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
      *
       3400-CHECK-PERIOD.
           COMPUTE WS-CANCEL-INT =
               FUNCTION INTEGER-OF-DATE(CAN-CANCEL-DT)
           COMPUTE WS-ELAPSED-DAYS =
               WS-TODAY-INT - WS-CANCEL-INT

           IF WS-ELAPSED-DAYS < 0
              PERFORM 7240-WRITE-EXCEPTION-LATE
           ELSE
              IF WS-ELAPSED-DAYS > CNST-CANCEL-LIMIT
                 PERFORM 7240-WRITE-EXCEPTION-LATE
              ELSE
                 PERFORM 3500-CHECK-OS
              END-IF
           END-IF.
      *
       3500-CHECK-OS.
           MOVE "N" TO SW-OS-FOUND
           MOVE CAN-CARD-NO TO OS-CARD-NO

           READ CDOSF KEY IS OS-CARD-NO
              INVALID KEY
                 PERFORM 7250-WRITE-EXCEPTION-OS
              NOT INVALID KEY
                 IF FS-CDOSF = "00"
                    SET OS-FOUND TO TRUE
                 ELSE
                    DISPLAY "CDOSF 読込失敗 ST=" FS-CDOSF
                    PERFORM 9100-SET-ABEND
                 END-IF
           END-READ

           IF OS-FOUND
              IF OS-CYCLE-DT > WS-TODAY
                 PERFORM 7250-WRITE-EXCEPTION-OS
              ELSE
                 PERFORM 7000-WRITE-HISTORY
              END-IF
           END-IF.
      *
       7000-WRITE-HISTORY.
           ADD 1 TO WK-EVENT-SEQ
           INITIALIZE CDHISTF-REC
           MOVE CAN-CARD-NO TO HIS-CARD-NO
           MOVE CAN-PAY-ID TO HIS-PAY-ID
           MOVE WK-EVENT-SEQ TO HIS-EVENT-SEQ
           MOVE CNST-EVT-CANCEL TO HIS-EVENT-TYPE
           MOVE WK-RESTORE-AMT TO HIS-EVENT-AMT
           MOVE WS-TODAY TO HIS-EVENT-DT
           MOVE CNST-PGM-ID TO HIS-SOURCE-PROGRAM

           WRITE CDHISTF-REC
              INVALID KEY
                 PERFORM 7260-WRITE-EXCEPTION-DUP
              NOT INVALID KEY
                 IF FS-CDHISTF = "00"
                    ADD 1 TO CNT-HIST-WRITE
                 ELSE
                    DISPLAY "CDHISTF 書込失敗 ST=" FS-CDHISTF
                    PERFORM 9100-SET-ABEND
                 END-IF
           END-WRITE.
      *
       7200-WRITE-EXCEPTION-NOAPP.
           PERFORM 7300-SET-EXCEPTION-BASE
           MOVE CNST-EXP-NOAPP TO EXP-EXCEPTION-CD
           PERFORM 7400-WRITE-EXCEPTION.
      *
       7210-WRITE-EXCEPTION-CARD.
           PERFORM 7300-SET-EXCEPTION-BASE
           MOVE CNST-EXP-CARD TO EXP-EXCEPTION-CD
           PERFORM 7400-WRITE-EXCEPTION.
      *
       7220-WRITE-EXCEPTION-STAT.
           PERFORM 7300-SET-EXCEPTION-BASE
           MOVE CNST-EXP-STAT TO EXP-EXCEPTION-CD
           PERFORM 7400-WRITE-EXCEPTION.
      *
       7230-WRITE-EXCEPTION-AMT.
           PERFORM 7300-SET-EXCEPTION-BASE
           MOVE CNST-EXP-AMT TO EXP-EXCEPTION-CD
           PERFORM 7400-WRITE-EXCEPTION.
      *
       7240-WRITE-EXCEPTION-LATE.
           PERFORM 7300-SET-EXCEPTION-BASE
           MOVE CNST-EXP-LATE TO EXP-EXCEPTION-CD
           PERFORM 7400-WRITE-EXCEPTION.
      *
       7250-WRITE-EXCEPTION-OS.
           PERFORM 7300-SET-EXCEPTION-BASE
           MOVE CNST-EXP-OS TO EXP-EXCEPTION-CD
           PERFORM 7400-WRITE-EXCEPTION.
      *
       7260-WRITE-EXCEPTION-DUP.
           PERFORM 7300-SET-EXCEPTION-BASE
           MOVE CNST-EXP-DUP TO EXP-EXCEPTION-CD
           PERFORM 7400-WRITE-EXCEPTION.
      *
       7300-SET-EXCEPTION-BASE.
           ADD 1 TO WK-EXP-SEQ
           INITIALIZE CDEXCPF2-REC
           MOVE WK-EXP-SEQ TO EXP-EXCEPTION-ID
           MOVE CAN-PAY-ID TO EXP-PAY-ID
           MOVE CAN-CARD-NO TO EXP-CARD-NO
           MOVE WK-RESTORE-AMT TO EXP-EXCEPTION-AMT
           MOVE CNST-PGM-ID TO EXP-DETECTED-PROGRAM
           MOVE WS-TODAY TO EXP-DETECTED-DT.
      *
       7400-WRITE-EXCEPTION.
           WRITE CDEXCPF2-REC
           IF FS-CDEXCPF2 = "00"
              ADD 1 TO CNT-EXP-WRITE
           ELSE
              DISPLAY "CDEXCPF2 書込失敗 ST=" FS-CDEXCPF2
              PERFORM 9100-SET-ABEND
           END-IF.
      *
       9000-FINALIZE.
           CLOSE CDCANF CDAPPF CDOSF CDHISTF CDEXCPF2
           IF SW-ABEND-ON
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           DISPLAY "CB170B 件数 取消=" CNT-CAN-READ
                   " 消込=" CNT-APP-READ
                   " 履歴=" CNT-HIST-WRITE
                   " 例外=" CNT-EXP-WRITE
                   " 対象外=" CNT-SKIP.
      *
       9100-SET-ABEND.
           MOVE "Y" TO SW-ABEND
           MOVE 8 TO RETURN-CODE.
