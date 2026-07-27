       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG530B.
       AUTHOR.     TRUST-BATCH.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGZENF
               ASSIGN TO "TGZENF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGZENF.
      *
           SELECT TGNETCF
               ASSIGN TO "TGNETCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NC-COUNTER-BANK
               FILE STATUS IS FS-TGNETCF.
      *
           SELECT TGCLRF
               ASSIGN TO "TGCLRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGCLRF.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  TGZENF.
           COPY TGZENFC.
      *
       FD  TGNETCF.
           COPY TGNETCFC.
      *
       FD  TGCLRF.
           COPY TGCLRFC.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-FILE-STATUS.
           05 FS-TGZENF              PIC X(02) VALUE SPACES.
           05 FS-TGNETCF             PIC X(02) VALUE SPACES.
           05 FS-TGCLRF              PIC X(02) VALUE SPACES.
      *
       01  WS-FILE-OPEN-SW.
           05 SW-ZENF-OPEN           PIC X(01) VALUE "N".
           05 SW-NETCF-OPEN          PIC X(01) VALUE "N".
           05 SW-CLRF-OPEN           PIC X(01) VALUE "N".
      *
       01  WS-SETTLE-STATUS         PIC X(01) VALUE "H".
      *
       01  WS-CONSTANTS.
           05 CT-ZEN-PAY             PIC X(02) VALUE "01".
           05 CT-ZEN-RECV            PIC X(02) VALUE "02".
           05 CT-FLAG-YES            PIC X(01) VALUE "Y".
           05 CT-STATUS-SETTLED      PIC X(01) VALUE "S".
           05 CT-STATUS-HELD         PIC X(01) VALUE "H".
           05 CT-MAX-BANK            PIC 9(03) VALUE 200.
      *
       01  WS-RUN-DATE.
           05 WS-CURRENT-DATE        PIC X(21) VALUE SPACES.
           05 WS-YYYYMMDD            PIC X(08) VALUE SPACES.
      *
       01  WS-WORK.
           05 WS-EOF-SW              PIC X(01) VALUE "N".
           05 WS-FOUND-SW            PIC X(01) VALUE "N".
           05 WS-ABEND-SW            PIC X(01) VALUE "N".
           05 WS-TABLE-COUNT         PIC 9(03) VALUE ZERO.
           05 WS-SUB                 PIC 9(03) VALUE ZERO.
           05 WS-TARGET-SUB          PIC 9(03) VALUE ZERO.
      *
       01  WS-COUNTER-TABLE.
           05 WS-COUNTER-ENTRY OCCURS 200 TIMES.
              10 TB-COUNTER-BANK     PIC X(04) VALUE SPACES.
              10 TB-PAY-AMT          PIC S9(15) COMP-3 VALUE ZERO.
              10 TB-RECV-AMT         PIC S9(15) COMP-3 VALUE ZERO.
              10 TB-ITEM-COUNT       PIC 9(09) COMP-3 VALUE ZERO.
      *
           COPY LK-NET-PARM.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN SECTION.
      *
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF RETURN-CODE = 0
               PERFORM 2000-READ-ACCUMULATE
           END-IF
           IF RETURN-CODE = 0
               PERFORM 3000-EMIT-CLEARING
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.
      *
       1000-INITIALIZE SECTION.
      *
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-YYYYMMDD
      *
           OPEN INPUT TGZENF
           IF FS-TGZENF NOT = "00"
               DISPLAY "TGZENF OPEN ERR ST=" FS-TGZENF
               PERFORM 8000-ABEND
           ELSE
               MOVE "Y" TO SW-ZENF-OPEN
           END-IF
      *
           IF RETURN-CODE = 0
               OPEN INPUT TGNETCF
               IF FS-TGNETCF NOT = "00"
                   DISPLAY "TGNETCF OPEN ERR ST=" FS-TGNETCF
                   PERFORM 8000-ABEND
               ELSE
                   MOVE "Y" TO SW-NETCF-OPEN
               END-IF
           END-IF
      *
           IF RETURN-CODE = 0
               OPEN OUTPUT TGCLRF
               IF FS-TGCLRF NOT = "00"
                   DISPLAY "TGCLRF OPEN ERR ST=" FS-TGCLRF
                   PERFORM 8000-ABEND
               ELSE
                   MOVE "Y" TO SW-CLRF-OPEN
               END-IF
           END-IF.
      *
       2000-READ-ACCUMULATE SECTION.
      *
           PERFORM UNTIL WS-EOF-SW = "Y"
               READ TGZENF
                   AT END
                       MOVE "Y" TO WS-EOF-SW
                   NOT AT END
                       IF FS-TGZENF = "00"
                           PERFORM 2100-VALIDATE-ZEN
                           IF RETURN-CODE = 0
                               PERFORM 2200-FIND-OR-ADD
                           END-IF
                           IF RETURN-CODE = 0
                               PERFORM 2300-ACCUMULATE-ZEN
                           END-IF
                       ELSE
                           DISPLAY "TGZENF READ ERR ST=" FS-TGZENF
                           PERFORM 8000-ABEND
                       END-IF
               END-READ
           END-PERFORM.
      *
       2100-VALIDATE-ZEN SECTION.
      *
           IF ZE-COUNTER-BANK = SPACES
               DISPLAY "TGZENF BANK ERR SEQ=" ZE-CENTER-SEQ
               PERFORM 8000-ABEND
           END-IF
      *
           IF ZE-ZEN-TYPE NOT = CT-ZEN-PAY
              AND ZE-ZEN-TYPE NOT = CT-ZEN-RECV
               DISPLAY "TGZENF TYPE ERR SEQ=" ZE-CENTER-SEQ
               DISPLAY "TYPE=" ZE-ZEN-TYPE
               PERFORM 8000-ABEND
           END-IF
      *
           IF ZE-REMIT-AMT < ZERO
               DISPLAY "TGZENF AMT ERR SEQ=" ZE-CENTER-SEQ
               PERFORM 8000-ABEND
           END-IF.
      *
       2200-FIND-OR-ADD SECTION.
      *
           MOVE "N" TO WS-FOUND-SW
           MOVE ZERO TO WS-TARGET-SUB
           PERFORM VARYING WS-SUB FROM 1 BY 1
             UNTIL WS-SUB > WS-TABLE-COUNT OR WS-FOUND-SW = "Y"
               IF TB-COUNTER-BANK(WS-SUB) = ZE-COUNTER-BANK
                   MOVE "Y" TO WS-FOUND-SW
                   MOVE WS-SUB TO WS-TARGET-SUB
               END-IF
           END-PERFORM
      *
           IF WS-FOUND-SW = "N"
               IF WS-TABLE-COUNT >= CT-MAX-BANK
                   DISPLAY "BANK TABLE FULL"
                   DISPLAY "BANK=" ZE-COUNTER-BANK
                   PERFORM 8000-ABEND
               ELSE
                   ADD 1 TO WS-TABLE-COUNT
                   MOVE WS-TABLE-COUNT TO WS-TARGET-SUB
                   MOVE ZE-COUNTER-BANK
                     TO TB-COUNTER-BANK(WS-TARGET-SUB)
                   MOVE ZERO TO TB-PAY-AMT(WS-TARGET-SUB)
                   MOVE ZERO TO TB-RECV-AMT(WS-TARGET-SUB)
                   MOVE ZERO TO TB-ITEM-COUNT(WS-TARGET-SUB)
               END-IF
           END-IF.
      *
       2300-ACCUMULATE-ZEN SECTION.
      *
           IF ZE-ZEN-TYPE = CT-ZEN-PAY
               ADD ZE-REMIT-AMT TO TB-PAY-AMT(WS-TARGET-SUB)
           ELSE
               ADD ZE-REMIT-AMT TO TB-RECV-AMT(WS-TARGET-SUB)
           END-IF
           ADD 1 TO TB-ITEM-COUNT(WS-TARGET-SUB).
      *
       3000-EMIT-CLEARING SECTION.
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
             UNTIL WS-SUB > WS-TABLE-COUNT OR RETURN-CODE NOT = 0
               PERFORM 3100-READ-NET-CONTROL
               IF RETURN-CODE = 0
                   PERFORM 3200-CALL-NET-SUB
               END-IF
               IF RETURN-CODE = 0
                   PERFORM 3300-WRITE-CLEARING
               END-IF
           END-PERFORM.
      *
       3100-READ-NET-CONTROL SECTION.
      *
           MOVE TB-COUNTER-BANK(WS-SUB) TO NC-COUNTER-BANK
           READ TGNETCF
               KEY IS NC-COUNTER-BANK
               INVALID KEY
                   MOVE CT-STATUS-HELD TO WS-SETTLE-STATUS
                   MOVE SPACES TO NC-OUT-FLAG
                   MOVE SPACES TO NC-IN-FLAG
           END-READ
      *
           IF FS-TGNETCF = "00"
               IF NC-OUT-FLAG = CT-FLAG-YES
                  AND NC-IN-FLAG = CT-FLAG-YES
                   MOVE CT-STATUS-SETTLED TO WS-SETTLE-STATUS
               ELSE
                   MOVE CT-STATUS-HELD TO WS-SETTLE-STATUS
               END-IF
           ELSE
               IF FS-TGNETCF = "23"
                   DISPLAY "TGNETCF NOT FOUND"
                   DISPLAY "BANK=" TB-COUNTER-BANK(WS-SUB)
               ELSE
                   DISPLAY "TGNETCF READ ERR ST=" FS-TGNETCF
                   DISPLAY "BANK=" TB-COUNTER-BANK(WS-SUB)
                   PERFORM 8000-ABEND
               END-IF
           END-IF.
      *
       3200-CALL-NET-SUB SECTION.
      *
           INITIALIZE LK-NET-PARM
           MOVE TB-PAY-AMT(WS-SUB) TO LK-NET-PAY-AMT
           MOVE TB-RECV-AMT(WS-SUB) TO LK-NET-RECV-AMT
      *
           CALL "TG935S" USING LK-NET-PARM
      *
           IF LK-NET-RET NOT = "00"
               DISPLAY "TG935S ERR RC=" LK-NET-RET
               DISPLAY "BANK=" TB-COUNTER-BANK(WS-SUB)
               PERFORM 8000-ABEND
           END-IF.
      *
       3300-WRITE-CLEARING SECTION.
      *
           INITIALIZE TGCLRF-REC
           MOVE WS-YYYYMMDD TO CL-SETTLE-DT
           MOVE TB-COUNTER-BANK(WS-SUB) TO CL-COUNTER-BANK
           MOVE LK-NET-AMT TO CL-NET-AMT
           MOVE TB-ITEM-COUNT(WS-SUB) TO CL-ITEM-COUNT
           MOVE WS-SETTLE-STATUS TO CL-SETTLE-STATUS
      *
           WRITE TGCLRF-REC
           IF FS-TGCLRF NOT = "00"
               DISPLAY "TGCLRF WRITE ERR ST=" FS-TGCLRF
               DISPLAY "BANK=" TB-COUNTER-BANK(WS-SUB)
               PERFORM 8000-ABEND
           END-IF.
      *
       8000-ABEND SECTION.
      *
           MOVE "Y" TO WS-ABEND-SW
           MOVE 8 TO RETURN-CODE.
      *
       9000-TERMINATE SECTION.
      *
           IF SW-CLRF-OPEN = "Y"
               CLOSE TGCLRF
               IF FS-TGCLRF NOT = "00"
                   DISPLAY "TGCLRF CLOSE ERR ST=" FS-TGCLRF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
      *
           IF SW-NETCF-OPEN = "Y"
               CLOSE TGNETCF
               IF FS-TGNETCF NOT = "00"
                   DISPLAY "TGNETCF CLOSE ERR ST=" FS-TGNETCF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
      *
           IF SW-ZENF-OPEN = "Y"
               CLOSE TGZENF
               IF FS-TGZENF NOT = "00"
                   DISPLAY "TGZENF CLOSE ERR ST=" FS-TGZENF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
      *
           IF WS-ABEND-SW NOT = "Y"
              AND RETURN-CODE = 0
               DISPLAY "TG530B NORMAL END COUNT=" WS-TABLE-COUNT
           ELSE
               DISPLAY "TG530B ABNORMAL END RC=" RETURN-CODE
           END-IF.
