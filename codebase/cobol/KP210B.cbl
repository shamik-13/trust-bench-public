       IDENTIFICATION DIVISION.
       PROGRAM-ID. KP210B.
       AUTHOR. JPBC.
      *================================================================*
      * Delinquency penalty calculation batch                           *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KPDELINF
               ASSIGN TO "KPDELINF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DL-ACCT-ID
               FILE STATUS IS WS-KPDELINF-ST.

           SELECT KPPENRF
               ASSIGN TO "KPPENRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PR-PENALTY-RATE-CD
               FILE STATUS IS WS-KPPENRF-ST.

           SELECT KZACCTF
               ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-KZACCTF-ST.

           SELECT KPPENHF
               ASSIGN TO "KPPENHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KPPENHF-ST.

           SELECT KZTRANF
               ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZTRANF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  KPDELINF.
           COPY KPDELINFC.

       FD  KPPENRF.
           COPY KPPENRFC.

       FD  KZACCTF.
           COPY KZACCTC.

       FD  KPPENHF.
           COPY KPPENHFC.

       FD  KZTRANF.
           COPY KZTRANC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KPDELINF-ST        PIC XX VALUE SPACES.
           05 WS-KPPENRF-ST         PIC XX VALUE SPACES.
           05 WS-KZACCTF-ST         PIC XX VALUE SPACES.
           05 WS-KPPENHF-ST         PIC XX VALUE SPACES.
           05 WS-KZTRANF-ST         PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-SW             PIC X VALUE "N".
              88 WS-EOF                   VALUE "Y".
              88 WS-NOT-EOF               VALUE "N".
           05 WS-HARD-ERR-SW        PIC X VALUE "N".
              88 WS-HARD-ERR              VALUE "Y".
              88 WS-NO-HARD-ERR           VALUE "N".
           05 WS-SKIP-SW            PIC X VALUE "N".
              88 WS-SKIP-REC              VALUE "Y".
              88 WS-NO-SKIP-REC           VALUE "N".

       01  WS-COUNTERS.
           05 WS-READ-CNT           PIC 9(9) VALUE 0.
           05 WS-SKIP-CNT           PIC 9(9) VALUE 0.
           05 WS-WRITE-CNT          PIC 9(9) VALUE 0.
           05 WS-ERR-CNT            PIC 9(9) VALUE 0.

       01  WS-DATE-AREA.
           05 WS-CURRENT-TS         PIC X(21) VALUE SPACES.
           05 WS-BATCH-DT           PIC 9(8) VALUE 0.

       01  WS-CALC-AREA.
           05 WS-WORK-ACCT-ID       PIC X(20) VALUE SPACES.
           05 WS-WORK-RATE-CD       PIC X(10) VALUE SPACES.
           05 WS-BASE-GROUP         PIC X(04) VALUE SPACES.
           05 WS-BASE-DUE-AMT       PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-BASE-DAYS          PIC S9(5) COMP-3 VALUE 0.
           05 WS-APR                PIC S9(3)V9(7) COMP-3 VALUE 0.
           05 WS-STD-FALLBACK-APR   PIC S9(3)V9(7) COMP-3
                                     VALUE 0.0150.
           05 WS-ZERO-APR           PIC S9(3)V9(7) COMP-3 VALUE 0.
           05 WS-DAY-RATE           PIC S9(3)V9(12) COMP-3 VALUE 0.
           05 WS-PENALTY-RAW        PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-PENALTY-AMT        PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-ROUND-MODE         PIC X(02) VALUE SPACES.
           05 WS-TRAN-CD            PIC X(04) VALUE "DLYP".

       01  WS-DISPLAY-AREA.
           05 WS-DISP-READ          PIC Z(9).
           05 WS-DISP-WRITE         PIC Z(9).
           05 WS-DISP-SKIP          PIC Z(9).
           05 WS-DISP-ERR           PIC Z(9).

       01  LK-KP211S-PARM.
           05 LK-PENALTY-RATE-CD    PIC X(10).
           05 LK-BASE-DT            PIC 9(8).
           05 LK-APR                PIC S9(3)V9(7) COMP-3.
           05 LK-ROUND-MODE         PIC X(02).
           05 LK-ABEND-KBN          PIC X.
           05 LK-FILE-STATUS        PIC XX.

           COPY LK-RND-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS UNTIL WS-EOF OR WS-HARD-ERR
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           SET WS-NOT-EOF TO TRUE
           SET WS-NO-HARD-ERR TO TRUE
           SET WS-NO-SKIP-REC TO TRUE

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TS
           MOVE WS-CURRENT-TS(1:8) TO WS-BATCH-DT

           OPEN INPUT KPDELINF
           IF WS-KPDELINF-ST NOT = "00"
              DISPLAY "KPDELINF OPEN ERROR ST=" WS-KPDELINF-ST
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT KPPENRF
           IF WS-KPPENRF-ST NOT = "00"
              DISPLAY "KPPENRF OPEN ERROR ST=" WS-KPPENRF-ST
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZACCTF
           IF WS-KZACCTF-ST NOT = "00"
              DISPLAY "KZACCTF OPEN ERROR ST=" WS-KZACCTF-ST
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN EXTEND KPPENHF
           IF WS-KPPENHF-ST NOT = "00"
              DISPLAY "KPPENHF OPEN ERROR ST=" WS-KPPENHF-ST
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT KZTRANF
           IF WS-KZTRANF-ST NOT = "00"
              DISPLAY "KZTRANF OPEN ERROR ST=" WS-KZTRANF-ST
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           PERFORM 2100-READ-DELIN.

       2000-PROCESS.
           SET WS-NO-SKIP-REC TO TRUE
           ADD 1 TO WS-READ-CNT

           PERFORM 3000-VALIDATE-DELIN

           IF WS-NO-HARD-ERR AND WS-NO-SKIP-REC
              PERFORM 4000-READ-ACCOUNT
           END-IF
           IF WS-NO-HARD-ERR AND WS-NO-SKIP-REC
              PERFORM 5000-GET-PENALTY-RATE
           END-IF
           IF WS-NO-HARD-ERR AND WS-NO-SKIP-REC
              PERFORM 6000-CALCULATE-PENALTY
           END-IF
           IF WS-NO-HARD-ERR AND WS-NO-SKIP-REC
              PERFORM 7000-WRITE-OUTPUTS
           END-IF
           IF WS-NO-HARD-ERR
              PERFORM 2100-READ-DELIN
           END-IF.

       2100-READ-DELIN.
           READ KPDELINF NEXT RECORD
              AT END
                 SET WS-EOF TO TRUE
              NOT AT END
                 IF WS-KPDELINF-ST NOT = "00"
                    DISPLAY "KPDELINF READ ERROR ST="
                            WS-KPDELINF-ST
                    PERFORM 9100-SET-HARD-ERROR
                 END-IF
           END-READ.

       3000-VALIDATE-DELIN.
           MOVE DL-ACCT-ID TO WS-WORK-ACCT-ID

           IF DL-ACCT-ID = SPACES
              DISPLAY "DELIN ACCT-ID IS BLANK"
              ADD 1 TO WS-ERR-CNT
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           IF DL-DAYS-PAST-DUE <= 0
              DISPLAY "DELIN DAYS NOT TARGET ACCT="
                      WS-WORK-ACCT-ID
              ADD 1 TO WS-SKIP-CNT
              SET WS-SKIP-REC TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF DL-DUE-AMT <= 0
              DISPLAY "DELIN AMOUNT NOT TARGET ACCT="
                      WS-WORK-ACCT-ID
              ADD 1 TO WS-SKIP-CNT
              SET WS-SKIP-REC TO TRUE
              EXIT PARAGRAPH
           END-IF.

       4000-READ-ACCOUNT.
           MOVE WS-WORK-ACCT-ID TO AC-ACCT-ID
           READ KZACCTF
              INVALID KEY
                 DISPLAY "KZACCTF ACCT NOT FOUND ACCT="
                         WS-WORK-ACCT-ID
                 ADD 1 TO WS-ERR-CNT
                 PERFORM 9100-SET-HARD-ERROR
              NOT INVALID KEY
                 IF WS-KZACCTF-ST NOT = "00"
                    DISPLAY "KZACCTF READ ERROR ST="
                            WS-KZACCTF-ST
                    ADD 1 TO WS-ERR-CNT
                    PERFORM 9100-SET-HARD-ERROR
                 END-IF
           END-READ

           IF WS-HARD-ERR
              EXIT PARAGRAPH
           END-IF

           EVALUATE AC-GROUP-CODE
              WHEN "STD0"
              WHEN "GLD1"
              WHEN "PLT2"
              WHEN "EXMP"
                 MOVE AC-GROUP-CODE TO WS-BASE-GROUP
              WHEN "PREM"
                 MOVE AC-GROUP-CODE TO WS-BASE-GROUP
              WHEN OTHER
                 DISPLAY "INVALID GROUP CODE ACCT="
                         WS-WORK-ACCT-ID
                 ADD 1 TO WS-ERR-CNT
                 PERFORM 9100-SET-HARD-ERROR
           END-EVALUATE

           IF WS-HARD-ERR
              EXIT PARAGRAPH
           END-IF

           IF AC-KYC-STATUS NOT = "OK"
              DISPLAY "INVALID KYC STATUS ACCT="
                      WS-WORK-ACCT-ID
              ADD 1 TO WS-ERR-CNT
              PERFORM 9100-SET-HARD-ERROR
           END-IF.

       5000-GET-PENALTY-RATE.
           MOVE WS-BASE-GROUP TO WS-WORK-RATE-CD

           IF WS-BASE-GROUP = "EXMP"
              MOVE WS-ZERO-APR TO WS-APR
              MOVE "00" TO WS-ROUND-MODE
              EXIT PARAGRAPH
           END-IF

           MOVE WS-WORK-RATE-CD TO PR-PENALTY-RATE-CD
           READ KPPENRF
              INVALID KEY
                 DISPLAY "KPPENRF RATE NOT FOUND ACCT="
                         WS-WORK-ACCT-ID
                 ADD 1 TO WS-ERR-CNT
                 PERFORM 9100-SET-HARD-ERROR
              NOT INVALID KEY
                 IF WS-KPPENRF-ST NOT = "00"
                    DISPLAY "KPPENRF READ ERROR ST="
                            WS-KPPENRF-ST
                    ADD 1 TO WS-ERR-CNT
                    PERFORM 9100-SET-HARD-ERROR
                 END-IF
           END-READ

           IF WS-HARD-ERR
              EXIT PARAGRAPH
           END-IF

           MOVE WS-WORK-RATE-CD TO LK-PENALTY-RATE-CD
           MOVE WS-BATCH-DT     TO LK-BASE-DT
           MOVE 0               TO LK-APR
           MOVE SPACES          TO LK-ROUND-MODE
           MOVE SPACE           TO LK-ABEND-KBN
           MOVE SPACES          TO LK-FILE-STATUS

           CALL "KP211S" USING LK-KP211S-PARM
           END-CALL

           IF LK-ABEND-KBN NOT = SPACE
              DISPLAY "KP211S RATE ERROR ACCT="
                      WS-WORK-ACCT-ID
                      " ST="
                      LK-FILE-STATUS
              ADD 1 TO WS-ERR-CNT
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           IF LK-APR < 0
              DISPLAY "KP211S INVALID APR ACCT="
                      WS-WORK-ACCT-ID
              ADD 1 TO WS-ERR-CNT
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           IF WS-BASE-GROUP = "STD0" AND LK-APR = 0
              MOVE WS-STD-FALLBACK-APR TO WS-APR
              DISPLAY "STD FALLBACK APR APPLIED ACCT="
                      WS-WORK-ACCT-ID
           ELSE
              MOVE LK-APR TO WS-APR
           END-IF

           MOVE LK-ROUND-MODE TO WS-ROUND-MODE.

       6000-CALCULATE-PENALTY.
           MOVE DL-DUE-AMT TO WS-BASE-DUE-AMT
           MOVE DL-DAYS-PAST-DUE TO WS-BASE-DAYS
           MOVE 0 TO WS-PENALTY-RAW
           MOVE 0 TO WS-PENALTY-AMT

           COMPUTE WS-DAY-RATE ROUNDED = WS-APR / 365
           COMPUTE WS-PENALTY-RAW ROUNDED =
                   WS-BASE-DUE-AMT
                 * WS-DAY-RATE
                 * WS-BASE-DAYS

           EVALUATE WS-ROUND-MODE
              WHEN "00"
                 MOVE WS-PENALTY-RAW TO WS-PENALTY-AMT
              WHEN "FL"
                 MOVE WS-PENALTY-RAW TO LK-AMT-RAW
                 CALL "KZ130S" USING LK-RND-PARM
                 END-CALL
                 MOVE LK-AMT-FLOORED TO WS-PENALTY-AMT
              WHEN "01"
              WHEN "RN"
                 ADD 0.50 TO WS-PENALTY-RAW
                 MOVE WS-PENALTY-RAW TO LK-AMT-RAW
                 CALL "KZ130S" USING LK-RND-PARM
                 END-CALL
                 MOVE LK-AMT-FLOORED TO WS-PENALTY-AMT
              WHEN OTHER
                 DISPLAY "INVALID ROUND MODE ACCT="
                         WS-WORK-ACCT-ID
                 ADD 1 TO WS-ERR-CNT
                 PERFORM 9100-SET-HARD-ERROR
           END-EVALUATE

           IF WS-HARD-ERR
              EXIT PARAGRAPH
           END-IF

           IF WS-PENALTY-AMT < 0
              DISPLAY "INVALID PENALTY AMOUNT ACCT="
                      WS-WORK-ACCT-ID
              ADD 1 TO WS-ERR-CNT
              PERFORM 9100-SET-HARD-ERROR
           END-IF.

       7000-WRITE-OUTPUTS.
           MOVE SPACES TO KPPENHF-REC
           MOVE WS-WORK-ACCT-ID TO PH-ACCT-ID
           MOVE WS-BATCH-DT TO PH-PENALTY-DT
           MOVE WS-PENALTY-AMT TO PH-PENALTY-AMT
           MOVE DL-DAYS-PAST-DUE TO PH-DAYS-PAST-DUE
           MOVE WS-WORK-RATE-CD TO PH-RATE-CD

           WRITE KPPENHF-REC
           IF WS-KPPENHF-ST NOT = "00"
              DISPLAY "KPPENHF WRITE ERROR ST="
                      WS-KPPENHF-ST
              ADD 1 TO WS-ERR-CNT
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO KZTRANF-REC
           MOVE WS-WORK-ACCT-ID TO TR-ACCT-ID
           MOVE WS-TRAN-CD TO TR-TRAN-CD
           MOVE WS-PENALTY-AMT TO TR-TRAN-AMT

           WRITE KZTRANF-REC
           IF WS-KZTRANF-ST NOT = "00"
              DISPLAY "KZTRANF WRITE ERROR ST="
                      WS-KZTRANF-ST
              ADD 1 TO WS-ERR-CNT
              PERFORM 9100-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-WRITE-CNT.

       9000-FINALIZE.
           IF WS-KPDELINF-ST NOT = SPACES
              CLOSE KPDELINF
           END-IF
           IF WS-KPPENRF-ST NOT = SPACES
              CLOSE KPPENRF
           END-IF
           IF WS-KZACCTF-ST NOT = SPACES
              CLOSE KZACCTF
           END-IF
           IF WS-KPPENHF-ST NOT = SPACES
              CLOSE KPPENHF
           END-IF
           IF WS-KZTRANF-ST NOT = SPACES
              CLOSE KZTRANF
           END-IF

           MOVE WS-READ-CNT TO WS-DISP-READ
           MOVE WS-WRITE-CNT TO WS-DISP-WRITE
           MOVE WS-SKIP-CNT TO WS-DISP-SKIP
           MOVE WS-ERR-CNT TO WS-DISP-ERR

           DISPLAY "KP210B END READ=" WS-DISP-READ
                   " WRITE=" WS-DISP-WRITE
                   " SKIP=" WS-DISP-SKIP
                   " ERROR=" WS-DISP-ERR

           IF WS-HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.

       9100-SET-HARD-ERROR.
           SET WS-HARD-ERR TO TRUE
           MOVE 8 TO RETURN-CODE.
