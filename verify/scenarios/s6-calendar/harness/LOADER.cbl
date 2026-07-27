       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S6 harness: load fixed-width text fixtures into KZCYCF (sequential)
      * + KZCALF (indexed) for the cycle-boundary engine.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAL-TXT ASSIGN TO "cal.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CYC-TXT ASSIGN TO "cyc.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS CA-CAL-DT FILE STATUS IS WS-ST.
           SELECT KZCYCF ASSIGN TO "KZCYCF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CAL-TXT.
       01  ICA-REC.
           05  ICA-DT       PIC 9(08).
           05  ICA-FLAG     PIC X(01).
       FD  CYC-TXT.
       01  ICY-REC.
           05  ICY-ID       PIC X(10).
           05  ICY-DT       PIC 9(08).
       FD  KZCALF.
       COPY KZCALFC.
       FD  KZCYCF.
       COPY KZCYCFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZCALF KZCYCF
           PERFORM 100-LOAD-CAL
           PERFORM 200-LOAD-CYC
           CLOSE KZCALF KZCYCF
           DISPLAY "LOADER DONE"
           STOP RUN.
       100-LOAD-CAL.
           OPEN INPUT CAL-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CAL-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZCALF-REC
                   MOVE ICA-DT   TO CA-CAL-DT
                   MOVE ICA-FLAG TO CA-HOLIDAY-FLAG
                   WRITE KZCALF-REC
               END-READ
           END-PERFORM
           CLOSE CAL-TXT.
       200-LOAD-CYC.
           OPEN INPUT CYC-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CYC-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZCYCF-REC
                   MOVE ICY-ID TO CY-CYCLE-ID
                   MOVE ICY-DT TO CY-NOMINAL-DT
                   WRITE KZCYCF-REC
               END-READ
           END-PERFORM
           CLOSE CYC-TXT.
