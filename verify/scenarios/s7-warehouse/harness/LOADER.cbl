       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S7 harness: load fixed-width text fixture into KZFEEHF (sequential)
      * for the DWH extract engine.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FEE-TXT ASSIGN TO "fee.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  FEE-TXT.
       01  IF-REC.
           05  IF-ACCT     PIC X(16).
           05  IF-CYCLE    PIC 9(08).
           05  IF-FEE      PIC 9(11)V99.
           05  IF-YTD      PIC 9(11)V99.
           05  IF-CONFIRM  PIC X(01).
       FD  KZFEEHF.
       COPY KZFEEHFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZFEEHF
           OPEN INPUT FEE-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ FEE-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZFEEHF-REC
                   MOVE IF-ACCT    TO FH-ACCT-NO
                   MOVE IF-CYCLE   TO FH-CYCLE-DT
                   MOVE IF-FEE     TO FH-FEE-AMT
                   MOVE IF-YTD     TO FH-FEE-YTD
                   MOVE IF-CONFIRM TO FH-CONFIRM-FLAG
                   WRITE KZFEEHF-REC
               END-READ
           END-PERFORM
           CLOSE FEE-TXT KZFEEHF
           DISPLAY "LOADER DONE"
           STOP RUN.
