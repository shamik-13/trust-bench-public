       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S9 harness: load fixed-width text fixture into KZDLQF (sequential)
      * for the delinquency-assessment engine KZ510B.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DLQ-TXT ASSIGN TO "dlq.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZDLQF ASSIGN TO "KZDLQF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  DLQ-TXT.
       01  IL-REC.
           05  IL-ACCT     PIC X(16).
           05  IL-AMT      PIC 9(11)V99.
           05  IL-DUE      PIC 9(08).
           05  IL-ASOF     PIC 9(08).
           05  IL-CURR     PIC X(02).
       FD  KZDLQF.
       COPY KZDLQFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZDLQF
           OPEN INPUT DLQ-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ DLQ-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZDLQF-REC
                   MOVE IL-ACCT TO DQ-ACCT-NO
                   MOVE IL-AMT  TO DQ-OVERDUE-AMT
                   MOVE IL-DUE  TO DQ-DUE-DT
                   MOVE IL-ASOF TO DQ-ASOF-DT
                   MOVE IL-CURR TO DQ-CURR-STATUS
                   WRITE KZDLQF-REC
               END-READ
           END-PERFORM
           CLOSE DLQ-TXT KZDLQF
           DISPLAY "LOADER DONE"
           STOP RUN.
