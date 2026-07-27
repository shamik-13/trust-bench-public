       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S8 harness: load fixed-width text fixtures into KZDBALF + KZCYRF
      * (both sequential) for the interest-accrual engine KZ410B.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DBAL-TXT ASSIGN TO "dbal.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CYR-TXT ASSIGN TO "cyr.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZDBALF ASSIGN TO "KZDBALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST2.
       DATA DIVISION.
       FILE SECTION.
       FD  DBAL-TXT.
       01  ID-REC.
           05  ID-ACCT      PIC X(16).
           05  ID-CYCLE     PIC X(10).
           05  ID-BAL       PIC 9(11)V99.
           05  ID-RATE      PIC 9(01)V9(04).
           05  ID-START     PIC 9(08).
       FD  CYR-TXT.
       01  IC-REC.
           05  IC-CYCLE     PIC X(10).
           05  IC-NOMINAL   PIC 9(08).
           05  IC-RESOLVED  PIC 9(08).
           05  IC-ROLLED    PIC X(01).
       FD  KZDBALF.
       COPY KZDBALFC.
       FD  KZCYRF.
       COPY KZCYRFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-ST2  PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZDBALF
           OPEN INPUT DBAL-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ DBAL-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZDBALF-REC
                   MOVE ID-ACCT  TO DB-ACCT-NO
                   MOVE ID-CYCLE TO DB-CYCLE-ID
                   MOVE ID-BAL   TO DB-AVG-DAILY-BAL
                   MOVE ID-RATE  TO DB-INT-RATE
                   MOVE ID-START TO DB-PERIOD-START-DT
                   WRITE KZDBALF-REC
               END-READ
           END-PERFORM
           CLOSE DBAL-TXT KZDBALF

           OPEN OUTPUT KZCYRF
           OPEN INPUT CYR-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CYR-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZCYRF-REC
                   MOVE IC-CYCLE    TO CR-CYCLE-ID
                   MOVE IC-NOMINAL  TO CR-NOMINAL-DT
                   MOVE IC-RESOLVED TO CR-RESOLVED-DT
                   MOVE IC-ROLLED   TO CR-ROLLED-FLAG
                   WRITE KZCYRF-REC
               END-READ
           END-PERFORM
           CLOSE CYR-TXT KZCYRF
           DISPLAY "LOADER DONE"
           STOP RUN.
