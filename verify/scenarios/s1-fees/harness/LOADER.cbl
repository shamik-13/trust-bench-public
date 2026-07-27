       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * Test harness: load fixed-width DISPLAY text fixtures into the
      * real (COMP-3 / indexed) KZ* files the fee engine reads. NOT part
      * of the benchmark corpus — lives under verify/harness/.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-TXT ASSIGN TO "acct.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CARD-TXT ASSIGN TO "card.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DGRP-TXT ASSIGN TO "dgrp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS AC-ACCT-ID FILE STATUS IS WS-ST.
           SELECT KZCARDF ASSIGN TO "KZCARDF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS CD-CARD-NO FILE STATUS IS WS-ST.
           SELECT KZDGRPF ASSIGN TO "KZDGRPF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS DG-GROUP-CODE FILE STATUS IS WS-ST.
           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-TXT.
       01  IA-REC.
           05  IA-ACCT-ID    PIC X(10).
           05  IA-CARD-NO    PIC X(16).
           05  IA-GROUP      PIC X(04).
           05  IA-CYCLE-BAL  PIC 9(11)V99.
           05  IA-CREDIT     PIC 9(11)V99.
           05  IA-OVER       PIC 9(11)V99.
           05  IA-KYC        PIC X(02).
       FD  CARD-TXT.
       01  IC-REC.
           05  IC-CARD-NO    PIC X(16).
           05  IC-ACCT-ID    PIC X(10).
           05  IC-STATUS     PIC X(02).
       FD  DGRP-TXT.
       01  ID-REC.
           05  ID-GROUP      PIC X(04).
           05  ID-RATE-NEW   PIC 9(01)V9(04).
           05  ID-RATE-OLD   PIC 9(01)V9(04).
           05  ID-EXEMPT     PIC X(01).
       FD  KZACCTF.
       COPY KZACCTC.
       FD  KZCARDF.
       COPY KZCARDC.
       FD  KZDGRPF.
       COPY KZDGRPC.
       FD  KZTRANF.
       COPY KZTRANC.
       WORKING-STORAGE SECTION.
       01  WS-ST    PIC X(02).
       01  WS-EOF   PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZACCTF KZCARDF KZDGRPF KZTRANF
           CLOSE KZTRANF
           PERFORM 100-LOAD-ACCT
           PERFORM 200-LOAD-CARD
           PERFORM 300-LOAD-DGRP
           CLOSE KZACCTF KZCARDF KZDGRPF
           DISPLAY "LOADER DONE"
           STOP RUN.
       100-LOAD-ACCT.
           OPEN INPUT ACCT-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCT-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZACCTF-REC
                   MOVE IA-ACCT-ID   TO AC-ACCT-ID
                   MOVE IA-CARD-NO   TO AC-CARD-NO
                   MOVE IA-GROUP     TO AC-GROUP-CODE
                   MOVE IA-CYCLE-BAL TO AC-CYCLE-BAL
                   MOVE IA-CREDIT    TO AC-CREDIT-LIMIT
                   MOVE IA-OVER      TO AC-OVER-AMT
                   MOVE IA-KYC       TO AC-KYC-STATUS
                   WRITE KZACCTF-REC
               END-READ
           END-PERFORM
           CLOSE ACCT-TXT.
       200-LOAD-CARD.
           OPEN INPUT CARD-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CARD-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZCARDF-REC
                   MOVE IC-CARD-NO TO CD-CARD-NO
                   MOVE IC-ACCT-ID TO CD-ACCT-ID
                   MOVE IC-STATUS  TO CD-CARD-STATUS
                   WRITE KZCARDF-REC
               END-READ
           END-PERFORM
           CLOSE CARD-TXT.
       300-LOAD-DGRP.
           OPEN INPUT DGRP-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ DGRP-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZDGRPF-REC
                   MOVE ID-GROUP    TO DG-GROUP-CODE
                   MOVE ID-RATE-NEW TO DG-OL-FEE-RATE
                   MOVE ID-RATE-OLD TO DG-OL-FEE-RATE-OLD
                   MOVE ID-EXEMPT   TO DG-EXEMPT-FLAG
                   WRITE KZDGRPF-REC
               END-READ
           END-PERFORM
           CLOSE DGRP-TXT.
