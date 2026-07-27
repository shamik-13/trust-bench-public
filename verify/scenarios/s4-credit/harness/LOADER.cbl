       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S4 harness: load fixed-width text fixtures into KZACCTF + KZCUSTF
      * (indexed) and KZEXPF (sequential) for the credit engines.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-TXT ASSIGN TO "acct.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUST-TXT ASSIGN TO "cust.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EXP-TXT ASSIGN TO "exp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO FILE STATUS IS WS-ST.
           SELECT KZCUSTF ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS CU-CUST-ID FILE STATUS IS WS-ST.
           SELECT KZEXPF ASSIGN TO "KZEXPF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-TXT.
       01  IA-REC.
           05  IA-ACCT-NO    PIC X(16).
           05  IA-CUST-ID    PIC X(10).
           05  IA-ACCT-TYPE  PIC X(02).
           05  IA-GROUP      PIC X(04).
           05  IA-CUR-BAL    PIC 9(11)V99.
           05  IA-AVG-BAL    PIC 9(11)V99.
           05  IA-LIMIT      PIC 9(09)V99.
       FD  CUST-TXT.
       01  IC-REC.
           05  IC-CUST-ID    PIC X(10).
           05  IC-NAME       PIC X(40).
           05  IC-BRANCH     PIC X(04).
           05  IC-KYC        PIC X(01).
       FD  EXP-TXT.
       01  IE-REC.
           05  IE-CUST-ID    PIC X(10).
           05  IE-PRODUCT    PIC X(02).
           05  IE-EXPOSURE   PIC 9(11)V99.
       FD  KZACCTF.
       COPY KZACCTC4.
       FD  KZCUSTF.
       COPY KZCUSTC.
       FD  KZEXPF.
       COPY KZEXPFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZACCTF KZCUSTF KZEXPF
           PERFORM 100-LOAD-ACCT
           PERFORM 200-LOAD-CUST
           PERFORM 300-LOAD-EXP
           CLOSE KZACCTF KZCUSTF KZEXPF
           DISPLAY "LOADER DONE"
           STOP RUN.
       100-LOAD-ACCT.
           OPEN INPUT ACCT-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCT-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZACCTF-REC
                   MOVE IA-ACCT-NO   TO AC-ACCT-NO
                   MOVE IA-CUST-ID   TO AC-CUST-ID
                   MOVE IA-ACCT-TYPE TO AC-ACCT-TYPE
                   MOVE IA-GROUP     TO AC-GROUP-CODE
                   MOVE IA-CUR-BAL   TO AC-CUR-BAL
                   MOVE IA-AVG-BAL   TO AC-AVG-BAL
                   MOVE IA-LIMIT     TO AC-CREDIT-LIMIT
                   WRITE KZACCTF-REC
               END-READ
           END-PERFORM
           CLOSE ACCT-TXT.
       200-LOAD-CUST.
           OPEN INPUT CUST-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUST-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZCUSTF-REC
                   MOVE IC-CUST-ID TO CU-CUST-ID
                   MOVE IC-NAME    TO CU-CUST-NAME
                   MOVE IC-BRANCH  TO CU-BRANCH-CODE
                   MOVE IC-KYC     TO CU-KYC-STATUS
                   WRITE KZCUSTF-REC
               END-READ
           END-PERFORM
           CLOSE CUST-TXT.
       300-LOAD-EXP.
           OPEN INPUT EXP-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ EXP-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZEXPF-REC
                   MOVE IE-CUST-ID TO EX-CUST-ID
                   MOVE IE-PRODUCT TO EX-PRODUCT-TYPE
                   MOVE IE-EXPOSURE TO EX-EXPOSURE-AMT
                   WRITE KZEXPF-REC
               END-READ
           END-PERFORM
           CLOSE EXP-TXT.
