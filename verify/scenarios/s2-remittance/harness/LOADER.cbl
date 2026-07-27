       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S2 harness: load fixed-width text fixtures into the real KZACCTF
      * (indexed) + TGINRMF (sequential) files the inbound engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-TXT ASSIGN TO "acct.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INRM-TXT ASSIGN TO "inrm.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO FILE STATUS IS WS-ST.
           SELECT TGINRMF ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-TXT.
       01  IA-REC.
           05  IA-ACCT-NO    PIC X(16).
           05  IA-BRANCH     PIC X(04).
           05  IA-ACCT-TYPE  PIC X(02).
           05  IA-CUST       PIC X(10).
           05  IA-NAME       PIC X(40).
           05  IA-STATUS     PIC X(02).
       FD  INRM-TXT.
       01  II-REC.
           05  II-REMIT-DT   PIC 9(08).
           05  II-CENTER-SEQ PIC X(10).
           05  II-TYPE       PIC X(02).
           05  II-SBANK      PIC X(04).
           05  II-SBR        PIC X(04).
           05  II-SNAME      PIC X(40).
           05  II-PBANK      PIC X(04).
           05  II-PBR        PIC X(04).
           05  II-PTYPE      PIC X(02).
           05  II-PACCT-NO   PIC X(16).
           05  II-PNAME      PIC X(40).
           05  II-AMT        PIC 9(11)V99.
           05  II-MSG        PIC X(20).
       FD  KZACCTF.
       COPY KZACCTC2.
       FD  TGINRMF.
       COPY TGINRMFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZACCTF TGINRMF
           PERFORM 100-LOAD-ACCT
           PERFORM 200-LOAD-INRM
           CLOSE KZACCTF TGINRMF
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
                   MOVE IA-BRANCH    TO AC-BRANCH
                   MOVE IA-ACCT-TYPE TO AC-ACCT-TYPE
                   MOVE IA-CUST      TO AC-CUSTOMER-ID
                   MOVE IA-NAME      TO AC-ACCT-NAME-KANA
                   MOVE IA-STATUS    TO AC-STATUS
                   WRITE KZACCTF-REC
               END-READ
           END-PERFORM
           CLOSE ACCT-TXT.
       200-LOAD-INRM.
           OPEN INPUT INRM-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ INRM-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE TGINRMF-REC
                   MOVE II-REMIT-DT   TO IR-REMIT-DT
                   MOVE II-CENTER-SEQ TO IR-CENTER-SEQ
                   MOVE II-TYPE       TO IR-REMIT-TYPE
                   MOVE II-SBANK      TO IR-SENDER-BANK
                   MOVE II-SBR        TO IR-SENDER-BRANCH
                   MOVE II-SNAME      TO IR-SENDER-NAME-KANA
                   MOVE II-PBANK      TO IR-PAYEE-BANK
                   MOVE II-PBR        TO IR-PAYEE-BRANCH
                   MOVE II-PTYPE      TO IR-PAYEE-ACCT-TYPE
                   MOVE II-PACCT-NO   TO IR-PAYEE-ACCT-NO
                   MOVE II-PNAME      TO IR-PAYEE-NAME-KANA
                   MOVE II-AMT        TO IR-REMIT-AMT
                   MOVE II-MSG        TO IR-REMIT-MSG
                   WRITE TGINRMF-REC
               END-READ
           END-PERFORM
           CLOSE INRM-TXT.
