       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S3 harness: load fixed-width text fixtures into the real TGZENF
      * (sequential) + TGNETCF (indexed) files the 交換尻 engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ZEN-TXT ASSIGN TO "zen.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT NETC-TXT ASSIGN TO "netc.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TGZENF ASSIGN TO "TGZENF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
           SELECT TGNETCF ASSIGN TO "TGNETCF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS NC-COUNTER-BANK FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  ZEN-TXT.
       01  IZ-REC.
           05  IZ-VALUE-DT   PIC 9(08).
           05  IZ-CENTER-SEQ PIC X(10).
           05  IZ-ZEN-TYPE   PIC X(02).
           05  IZ-CBANK      PIC X(04).
           05  IZ-CBRANCH    PIC X(04).
           05  IZ-RECV-ACCT  PIC X(16).
           05  IZ-AMT        PIC 9(11)V99.
           05  IZ-NAME       PIC X(40).
       FD  NETC-TXT.
       01  IN-REC.
           05  IN-CBANK      PIC X(04).
           05  IN-OUT-FLAG   PIC X(01).
           05  IN-IN-FLAG    PIC X(01).
           05  IN-CTL-DT     PIC 9(08).
       FD  TGZENF.
       COPY TGZENFC.
       FD  TGNETCF.
       COPY TGNETCFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT TGZENF TGNETCF
           PERFORM 100-LOAD-ZEN
           PERFORM 200-LOAD-NETC
           CLOSE TGZENF TGNETCF
           DISPLAY "LOADER DONE"
           STOP RUN.
       100-LOAD-ZEN.
           OPEN INPUT ZEN-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ ZEN-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE TGZENF-REC
                   MOVE IZ-VALUE-DT   TO ZE-VALUE-DT
                   MOVE IZ-CENTER-SEQ TO ZE-CENTER-SEQ
                   MOVE IZ-ZEN-TYPE   TO ZE-ZEN-TYPE
                   MOVE IZ-CBANK      TO ZE-COUNTER-BANK
                   MOVE IZ-CBRANCH    TO ZE-COUNTER-BRANCH
                   MOVE IZ-RECV-ACCT  TO ZE-RECV-ACCT-NO
                   MOVE IZ-AMT        TO ZE-REMIT-AMT
                   MOVE IZ-NAME       TO ZE-REMIT-NAME-KANA
                   WRITE TGZENF-REC
               END-READ
           END-PERFORM
           CLOSE ZEN-TXT.
       200-LOAD-NETC.
           OPEN INPUT NETC-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ NETC-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE TGNETCF-REC
                   MOVE IN-CBANK    TO NC-COUNTER-BANK
                   MOVE IN-OUT-FLAG TO NC-OUT-FLAG
                   MOVE IN-IN-FLAG  TO NC-IN-FLAG
                   MOVE IN-CTL-DT   TO NC-CTL-DT
                   WRITE TGNETCF-REC
               END-READ
           END-PERFORM
           CLOSE NETC-TXT.
