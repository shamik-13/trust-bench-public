       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OS-TXT ASSIGN TO "os.txt" ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAY-TXT ASSIGN TO "pay.txt" ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CDOSF ASSIGN TO "CDOSF" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS OS-CARD-NO FILE STATUS IS WS-ST.
           SELECT CDPAYF ASSIGN TO "CDPAYF" ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OS-TXT.
       01  IO-REC.
           05  IO-CARD-NO   PIC X(16).
           05  IO-FEE       PIC 9(11)V99.
           05  IO-INT       PIC 9(11)V99.
           05  IO-PRIN      PIC 9(11)V99.
           05  IO-CYCLE-DT  PIC 9(08).
       FD  PAY-TXT.
       01  IP-REC.
           05  IP-PAY-ID    PIC X(10).
           05  IP-CARD-NO   PIC X(16).
           05  IP-PAY-AMT   PIC 9(11)V99.
           05  IP-PAY-DT    PIC 9(08).
           05  IP-METHOD    PIC X(10).
       FD  CDOSF.
       COPY CDOSFC.
       FD  CDPAYF.
       COPY CDPAYFC.
       WORKING-STORAGE SECTION.
       01  WS-ST PIC X(02).
       01  WS-EOF PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT CDOSF CDPAYF
           PERFORM 100-LOAD-OS
           PERFORM 200-LOAD-PAY
           CLOSE CDOSF CDPAYF
           DISPLAY "LOADER DONE"
           STOP RUN.
       100-LOAD-OS.
           OPEN INPUT OS-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ OS-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CDOSF-REC
                   MOVE IO-CARD-NO  TO OS-CARD-NO
                   MOVE IO-FEE      TO OS-FEE-BAL-AMT
                   MOVE IO-INT      TO OS-INTEREST-BAL-AMT
                   MOVE IO-PRIN     TO OS-PRINCIPAL-BAL-AMT
                   MOVE IO-CYCLE-DT TO OS-CYCLE-DT
                   WRITE CDOSF-REC
               END-READ
           END-PERFORM
           CLOSE OS-TXT.
       200-LOAD-PAY.
           OPEN INPUT PAY-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ PAY-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CDPAYF-REC
                   MOVE IP-PAY-ID  TO PY-PAY-ID
                   MOVE IP-CARD-NO TO PY-CARD-NO
                   MOVE IP-PAY-AMT TO PY-PAY-AMT
                   MOVE IP-PAY-DT  TO PY-PAY-DT
                   MOVE IP-METHOD  TO PY-PAY-METHOD
                   WRITE CDPAYF-REC
               END-READ
           END-PERFORM
           CLOSE PAY-TXT.
