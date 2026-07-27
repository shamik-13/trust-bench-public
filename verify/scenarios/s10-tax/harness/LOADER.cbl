       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S10 harness: load fixed-width text fixture into KZTXIF (sequential)
      * for the withholding-tax engine KZ620B.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TXI-TXT ASSIGN TO "txi.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KZTXIF ASSIGN TO "KZTXIF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  TXI-TXT.
       01  IT-REC.
           05  IT-ACCT     PIC X(16).
           05  IT-TYPE     PIC X(02).
           05  IT-INT      PIC 9(11)V99.
       FD  KZTXIF.
       COPY KZTXIFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT KZTXIF
           OPEN INPUT TXI-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ TXI-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE KZTXIF-REC
                   MOVE IT-ACCT TO TI-ACCT-NO
                   MOVE IT-TYPE TO TI-ACCT-TYPE
                   MOVE IT-INT  TO TI-INT-AMT
                   WRITE KZTXIF-REC
               END-READ
           END-PERFORM
           CLOSE TXI-TXT KZTXIF
           DISPLAY "LOADER DONE"
           STOP RUN.
