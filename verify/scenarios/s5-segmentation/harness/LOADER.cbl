       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * S5 harness: load fixed-width text fixture into JHCBALF (sequential)
      * for the segment-assignment engine.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CBAL-TXT ASSIGN TO "cbal.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT JHCBALF ASSIGN TO "JHCBALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CBAL-TXT.
       01  IB-REC.
           05  IB-CUST-ID    PIC X(10).
           05  IB-AVG-BAL    PIC 9(11)V99.
       FD  JHCBALF.
       COPY JHCBALFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT JHCBALF
           OPEN INPUT CBAL-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CBAL-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE JHCBALF-REC
                   MOVE IB-CUST-ID TO CB-CUST-ID
                   MOVE IB-AVG-BAL TO CB-AVG-BAL
                   WRITE JHCBALF-REC
               END-READ
           END-PERFORM
           CLOSE CBAL-TXT JHCBALF
           DISPLAY "LOADER DONE"
           STOP RUN.
