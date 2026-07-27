       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * s28 harness: load fixed-width text fixture into the real LFPOLF
      * (sequential) policy-master file the premium engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POL-TXT ASSIGN TO "pol.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  POL-TXT.
       01  IP-REC.
           05  IP-POL-NO      PIC X(16).
           05  IP-AGE         PIC 9(08).
           05  IP-SEX         PIC X(02).
           05  IP-SUM         PIC 9(11)V99.
           05  IP-STATUS      PIC X(02).
       FD  LFPOLF.
       COPY LFPOLFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT LFPOLF
           OPEN INPUT POL-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ POL-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE LFPOLF-REC
                   MOVE IP-POL-NO  TO PO-POL-NO
                   MOVE IP-AGE     TO PO-ENTRY-AGE-CNT
                   MOVE IP-SEX     TO PO-SEX-KBN
                   MOVE IP-SUM     TO PO-SUM-ASSURED-AMT
                   MOVE IP-STATUS  TO PO-POL-STATUS-KBN
                   WRITE LFPOLF-REC
               END-READ
           END-PERFORM
           CLOSE POL-TXT LFPOLF
           DISPLAY "LOADER DONE"
           STOP RUN.
