       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * s29 harness: load fixed-width text fixture into the real LFCVPF
      * (sequential) surrender-input file the engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CV-TXT ASSIGN TO "cv.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LFCVPF ASSIGN TO "LFCVPF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CV-TXT.
       01  IC-REC.
           05  IC-POL-NO      PIC X(16).
           05  IC-RESERVE     PIC 9(11)V99.
           05  IC-COST        PIC 9(11)V99.
           05  IC-ELAPSED     PIC 9(08).
           05  IC-STATUS      PIC X(02).
       FD  LFCVPF.
       COPY LFCVPFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT LFCVPF
           OPEN INPUT CV-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CV-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE LFCVPF-REC
                   MOVE IC-POL-NO   TO CI-POL-NO
                   MOVE IC-RESERVE  TO CI-RESERVE-AMT
                   MOVE IC-COST     TO CI-NEWBIZ-COST-AMT
                   MOVE IC-ELAPSED  TO CI-ELAPSED-MONTH-CNT
                   MOVE IC-STATUS   TO CI-CV-STATUS-KBN
                   WRITE LFCVPF-REC
               END-READ
           END-PERFORM
           CLOSE CV-TXT LFCVPF
           DISPLAY "LOADER DONE"
           STOP RUN.
