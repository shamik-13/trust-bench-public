       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPSEG.
      *================================================================
      * Read JHSEGRF (segment-assignment result) and DISPLAY each as:
      * SEG|cust|seg-cd  (grader parses these). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHSEGRF ASSIGN TO "JHSEGRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  JHSEGRF.
       COPY JHSEGRFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT JHSEGRF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPSEG OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ JHSEGRF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   DISPLAY "SEG|" FUNCTION TRIM(SR-CUST-ID)
                           "|" FUNCTION TRIM(SR-SEG-CD)
               END-READ
           END-PERFORM
           CLOSE JHSEGRF
           STOP RUN.
