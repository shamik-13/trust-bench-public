       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPREJ.
      *================================================================
      * Read TGREJLF (the reject log the engine wrote) and DISPLAY each
      * record as: REJ|center-seq|reason  (grader parses these). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGREJLF ASSIGN TO "TGREJLF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  TGREJLF.
       COPY TGREJLFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT TGREJLF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPREJ OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ TGREJLF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   DISPLAY "REJ|" FUNCTION TRIM(RJ-CENTER-SEQ)
                           "|" FUNCTION TRIM(RJ-REJ-REASON)
               END-READ
           END-PERFORM
           CLOSE TGREJLF
           STOP RUN.
