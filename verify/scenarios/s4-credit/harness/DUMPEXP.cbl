       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPEXP.
      *================================================================
      * Read KZEXPRF (exposure aggregation result) and DISPLAY each as:
      * EXP|cust|product|over-flag|capped-amt  (grader parses these).
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZEXPRF ASSIGN TO "KZEXPRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZEXPRF.
       COPY KZEXPRC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       01  WS-CAP-DISP PIC 9(11).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT KZEXPRF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPEXP OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ KZEXPRF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE XR-CAPPED-AMT TO WS-CAP-DISP
                   DISPLAY "EXP|" FUNCTION TRIM(XR-CUST-ID)
                           "|" FUNCTION TRIM(XR-PRODUCT-TYPE)
                           "|" FUNCTION TRIM(XR-OVER-FLAG)
                           "|" FUNCTION TRIM(WS-CAP-DISP)
               END-READ
           END-PERFORM
           CLOSE KZEXPRF
           STOP RUN.
