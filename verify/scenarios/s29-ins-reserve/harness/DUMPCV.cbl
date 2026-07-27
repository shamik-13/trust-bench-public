       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPCV.
      *================================================================
      * Read LFCVRF (the surrender-value file the engine wrote) and
      * DISPLAY each record as: CVAL|pol-no|cv-yen|calc-status
      * (grader parses these). cv shown as integer yen. Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCVRF ASSIGN TO "LFCVRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  LFCVRF.
       COPY LFCVRFC.
       WORKING-STORAGE SECTION.
       01  WS-ST       PIC X(02).
       01  WS-EOF      PIC X(01) VALUE 'N'.
       01  WS-CV-YEN   PIC 9(13).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT LFCVRF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPCV OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ LFCVRF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE CO-CV-AMT TO WS-CV-YEN
                   DISPLAY "CVAL|" FUNCTION TRIM(CO-POL-NO)
                           "|" WS-CV-YEN
                           "|" FUNCTION TRIM(CO-CALC-STATUS-KBN)
               END-READ
           END-PERFORM
           CLOSE LFCVRF
           STOP RUN.
