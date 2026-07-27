       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPDLR.
      *================================================================
      * Read KZDLRF (the assessment result KZ510B wrote) and DISPLAY each:
      * DLR|acct|late-charge|status|days|bucket  (grader parses acct,
      * late-charge, status). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLRF ASSIGN TO "KZDLRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZDLRF.
       COPY KZDLRFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       01  WS-AMT-Z  PIC -(13)9.
       01  WS-DAY-Z  PIC ZZZZ9.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT KZDLRF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPDLR OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ KZDLRF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE DR-LATE-CHARGE-AMT TO WS-AMT-Z
                   MOVE DR-DAYS-OVERDUE    TO WS-DAY-Z
                   DISPLAY "DLR|" FUNCTION TRIM(DR-ACCT-NO)
                       "|" FUNCTION TRIM(WS-AMT-Z)
                       "|" DR-NEW-STATUS
                       "|" FUNCTION TRIM(WS-DAY-Z)
                       "|" DR-AGING-BUCKET
               END-READ
           END-PERFORM
           CLOSE KZDLRF
           STOP RUN.
