       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPINT.
      *================================================================
      * Read KZINTF (the accrual postings KZ410B wrote) and DISPLAY each:
      * INT|acct|amt|days  (grader parses acct + amt). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZINTF ASSIGN TO "KZINTF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZINTF.
       COPY KZINTFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       01  WS-AMT-Z  PIC -(13)9.
       01  WS-DAY-Z  PIC ZZZZ9.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT KZINTF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPINT OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ KZINTF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE IN-INT-AMT  TO WS-AMT-Z
                   MOVE IN-ACCR-DAYS TO WS-DAY-Z
                   DISPLAY "INT|" FUNCTION TRIM(IN-ACCT-NO)
                       "|" FUNCTION TRIM(WS-AMT-Z)
                       "|" FUNCTION TRIM(WS-DAY-Z)
               END-READ
           END-PERFORM
           CLOSE KZINTF
           STOP RUN.
