       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPTXR.
      *================================================================
      * Read KZTXRF (the withholding result KZ620B wrote) and DISPLAY each:
      * TXR|acct|total-tax|national|local|net  (grader parses acct +
      * total-tax). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZTXRF.
       COPY KZTXRFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       01  WS-TOT-Z  PIC -(13)9.
       01  WS-NAT-Z  PIC -(13)9.
       01  WS-LOC-Z  PIC -(13)9.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT KZTXRF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPTXR OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ KZTXRF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE TR-TOTAL-TAX-AMT    TO WS-TOT-Z
                   MOVE TR-NATIONAL-TAX-AMT TO WS-NAT-Z
                   MOVE TR-LOCAL-TAX-AMT    TO WS-LOC-Z
                   DISPLAY "TXR|" FUNCTION TRIM(TR-ACCT-NO)
                       "|" FUNCTION TRIM(WS-TOT-Z)
                       "|" FUNCTION TRIM(WS-NAT-Z)
                       "|" FUNCTION TRIM(WS-LOC-Z)
               END-READ
           END-PERFORM
           CLOSE KZTXRF
           STOP RUN.
